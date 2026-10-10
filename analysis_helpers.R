hf_api_json <- function(url) {
  tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(error) {
      stop("Dataset API request failed: ", conditionMessage(error), call. = FALSE)
    }
  )
}

search_hf_datasets <- function(query, limit = 12) {
  query <- trimws(query)
  if (!nzchar(query)) stop("Enter a dataset topic to search.", call. = FALSE)
  endpoint <- sprintf(
    "https://huggingface.co/api/datasets?search=%s&limit=%d&sort=downloads&direction=-1",
    utils::URLencode(query, reserved = TRUE), as.integer(limit)
  )
  results <- hf_api_json(endpoint)
  if (!length(results)) return(data.frame())
  data.frame(
    id = vapply(results, function(item) item$id %||% "", character(1)),
    downloads = vapply(results, function(item) as.numeric(item$downloads %||% 0), numeric(1)),
    likes = vapply(results, function(item) as.numeric(item$likes %||% 0), numeric(1)),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(value, fallback) {
  if (is.null(value) || length(value) == 0) fallback else value
}

hf_rows_to_frame <- function(rows, columns) {
  if (!length(rows)) return(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)))
  values_by_column <- lapply(columns, function(column) {
    values <- lapply(rows, function(item) item$row[[column]])
    scalar_values <- vapply(values, function(value) {
      is.null(value) || (is.atomic(value) && length(value) <= 1)
    }, logical(1))
    if (!all(scalar_values)) {
      return(vapply(values, function(value) {
        if (is.null(value)) NA_character_ else as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
      }, character(1)))
    }
    json <- jsonlite::toJSON(values, auto_unbox = TRUE, null = "null", na = "null")
    result <- jsonlite::fromJSON(json, simplifyVector = TRUE)
    if (is.list(result) && !is.data.frame(result)) {
      vapply(result, function(value) if (is.null(value)) NA_character_ else as.character(value), character(1))
    } else {
      result
    }
  })
  names(values_by_column) <- make.names(columns, unique = TRUE)
  as.data.frame(values_by_column, check.names = FALSE, stringsAsFactors = FALSE)
}

fetch_hf_dataset <- function(dataset_id, max_rows = 5000) {
  encoded_id <- utils::URLencode(dataset_id, reserved = TRUE)
  split_data <- hf_api_json(paste0("https://datasets-server.huggingface.co/splits?dataset=", encoded_id))
  splits <- split_data$splits
  if (!length(splits)) {
    detail <- if (length(split_data$failed)) " The dataset server reported a failed configuration." else ""
    stop("No accessible dataset split was found for ", dataset_id, ".", detail, call. = FALSE)
  }
  selected <- splits[[1]]
  train_split <- which(vapply(splits, function(item) identical(item$split, "train"), logical(1)))
  if (length(train_split)) selected <- splits[[train_split[[1]]]]

  first_url <- sprintf(
    "https://datasets-server.huggingface.co/rows?dataset=%s&config=%s&split=%s&offset=0&length=100",
    encoded_id, utils::URLencode(selected$config, reserved = TRUE),
    utils::URLencode(selected$split, reserved = TRUE)
  )
  first_page <- hf_api_json(first_url)
  columns <- vapply(first_page$features, function(feature) feature$name, character(1))
  if (!length(columns)) stop("The selected dataset has no tabular features.", call. = FALSE)
  total_rows <- as.integer(first_page$num_rows_total %||% length(first_page$rows))
  row_limit <- min(as.integer(max_rows), total_rows)
  collected <- first_page$rows
  if (row_limit > length(collected)) {
    offsets <- seq(100, row_limit - 1, by = 100)
    for (offset in offsets) {
      page_length <- min(100, row_limit - offset)
      page_url <- sprintf(
        "https://datasets-server.huggingface.co/rows?dataset=%s&config=%s&split=%s&offset=%d&length=%d",
        encoded_id, utils::URLencode(selected$config, reserved = TRUE),
        utils::URLencode(selected$split, reserved = TRUE), offset, page_length
      )
      page <- hf_api_json(page_url)
      if (!length(page$rows)) break
      collected <- c(collected, page$rows)
    }
  }
  list(
    data = hf_rows_to_frame(collected, columns),
    source = sprintf("Hugging Face: %s (%s / %s; %s of %s rows)",
                     dataset_id, selected$config, selected$split,
                     length(collected), total_rows),
    total_rows = total_rows,
    rows_loaded = length(collected)
  )
}

read_uploaded_dataset <- function(path, filename) {
  extension <- tolower(tools::file_ext(filename))
  data <- switch(
    extension,
    csv = utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                          na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8"),
    xls = readxl::read_excel(path),
    xlsx = readxl::read_excel(path),
    stop("Upload a CSV, XLS, or XLSX file.", call. = FALSE)
  )
  as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
}

clean_dataset <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("The selected source contains no tabular rows or columns.", call. = FALSE)
  }
  original_rows <- nrow(data)
  names(data) <- make.names(trimws(names(data)), unique = TRUE)
  for (column in names(data)) {
    if (is.character(data[[column]])) {
      data[[column]] <- trimws(data[[column]])
      data[[column]][!nzchar(data[[column]])] <- NA_character_
    }
    if (is.numeric(data[[column]])) data[[column]][!is.finite(data[[column]])] <- NA_real_
    if (is.list(data[[column]]) && !is.data.frame(data[[column]])) {
      data[[column]] <- vapply(data[[column]], function(value) {
        if (is.null(value) || length(value) == 0) NA_character_
        else if (length(value) == 1 && is.atomic(value)) as.character(value)
        else as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
      }, character(1))
    }
  }
  data <- data[!duplicated(data), , drop = FALSE]
  all_missing <- vapply(data, function(column) all(is.na(column)), logical(1))
  dropped_columns <- names(data)[all_missing]
  if (any(all_missing)) data <- data[, !all_missing, drop = FALSE]
  if (ncol(data) == 0) stop("Every column in the selected source is empty.", call. = FALSE)
  list(
    data = data,
    rows_before = original_rows,
    duplicate_rows_removed = original_rows - nrow(data),
    empty_columns_removed = dropped_columns,
    missing_by_column = colSums(is.na(data))
  )
}

dataset_profile <- function(data) {
  rows <- lapply(names(data), function(column) {
    values <- data[[column]]
    missing <- sum(is.na(values))
    if (is.numeric(values) || is.integer(values)) {
      present <- values[!is.na(values)]
      data.frame(
        column = column, type = "numeric", missing = missing,
        distinct = length(unique(present)), summary = if (length(present)) {
          sprintf("mean %s; median %s; range %s to %s",
                  signif(mean(present), 5), signif(stats::median(present), 5),
                  signif(min(present), 5), signif(max(present), 5))
        } else "No observed values",
        stringsAsFactors = FALSE
      )
    } else {
      present <- as.character(values[!is.na(values)])
      counts <- sort(table(present), decreasing = TRUE)
      summary <- if (length(counts)) {
        paste(sprintf("%s (%d)", names(counts)[seq_len(min(3, length(counts)))],
                      as.integer(counts[seq_len(min(3, length(counts)))])), collapse = "; ")
      } else "No observed values"
      data.frame(column = column, type = "categorical", missing = missing,
                 distinct = length(unique(present)), summary = summary,
                 stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, rows)
}

mode_value <- function(values) {
  observed <- values[!is.na(values)]
  if (!length(observed)) return(NA_character_)
  names(sort(table(observed), decreasing = TRUE))[[1]]
}

prepare_model_data <- function(data, target) {
  if (is.null(target) || !target %in% names(data)) stop("Select a target column for model evaluation.", call. = FALSE)
  y <- data[[target]]
  classification <- is.character(y) || is.factor(y) || is.logical(y)
  if (classification) y <- droplevels(factor(y)) else y <- as.numeric(y)
  valid_target <- !is.na(y)
  data <- data[valid_target, , drop = FALSE]
  y <- y[valid_target]
  if (length(y) < 20) stop("At least 20 rows with a non-missing target are needed for a holdout evaluation.", call. = FALSE)
  if (classification) {
    y <- droplevels(y)
    class_counts <- table(y)
    if (length(class_counts) < 2 || any(class_counts < 2)) {
      stop("Classification needs at least two target classes, with at least two rows per class.", call. = FALSE)
    }
  }

  set.seed(2026)
  if (classification) {
    train_index <- unlist(lapply(levels(y), function(level) {
      members <- which(y == level)
      sample(members, size = max(1, min(length(members) - 1, floor(0.8 * length(members)))) )
    }), use.names = FALSE)
    test_index <- setdiff(seq_along(y), train_index)
  } else {
    train_index <- sample(seq_along(y), size = floor(0.8 * length(y)))
    test_index <- setdiff(seq_along(y), train_index)
  }
  feature_names <- setdiff(names(data), target)
  feature_names <- head(feature_names, 40)
  train_features <- data[train_index, feature_names, drop = FALSE]
  test_features <- data[test_index, feature_names, drop = FALSE]
  usable <- vapply(feature_names, function(column) {
    values <- data[[column]]
    if (is.numeric(values) || is.integer(values)) {
      any(!is.na(values)) && length(unique(values[!is.na(values)])) > 1
    } else {
      observed <- as.character(values[!is.na(values)])
      length(unique(observed)) > 1 && length(unique(observed)) <= 40
    }
  }, logical(1))
  feature_names <- feature_names[usable]
  if (!length(feature_names)) stop("No usable predictor columns remain after cleaning.", call. = FALSE)
  train_features <- train_features[, feature_names, drop = FALSE]
  test_features <- test_features[, feature_names, drop = FALSE]

  for (column in feature_names) {
    if (is.numeric(train_features[[column]]) || is.integer(train_features[[column]])) {
      replacement <- stats::median(train_features[[column]], na.rm = TRUE)
      if (!is.finite(replacement)) replacement <- 0
      train_features[[column]][is.na(train_features[[column]])] <- replacement
      test_features[[column]][is.na(test_features[[column]])] <- replacement
    } else {
      train_values <- as.character(train_features[[column]])
      test_values <- as.character(test_features[[column]])
      replacement <- mode_value(train_values)
      if (is.na(replacement)) replacement <- "(missing)"
      levels_seen <- unique(c(train_values, test_values, replacement))
      train_values[is.na(train_values)] <- replacement
      test_values[is.na(test_values)] <- replacement
      train_features[[column]] <- factor(train_values, levels = levels_seen)
      test_features[[column]] <- factor(test_values, levels = levels_seen)
    }
  }
  names(train_features) <- make.names(names(train_features), unique = TRUE)
  names(test_features) <- names(train_features)
  combined <- rbind(train_features, test_features)
  model_matrix <- stats::model.matrix(~ ., data = combined)
  model_matrix <- model_matrix[, colnames(model_matrix) != "(Intercept)", drop = FALSE]
  colnames(model_matrix) <- make.names(colnames(model_matrix), unique = TRUE)
  if (ncol(model_matrix) > 250) model_matrix <- model_matrix[, seq_len(250), drop = FALSE]
  row_count <- length(train_index)
  list(
    x_train = as.data.frame(model_matrix[seq_len(row_count), , drop = FALSE]),
    x_test = as.data.frame(model_matrix[row_count + seq_along(test_index), , drop = FALSE]),
    y_train = y[train_index], y_test = y[test_index],
    train_rows = length(train_index), test_rows = length(test_index),
    classification = classification
  )
}

evaluate_dataset <- function(data, target) {
  prepared <- prepare_model_data(data, target)
  if (!prepared$classification) {
    linear_model <- stats::lm(target_value ~ ., data = data.frame(target_value = prepared$y_train, prepared$x_train))
    linear_prediction <- as.numeric(stats::predict(linear_model, newdata = prepared$x_test))
    forest_model <- randomForest::randomForest(
      x = prepared$x_train, y = prepared$y_train, ntree = 200, importance = TRUE
    )
    forest_prediction <- as.numeric(stats::predict(forest_model, newdata = prepared$x_test))
    metric_row <- function(name, actual, predicted) {
      data.frame(
        model = name,
        RMSE = sqrt(mean((actual - predicted)^2)),
        MAE = mean(abs(actual - predicted)),
        R_squared = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
        accuracy = NA_real_, sensitivity = NA_real_, specificity = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    metrics <- rbind(
      metric_row("Linear regression", prepared$y_test, linear_prediction),
      metric_row("Random Forest (200 trees)", prepared$y_test, forest_prediction)
    )
    predictions <- data.frame(
      actual = prepared$y_test, linear_regression = linear_prediction,
      random_forest = forest_prediction
    )
    importance <- randomForest::importance(forest_model)
    importance <- data.frame(feature = rownames(importance), importance = importance[, 1],
                             row.names = NULL, stringsAsFactors = FALSE)
    importance <- importance[order(importance$importance, decreasing = TRUE), , drop = FALSE]
    list(metrics = metrics, predictions = predictions, confusion = NULL,
         importance = head(importance, 20), prepared = prepared, model_type = "Regression")
  } else {
    forest_model <- randomForest::randomForest(
      x = prepared$x_train, y = prepared$y_train, ntree = 200, importance = TRUE
    )
    prediction <- factor(stats::predict(forest_model, newdata = prepared$x_test),
                         levels = levels(prepared$y_train))
    confusion <- table(Actual = prepared$y_test, Predicted = prediction)
    accuracy <- sum(diag(confusion)) / sum(confusion)
    sensitivity <- specificity <- NA_real_
    if (nlevels(prepared$y_train) == 2) {
      positive <- levels(prepared$y_train)[[2]]
      negative <- levels(prepared$y_train)[[1]]
      tp <- confusion[positive, positive]
      fn <- confusion[positive, negative]
      tn <- confusion[negative, negative]
      fp <- confusion[negative, positive]
      sensitivity <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
      specificity <- if (tn + fp > 0) tn / (tn + fp) else NA_real_
    }
    importance <- randomForest::importance(forest_model, type = 1)
    importance <- data.frame(feature = rownames(importance), importance = importance[, 1],
                             row.names = NULL, stringsAsFactors = FALSE)
    importance <- importance[order(importance$importance, decreasing = TRUE), , drop = FALSE]
    metrics <- data.frame(
      model = "Random Forest classifier (200 trees)", RMSE = NA_real_, MAE = NA_real_,
      R_squared = NA_real_, accuracy = accuracy, sensitivity = sensitivity,
      specificity = specificity, stringsAsFactors = FALSE
    )
    list(metrics = metrics, predictions = data.frame(actual = prepared$y_test, predicted = prediction),
         confusion = confusion, importance = head(importance, 20), prepared = prepared,
         model_type = "Classification")
  }
}

make_distribution_plot <- function(data, column) {
  values <- data[[column]]
  if (is.numeric(values) || is.integer(values)) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[column]])) +
      ggplot2::geom_histogram(bins = 35, fill = "#286f6c", color = "white", na.rm = TRUE) +
      ggplot2::labs(title = paste("Distribution of", column), x = column, y = "Rows") +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    counts <- sort(table(as.character(values), useNA = "ifany"), decreasing = TRUE)
    counts <- head(counts, 20)
    plot_data <- data.frame(value = names(counts), count = as.integer(counts))
    ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(value, count), y = count)) +
      ggplot2::geom_col(fill = "#c45b37") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Most common values in", column), x = NULL, y = "Rows") +
      ggplot2::theme_minimal(base_size = 12)
  }
}

make_relationship_plot <- function(data, x_column, target) {
  x <- data[[x_column]]
  y <- data[[target]]
  if (is.numeric(x) && is.numeric(y)) {
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_column]], y = .data[[target]])) +
      ggplot2::geom_point(alpha = 0.45, color = "#286f6c", na.rm = TRUE) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#c45b37", na.rm = TRUE) +
      ggplot2::labs(title = paste(target, "by", x_column), x = x_column, y = target) +
      ggplot2::theme_minimal(base_size = 12)
  } else if (!is.numeric(x) && is.numeric(y)) {
    plot_data <- data.frame(group = as.character(x), value = y)
    keep <- names(sort(table(plot_data$group), decreasing = TRUE))[seq_len(min(12, length(unique(plot_data$group))))]
    plot_data <- plot_data[plot_data$group %in% keep, , drop = FALSE]
    ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(group, value, median), y = value)) +
      ggplot2::geom_boxplot(fill = "#d6a64f", outlier.alpha = 0.2, na.rm = TRUE) +
      ggplot2::coord_flip() + ggplot2::labs(title = paste(target, "by", x_column), x = NULL, y = target) +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    plot_data <- data.frame(group = as.character(x), target_value = as.character(y))
    plot_data <- plot_data[!is.na(plot_data$group) & !is.na(plot_data$target_value), , drop = FALSE]
    keep <- names(sort(table(plot_data$group), decreasing = TRUE))[seq_len(min(12, length(unique(plot_data$group))))]
    plot_data <- plot_data[plot_data$group %in% keep, , drop = FALSE]
    ggplot2::ggplot(plot_data, ggplot2::aes(x = group, fill = target_value)) +
      ggplot2::geom_bar(position = "fill") + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Target composition by", x_column), x = NULL,
                    y = "Proportion", fill = target) + ggplot2::theme_minimal(base_size = 12)
  }
}

make_correlation_plot <- function(data) {
  numeric_columns <- names(data)[vapply(data, function(column) is.numeric(column) || is.integer(column), logical(1))]
  if (length(numeric_columns) < 2) return(NULL)
  numeric_columns <- head(numeric_columns, 18)
  correlation <- stats::cor(data[, numeric_columns, drop = FALSE], use = "pairwise.complete.obs")
  plot_data <- as.data.frame(as.table(correlation), stringsAsFactors = FALSE)
  names(plot_data) <- c("column_x", "column_y", "correlation")
  ggplot2::ggplot(plot_data, ggplot2::aes(x = column_x, y = column_y, fill = correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#b34f3c", mid = "white", high = "#286f6c", limits = c(-1, 1)) +
    ggplot2::labs(title = "Numeric feature correlations", x = NULL, y = NULL, fill = "Correlation") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}