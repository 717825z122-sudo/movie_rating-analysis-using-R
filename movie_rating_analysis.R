required_packages <- c("ggplot2movies", "ggplot2", "randomForest", "car", "corrplot")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf("\"%s\"", missing_packages), collapse = ", "), ")).",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(randomForest)
})

set.seed(2026)
output_dir <- file.path(getwd(), "outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

raw_movies <- ggplot2movies::movies
rows_before_deduplication <- nrow(raw_movies)
movies <- raw_movies[!duplicated(raw_movies), , drop = FALSE]
duplicates_removed <- rows_before_deduplication - nrow(movies)

genre_columns <- intersect(
  c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance",
    "Short", "Western", "Horror", "Musical", "Mystery", "Film.Noir",
    "Crime", "War", "Family", "Fantasy", "Sci.Fi", "Thriller"),
  names(movies)
)
if (length(genre_columns) == 0) {
  stop("No expected genre indicator columns were found in ggplot2movies::movies.")
}

model_columns <- c("rating", "year", "length", "votes", genre_columns)
movies <- movies[, model_columns, drop = FALSE]
movies <- movies[!is.na(movies$rating), , drop = FALSE]
missing_before_imputation <- colSums(is.na(movies))

for (column in c("year", "length", "votes")) {
  replacement <- median(movies[[column]], na.rm = TRUE)
  if (!is.finite(replacement)) replacement <- 0
  movies[[column]][is.na(movies[[column]])] <- replacement
}
for (column in genre_columns) {
  movies[[column]][is.na(movies[[column]])] <- 0
  movies[[column]] <- as.integer(movies[[column]] > 0)
}

movies$decade <- floor(movies$year / 10) * 10
movies$log_votes <- log1p(pmax(movies$votes, 0))
movies$rating_class <- factor(
  ifelse(movies$rating >= 7, "7 or higher", "Below 7"),
  levels = c("Below 7", "7 or higher")
)

train_indices <- sample(seq_len(nrow(movies)), size = floor(0.8 * nrow(movies)))
train_raw <- movies[train_indices, , drop = FALSE]
test_raw <- movies[-train_indices, , drop = FALSE]

scaled_columns <- c("year", "length", "log_votes")
scale_parameters <- lapply(scaled_columns, function(column) {
  center <- mean(train_raw[[column]])
  spread <- sd(train_raw[[column]])
  if (!is.finite(spread) || spread == 0) spread <- 1
  c(center = center, spread = spread)
})
names(scale_parameters) <- scaled_columns

add_scaled_features <- function(data) {
  for (column in scaled_columns) {
    parameters <- scale_parameters[[column]]
    data[[paste0(column, "_z")]] <- (data[[column]] - parameters[["center"]]) / parameters[["spread"]]
  }
  data
}

train <- add_scaled_features(train_raw)
test <- add_scaled_features(test_raw)
predictors <- c(paste0(scaled_columns, "_z"), genre_columns)
train_x <- train[, predictors, drop = FALSE]
test_x <- test[, predictors, drop = FALSE]
train_y <- train$rating
test_y <- test$rating

linear_model <- lm(rating ~ ., data = data.frame(rating = train_y, train_x))
forest_model <- randomForest(x = train_x, y = train_y, ntree = 200, importance = TRUE)
gamma_model <- glm(rating ~ ., data = data.frame(rating = train_y, train_x), family = Gamma(link = "log"))
logistic_model <- glm(
  I(rating >= 7) ~ ., data = data.frame(rating = train_y, train_x),
  family = binomial(link = "logit")
)

linear_predictions <- as.numeric(predict(linear_model, newdata = test_x))
forest_predictions <- as.numeric(predict(forest_model, newdata = test_x))
gamma_predictions <- as.numeric(predict(gamma_model, newdata = test_x, type = "response"))
logistic_probabilities <- as.numeric(predict(logistic_model, newdata = test_x, type = "response"))
logistic_predictions <- factor(
  ifelse(logistic_probabilities >= 0.5, "7 or higher", "Below 7"),
  levels = levels(movies$rating_class)
)
logistic_actual <- test$rating_class

regression_metrics <- function(actual, predicted) {
  c(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

confusion <- table(
  Actual = logistic_actual,
  Predicted = logistic_predictions,
  dnn = c("Actual", "Predicted")
)
true_negative <- confusion["Below 7", "Below 7"]
false_positive <- confusion["Below 7", "7 or higher"]
false_negative <- confusion["7 or higher", "Below 7"]
true_positive <- confusion["7 or higher", "7 or higher"]
logistic_metrics <- c(
  Accuracy = sum(diag(confusion)) / sum(confusion),
  Sensitivity = if ((true_positive + false_negative) > 0) true_positive / (true_positive + false_negative) else NA_real_,
  Specificity = if ((true_negative + false_positive) > 0) true_negative / (true_negative + false_positive) else NA_real_
)

model_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest Regression (200 trees)", "Gamma GLM", "Logistic Regression"),
  Type = c("Regression", "Regression", "Regression", "Binary classification"),
  RMSE = c(regression_metrics(test_y, linear_predictions)[["RMSE"]],
           regression_metrics(test_y, forest_predictions)[["RMSE"]],
           regression_metrics(test_y, gamma_predictions)[["RMSE"]], NA_real_),
  MAE = c(regression_metrics(test_y, linear_predictions)[["MAE"]],
          regression_metrics(test_y, forest_predictions)[["MAE"]],
          regression_metrics(test_y, gamma_predictions)[["MAE"]], NA_real_),
  R2 = c(regression_metrics(test_y, linear_predictions)[["R2"]],
         regression_metrics(test_y, forest_predictions)[["R2"]],
         regression_metrics(test_y, gamma_predictions)[["R2"]], NA_real_),
  Accuracy = c(NA_real_, NA_real_, NA_real_, logistic_metrics[["Accuracy"]]),
  Sensitivity = c(NA_real_, NA_real_, NA_real_, logistic_metrics[["Sensitivity"]]),
  Specificity = c(NA_real_, NA_real_, NA_real_, logistic_metrics[["Specificity"]])
)
write.csv(model_comparison, file.path(output_dir, "model_comparison.csv"), row.names = FALSE)
write.csv(as.data.frame.matrix(confusion), file.path(output_dir, "logistic_confusion_matrix.csv"))

save_plot <- function(plot, filename, width = 9, height = 6) {
  ggsave(file.path(output_dir, filename), plot = plot, width = width, height = height, dpi = 160)
}

save_plot(
  ggplot(movies, aes(x = rating)) +
    geom_histogram(binwidth = 0.25, fill = "#286f6c", color = "white") +
    labs(title = "IMDb Rating Distribution", x = "IMDb rating", y = "Movies") +
    theme_minimal(base_size = 12),
  "rating_distribution.png"
)
save_plot(
  ggplot(movies, aes(x = log_votes)) +
    geom_histogram(bins = 45, fill = "#c45b37", color = "white") +
    labs(title = "Distribution of Log Votes", x = "log(1 + votes)", y = "Movies") +
    theme_minimal(base_size = 12),
  "log_votes_distribution.png"
)

correlation_data <- movies[, c("rating", "year", "length", "log_votes", genre_columns), drop = FALSE]
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
png(file.path(output_dir, "correlation_matrix.png"), width = 1600, height = 1400, res = 160)
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.65,
                   number.cex = 0.45, col = grDevices::colorRampPalette(c("#b34f3c", "white", "#286f6c"))(200))
dev.off()

decade_summary <- aggregate(rating ~ decade, data = movies, FUN = mean)
decade_summary$count <- as.numeric(table(movies$decade)[as.character(decade_summary$decade)])
save_plot(
  ggplot(decade_summary, aes(x = decade, y = rating)) +
    geom_line(color = "#286f6c", linewidth = 0.8) +
    geom_point(aes(size = count), color = "#c45b37", alpha = 0.8) +
    scale_size_continuous(name = "Movie count", range = c(1.5, 6)) +
    labs(title = "Average IMDb Rating by Decade", x = "Release decade", y = "Mean rating") +
    theme_minimal(base_size = 12),
  "ratings_by_decade.png"
)

genre_summary <- do.call(rbind, lapply(genre_columns, function(genre) {
  selected <- movies[[genre]] == 1
  data.frame(genre = gsub("\\.", " ", genre), mean_rating = mean(movies$rating[selected]),
             count = sum(selected))
}))
genre_summary <- genre_summary[order(genre_summary$mean_rating), , drop = FALSE]
genre_summary$genre <- factor(genre_summary$genre, levels = genre_summary$genre)
save_plot(
  ggplot(genre_summary, aes(x = genre, y = mean_rating, size = count)) +
    geom_point(color = "#286f6c", alpha = 0.85) + coord_flip() +
    scale_size_continuous(name = "Movie count", range = c(2, 7)) +
    labs(title = "Average IMDb Rating by Genre", x = NULL, y = "Mean rating") +
    theme_minimal(base_size = 12),
  "ratings_by_genre.png", width = 9, height = 8
)

boxplot_data <- do.call(rbind, lapply(genre_columns, function(genre) {
  data.frame(genre = gsub("\\.", " ", genre)[movies[[genre]] == 1],
             rating = movies$rating[movies[[genre]] == 1])
}))
save_plot(
  ggplot(boxplot_data, aes(x = reorder(genre, rating, median), y = rating)) +
    geom_boxplot(fill = "#d6a64f", color = "#343d3d", outlier.alpha = 0.12) + coord_flip() +
    labs(title = "Rating Boxplots by Genre", x = NULL, y = "IMDb rating") +
    theme_minimal(base_size = 12),
  "rating_boxplots_by_genre.png", width = 9, height = 8
)

vif_values <- car::vif(lm(rating ~ ., data = data.frame(rating = train_y, train_x)))
if (is.matrix(vif_values)) vif_values <- vif_values[, "GVIF"]^(1 / (2 * vif_values[, "Df"]))
vif_data <- data.frame(feature = names(vif_values), VIF = as.numeric(vif_values))
write.csv(vif_data, file.path(output_dir, "vif_values.csv"), row.names = FALSE)
save_plot(
  ggplot(vif_data, aes(x = reorder(feature, VIF), y = VIF)) +
    geom_col(fill = "#c45b37") + coord_flip() +
    labs(title = "Variance Inflation Factors", x = NULL, y = "VIF") +
    theme_minimal(base_size = 12),
  "vif.png", width = 9, height = 7
)

importance_values <- randomForest::importance(forest_model, type = 1)
importance_data <- data.frame(feature = rownames(importance_values),
                              importance = importance_values[, 1])
importance_data <- importance_data[order(importance_data$importance, decreasing = TRUE), , drop = FALSE]
importance_data <- head(importance_data, 15)
save_plot(
  ggplot(importance_data, aes(x = reorder(feature, importance), y = importance)) +
    geom_col(fill = "#286f6c") + coord_flip() +
    labs(title = "Random Forest Feature Importance", x = NULL, y = "%IncMSE") +
    theme_minimal(base_size = 12),
  "random_forest_feature_importance.png", width = 9, height = 7
)

prediction_data <- data.frame(
  actual = rep(test_y, 3),
  predicted = c(linear_predictions, forest_predictions, gamma_predictions),
  model = factor(rep(c("Linear Regression", "Random Forest", "Gamma GLM"), each = length(test_y)))
)
save_plot(
  ggplot(prediction_data, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.18, color = "#286f6c", size = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "#c45b37", linewidth = 0.8) +
    facet_wrap(~ model, nrow = 1) +
    coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10)) +
    labs(title = "Actual vs Predicted IMDb Ratings", x = "Actual rating (0-10)", y = "Predicted rating (0-10)") +
    theme_minimal(base_size = 12),
  "actual_vs_predicted_ratings.png", width = 12, height = 5
)

predict_new_movie <- function(movie) {
  required_fields <- c("year", "length", "votes", genre_columns)
  absent <- setdiff(required_fields, names(movie))
  if (length(absent) > 0) {
    stop("New movie is missing fields: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  new_data <- as.data.frame(movie[required_fields], check.names = FALSE)
  new_data$log_votes <- log1p(pmax(as.numeric(new_data$votes), 0))
  new_data <- add_scaled_features(new_data)
  new_x <- new_data[, predictors, drop = FALSE]
  data.frame(
    Linear_Regression = as.numeric(predict(linear_model, newdata = new_x)),
    Random_Forest = as.numeric(predict(forest_model, newdata = new_x)),
    Gamma_GLM = as.numeric(predict(gamma_model, newdata = new_x, type = "response")),
    Logistic_Hit_Probability = as.numeric(predict(logistic_model, newdata = new_x, type = "response"))
  )
}

example_movie <- as.list(setNames(rep(0, length(genre_columns)), genre_columns))
example_movie$year <- 2020
example_movie$length <- 110
example_movie$votes <- 5000
if ("Comedy" %in% genre_columns) example_movie$Comedy <- 1
if ("Drama" %in% genre_columns) example_movie$Drama <- 1
example_prediction <- cbind(Movie = "Example new movie", predict_new_movie(example_movie))
write.csv(example_prediction, file.path(output_dir, "example_new_movie_prediction.csv"), row.names = FALSE)

display_metric <- function(value) {
  if (is.na(value)) "N/A" else sprintf("%.4f", value)
}
model_rows <- vapply(seq_len(nrow(model_comparison)), function(index) {
  row <- model_comparison[index, ]
  sprintf(
    "<tr><th scope=\"row\">%s</th><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    row$Model, row$Type, display_metric(row$RMSE), display_metric(row$MAE),
    display_metric(row$R2), display_metric(row$Accuracy), display_metric(row$Sensitivity),
    display_metric(row$Specificity)
  )
}, character(1))
chart_info <- data.frame(
  file = c("rating_distribution.png", "log_votes_distribution.png", "correlation_matrix.png",
           "ratings_by_decade.png", "ratings_by_genre.png", "rating_boxplots_by_genre.png",
           "vif.png", "random_forest_feature_importance.png", "actual_vs_predicted_ratings.png"),
  title = c("Rating distribution", "Log votes", "Correlation matrix", "Ratings by decade",
            "Ratings by genre", "Rating boxplots by genre", "Variance inflation factors",
            "Random Forest feature importance", "Actual vs predicted ratings"),
  stringsAsFactors = FALSE
)
chart_cards <- vapply(seq_len(nrow(chart_info)), function(index) {
  sprintf("<figure><a href=\"%s\"><img src=\"%s\" alt=\"%s\" loading=\"lazy\"></a><figcaption>%s</figcaption></figure>",
          chart_info$file[index], chart_info$file[index], chart_info$title[index], chart_info$title[index])
}, character(1))
confusion_html <- sprintf(
  "<tr><th>Below 7</th><td>%d</td><td>%d</td></tr><tr><th>7 or higher</th><td>%d</td><td>%d</td></tr>",
  true_negative, false_positive, false_negative, true_positive
)
prediction_html <- sprintf(
  "<tr><th>Linear regression</th><td>%.4f</td></tr><tr><th>Random Forest</th><td>%.4f</td></tr><tr><th>Gamma GLM</th><td>%.4f</td></tr><tr><th>Logistic hit probability</th><td>%.4f</td></tr>",
  example_prediction$Linear_Regression, example_prediction$Random_Forest,
  example_prediction$Gamma_GLM, example_prediction$Logistic_Hit_Probability
)
dashboard_html <- c(
  "<!doctype html>",
  "<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>IMDb Movie Rating Analysis</title>",
  "<style>",
  ":root{color-scheme:light;--ink:#202a2a;--muted:#5d6b68;--paper:#f4f3ed;--panel:#fffefa;--teal:#286f6c;--rust:#c45b37;--line:#d9ded8}*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font:15px/1.55 'Segoe UI',sans-serif}header{padding:42px max(24px,calc((100vw - 1200px)/2));background:#183b3a;color:white;border-bottom:6px solid var(--rust)}header p{margin:0;color:#cbd8d2}h1{font:600 34px/1.15 Georgia,serif;margin:8px 0 12px}.wrap{max-width:1200px;margin:auto;padding:26px 24px 56px}.summary{display:flex;gap:12px;flex-wrap:wrap;margin-bottom:22px}.stat{background:var(--panel);border:1px solid var(--line);padding:14px 18px;min-width:155px;flex:1}.stat strong{display:block;font:600 24px Georgia,serif;color:var(--teal)}.stat span{color:var(--muted);font-size:13px}h2{font:600 23px Georgia,serif;margin:30px 0 12px}section{margin:18px 0 30px}.table-wrap{overflow-x:auto;background:var(--panel);border:1px solid var(--line)}table{border-collapse:collapse;width:100%;font-size:13px}th,td{text-align:right;padding:10px 12px;border-bottom:1px solid var(--line);white-space:nowrap}thead th{background:#e7eeea;color:#183b3a}tbody th{text-align:left}td:first-of-type{text-align:left}.charts{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px}figure{margin:0;background:var(--panel);border:1px solid var(--line);padding:10px}figure img{display:block;width:100%;height:auto}figcaption{font-weight:600;padding:8px 4px 2px}.two-col{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:18px}.note{color:var(--muted);font-size:13px}footer{border-top:1px solid var(--line);padding-top:14px;color:var(--muted);font-size:13px}@media(max-width:700px){header{padding:28px 20px}h1{font-size:28px}.wrap{padding:18px 14px 36px}.charts,.two-col{grid-template-columns:1fr}.stat{min-width:125px}}",
  "</style></head><body>",
  "<header><p>DATA ANALYSIS / MODEL EVALUATION</p><h1>IMDb Movie Rating Analysis</h1>",
  sprintf("<p>ggplot2movies dataset | seed 2026 | %s movies analyzed</p></header>", format(nrow(movies), big.mark = ",")),
  "<main class=\"wrap\"><div class=\"summary\">",
  sprintf("<div class=\"stat\"><strong>%s</strong><span>Movies loaded</span></div>", format(rows_before_deduplication, big.mark = ",")),
  sprintf("<div class=\"stat\"><strong>%s</strong><span>Duplicates removed</span></div>", format(duplicates_removed, big.mark = ",")),
  sprintf("<div class=\"stat\"><strong>%s / %s</strong><span>Train / test rows</span></div>", format(nrow(train), big.mark = ","), format(nrow(test), big.mark = ",")),
  sprintf("<div class=\"stat\"><strong>%s</strong><span>PNG charts</span></div>", nrow(chart_info)),
  "</div><section><h2>Held-out model results</h2><div class=\"table-wrap\"><table><thead><tr><th>Model</th><th>Type</th><th>RMSE</th><th>MAE</th><th>R-squared</th><th>Accuracy</th><th>Sensitivity</th><th>Specificity</th></tr></thead><tbody>",
  model_rows,
  "</tbody></table></div><p class=\"note\">Regression metrics use the held-out test set. Logistic regression classifies ratings of 7 or higher. N/A means that metric does not apply to that model.</p></section>",
  "<section class=\"two-col\"><div><h2>Logistic confusion matrix</h2><div class=\"table-wrap\"><table><thead><tr><th>Actual \\ Predicted</th><th>Below 7</th><th>7 or higher</th></tr></thead><tbody>",
  confusion_html,
  "</tbody></table></div></div><div><h2>Example new-movie prediction</h2><div class=\"table-wrap\"><table><thead><tr><th>Model output</th><th>Value</th></tr></thead><tbody>",
  prediction_html,
  "</tbody></table></div><p class=\"note\">Illustrative input: 2020, 110 minutes, 5,000 votes, Comedy and Drama. Predictions are not known IMDb ratings.</p></div></section>",
  "<section><h2>Charts</h2><div class=\"charts\">",
  chart_cards,
  "</div></section><footer>Generated by movie_rating_analysis.R. Open the CSV files in this folder for full-precision results.</footer></main></body></html>"
)
writeLines(dashboard_html, file.path(output_dir, "index.html"), useBytes = TRUE)

cat("Movie Rating Analysis completed.\n")
cat("Rows loaded:", rows_before_deduplication, "| duplicates removed:", duplicates_removed,
    "| rows modeled:", nrow(movies), "| train:", nrow(train), "| test:", nrow(test), "\n")
cat("Missing values before imputation (selected columns):\n")
print(missing_before_imputation[missing_before_imputation > 0])
cat("\nTest-set model comparison:\n")
print(model_comparison, row.names = FALSE, digits = 4)
cat("\nLogistic regression confusion matrix:\n")
print(confusion)
cat("\nExample new movie prediction:\n")
print(example_prediction, row.names = FALSE, digits = 4)
cat("\nOutputs saved to:", normalizePath(output_dir), "\n")