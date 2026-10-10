required_packages <- c("shiny", "jsonlite", "ggplot2", "randomForest", "readxl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages with install.packages(c(",
       paste(sprintf("\"%s\"", missing_packages), collapse = ", "), ")).", call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
})
source("analysis_helpers.R", local = TRUE)

default_target <- function(data) {
  preferred <- grep("(^|\\.)(target|label|class|species|rating|score|outcome|price|sales)$",
                    names(data), ignore.case = TRUE, value = TRUE)
  if (length(preferred)) return(preferred[[1]])
  categorical <- names(data)[vapply(data, function(column) {
    (is.character(column) || is.factor(column) || is.logical(column)) &&
      length(unique(stats::na.omit(column))) >= 2 && length(unique(stats::na.omit(column))) <= 20
  }, logical(1))]
  if (length(categorical)) return(categorical[[1]])
  numeric <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(numeric)) return(numeric[[1]])
  names(data)[[1]]
}

ui <- fluidPage(
  tags$head(
    tags$title("Dataset Lab | Search and Analyze"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=DM+Mono:wght@400;500&family=Manrope:wght@400;500;600;700;800&display=swap');
      :root{--ink:#202a2a;--muted:#65716e;--paper:#f3f4ef;--surface:#fff;--teal:#286f6c;--rust:#c45b37;--line:#dce2dc;--lime:#d8e777}
      body{background:var(--paper);color:var(--ink);font-family:'Manrope','Segoe UI',sans-serif;font-size:14px}
      .container-fluid{max-width:1500px;padding:0 28px 36px}
      .masthead{margin:0 -28px 24px;padding:24px 34px 22px;background:#183b3a;color:white;border-bottom:5px solid var(--lime)}
      .masthead .eyebrow{font:500 11px 'DM Mono',monospace;letter-spacing:0;color:#c6d8d1}
      .masthead h1{margin:8px 0;font-size:30px;font-weight:700;letter-spacing:0}
      .masthead p{margin:0;color:#d5e0da}
      .row{margin-left:-9px;margin-right:-9px}.col-sm-4,.col-sm-8{padding-left:9px;padding-right:9px}
      .control-panel,.main-panel{background:var(--surface);border:1px solid var(--line);padding:20px;margin-bottom:18px}
      .control-panel{border-top:3px solid var(--teal)}
      h2,h3,h4{color:var(--ink);font-weight:700;letter-spacing:0}
      h3{font-size:18px;margin-top:4px}.control-label{font-weight:700;font-size:12px;color:#43514e}
      .form-control{border-radius:2px;border-color:#cbd5cf;box-shadow:none;height:auto;min-height:38px}
      .btn{border-radius:2px;font-weight:700;letter-spacing:0;padding:9px 13px}
      .btn-primary{background:var(--teal);border-color:var(--teal)}.btn-primary:hover{background:#1d5552;border-color:#1d5552}
      .btn-default{border-color:#bdc9c2;color:#263330;background:#fff}
      .nav-tabs{border-bottom:1px solid var(--line);margin-bottom:16px}.nav-tabs>li>a{color:#52615d;font-weight:700;border-radius:0}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover{color:var(--teal);border:0;border-bottom:3px solid var(--rust);background:transparent}
      .well{background:#f4f7f2;border:1px solid var(--line);border-radius:0;box-shadow:none}
      .status-line{font-size:12px;color:var(--muted);min-height:20px;margin:10px 0}
      .stat-strip{display:flex;gap:9px;flex-wrap:wrap;margin-bottom:18px}.stat-cell{flex:1;min-width:115px;padding:12px;border:1px solid var(--line);background:#fbfcf8}
      .stat-cell strong{display:block;font-size:20px;color:var(--teal)}.stat-cell span{font-size:11px;color:var(--muted)}
      table{font-size:12px!important}th{color:#26423f} .data-preview{overflow-x:auto}
      .help-text{font-size:12px;line-height:1.55;color:var(--muted)}
      .shiny-output-error{color:#9f3725}.shiny-output-error:before{content:'Analysis error: '}
      @media(max-width:767px){.container-fluid{padding:0 14px 26px}.masthead{margin:0 -14px 16px;padding:20px}.masthead h1{font-size:25px}.control-panel,.main-panel{padding:15px}}
    "))
  ),
  div(class = "masthead",
      p(class = "eyebrow", "DATA LAB / LIVE DATA WORKBENCH"),
      h1("Search a dataset. Analyze the data."),
      p("Search Hugging Face's public dataset catalog or upload a CSV / Excel file. Every result is computed from the selected data.")),
  fluidRow(
    column(4,
      div(class = "control-panel",
        h3("Choose a data source"),
        radioButtons("source_type", NULL,
                     choices = c("Search online" = "online", "Upload a file" = "upload"),
                     selected = "online", inline = TRUE),
        conditionalPanel("input.source_type == 'online'",
          textInput("topic", "Dataset topic", placeholder = "e.g. air quality, housing prices, iris"),
          actionButton("search_button", "Search datasets", icon = icon("search"), class = "btn-default"),
          selectInput("dataset_choice", "Search results", choices = character(0)),
          numericInput("max_rows", "Maximum rows to load", value = 5000, min = 100, max = 20000, step = 500),
          p(class = "help-text", "Online data are loaded from the selected dataset's available train split, up to this row limit.")
        ),
        conditionalPanel("input.source_type == 'upload'",
          fileInput("dataset_file", "Choose CSV or Excel file", accept = c(".csv", ".xls", ".xlsx")),
          p(class = "help-text", "CSV, XLS, and XLSX files are read locally. Workbook uploads use the first sheet.")
        ),
        actionButton("analyze_button", "Analyze Dataset", icon = icon("play"), class = "btn-primary btn-block"),
        uiOutput("status"),
        hr(),
        uiOutput("target_selector"),
        uiOutput("chart_selector"),
        downloadButton("download_metrics", "Download analysis results", class = "btn-default btn-block"),
        br(),
        downloadButton("download_predictions", "Download test predictions", class = "btn-default btn-block"),
        br(),
        downloadButton("download_profile", "Download column profile", class = "btn-default btn-block"),
        br(),
        downloadButton("download_cleaned", "Download cleaned dataset", class = "btn-default btn-block"),
        br(),
        downloadButton("download_chart", "Download selected chart PNG", class = "btn-default btn-block")
      )
    ),
    column(8,
      div(class = "main-panel",
        uiOutput("dataset_summary"),
        tabsetPanel(
          tabPanel("Insights", br(), tableOutput("profile_table"),
                   h4("Data preview"), div(class = "data-preview", tableOutput("preview_table"))),
          tabPanel("Charts", br(), plotOutput("distribution_plot", height = 360),
                   plotOutput("relationship_plot", height = 360),
                   plotOutput("correlation_plot", height = 480)),
          tabPanel("Models", br(), h4("Held-out evaluation"), tableOutput("metrics_table"),
                   uiOutput("model_note"), h4("Confusion matrix"), tableOutput("confusion_table"),
                   h4("Feature importance"), tableOutput("importance_table"))
        )
      )
    )
  ),
  tags$footer(class = "help-text", "Online dataset search and rows provided by Hugging Face Hub / Datasets Server. Uploaded files remain local to this app session.")
)

server <- function(input, output, session) {
  search_results <- reactiveVal(data.frame())
  status_message <- reactiveVal("Search for an online dataset or upload a file to begin.")

  observeEvent(input$search_button, {
    status_message("Searching Hugging Face's public dataset catalog...")
    tryCatch({
      results <- search_hf_datasets(input$topic, limit = 12)
      search_results(results)
      if (!nrow(results)) {
        updateSelectInput(session, "dataset_choice", choices = character(0))
        status_message("No datasets matched that topic. Try a broader search term.")
      } else {
        labels <- sprintf("%s  |  %s downloads", results$id, format(results$downloads, big.mark = ","))
        choices <- stats::setNames(results$id, labels)
        updateSelectInput(session, "dataset_choice", choices = choices, selected = results$id[[1]])
        status_message(sprintf("Found %d live dataset matches. Select one and click Analyze Dataset.", nrow(results)))
      }
    }, error = function(error) {
      search_results(data.frame())
      status_message(conditionMessage(error))
    })
  })

  loaded_dataset <- eventReactive(input$analyze_button, {
    status_message("Loading and cleaning the selected data...")
    tryCatch({
      if (identical(input$source_type, "online")) {
        validate(need(nzchar(input$dataset_choice %||% ""), "Search for and select an online dataset first."))
        fetched <- fetch_hf_dataset(input$dataset_choice, max_rows = input$max_rows %||% 5000)
        cleaned <- clean_dataset(fetched$data)
        list(cleaning = cleaned, source = fetched$source, raw_total_rows = fetched$total_rows)
      } else {
        file <- input$dataset_file
        validate(need(!is.null(file) && nrow(file) == 1, "Choose a CSV or Excel file first."))
        raw <- read_uploaded_dataset(file$datapath, file$name)
        cleaned <- clean_dataset(raw)
        list(cleaning = cleaned, source = paste("Uploaded file:", file$name), raw_total_rows = nrow(raw))
      }
    }, error = function(error) {
      status_message(conditionMessage(error))
      NULL
    })
  }, ignoreInit = TRUE)

  observeEvent(loaded_dataset(), {
    loaded <- loaded_dataset()
    if (is.null(loaded)) return()
    data <- loaded$cleaning$data
    selected <- default_target(data)
    updateSelectInput(session, "target", choices = names(data), selected = selected)
    numeric_columns <- names(data)[vapply(data, function(column) is.numeric(column) || is.integer(column), logical(1))]
    chart_default <- if (length(numeric_columns)) numeric_columns[[1]] else names(data)[[1]]
    updateSelectInput(session, "chart_column", choices = names(data), selected = chart_default)
    status_message(sprintf("Loaded %s rows x %s columns from %s", format(nrow(data), big.mark = ","),
                           ncol(data), loaded$source))
  }, ignoreInit = TRUE)

  output$status <- renderUI({
    loaded <- loaded_dataset()
    message <- if (is.null(loaded)) status_message() else {
      sprintf("Loaded %s rows x %s columns from %s",
              format(nrow(loaded$cleaning$data), big.mark = ","),
              ncol(loaded$cleaning$data), loaded$source)
    }
    div(class = "status-line", message)
  })

  output$target_selector <- renderUI({
    loaded <- loaded_dataset()
    if (is.null(loaded)) return(NULL)
    selectInput("target", "Outcome / target column", choices = names(loaded$cleaning$data),
                selected = default_target(loaded$cleaning$data))
  })
  output$chart_selector <- renderUI({
    loaded <- loaded_dataset()
    if (is.null(loaded)) return(NULL)
    selectInput("chart_column", "Explore a column", choices = names(loaded$cleaning$data))
  })

  analysis_state <- reactive({
    loaded <- loaded_dataset()
    req(loaded)
    tryCatch({
      result <- evaluate_dataset(loaded$cleaning$data, input$target %||% default_target(loaded$cleaning$data))
      list(result = result, error = NULL)
    }, error = function(error) list(result = NULL, error = conditionMessage(error)))
  })

  output$dataset_summary <- renderUI({
    loaded <- loaded_dataset()
    req(loaded)
    data <- loaded$cleaning$data
    div(class = "stat-strip",
      div(class = "stat-cell", strong(format(nrow(data), big.mark = ",")), span("Rows after cleaning")),
      div(class = "stat-cell", strong(ncol(data)), span("Columns")),
      div(class = "stat-cell", strong(loaded$cleaning$duplicate_rows_removed), span("Duplicate rows removed")),
      div(class = "stat-cell", strong(sum(loaded$cleaning$missing_by_column)), span("Missing cells remaining"))
    )
  })

  output$profile_table <- renderTable({
    loaded <- loaded_dataset()
    req(loaded)
    dataset_profile(loaded$cleaning$data)
  }, striped = TRUE, bordered = FALSE, spacing = "s", na = "")

  output$preview_table <- renderTable({
    loaded <- loaded_dataset()
    req(loaded)
    utils::head(loaded$cleaning$data, 8)
  }, striped = TRUE, bordered = FALSE, spacing = "s", na = "")

  output$distribution_plot <- renderPlot({
    loaded <- loaded_dataset()
    req(loaded, input$chart_column)
    make_distribution_plot(loaded$cleaning$data, input$chart_column)
  })

  output$relationship_plot <- renderPlot({
    loaded <- loaded_dataset()
    req(loaded, input$chart_column, input$target)
    validate(need(input$chart_column != input$target, "Choose a different column to explore its relationship with the target."))
    make_relationship_plot(loaded$cleaning$data, input$chart_column, input$target)
  })

  output$correlation_plot <- renderPlot({
    loaded <- loaded_dataset()
    req(loaded)
    plot <- make_correlation_plot(loaded$cleaning$data)
    validate(need(!is.null(plot), "At least two numeric columns are needed for a correlation chart."))
    plot
  })

  output$metrics_table <- renderTable({
    state <- analysis_state()
    validate(need(is.null(state$error), state$error))
    state$result$metrics
  }, digits = 4, striped = TRUE, bordered = FALSE, spacing = "s", na = "N/A")

  output$model_note <- renderUI({
    state <- analysis_state()
    if (!is.null(state$error)) return(p(class = "help-text", state$error))
    result <- state$result
    text <- if (identical(result$model_type, "Regression")) {
      sprintf("Regression target: %s. Seed 2026; %s training rows and %s held-out test rows. Random Forest uses 200 trees.",
              input$target, result$prepared$train_rows, result$prepared$test_rows)
    } else {
      positive <- if (nlevels(result$prepared$y_train) == 2) levels(result$prepared$y_train)[[2]] else "not applicable for multiclass"
      sprintf("Classification target: %s. Seed 2026; %s training rows and %s held-out test rows. Positive class for sensitivity: %s.",
              input$target, result$prepared$train_rows, result$prepared$test_rows, positive)
    }
    p(class = "help-text", text)
  })

  output$confusion_table <- renderTable({
    state <- analysis_state()
    validate(need(is.null(state$error), state$error))
    validate(need(!is.null(state$result$confusion), "Confusion matrices apply to classification targets."))
    as.data.frame.matrix(state$result$confusion)
  }, rownames = TRUE, striped = TRUE, bordered = FALSE, spacing = "s")

  output$importance_table <- renderTable({
    state <- analysis_state()
    validate(need(is.null(state$error), state$error))
    state$result$importance
  }, digits = 4, striped = TRUE, bordered = FALSE, spacing = "s")

  output$download_metrics <- downloadHandler(
    filename = function() "dataset_analysis_metrics.csv",
    content = function(file) {
      state <- analysis_state()
      validate(need(is.null(state$error), state$error))
      write.csv(state$result$metrics, file, row.names = FALSE, na = "")
    }
  )
  output$download_predictions <- downloadHandler(
    filename = function() "dataset_test_predictions.csv",
    content = function(file) {
      state <- analysis_state()
      validate(need(is.null(state$error), state$error))
      write.csv(state$result$predictions, file, row.names = FALSE, na = "")
    }
  )
  output$download_profile <- downloadHandler(
    filename = function() "dataset_column_profile.csv",
    content = function(file) {
      loaded <- loaded_dataset()
      req(loaded)
      write.csv(dataset_profile(loaded$cleaning$data), file, row.names = FALSE, na = "")
    }
  )
  output$download_cleaned <- downloadHandler(
    filename = function() "cleaned_dataset.csv",
    content = function(file) {
      loaded <- loaded_dataset()
      req(loaded)
      write.csv(loaded$cleaning$data, file, row.names = FALSE, na = "")
    }
  )
  output$download_chart <- downloadHandler(
    filename = function() paste0("", gsub("[^A-Za-z0-9_-]", "_", input$chart_column %||% "dataset"), "_distribution.png"),
    content = function(file) {
      loaded <- loaded_dataset()
      req(loaded, input$chart_column)
      ggplot2::ggsave(file, make_distribution_plot(loaded$cleaning$data, input$chart_column),
                      width = 9, height = 6, dpi = 160)
    }
  )
}

shiny::shinyApp(ui = ui, server = server)