# Dataset Lab: Search and Analyze

This project now includes a general-purpose R Shiny application for searching real datasets, uploading local files, profiling and cleaning selected data, visualizing it, evaluating suitable machine-learning models, and downloading results. The original IMDb movie-rating analysis remains available as a separate script.

## Requirements

- R 4.1 or newer
- Internet access for installing packages and searching/loading public Hugging Face datasets
- A browser

Install the app dependencies once in the VS Code terminal:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "install.packages(c('shiny','jsonlite','ggplot2','randomForest','readxl'), repos='https://cloud.r-project.org')"
```

If `Rscript` is on your `PATH`, the shorter install command is:

```r
install.packages(c("shiny", "jsonlite", "ggplot2", "randomForest", "readxl"))
```

## Run In VS Code

Open this folder in VS Code, then run this exact command in the integrated PowerShell terminal:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' -e "shiny::runApp('.', host='127.0.0.1', port=8787, launch.browser=FALSE)"
```

Then open `http://127.0.0.1:8787` in a browser. Keep the terminal running while using the app; press `Ctrl+C` there to stop it. If port 8787 is busy, change `port=8787` to another available port.

## Use The App

1. Choose **Search online**, enter a topic, and click **Search datasets**. The results come from Hugging Face's public dataset catalog.
2. Select a real dataset result, choose a maximum row count, and click **Analyze Dataset**. The app loads the selected dataset's available `train` split (or its first available split) from the Hugging Face Datasets Server. Some catalog entries are not served as tabular rows; those will show an API error rather than fabricated results.
3. Or choose **Upload a file** and select a CSV, XLS, or XLSX file. Excel workbooks use the first sheet. Uploaded files are read locally by the app.
4. Review the profile and preview, choose an outcome/target column, and inspect the updated charts and evaluation results.
5. Download the analysis metrics CSV, cleaned dataset CSV, or the currently selected distribution chart as a PNG.

## Data Handling And Models

The app sanitizes column names, trims text, converts blank strings and non-finite numeric values to missing values, removes duplicate rows, and drops entirely empty columns. Missing predictor values are median-imputed for numeric features and mode-imputed for categorical features using training data only. Rows with missing targets are excluded from model evaluation.

Numeric targets use linear regression and Random Forest regression (200 trees), evaluated on a seeded 80/20 split with RMSE, MAE, and R-squared. Categorical targets use a Random Forest classifier (200 trees), with accuracy and a confusion matrix; sensitivity and specificity are reported for binary targets. New/unseen classes and model errors are surfaced in the interface. The target is selected from the active dataset; there are no precomputed demo metrics.

The searchable catalog and data rows use the public Hugging Face Hub and Datasets Server APIs. Availability, row limits, and dataset-server support depend on the selected public dataset.

## Project Files

- `app.R`: Shiny application, live results dashboard, and downloads.
- `analysis_helpers.R`: API access, upload parsing, data cleaning, profiling, model evaluation, and dynamic charts.
- `movie_rating_analysis.R`: Preserved standalone IMDb rating mini project using `ggplot2movies`.
- `outputs/`: Existing generated artifacts from the standalone movie-rating analysis. The Shiny application computes its own results from each selected dataset and does not use those saved values.

To rerun the original movie analysis separately:

```powershell
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' .\movie_rating_analysis.R
```