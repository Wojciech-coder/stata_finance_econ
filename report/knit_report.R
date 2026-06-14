if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cloud.r-project.org")
}

if (!file.exists("output/tables/summary_stats.csv")) {
  source("run_all.R")
}

rmarkdown::render(
  input = "report/report.Rmd",
  output_file = "Boston_Housing_Report.docx",
  output_dir = "report",
  quiet = TRUE
)

cat("Report saved to: report/Boston_Housing_Report.docx\n")
