library(officer)
library(flextable)
library(dplyr)

if (!file.exists("output/tables/summary_stats.csv")) {
  source("run_all.R")
}

tables_dir <- "output/tables"
figures_dir <- "output/figures"
out_file <- "report/Boston_Housing_Report.docx"

summary_stats <- read.csv(file.path(tables_dir, "summary_stats.csv"))
slr_comp <- read.csv(file.path(tables_dir, "simple_models_comparison.csv"))
mr_comp <- read.csv(file.path(tables_dir, "multiple_models_comparison.csv"))
all_comp <- read.csv(file.path(tables_dir, "model_comparison.csv"))
vif_tab <- read.csv(file.path(tables_dir, "vif.csv"))
high_cor <- read.csv(file.path(tables_dir, "high_correlations.csv"))
nonlinear_comp <- read.csv(file.path(tables_dir, "nonlinear_comparison.csv"))
quad_terms <- read.csv(file.path(tables_dir, "nonlinear_quad_terms.csv"))
anova_results <- read.csv(file.path(tables_dir, "nonlinear_anova.csv"))

round_df <- function(df) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, 3) else x)
  df
}

add_table <- function(doc, df, caption = NULL) {
  ft <- flextable(round_df(df))
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  if (!is.null(caption)) ft <- set_caption(ft, caption = caption)
  body_add_flextable(doc, ft)
}

add_figure <- function(doc, path, width = 6, height = 4) {
  if (file.exists(path)) {
    body_add_img(doc, src = path, width = width, height = height)
  } else {
    body_add_par(doc, paste("Figure not found:", path))
  }
}

doc <- read_docx()

doc <- body_add_par(doc, "Predicting Housing Prices in Greater Boston", style = "heading 1")
doc <- body_add_par(doc, "Application of Statistics and Econometrics in Finance and Accounting", style = "heading 2")
doc <- body_add_par(doc, paste("Date:", format(Sys.Date(), "%B %d, %Y")))
doc <- body_add_par(doc, "")

doc <- body_add_par(doc, "1. Introduction", style = "heading 1")
doc <- body_add_par(doc, "This report analyses the Boston housing market using the Boston dataset from the MASS package. The goal is to identify which socio-economic and structural factors best explain median home values across Boston census tracts.")
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Research question: What factors best predict housing prices in Boston?")
doc <- body_add_par(doc, "Dependent variable: medv (median value of owner-occupied homes in $1,000s).")
doc <- body_add_par(doc, "Main predictors: rm (rooms), lstat (% lower-status population), nox (air pollution), ptratio (pupil-teacher ratio), dis (distance to employment centres), and other neighbourhood variables.")
doc <- body_add_par(doc, "")

doc <- body_add_par(doc, "2. Data Exploration", style = "heading 1")
doc <- body_add_par(doc, "The dataset contains 506 observations and 14 variables. The data come from the 1970s and should be interpreted in that historical context.")
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Table 1. Summary statistics", style = "heading 2")
doc <- add_table(doc, summary_stats)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Data issues:")
doc <- body_add_par(doc, "- medv is censored at 50 ($50,000) for 16 tracts.")
doc <- body_add_par(doc, "- crim is highly right-skewed.")
doc <- body_add_par(doc, "- Strong correlations among predictors suggest multicollinearity.")
doc <- body_add_par(doc, "- Cross-sectional data limits causal interpretation.")
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Table 2. High correlations among predictors (|r| > 0.7)", style = "heading 2")
doc <- add_table(doc, high_cor)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 1. Distribution of medv", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "medv_histogram.png"), 5.5, 3.5)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 2. Correlation matrix", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "correlation_matrix.png"), 5.5, 4.5)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 3. medv vs lstat", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "scatter_medv_lstat.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 4. medv vs rm", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "scatter_medv_rm.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "3. Regression Analysis", style = "heading 1")
doc <- body_add_par(doc, "3.1 Simple linear regression", style = "heading 2")
doc <- body_add_par(doc, "Four simple models were estimated: medv ~ rm, medv ~ lstat, medv ~ nox, and medv ~ ptratio.")
doc <- body_add_par(doc, "")
doc <- add_table(doc, slr_comp, "Table 3. Simple regression models")

best_slr <- slr_comp$model[which.max(slr_comp$adj_r_sq)]
best_slr_r2 <- max(slr_comp$adj_r_sq)
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, paste0("The best simple model is ", best_slr, " with adjusted R-squared = ", round(best_slr_r2, 3), "."))
doc <- body_add_par(doc, "A higher lstat is associated with lower median home values. More rooms (rm) are associated with higher values.")

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 5. Simple regression: medv ~ lstat", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "slr_lstat.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 6. Simple regression: medv ~ rm", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "slr_rm.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "3.2 Multiple regression", style = "heading 2")
doc <- body_add_par(doc, "Three manual multiple regression models were estimated:")
doc <- body_add_par(doc, "MR1: medv ~ rm + lstat + nox + dis")
doc <- body_add_par(doc, "MR2: medv ~ rm + age + tax + ptratio")
doc <- body_add_par(doc, "MR3: medv ~ rm + lstat + ptratio + dis + nox + chas")
doc <- body_add_par(doc, "")
doc <- add_table(doc, mr_comp, "Table 4. Multiple regression models")

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "3.3 Model selection", style = "heading 2")
doc <- body_add_par(doc, "stepAIC() and regsubsets() were used for automated model selection. Both methods selected the same final model:")
doc <- body_add_par(doc, "medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat")
doc <- body_add_par(doc, "")
doc <- add_table(doc, all_comp, "Table 5. Comparison of all models")

final_r2 <- all_comp$adj_r_sq[all_comp$model == "stepAIC"]
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, paste0("The final model explains about ", round(final_r2 * 100, 1), "% of the variation in medv (adjusted R-squared = ", round(final_r2, 3), ")."))

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "3.4 Linear vs nonlinear models (diminishing returns)", style = "heading 2")
doc <- body_add_par(doc, "To test whether house size and neighbourhood status show diminishing returns, quadratic terms were added to the simple models:")
doc <- body_add_par(doc, "- medv ~ rm + rm^2")
doc <- body_add_par(doc, "- medv ~ lstat + lstat^2")
doc <- body_add_par(doc, "")
doc <- add_table(doc, nonlinear_comp, "Table 6. Linear vs nonlinear model comparison")
doc <- body_add_par(doc, "")
doc <- add_table(doc, quad_terms, "Table 7. Quadratic term estimates")
doc <- body_add_par(doc, "")
doc <- add_table(doc, anova_results, "Table 8. ANOVA tests: linear vs quadratic models")

rm_p <- anova_results$p_value[anova_results$comparison == "rm linear vs rm + rm^2"]
lstat_p <- anova_results$p_value[anova_results$comparison == "lstat linear vs lstat + lstat^2"]
rm_quad_est <- quad_terms$estimate[quad_terms$model == "rm + rm^2"]
lstat_quad_est <- quad_terms$estimate[quad_terms$model == "lstat + lstat^2"]

doc <- body_add_par(doc, "")
if (rm_p < 0.05) {
  if (rm_quad_est < 0) {
    doc <- body_add_par(doc, paste0("For rm, the quadratic term is significant (p = ", round(rm_p, 4), ") and negative. This suggests diminishing returns to additional rooms: each extra room adds less value as house size increases."))
  } else {
    doc <- body_add_par(doc, paste0("For rm, the quadratic term is significant (p = ", round(rm_p, 4), ") and positive. This suggests increasing returns to additional rooms."))
  }
} else {
  doc <- body_add_par(doc, paste0("For rm, the quadratic term is not significant (p = ", round(rm_p, 4), "). A linear relationship is sufficient."))
}

if (lstat_p < 0.05) {
  if (lstat_quad_est > 0) {
    doc <- body_add_par(doc, paste0("For lstat, the quadratic term is significant (p = ", round(lstat_p, 4), ") and positive. The negative effect of socioeconomic status weakens at higher levels of lstat."))
  } else {
    doc <- body_add_par(doc, paste0("For lstat, the quadratic term is significant (p = ", round(lstat_p, 4), ") and negative. The negative effect of socioeconomic status becomes stronger at higher levels of lstat."))
  }
} else {
  doc <- body_add_par(doc, paste0("For lstat, the quadratic term is not significant (p = ", round(lstat_p, 4), "). A linear relationship is sufficient."))
}

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 10. medv vs rm: linear vs quadratic fit", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "nonlinear_rm.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 11. medv vs lstat: linear vs quadratic fit", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "nonlinear_lstat.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "4. Model Diagnostics", style = "heading 1")
doc <- body_add_par(doc, "Table 9. Variance Inflation Factors", style = "heading 2")
doc <- add_table(doc, vif_tab)
doc <- body_add_par(doc, "rad and tax have VIF above 5, indicating moderate multicollinearity.")

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 7. Residuals vs fitted values", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "residuals.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 8. Q-Q plot of residuals", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "qq_plot.png"), 5.5, 3.8)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Figure 9. Regression diagnostics", style = "heading 2")
doc <- add_figure(doc, file.path(figures_dir, "diagnostics.png"), 5.8, 4.5)

doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "5. Interpretation", style = "heading 1")
doc <- body_add_par(doc, "In the final model, the strongest effects come from lstat, rm, nox, and dis. Each additional room increases median home value by about $3,800. A 10-point increase in lstat reduces median value by about $5,200.")
doc <- body_add_par(doc, "Multiple regression clearly outperforms simple regression (adjusted R-squared increases from about 0.54 to 0.73).")
doc <- body_add_par(doc, "")

doc <- body_add_par(doc, "6. Limitations", style = "heading 1")
doc <- body_add_par(doc, "- Cross-sectional data; causal claims are not possible.")
doc <- body_add_par(doc, "- Omitted variables such as renovation quality or local amenities.")
doc <- body_add_par(doc, "- medv is censored at $50,000.")
doc <- body_add_par(doc, "- Data reflect 1970s conditions.")
doc <- body_add_par(doc, "- Multicollinearity affects precision of individual coefficients.")
doc <- body_add_par(doc, "")

doc <- body_add_par(doc, "7. Conclusion", style = "heading 1")
doc <- body_add_par(doc, "Median housing prices in Boston are best explained by a combination of housing size, socioeconomic composition, environmental quality, accessibility, and school quality. For real estate analysis, a multivariate model is preferable to single-predictor models.")
doc <- body_add_par(doc, "")
doc <- body_add_par(doc, "Appendix", style = "heading 1")
doc <- body_add_par(doc, "All results can be reproduced with:")
doc <- body_add_par(doc, "source('install_packages.R'); source('run_all.R')")

dir.create("report", showWarnings = FALSE)
print(doc, target = out_file)
cat("Report saved to:", out_file, "\n")
