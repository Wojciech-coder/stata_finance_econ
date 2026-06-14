library(MASS)
library(ggplot2)
library(corrplot)
library(car)
library(leaps)
library(broom)
library(dplyr)
library(tidyr)

options(digits = 3)

figures_dir <- "output/figures"
tables_dir <- "output/tables"
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

save_plot <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(figures_dir, name), p, width = w, height = h)
}

model_stats <- function(model, name) {
  s <- summary(model)
  data.frame(
    model = name,
    r_squared = s$r.squared,
    adj_r_sq = s$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model)
  )
}
