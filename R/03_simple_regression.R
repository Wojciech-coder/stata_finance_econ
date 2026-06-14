# simple linear regression models

slr_rm <- lm(medv ~ rm, data = Boston)
slr_lstat <- lm(medv ~ lstat, data = Boston)
slr_nox <- lm(medv ~ nox, data = Boston)
slr_ptratio <- lm(medv ~ ptratio, data = Boston)

summary(slr_rm)
summary(slr_lstat)
summary(slr_nox)
summary(slr_ptratio)

slr_comparison <- bind_rows(
  model_stats(slr_rm, "rm"),
  model_stats(slr_lstat, "lstat"),
  model_stats(slr_nox, "nox"),
  model_stats(slr_ptratio, "ptratio")
)
write.csv(slr_comparison, file.path(tables_dir, "simple_models_comparison.csv"), row.names = FALSE)
print(slr_comparison)

# plots
plot_slr <- function(model, xvar) {
  ggplot(Boston, aes(x = .data[[xvar]], y = medv)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(x = xvar, y = "medv", title = paste("medv ~", xvar))
}

save_plot(plot_slr(slr_rm, "rm"), "slr_rm.png")
save_plot(plot_slr(slr_lstat, "lstat"), "slr_lstat.png")
save_plot(plot_slr(slr_nox, "nox"), "slr_nox.png")
save_plot(plot_slr(slr_ptratio, "ptratio"), "slr_ptratio.png")
