# diagnostics for final model

vif(final_model)
vif_tab <- data.frame(variable = names(vif(final_model)), VIF = as.numeric(vif(final_model)))
write.csv(vif_tab, file.path(tables_dir, "vif.csv"), row.names = FALSE)

png(file.path(figures_dir, "diagnostics.png"), width = 900, height = 700, res = 120)
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))
dev.off()

resid_df <- data.frame(fitted = fitted(final_model), residuals = residuals(final_model))

p_resid <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs fitted", x = "Fitted", y = "Residuals")
save_plot(p_resid, "residuals.png")

p_qq <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q plot of residuals")
save_plot(p_qq, "qq_plot.png")

cooks_d <- cooks.distance(final_model)
which(cooks_d > 4 / length(cooks_d))
