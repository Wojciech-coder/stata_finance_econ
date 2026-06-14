# linear vs nonlinear comparison (rm^2, lstat^2)

# --- rm: linear vs quadratic --------------------------------------------------
rm_linear <- lm(medv ~ rm, data = Boston)
rm_quad   <- lm(medv ~ rm + I(rm^2), data = Boston)

summary(rm_linear)
summary(rm_quad)
anova(rm_linear, rm_quad)

# --- lstat: linear vs quadratic -----------------------------------------------
lstat_linear <- lm(medv ~ lstat, data = Boston)
lstat_quad   <- lm(medv ~ lstat + I(lstat^2), data = Boston)

summary(lstat_linear)
summary(lstat_quad)
anova(lstat_linear, lstat_quad)

# --- comparison table ---------------------------------------------------------
nonlinear_comp <- bind_rows(
  model_stats(rm_linear, "rm linear"),
  model_stats(rm_quad, "rm + rm^2"),
  model_stats(lstat_linear, "lstat linear"),
  model_stats(lstat_quad, "lstat + lstat^2")
)
write.csv(nonlinear_comp, file.path(tables_dir, "nonlinear_comparison.csv"), row.names = FALSE)
print(nonlinear_comp)

quad_terms <- bind_rows(
  broom::tidy(rm_quad) %>% filter(grepl("\\^2", term)),
  broom::tidy(lstat_quad) %>% filter(grepl("\\^2", term))
) %>%
  mutate(model = c("rm + rm^2", "lstat + lstat^2"), .before = 1) %>%
  select(model, term, estimate, std.error, p.value)

write.csv(quad_terms, file.path(tables_dir, "nonlinear_quad_terms.csv"), row.names = FALSE)
print(quad_terms)

anova_results <- data.frame(
  comparison = c("rm linear vs rm + rm^2", "lstat linear vs lstat + lstat^2"),
  p_value = c(
    anova(rm_linear, rm_quad)$`Pr(>F)`[2],
    anova(lstat_linear, lstat_quad)$`Pr(>F)`[2]
  )
)
write.csv(anova_results, file.path(tables_dir, "nonlinear_anova.csv"), row.names = FALSE)
print(anova_results)

# --- plots: linear vs quadratic fit -------------------------------------------
plot_nonlinear <- function(data, xvar, linear_model, quad_model, filename, title) {
  x_seq <- seq(min(data[[xvar]]), max(data[[xvar]]), length.out = 200)
  pred_linear <- data.frame(x = x_seq)
  names(pred_linear) <- xvar
  pred_quad <- pred_linear

  pred_linear$fit <- predict(linear_model, newdata = pred_linear)
  pred_quad$fit <- predict(quad_model, newdata = pred_quad)

  p <- ggplot(data, aes(x = .data[[xvar]], y = medv)) +
    geom_point(alpha = 0.45, color = "gray40") +
    geom_line(data = pred_linear, aes(x = .data[[xvar]], y = fit),
              color = "red", linewidth = 1) +
    geom_line(data = pred_quad, aes(x = .data[[xvar]], y = fit),
              color = "blue", linewidth = 1, linetype = "dashed") +
    labs(
      title = title,
      subtitle = "Red = linear, Blue dashed = quadratic",
      x = xvar,
      y = "medv"
    )

  save_plot(p, filename)
}

plot_nonlinear(Boston, "rm", rm_linear, rm_quad,
               "nonlinear_rm.png", "medv vs rm: linear vs quadratic")
plot_nonlinear(Boston, "lstat", lstat_linear, lstat_quad,
               "nonlinear_lstat.png", "medv vs lstat: linear vs quadratic")

# --- multiple model with squared terms ----------------------------------------
mr_nonlinear <- lm(medv ~ rm + I(rm^2) + lstat + I(lstat^2) + nox + dis, data = Boston)
summary(mr_nonlinear)

nonlinear_comp <- bind_rows(
  nonlinear_comp,
  model_stats(mr1, "mr1 linear"),
  model_stats(mr_nonlinear, "mr1 + rm^2 + lstat^2")
)
write.csv(nonlinear_comp, file.path(tables_dir, "nonlinear_comparison.csv"), row.names = FALSE)

anova(mr1, mr_nonlinear)

cat("\nNonlinear comparison complete.\n")
