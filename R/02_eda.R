# summary stats
summary_stats <- Boston %>%
  summarise(across(everything(), list(
    mean = ~mean(.x), sd = ~sd(.x), min = ~min(.x), max = ~max(.x)
  ))) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("variable", "measure"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = measure, values_from = value)

write.csv(summary_stats, file.path(tables_dir, "summary_stats.csv"), row.names = FALSE)
print(summary_stats)

# histogram of medv
p1 <- ggplot(Boston, aes(x = medv)) +
  geom_histogram(bins = 30, fill = "gray70", color = "white") +
  labs(title = "Distribution of medv", x = "Median home value ($1000s)", y = "Count")
save_plot(p1, "medv_histogram.png")

# histograms of selected predictors
preds <- c("rm", "lstat", "nox", "ptratio", "crim", "dis")
Boston_long <- Boston %>% select(all_of(preds)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

p2 <- ggplot(Boston_long, aes(x = value)) +
  geom_histogram(bins = 25, fill = "gray70", color = "white") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Predictor distributions", x = NULL, y = "Count")
save_plot(p2, "predictor_histograms.png", w = 10, h = 6)

# correlation matrix
cor_matrix <- cor(Boston)
write.csv(cor_matrix, file.path(tables_dir, "correlation_matrix.csv"))

png(file.path(figures_dir, "correlation_matrix.png"), width = 800, height = 700, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
dev.off()

# high correlations between predictors
predictors <- setdiff(names(Boston), "medv")
high_cor <- which(abs(cor_matrix[predictors, predictors]) > 0.7 &
                    upper.tri(cor_matrix[predictors, predictors]), arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  high_cor_df <- data.frame(
    var1 = predictors[high_cor[, 1]],
    var2 = predictors[high_cor[, 2]],
    r = cor_matrix[predictors, predictors][high_cor]
  )
  write.csv(high_cor_df, file.path(tables_dir, "high_correlations.csv"), row.names = FALSE)
  print(high_cor_df)
}

# scatterplots
for (v in c("rm", "lstat", "nox", "ptratio")) {
  p <- ggplot(Boston, aes(x = .data[[v]], y = medv)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(x = v, y = "medv", title = paste("medv vs", v))
  save_plot(p, paste0("scatter_medv_", v, ".png"))
}
