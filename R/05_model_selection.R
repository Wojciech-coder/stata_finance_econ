# model selection

full_model <- lm(medv ~ ., data = Boston)
step_model <- stepAIC(full_model, direction = "both", trace = 0)
summary(step_model)

best_sub <- regsubsets(medv ~ ., data = Boston, nvmax = 14)
sub_sum <- summary(best_sub)
best_size <- which.max(sub_sum$adjr2)
cat("Best subset size:", best_size, "\n")
cat("Adj R2:", sub_sum$adjr2[best_size], "\n")

best_vars <- names(coef(best_sub, best_size))
best_vars <- best_vars[best_vars != "(Intercept)"]
regsub_model <- lm(as.formula(paste("medv ~", paste(best_vars, collapse = "+"))), data = Boston)
summary(regsub_model)

all_comparison <- bind_rows(
  slr_comparison,
  mr_comparison,
  model_stats(step_model, "stepAIC"),
  model_stats(regsub_model, "regsubsets")
)
write.csv(all_comparison, file.path(tables_dir, "model_comparison.csv"), row.names = FALSE)
print(all_comparison)

final_model <- step_model
