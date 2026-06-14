# multiple regression models

mr1 <- lm(medv ~ rm + lstat + nox + dis, data = Boston)
mr2 <- lm(medv ~ rm + age + tax + ptratio, data = Boston)
mr3 <- lm(medv ~ rm + lstat + ptratio + dis + nox + chas, data = Boston)

summary(mr1)
summary(mr2)
summary(mr3)

mr_comparison <- bind_rows(
  model_stats(mr1, "mr1"),
  model_stats(mr2, "mr2"),
  model_stats(mr3, "mr3")
)
write.csv(mr_comparison, file.path(tables_dir, "multiple_models_comparison.csv"), row.names = FALSE)
print(mr_comparison)
