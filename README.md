# Boston Housing Prices — Regression Analysis

Final project for *Application of Statistics and Econometrics in Finance and Accounting*.

Research question: which factors best predict median housing prices (`medv`) in Boston?

Data: `Boston` dataset from the `MASS` package (506 observations, 14 variables).

## How to run

```r
source("run_all.R")
```

Install packages first if needed:

```r
source("install_packages.R")
```

## Files

- `R/01_data_load.R` — load data
- `R/02_eda.R` — exploratory analysis
- `R/03_simple_regression.R` — simple linear models
- `R/04_multiple_regression.R` — multiple regression
- `R/05_model_selection.R` — stepAIC and regsubsets
- `R/06_diagnostics.R` — VIF, residual plots
- `output/figures/` — plots
- `output/tables/` — csv tables

## Variables

| Variable | Description |
|----------|-------------|
| medv | Median home value ($1000s) — dependent variable |
| rm | Average number of rooms |
| lstat | % lower status population |
| nox | Nitric oxides concentration |
| ptratio | Pupil-teacher ratio |
| dis | Distance to employment centres |
| crim, zn, indus, chas, age, rad, tax, black | Other predictors |

See `?Boston` in R for full variable descriptions.
