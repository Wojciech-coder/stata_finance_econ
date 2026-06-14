packages <- c("MASS", "ggplot2", "corrplot", "car", "leaps", "broom", "dplyr", "tidyr", "officer", "flextable")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
