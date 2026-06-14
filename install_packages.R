packages <- c("MASS", "ggplot2", "corrplot", "car", "leaps", "broom", "dplyr", "tidyr")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
