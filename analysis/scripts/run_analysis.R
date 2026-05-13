setwd("")


set.seed(2024)
source("analysis/R/01_data_prep.R")
source("analysis/R/02_estimators.R")
source("analysis/R/03_bootstrap.R")
source("analysis/R/04_summary.R")

K <- 4
eps <- 0.05
SL_lib <- c("SL.glm", "SL.randomForest")

res <- run_all_estimators(df_clean, K = K, B = 1000)

df <- to_df(res)

write.csv(df, "analysis/results/results.csv", row.names = FALSE)
print(df)
