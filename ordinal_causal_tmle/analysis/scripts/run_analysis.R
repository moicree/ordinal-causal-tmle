setwd("C:/Users/par/Desktop/ordinal_causal_tmle")

source("analysis/R/01_data_prep.R")
source("analysis/R/02_estimators.R")
source("analysis/R/03_bootstrap.R")
source("analysis/R/04_run_analysis.R")

res <- run_all_estimators(df_clean, K = K, B = 10)

df <- to_df(res)

write.csv(df, "analysis/results/results.csv", row.names = FALSE)
print(df)
