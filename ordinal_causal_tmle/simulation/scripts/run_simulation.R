setwd("C:/Users/par/Desktop/ordinal_causal_tmle")


source("simulation/R/01_dgm.R")
source("simulation/R/02_estimators.R")
source("simulation/R/03_simulation.R")
source("simulation/R/04_summary.R")

K <- 4
theta <- c(-1.2, 0, 1.2)
eps <- 0.05

# true value
psi_true <- true_psi()$psi

# run
res_list <- list(
  S1 = run_sim(50, 20, FALSE, FALSE),
  S2 = run_sim(50, 20, TRUE,  FALSE),
  S3 = run_sim(50, 20, FALSE, TRUE),
  S4 = run_sim(50, 20, TRUE,  TRUE)
)

summary_tab <- summary_results(res_list, psi_true)

write.csv(summary_tab, "simulation/results/summary.csv", row.names=FALSE)