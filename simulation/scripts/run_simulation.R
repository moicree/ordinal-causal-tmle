setwd("")
set.seed(1234)

source("simulation/R/01_dgm.R")
source("simulation/R/02_estimators.R")
source("simulation/R/03_simulation.R")
source("simulation/R/04_summary.R")


K <- 4
theta <- c(-1.2, 0, 1.2)
eps <- 0.05 # eps=0.01, 0.05
SL_cv_control <- list(V = 10,shuffle = FALSE) # V=5, 10


# true value
psi_true <- true_psi()$psi

# run
B_sim <- 500 # B=500
n_sim <- 500   # n=200,500 

res_list <- list(
  S1 = run_sim(B_sim, n_sim, FALSE, FALSE, "S1"),
  S2 = run_sim(B_sim, n_sim, TRUE,  FALSE, "S2"),
  S3 = run_sim(B_sim, n_sim, FALSE, TRUE,  "S3"),
  S4 = run_sim(B_sim, n_sim, TRUE,  TRUE,  "S4"))
  


summary_tab <- summary_results(res_list, psi_true)

write.csv(summary_tab, "simulation/results/summary.csv", row.names=FALSE)