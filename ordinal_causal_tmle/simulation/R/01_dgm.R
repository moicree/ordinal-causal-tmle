# 1. package
suppressPackageStartupMessages({
  library(MASS)
  library(tmle)
  library(SuperLearner)
  library(dplyr)  
  library(tidyr) 
  library(randomForest)
})

set.seed(1234)

# --------------------------------------------------------------
# 0) GLOBAL SETTINGS
# --------------------------------------------------------------

K     <- 4
theta <- c(-1.2, 0, 1.2)
eps   <- 0.05 # V=0.01, 0.05

SL_cv_control <- list(V =10) # V=5, 10


# --------------------------------------------------------------
# 1) DATA GENERATING MECHANISM
# --------------------------------------------------------------

generate_data <- function(n) {
  
  W1 <- rbinom(n, 1, 0.5)
  W2 <- rnorm(n)
  W3 <- rbinom(n, 1, 0.4)
  
  # Treatment mechanism (Propensity Score)
  g <- plogis(-0.3 + 1.5*W1 - 2.5*W2 + 1.2*W3 + 1.0*W2*W3)
  A <- rbinom(n, 1, g)
  
  # Latent outcome variable
  eta <- 1.5*A + 1.0*W1 - 2.0*W2 + 2.0*W2^2 + 0.8*W3
  
  # Ordinal probabilities
  p_cum <- matrix(0, nrow = n, ncol = K)
  for (k in 1:(K-1)) {
    p_cum[, k] <- plogis(theta[k] - eta)
  }
  p_cum[, K] <- 1.0
  
  prob <- matrix(0, nrow = n, ncol = K)
  prob[, 1] <- p_cum[, 1]
  for (k in 2:K) {
    prob[, k] <- p_cum[, k] - p_cum[, k-1]
  }
  
  prob <- pmax(prob, eps)
  prob <- prob / rowSums(prob)
  
  Y <- apply(prob, 1, function(p) sample(1:K, 1, prob = p))
  
  data.frame(W1, W2, W3, A, Y)
}
# --------------------------------------------------------------
# 2) TRUE ATE (Monte Carlo)
# --------------------------------------------------------------


true_psi <- function(n_mc = 2e5) {
  
  W1 <- rbinom(n_mc, 1, 0.5)
  W2 <- rnorm(n_mc)
  W3 <- rbinom(n_mc, 1, 0.4)
  
  simulate_Y <- function(A_fixed) {
    
    eta <- 1.5*A_fixed + 1.0*W1 - 2.0*W2 + 2.0*W2^2 + 0.8*W3
    
    cdf_mat <- matrix(0, nrow = n_mc, ncol = K-1)
    
    for(k in 1:(K-1)) {
      cdf_mat[, k] <- plogis(theta[k] - eta)
    }
    
    # E[Y] = 1 + sum(1 - P(Y ≤ k))
    E_Y <- 1 + rowSums(1 - cdf_mat)
    
    return(E_Y)
  }
  
  EY1 <- simulate_Y(1)
  EY0 <- simulate_Y(0)
  
  diff <- (EY1 - 1)/(K - 1) - (EY0 - 1)/(K - 1)
  
  psi_hat <- mean(diff)
  
  # Monte Carlo SE
  se <- sd(diff) / sqrt(n_mc)
  
  lower <- psi_hat - 1.96 * se
  upper <- psi_hat + 1.96 * se
  
  list(
    psi = psi_hat,
    se  = se,
    CI  = c(lower = lower, upper = upper)
  )
}

res <- true_psi()
psi_true <- res$psi

cat("True Psi:", round(res$psi, 4), "\n")
cat("SE:", round(res$se, 4), "\n")
cat("95% CI:", round(res$CI, 4), "\n")

