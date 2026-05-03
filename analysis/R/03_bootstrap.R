# --------------------------------------------------------------
# 3. Bootstrap CI
# --------------------------------------------------------------

boot_ci_debug <- function(est_fun, dat, K, B = 500, eps = 0.05,
                          verbose = TRUE, print_every = 10) {
  
  n <- nrow(dat)
  boot_est <- numeric(B)
  
  if (verbose) {
    cat("\n==============================\n")
    cat(" Bootstrap START (B =", B, ")\n")
    cat("==============================\n")
  }
  
  for (b in 1:B) {
    
    # ---------------------------
    # Step 1: Resampling
    # ---------------------------
    idx <- sample(seq_len(n), replace = TRUE)
    dat_b <- dat[idx, ]
    
    # ---------------------------
    # Step 2: Estimation
    # ---------------------------
    est_b <- tryCatch(
      est_fun(dat_b, K = K, eps = eps)$estimate,
      error = function(e) {
        if (verbose) cat("Error at iteration", b, "\n")
        return(NA_real_)
      }
    )
    
    boot_est[b] <- est_b
    
    # ---------------------------
    # Step 3: Progress print
    # ---------------------------
    if (verbose && (b %% print_every == 0 || b == 1)) {
      cat(sprintf("Iteration %d / %d | current est = %.5f\n", b, B, est_b))
      
      cat("  mean so far:", mean(boot_est[1:b], na.rm = TRUE), "\n")
      cat("  sd   so far:", sd(boot_est[1:b], na.rm = TRUE), "\n")
    }
  }
  
  # ---------------------------
  # Step 4: Clean NA
  # ---------------------------
  boot_est <- boot_est[!is.na(boot_est)]
  
  if (length(boot_est) == 0) {
    warning("All bootstrap failed!")
    return(list(
      boot.se = NA,
      boot.low = NA,
      boot.high = NA,
      boot.est = NA
    ))
  }
  
  # ---------------------------
  # Step 5: Final CI
  # ---------------------------
  ci <- quantile(boot_est, c(0.025, 0.975), na.rm = TRUE)
  
  if (verbose) {
    cat("\n==============================\n")
    cat(" Bootstrap END\n")
    cat("==============================\n")
    cat("Final mean:", mean(boot_est), "\n")
    cat("Final SE  :", sd(boot_est), "\n")
    cat("95% CI    :", ci[1], ",", ci[2], "\n")
  }
  
  return(list(
    boot.se = sd(boot_est, na.rm = TRUE),
    boot.low = ci[1],
    boot.high = ci[2],
    boot.est = boot_est
  ))
}

