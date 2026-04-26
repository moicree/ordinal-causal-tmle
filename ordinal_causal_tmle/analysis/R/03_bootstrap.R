boot_ci_debug <- function(est_fun, dat, K, B = 500, eps = 0.05,
                          verbose = TRUE, print_every = 10) {
  
  n <- nrow(dat)
  boot_est <- numeric(B)
  
  for (b in 1:B) {
    idx <- sample(seq_len(n), replace = TRUE)
    dat_b <- dat[idx, ]
    
    est_b <- tryCatch(
      est_fun(dat_b, K = K, eps = eps)$estimate,
      error = function(e) NA_real_
    )
    
    boot_est[b] <- est_b
  }
  
  boot_est <- boot_est[!is.na(boot_est)]
  
  ci <- quantile(boot_est, c(0.025, 0.975), na.rm = TRUE)
  
  list(
    boot.se = sd(boot_est, na.rm = TRUE),
    boot.low = ci[1],
    boot.high = ci[2],
    boot.est = boot_est
  )
}