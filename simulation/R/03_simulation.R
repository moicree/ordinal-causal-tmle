
run_sim <- function(B = 50, n = 200,
                    misspec_Q = FALSE,
                    misspec_g = FALSE,
                    scenario_name = "") {
  
  out <- matrix(NA, B, 9)
  
  colnames(out) <- c(
    "adj",
    "gcomp_param",
    "gcomp_SL",
    "ipw_param",
    "ipw_SL",
    "tmle_bin_param",
    "tmle_bin_SL",
    "tmle_cont_param",
    "tmle_cont_SL"
  )
  
  cat("\nRunning", scenario_name, "\n")
  
  for (b in 1:B) {
    
    #진행률 표시
    cat(sprintf("\r%s: %d/%d (%.1f%%)",
                scenario_name, b, B, 100*b/B))
    
    dat <- generate_data(n)
    
    # -------------------------------
    # Parametric
    # -------------------------------
    out[b, "adj"] <- tryCatch(adj_est(dat), error = function(e) NA)
    
    out[b, "gcomp_param"] <- tryCatch(
      gcomp_param_est(dat, misspec_Q),
      error = function(e) NA
    )
    
    out[b, "ipw_param"] <- tryCatch(
      ipw_param_est(dat, misspec_g),
      error = function(e) NA
    )
    
    out[b, "tmle_bin_param"] <- tryCatch(
      tmle_bin_param_est(dat, K, misspec_Q, misspec_g),
      error = function(e) NA
    )
    
    out[b, "tmle_cont_param"] <- tryCatch(
      tmle_cont_param_est(dat, misspec_Q, misspec_g),
      error = function(e) NA
    )
    
    # -------------------------------
    # Super Learner
    # -------------------------------
    out[b, "gcomp_SL"] <- tryCatch(
      gcomp_SL_est(dat, misspec_Q),
      error = function(e) NA
    )
    
    out[b, "ipw_SL"] <- tryCatch(
      ipw_SL_est(dat, misspec_g),
      error = function(e) NA
    )
    
    out[b, "tmle_bin_SL"] <- tryCatch(
      tmle_bin_SL_est(dat, misspec_Q, misspec_g),
      error = function(e) NA
    )
    
    out[b, "tmle_cont_SL"] <- tryCatch(
      tmle_cont_SL_est(dat, misspec_Q, misspec_g),
      error = function(e) NA
    )
  }
  
  cat("\nDone:", scenario_name, "\n")
  
  as.data.frame(out)
}