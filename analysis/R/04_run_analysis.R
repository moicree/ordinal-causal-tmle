run_all_estimators <- function(dat, K, eps = 0.05, B = 500) {
  results <- list()
  
  add_result <- function(name, est_fun) {
    est_res  <- est_fun(dat, K = K, eps = eps)
    boot_res <- boot_ci_debug(est_fun, dat, K = K, B = B, eps = eps)
    
    results[[name]] <<- list(
      estimate = est_res$estimate,
      se = est_res$se,
      wald.low = est_res$conf.low,
      wald.high = est_res$conf.high,
      boot.se = boot_res$boot.se,
      boot.low = boot_res$boot.low,
      boot.high = boot_res$boot.high
    )
  }
  
  add_result("adj", est_adj)
  add_result("gcomp", est_gcomp)
  add_result("gcomp_sl", est_gcomp_sl)
  add_result("ipw", est_ipw)
  add_result("ipw_sl", est_ipw_sl)
  add_result("tmle_bin", est_tmle_bin)
  add_result("tmle_bin_sl", est_tmle_bin_sl)
  add_result("tmle_cont", est_tmle_cont)
  add_result("tmle_cont_sl", est_tmle_cont_sl)
  
  results
}

to_df <- function(res_list) {
  do.call(rbind, lapply(names(res_list), function(nm) {
    x <- res_list[[nm]]
    data.frame(
      method = nm,
      estimate = x$estimate,
      se = x$se,
      wald.low = x$wald.low,
      wald.high = x$wald.high,
      boot.low = x$boot.low,
      boot.high = x$boot.high
    )
  }))
}