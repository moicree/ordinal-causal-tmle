# 5) SUMMARY TABLE
# --------------------------------------------------------------

summary_results <- function(res_list, psi_true) {
  
  safe_mean <- function(x) if(all(is.na(x))) NA else mean(x, na.rm=TRUE)
  safe_sd   <- function(x) if(sum(!is.na(x)) <= 1) NA else sd(x, na.rm=TRUE)
  safe_rmse <- function(x) if(all(is.na(x))) NA else sqrt(mean(x^2, na.rm=TRUE))
  
  bind_rows(lapply(names(res_list), function(s) {
    
    res_list[[s]] %>%
      pivot_longer(everything(), names_to = "method", values_to = "est") %>%
      mutate(
        scenario = s,
        error = est - psi_true
      ) %>%
      group_by(scenario, method) %>%
      summarise(
        Mean_Est = safe_mean(est),
        True_Psi = psi_true,
        Bias     = safe_mean(error),
        SD       = safe_sd(est),
        RMSE     = safe_rmse(error),
        NA_count = sum(is.na(est)),
        .groups  = "drop"
      )
  })) %>%
    mutate(
      method = factor(method, levels = c(
        "adj",
        "gcomp_param", "gcomp_SL",
        "ipw_param", "ipw_SL",
        "tmle_bin_param", "tmle_bin_SL",
        "tmle_cont_param", "tmle_cont_SL"
      ))
    ) %>%
    arrange(scenario, method) %>%
    mutate(across(c(Mean_Est, True_Psi, Bias, SD, RMSE), ~ round(., 4)))
}