suppressPackageStartupMessages({
  library(SuperLearner)
  library(dplyr)
  library(randomForest)
  library(tidyr)
  library(survival)
})


# --------------------------------------------------------------
# 1. Data preparation
# --------------------------------------------------------------


data(pbc)

df_clean <- pbc %>%
  filter(!is.na(trt), !is.na(stage)) %>%
  mutate(
    A  = ifelse(trt == 1, 1, 0),
    Y  = as.numeric(stage),
    W1 = age,
    W2 = bili,
    W3 = albumin,
    W4 = protime,
    W5 = factor(edema, levels = c(0, 0.5, 1))
  ) %>%
  select(Y, A, W1, W2, W3, W4, W5) %>%
  filter(complete.cases(.))

