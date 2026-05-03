
# --------------------------------------------------------------
# 2. Estimators
# --------------------------------------------------------------

# [0] Adjusted estimator

est_adj <- function(dat, K = NULL, eps = NULL) {
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  Yc <- (dat$Y - 1) / (K - 1)
  
  fit <- lm(Yc ~ A + W1 + W2 + W3 + W4 + W5, data = dat)
  
  est <- coef(fit)["A"]
  se  <- summary(fit)$coefficients["A", "Std. Error"]
  
  df <- df.residual(fit)
  t_val <- qt(0.975, df)
  
  ci_low  <- est - t_val * se
  ci_high <- est + t_val * se
  
  list(
    estimate = est,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high
  )
}


# [1] G-computation (parametric)
est_gcomp <- function(dat, K = NULL, eps = NULL) {
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  dat$Yc <- (dat$Y - 1) / (K - 1)
  
  # ---------- Q model ----------
  Qfit <- lm(Yc ~ A + W1 + W2 + W3 + W4 + W5, data = dat)
  
  dat1 <- dat; dat1$A <- 1
  dat0 <- dat; dat0$A <- 0
  
  Q1 <- predict(Qfit, newdata = dat1)
  Q0 <- predict(Qfit, newdata = dat0)
  
  # ---------- ATE ----------
  est <- mean(Q1 - Q0)
  
  return(list(
    estimate = est
  ))
}


# [1b] G-computation (Super Learner)

est_gcomp_sl <- function(dat, K = NULL, eps = NULL) {
  
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  dat$Yc <- (dat$Y - 1) / (K - 1)
  
  Yc <- dat$Yc
  
  
  # ---------- Q model ----------
  X_Q <- dat[, c("A","W1","W2","W3","W4","W5")]
  
  Qfit <- SuperLearner(
    Y = Yc,
    X = X_Q,
    SL.library = SL_lib,
    family = gaussian(),
    cvControl = list(V = 10, shuffle = FALSE)
  )
  
  dat1 <- dat; dat1$A <- 1
  dat0 <- dat; dat0$A <- 0
  
  X1 <- dat1[, c("A","W1","W2","W3","W4","W5")]
  X0 <- dat0[, c("A","W1","W2","W3","W4","W5")]
  
  Q1 <- predict(Qfit, newdata = X1)$pred
  Q0 <- predict(Qfit, newdata = X0)$pred
  
  # ---------- ATE ----------
  est <- mean(Q1 - Q0)
  
  return(list(
    estimate = est
  ))
}


# [2] IPW (parametric)

est_ipw <- function(dat, K = NULL, eps = NULL) {
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  Yc <- (dat$Y - 1) / (K - 1)
  A  <- dat$A
  n  <- nrow(dat)
  
  gfit <- glm(A ~ W1 + W2 + W3 + W4 + W5,
              data = dat,
              family = binomial())
  
  g <- predict(gfit, type = "response")
  g <- pmin(pmax(g, eps), 1 - eps)
  
  est <- mean(A * Yc / g) - mean((1 - A) * Yc / (1 - g))
  
  IF <- A * Yc / g - (1 - A) * Yc / (1 - g) - est
  se <- sqrt(mean(IF^2) / n)
  
  ci_low  <- est - qnorm(0.975) * se
  ci_high <- est + qnorm(0.975) * se
  
  list(
    estimate = est,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    ps = g,
    IF = IF
  )
}


# [2b] IPW (Super Learner)

est_ipw_sl <- function(dat, K = NULL, eps = NULL) {
 
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  Yc <- (dat$Y - 1) / (K - 1)
  A  <- dat$A
  n  <- nrow(dat)
  
  X_g <- dat[, c("W1", "W2", "W3", "W4", "W5")]
  
  gfit <- SuperLearner(
    Y = A,
    X = X_g,
    SL.library = SL_lib,
    family = binomial(),
    cvControl = list(V = 10, shuffle = FALSE)
  )
  
  g <- pmin(pmax(gfit$SL.predict, eps), 1 - eps)
  
  est <- mean(A * Yc / g) - mean((1 - A) * Yc / (1 - g))
  
  IF <- A * Yc / g - (1 - A) * Yc / (1 - g) - est
  se <- sqrt(mean(IF^2) / n)
  
  ci_low  <- est - qnorm(0.975) * se
  ci_high <- est + qnorm(0.975) * se
  
  list(
    estimate = est,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    ps = g,
    IF = IF
  )
}


# [3] Threshold TMLE (parametric)
est_tmle_bin <- function(dat, K = NULL, eps = NULL) {
  
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  clip01 <- function(p) pmin(pmax(p, eps), 1 - eps)
  
  n <- nrow(dat)
  est <- rep(NA_real_, K - 1)
  EIF_mat <- matrix(NA_real_, nrow = n, ncol = K - 1)
  
  for (k in 1:(K - 1)) {
    Yk <- as.numeric(dat$Y <= k)
    if (length(unique(Yk)) < 2) next
    
    Qfit <- glm(Yk ~ A + W1 + W2 + W3 + W4 + W5,
                data = dat, family = binomial())
    
    QAW <- clip01(predict(Qfit, type = "response"))
    
    dat1 <- dat
    dat1$A <- 1
    dat1$W5 <- factor(dat1$W5, levels = levels(dat$W5))
    
    dat0 <- dat
    dat0$A <- 0
    dat0$W5 <- factor(dat0$W5, levels = levels(dat$W5))
    
    Q1W <- clip01(predict(Qfit, newdata = dat1, type = "response"))
    Q0W <- clip01(predict(Qfit, newdata = dat0, type = "response"))
    
    gfit <- glm(A ~ W1 + W2 + W3 + W4 + W5,
                data = dat, family = binomial())
    
    gW <- clip01(predict(gfit, type = "response"))
    
    H <- dat$A / gW - (1 - dat$A) / (1 - gW)
    
    eps_fit <- glm(Yk ~ -1 + offset(logit(QAW)) + H,
                   family = binomial())
    eps_hat <- coef(eps_fit)[1]
    if (is.na(eps_hat)) eps_hat <- 0
    
    QAW_star <- clip01(expit(logit(QAW) + eps_hat * H))
    Q1W_star <- clip01(expit(logit(Q1W) + eps_hat / gW))
    Q0W_star <- clip01(expit(logit(Q0W) - eps_hat / (1 - gW)))
    
    psi_k <- mean(Q1W_star - Q0W_star)
    est[k] <- psi_k
    
    EIF_mat[, k] <- H * (Yk - QAW_star) + (Q1W_star - Q0W_star) - psi_k
  }
  
  valid_k <- which(!is.na(est))
  if (length(valid_k) == 0) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      EIF = rep(NA_real_, n)
    ))
  }
  
  psi_hat <- -mean(est[valid_k])
  EIF <- -rowMeans(EIF_mat[, valid_k, drop = FALSE])
  
  se <- sqrt(mean(EIF^2) / n)
  
  ci_low  <- psi_hat - qnorm(0.975) * se
  ci_high <- psi_hat + qnorm(0.975) * se
  
  list(
    estimate = psi_hat,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    EIF = EIF,
    est_by_k = est
  )
}


# [3b] Threshold TMLE (Super Learner)
est_tmle_bin_sl <- function(dat, K = NULL, eps = NULL) {
  
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  clip01 <- function(p) pmin(pmax(p, eps), 1 - eps)
  
  n <- nrow(dat)
  est <- rep(NA_real_, K - 1)
  EIF_mat <- matrix(NA_real_, nrow = n, ncol = K - 1)
  
  for (k in 1:(K - 1)) {
    Yk <- as.numeric(dat$Y <= k)
    if (length(unique(Yk)) < 2) next
    
    X_Q <- dat[, c("A", "W1", "W2", "W3", "W4", "W5")]
    
    Qfit <- SuperLearner(
      Y = Yk,
      X = X_Q,
      SL.library = SL_lib,
      family = binomial(),
      cvControl = list(V = 10, shuffle = FALSE)
    )
    
    QAW <- clip01(Qfit$SL.predict)
    
    dat1 <- dat
    dat1$A <- 1
    dat1$W5 <- factor(dat1$W5, levels = levels(dat$W5))
    
    dat0 <- dat
    dat0$A <- 0
    dat0$W5 <- factor(dat0$W5, levels = levels(dat$W5))
    
    X1_Q <- dat1[, c("A", "W1", "W2", "W3", "W4", "W5")]
    X0_Q <- dat0[, c("A", "W1", "W2", "W3", "W4", "W5")]
    
    Q1W <- clip01(predict(Qfit, newdata = X1_Q)$pred)
    Q0W <- clip01(predict(Qfit, newdata = X0_Q)$pred)
    
    X_g <- dat[, c("W1", "W2", "W3", "W4", "W5")]
    gfit <- SuperLearner(
      Y = dat$A,
      X = X_g,
      SL.library = SL_lib,
      family = binomial(),
      cvControl = list(V = 10)
    )
    
    gW <- clip01(gfit$SL.predict)
    
    H <- dat$A / gW - (1 - dat$A) / (1 - gW)
    
    eps_fit <- glm(Yk ~ -1 + offset(logit(QAW)) + H,
                   family = binomial())
    eps_hat <- coef(eps_fit)[1]
    if (is.na(eps_hat)) eps_hat <- 0
    
    QAW_star <- clip01(expit(logit(QAW) + eps_hat * H))
    Q1W_star <- clip01(expit(logit(Q1W) + eps_hat / gW))
    Q0W_star <- clip01(expit(logit(Q0W) - eps_hat / (1 - gW)))
    
    psi_k <- mean(Q1W_star - Q0W_star)
    est[k] <- psi_k
    
    EIF_mat[, k] <- H * (Yk - QAW_star) + (Q1W_star - Q0W_star) - psi_k
  }
  
  valid_k <- which(!is.na(est))
  if (length(valid_k) == 0) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      EIF = rep(NA_real_, n)
    ))
  }
  
  psi_hat <- -mean(est[valid_k])
  EIF <- -rowMeans(EIF_mat[, valid_k, drop = FALSE])
  
  se <- sqrt(mean(EIF^2) / n)
  
  ci_low  <- psi_hat - qnorm(0.975) * se
  ci_high <- psi_hat + qnorm(0.975) * se
  
  list(
    estimate = psi_hat,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    EIF = EIF,
    est_by_k = est
  )
}


# [4] Continuous TMLE (parametric)
est_tmle_cont <- function(dat, K = NULL, eps = NULL) {
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  dat$Yc <- (dat$Y - 1) / (K - 1)
  Yc <- dat$Yc
  A  <- dat$A
  n  <- nrow(dat)
  
  Qfit <- lm(Yc ~ A + W1 + W2 + W3 + W4 + W5, data = dat)
  QAW <- predict(Qfit, newdata = dat)
  
  dat1 <- dat
  dat1$A <- 1
  dat1$W5 <- factor(dat1$W5, levels = levels(dat$W5))
  
  dat0 <- dat
  dat0$A <- 0
  dat0$W5 <- factor(dat0$W5, levels = levels(dat$W5))
  
  Q1W <- predict(Qfit, newdata = dat1)
  Q0W <- predict(Qfit, newdata = dat0)
  
  gfit <- glm(A ~ W1 + W2 + W3 + W4 + W5,
              data = dat, family = binomial())
  gW <- predict(gfit, type = "response")
  gW <- pmin(pmax(gW, eps), 1 - eps)
  
  H <- A / gW - (1 - A) / (1 - gW)
  
  eps_fit <- lm(Yc ~ -1 + offset(QAW) + H)
  eps_hat <- coef(eps_fit)[1]
  if (is.na(eps_hat)) eps_hat <- 0
  
  QAW_star <- QAW + eps_hat * H
  Q1W_star <- Q1W + eps_hat / gW
  Q0W_star <- Q0W - eps_hat / (1 - gW)
  
  psi_hat <- mean(Q1W_star - Q0W_star)
  
  EIF <- H * (Yc - QAW_star) + (Q1W_star - Q0W_star) - psi_hat
  se <- sqrt(mean(EIF^2) / n)
  
  ci_low  <- psi_hat - qnorm(0.975) * se
  ci_high <- psi_hat + qnorm(0.975) * se
  
  list(
    estimate = psi_hat,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    EIF = EIF,
    eps_hat = eps_hat
  )
}


# [4b] Continuous TMLE (Super Learner)
est_tmle_cont_sl <- function(dat, K = NULL, eps = NULL) {
  
  dat$W5 <- factor(dat$W5, levels = c(0, 0.5, 1))
  dat$Yc <- (dat$Y - 1) / (K - 1)
  Yc <- dat$Yc
  A  <- dat$A
  n  <- nrow(dat)
  
  X_Q <- dat[, c("A", "W1", "W2", "W3", "W4", "W5")]
  
  Qfit <- SuperLearner(
    Y = Yc,
    X = X_Q,
    SL.library = SL_lib,
    family = gaussian(),
    cvControl = list(V = 10, shuffle = FALSE)
  )
  
  QAW <- Qfit$SL.predict
  
  dat1 <- dat
  dat1$A <- 1
  dat1$W5 <- factor(dat1$W5, levels = levels(dat$W5))
  
  dat0 <- dat
  dat0$A <- 0
  dat0$W5 <- factor(dat0$W5, levels = levels(dat$W5))
  
  X1_Q <- dat1[, c("A", "W1", "W2", "W3", "W4", "W5")]
  X0_Q <- dat0[, c("A", "W1", "W2", "W3", "W4", "W5")]
  
  Q1W <- predict(Qfit, newdata = X1_Q)$pred
  Q0W <- predict(Qfit, newdata = X0_Q)$pred
  
  X_g <- dat[, c("W1", "W2", "W3", "W4", "W5")]
  gfit <- SuperLearner(
    Y = A,
    X = X_g,
    SL.library = SL_lib,
    family = binomial(),
    cvControl = list(V = 10)
  )
  
  gW <- pmin(pmax(gfit$SL.predict, eps), 1 - eps)
  
  H <- A / gW - (1 - A) / (1 - gW)
  
  eps_fit <- lm(Yc ~ -1 + offset(QAW) + H)
  eps_hat <- coef(eps_fit)[1]
  if (is.na(eps_hat)) eps_hat <- 0
  
  QAW_star <- QAW + eps_hat * H
  Q1W_star <- Q1W + eps_hat / gW
  Q0W_star <- Q0W - eps_hat / (1 - gW)
  
  psi_hat <- mean(Q1W_star - Q0W_star)
  
  EIF <- H * (Yc - QAW_star) + (Q1W_star - Q0W_star) - psi_hat
  se <- sqrt(mean(EIF^2) / n)
  
  ci_low  <- psi_hat - qnorm(0.975) * se
  ci_high <- psi_hat + qnorm(0.975) * se
  
  list(
    estimate = psi_hat,
    se = se,
    conf.low = ci_low,
    conf.high = ci_high,
    EIF = EIF,
    eps_hat = eps_hat
  )
}

