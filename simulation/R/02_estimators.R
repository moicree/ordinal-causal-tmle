adj_est <- function(dat) {
  coef(lm((Y - 1)/(K - 1) ~ A + W1 + W2 + I(W2^2) + W3, data = dat))["A"]
}


gcomp_param_est <- function(dat, misspec_Q = FALSE) {
  
  dat$Yc   <- (dat$Y - 1)/(K - 1)
  dat$W2sq <- dat$W2^2
  
  fit <- if (!misspec_Q) {
    lm(Yc ~ A + W1 + W2 + W2sq + W3, data = dat)
  } else {
    lm(Yc ~ A + W1 + W3, data = dat)
  }
  
  dat1 <- dat; dat1$A <- 1
  dat0 <- dat; dat0$A <- 0
  
  mu1 <- predict(fit, newdata = dat1)
  mu0 <- predict(fit, newdata = dat0)
  
  mean(mu1 - mu0)
}


gcomp_SL_est <- function(dat, misspec_Q = FALSE) {
  
  Yc <- (dat$Y - 1)/(K - 1)
  dat$W2sq <- dat$W2^2
  
  X <- if (!misspec_Q) {
    dat[, c("A","W1","W2","W3","W2sq")]
  } else {
    dat[, c("A","W1","W3")]
  }
  
  fit <- SuperLearner(
    Y = Yc,
    X = X,
    SL.library = c("SL.glm","SL.randomForest"),
    family = gaussian(),
    cvControl = SL_cv_control
  )
  
  dat1 <- dat; dat1$A <- 1
  dat0 <- dat; dat0$A <- 0
  
  X1 <- if (!misspec_Q) {
    dat1[, c("A","W1","W2","W3","W2sq")]
  } else {
    dat1[, c("A","W1","W3")]
  }
  
  X0 <- if (!misspec_Q) {
    dat0[, c("A","W1","W2","W3","W2sq")]
  } else {
    dat0[, c("A","W1","W3")]
  }
  
  Q1 <- predict(fit, newdata = X1)$pred
  Q0 <- predict(fit, newdata = X0)$pred
  
  mean(Q1 - Q0)
}


ipw_param_est <- function(dat, misspec_g = FALSE) {
  
  Yc <- (dat$Y - 1)/(K - 1)
  
  gfit <- if (!misspec_g) {
    glm(A ~ W1 + W2 + W3+I(W2*W3), data = dat, family = binomial())
  } else {
    glm(A ~ W1 + W3, data = dat, family = binomial())
  }
  
  g <- predict(gfit, type = "response")
  g <- pmin(pmax(g, eps), 1-eps)
  
  mean(dat$A * Yc / g) - mean((1 - dat$A) * Yc / (1 - g))
}

ipw_SL_est <- function(dat, misspec_g = FALSE) {
  
  Yc <- (dat$Y - 1)/(K - 1)
  
  # feature 생성
  dat$W2sq <- dat$W2^2
  dat$W2W3 <- dat$W2 * dat$W3
  
  # X 구성 (핵심)
  X <- if (!misspec_g) {
    dat[, c("W1","W2","W3","W2W3")]
  } else {
    dat[, c("W1","W3")]
  }
  
  # Super Learner
  gfit <- SuperLearner(
    Y = dat$A,
    X = X,
    SL.library = c("SL.glm","SL.randomForest"),
    family = binomial(),
    cvControl = SL_cv_control
  )
  
  g <- pmin(pmax(gfit$SL.predict, eps), 1-eps)
  
  mean(dat$A * Yc / g) - mean((1 - dat$A) * Yc / (1 - g))
}

tmle_bin_param_est <- function(dat,
                               K,
                               misspec_Q = FALSE,
                               misspec_g = FALSE) {
  
  dat$W2sq <- dat$W2^2
  
  est <- rep(NA_real_, K - 1)
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  
  for (k in 1:(K - 1)) {
    
    
    Yk <- as.numeric(dat$Y <= k)
    
    if (length(unique(Yk)) < 2) {
      est[k] <- NA
      next
    }
    
    
    Qfit <- if (!misspec_Q) {
      glm(Yk ~ A + W1 + W2 + W2sq + W3,
          data = dat, family = binomial())
    } else {
      glm(Yk ~ A + W1 + W3,
          data = dat, family = binomial())
    }
    
    QAW <- predict(Qfit, type = "response")
    
    
    dat1 <- dat; dat1$A <- 1
    dat0 <- dat; dat0$A <- 0
    
    Q1W <- predict(Qfit, newdata = dat1, type = "response")
    Q0W <- predict(Qfit, newdata = dat0, type = "response")
    
    
    gfit <- if (!misspec_g) {
      glm(A ~ W1 + W2 +  W3+I(W2*W3),
          data = dat, family = binomial())
    } else {
      glm(A ~ W1 + W3,
          data = dat, family = binomial())
    }
    
    gW <- predict(gfit, type = "response")
    gW <- pmin(pmax(gW, eps), 1-eps)
    
    H <- dat$A / gW - (1 - dat$A) / (1 - gW)
    
    
    eps_fit <- glm(Yk ~ -1 + offset(logit(QAW)) + H,
                   family = binomial())
    
    eps_hat <- coef(eps_fit)[1]
    
    QAW_star <- expit(logit(QAW) + eps_hat * H)
    Q1W_star <- expit(logit(Q1W) + eps_hat / gW)
    Q0W_star <- expit(logit(Q0W) - eps_hat / (1 - gW))
    
    
    est[k] <- mean(Q1W_star - Q0W_star)
  }
  
  
  if (all(is.na(est))) return(NA)
  
  return(-mean(est, na.rm = TRUE))
}



tmle_bin_SL_est <- function(dat,
                            misspec_Q = FALSE,
                            misspec_g = FALSE) {
  
  dat$W2sq <- dat$W2^2
  dat$W2W3 <- dat$W2 * dat$W3
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  clip01 <- function(p, eps = 1e-6) pmin(pmax(p, eps), 1 - eps)
  
  est <- rep(NA_real_, K - 1)
  
  for (k in 1:(K - 1)) {
    
    est[k] <- tryCatch({
      
      Yk <- as.numeric(dat$Y <= k)
      if (length(unique(Yk)) < 2) return(NA_real_)
      
      X_Q <- if (!misspec_Q) {
        dat[, c("A","W1","W2","W3","W2sq")]
      } else {
        dat[, c("A","W1","W3")]
      }
      
      X_g <- if (!misspec_g) {
        dat[, c("W1","W2","W3","W2W3")]
      } else {
        dat[, c("W1","W3")]
      }
      
      Qfit <- SuperLearner(
        Y = Yk,
        X = X_Q,
        SL.library = c("SL.glm", "SL.randomForest"),
        family = binomial(),
        cvControl = SL_cv_control
      )
      
      QAW <- clip01(Qfit$SL.predict)
      
      dat1 <- dat
      dat1$A <- 1
      dat0 <- dat
      dat0$A <- 0
      
      X1_Q <- if (!misspec_Q) {
        dat1[, c("A","W1","W2","W3","W2sq")]
      } else {
        dat1[, c("A","W1","W3")]
      }
      
      X0_Q <- if (!misspec_Q) {
        dat0[, c("A","W1","W2","W3","W2sq")]
      } else {
        dat0[, c("A","W1","W3")]
      }
      
      Q1W <- clip01(predict(Qfit, newdata = X1_Q)$pred)
      Q0W <- clip01(predict(Qfit, newdata = X0_Q)$pred)
      
      gfit <- SuperLearner(
        Y = dat$A,
        X = X_g,
        SL.library = c("SL.glm", "SL.randomForest"),
        family = binomial(),
        cvControl = SL_cv_control
      )
      
      gW <- clip01(gfit$SL.predict, eps = eps)
      
      H <- dat$A / gW - (1 - dat$A) / (1 - gW)
      
      eps_fit <- glm(
        Yk ~ -1 + offset(logit(QAW)) + H,
        family = binomial()
      )
      
      eps_hat <- coef(eps_fit)[1]
      
      Q1W_star <- expit(logit(Q1W) + eps_hat / gW)
      Q0W_star <- expit(logit(Q0W) - eps_hat / (1 - gW))
      
      mean(Q1W_star - Q0W_star)
      
    }, error = function(e) NA_real_)
  }
  
  if (all(is.na(est))) return(NA_real_)
  
  -mean(est, na.rm = TRUE)
}



tmle_cont_param_est <- function(dat,
                                misspec_Q = FALSE,
                                misspec_g = FALSE) {
  
  
  dat$W2sq <- dat$W2^2
  dat$A <- as.numeric(dat$A == 1)
  
  Yc <- (dat$Y - 1)/(K - 1)
  Yc <- pmin(pmax(Yc, 0), 1)
  
  
  Qfit <- if (!misspec_Q) {
    lm(Yc ~ A + W1 + W2 + W2sq + W3, data = dat)
  } else {
    lm(Yc ~ A + W1 + W3, data = dat)
  }
  
  QAW <- predict(Qfit)
  
  
  dat1 <- dat; dat1$A <- 1
  dat0 <- dat; dat0$A <- 0
  
  Q1W <- predict(Qfit, newdata = dat1)
  Q0W <- predict(Qfit, newdata = dat0)
  
  
  gfit <- if (!misspec_g) {
    glm(A ~ W1 + W2  + W3+I(W2*W3),
        data = dat, family = binomial())
  } else {
    glm(A ~ W1 + W3,
        data = dat, family = binomial())
  }
  
  gW <- predict(gfit, type = "response")
  gW <- pmin(pmax(gW, eps), 1-eps)
  
  
  H <- dat$A / gW - (1 - dat$A) / (1 - gW)
  
  
  eps_fit <- lm(Yc ~ -1 + offset(QAW) + H)
  eps_hat <- coef(eps_fit)[1]
  
  QAW_star <- QAW + eps_hat * H
  Q1W_star <- Q1W + eps_hat / gW
  Q0W_star <- Q0W - eps_hat / (1 - gW)
  
  
  psi <- mean(Q1W_star - Q0W_star)
  
  return(psi)
}



tmle_cont_SL_est <- function(dat,
                             misspec_Q = FALSE,
                             misspec_g = FALSE) {
  
  dat$W2sq <- dat$W2^2
  dat$W2W3 <- dat$W2 * dat$W3
  
  Yc <- (dat$Y - 1) / (K - 1)
  Yc <- pmin(pmax(Yc, 0), 1)
  
  X_Q <- if (!misspec_Q) {
    dat[, c("A","W1","W2","W3","W2sq")]
  } else {
    dat[, c("A","W1","W3")]
  }
  
  X_g <- if (!misspec_g) {
    dat[, c("W1","W2","W3","W2W3")]
  } else {
    dat[, c("W1","W3")]
  }
  
  psi <- tryCatch({
    
    Qfit <- SuperLearner(
      Y = Yc,
      X = X_Q,
      SL.library = c("SL.glm", "SL.randomForest"),
      family = gaussian(),
      cvControl = SL_cv_control
    )
    
    QAW <- Qfit$SL.predict
    
    dat1 <- dat
    dat1$A <- 1
    dat0 <- dat
    dat0$A <- 0
    
    X1_Q <- if (!misspec_Q) {
      dat1[, c("A","W1","W2","W3","W2sq")]
    } else {
      dat1[, c("A","W1","W3")]
    }
    
    X0_Q <- if (!misspec_Q) {
      dat0[, c("A","W1","W2","W3","W2sq")]
    } else {
      dat0[, c("A","W1","W3")]
    }
    
    Q1W <- predict(Qfit, newdata = X1_Q)$pred
    Q0W <- predict(Qfit, newdata = X0_Q)$pred
    
    gfit <- SuperLearner(
      Y = dat$A,
      X = X_g,
      SL.library = c("SL.glm", "SL.randomForest"),
      family = binomial(),
      cvControl = SL_cv_control
    )
    
    gW <- pmin(pmax(gfit$SL.predict, eps), 1 - eps)
    
    H <- dat$A / gW - (1 - dat$A) / (1 - gW)
    
    eps_fit <- lm(Yc ~ -1 + offset(QAW) + H)
    eps_hat <- coef(eps_fit)[1]
    
    Q1W_star <- Q1W + eps_hat / gW
    Q0W_star <- Q0W - eps_hat / (1 - gW)
    
    mean(Q1W_star - Q0W_star)
    
  }, error = function(e) NA_real_)
  
  psi
}
