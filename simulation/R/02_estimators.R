# --------------------------------------------------------------
# 3) ESTIMATORS

adj_est <- function(dat) {
  coef(lm((Y - 1)/(K - 1) ~ A + W1 + W2 + I(W2^2) + W3, data = dat))["A"]
}


gcomp_param_est <- function(dat, misspec_Q = FALSE) {
  
  dat$Yc   <- (dat$Y - 1)/(K - 1)
  dat$W2sq <- dat$W2^2
  
  fit <- if (!misspec_Q) {
    # correctly specified 
    lm(Yc ~ A + W1 + W2 + W2sq + W3 + sin(W3), data = dat)
  } else {
    # misspecified 
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
  dat$sinW3 <- sin(dat$W3)
  
  X <- if (!misspec_Q) {
    # correctly specified 
    dat[, c("A","W1","W2","W3","W2sq","sinW3")]
  } else {
    # misspecified
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
  
  
  dat1$W2sq <- dat1$W2^2
  dat0$W2sq <- dat0$W2^2
  dat1$sinW3 <- sin(dat1$W3)
  dat0$sinW3 <- sin(dat0$W3)
  
  X1 <- if (!misspec_Q) {
    dat1[, c("A","W1","W2","W3","W2sq","sinW3")]
  } else {
    dat1[, c("A","W1","W3")]
  }
  
  X0 <- if (!misspec_Q) {
    dat0[, c("A","W1","W2","W3","W2sq","sinW3")]
  } else {
    dat0[, c("A","W1","W3")]
  }
  
  Q1 <- predict(fit, newdata = X1)$pred
  Q0 <- predict(fit, newdata = X0)$pred
  
  Q1 <- pmin(pmax(Q1, eps), 1 - eps)
  Q0 <- pmin(pmax(Q0, eps), 1 - eps)
  
  mean(Q1 - Q0)
}




ipw_param_est <- function(dat, misspec_g = FALSE) {
  
  Yc <- (dat$Y - 1)/(K - 1)
  
  
  dat$W2W3  <- dat$W2 * dat$W3
  dat$logW2 <- log(abs(dat$W2) + 1)
  
  gfit <- if (!misspec_g) {
    # correctly specified 
    glm(A ~ W1 + W2 + W3 + W2W3 + logW2,
        data = dat, family = binomial())
  } else {
    # misspecified 
    glm(A ~ W1 + W3,
        data = dat, family = binomial())
  }
  
  g <- predict(gfit, type = "response")
  g <- pmin(pmax(g, eps), 1 - eps)
  
  mean(dat$A * Yc / g) - mean((1 - dat$A) * Yc / (1 - g))
}


ipw_SL_est <- function(dat, misspec_g = FALSE) {
  
  Yc <- (dat$Y - 1)/(K - 1)
  
  
  dat$W2W3  <- dat$W2 * dat$W3
  dat$logW2 <- log(abs(dat$W2) + 1)
  
  
  X <- if (!misspec_g) {
    # correctly specified
    dat[, c("W1","W2","W3","W2W3","logW2")]
  } else {
    # misspecified 
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
  
  g <- pmin(pmax(gfit$SL.predict, eps), 1 - eps)
  
  mean(dat$A * Yc / g) - mean((1 - dat$A) * Yc / (1 - g))
}



tmle_bin_param_est <- function(dat,
                               K,
                               misspec_Q = FALSE,
                               misspec_g = FALSE) {
  
  
  dat$W2sq  <- dat$W2^2
  dat$sinW3 <- sin(dat$W3)
  dat$W2W3  <- dat$W2 * dat$W3
  dat$logW2 <- log(abs(dat$W2) + 1)
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  
  est <- rep(NA_real_, K - 1)
  
  for (k in 1:(K - 1)) {
    
    est[k] <- tryCatch({
      
      Yk <- as.numeric(dat$Y <= k)
      if (length(unique(Yk)) < 2) return(NA_real_)
      
      # ---------------- Q model ----------------
      Qfit <- if (!misspec_Q) {
        glm(Yk ~ A + W1 + W2 + W2sq + W3 + sinW3,
            data = dat, family = binomial())
      } else {
        glm(Yk ~ A + W1 + W3,
            data = dat, family = binomial())
      }
      
      QAW <- predict(Qfit, type = "response")
      QAW <- pmin(pmax(QAW, eps), 1 - eps)
      
      dat1 <- dat; dat1$A <- 1
      dat0 <- dat; dat0$A <- 0
      
      Q1W <- predict(Qfit, newdata = dat1, type = "response")
      Q1W <- pmin(pmax(Q1W, eps), 1 - eps)
      
      Q0W <- predict(Qfit, newdata = dat0, type = "response")
      Q0W <- pmin(pmax(Q0W, eps), 1 - eps)
      
      # ---------------- g model ----------------
      gfit <- if (!misspec_g) {
        glm(A ~ W1 + W2 + W3 + W2W3 + logW2,
            data = dat, family = binomial())
      } else {
        glm(A ~ W1 + W3,
            data = dat, family = binomial())
      }
      
      gW <- predict(gfit, type = "response")
      gW <- pmin(pmax(gW, eps), 1 - eps)
      
      # ---------------- clever covariate ----------------
      H <- dat$A / gW - (1 - dat$A) / (1 - gW)
      
      # ---------------- targeting ----------------
      eps_fit <- glm(Yk ~ -1 + offset(logit(QAW)) + H,
                     family = binomial())
      
      eps_hat <- coef(eps_fit)[1]
      if (is.na(eps_hat)) eps_hat <- 0
      
      Q1W_star <- expit(logit(Q1W) + eps_hat / gW)
      Q0W_star <- expit(logit(Q0W) - eps_hat / (1 - gW))
      
    
      mean(Q1W_star - Q0W_star)
      
    }, error = function(e) NA_real_)
  }
  
  
  if (all(is.na(est))) return(NA_real_)
  
  return(-mean(est, na.rm = TRUE))
}




tmle_bin_SL_est <- function(dat,
                            K,
                            misspec_Q = FALSE,
                            misspec_g = FALSE) {
  
  dat$W2sq  <- dat$W2^2
  dat$sinW3 <- sin(dat$W3)
  dat$W2W3  <- dat$W2 * dat$W3
  dat$logW2 <- log(abs(dat$W2) + 1)
  
  logit <- function(p) log(p / (1 - p))
  expit <- function(x) 1 / (1 + exp(-x))
  
  est <- rep(NA_real_, K - 1)
  
  for (k in 1:(K - 1)) {
    
    est[k] <- tryCatch({
      
      Yk <- as.numeric(dat$Y <= k)
      if (length(unique(Yk)) < 2) return(NA_real_)
      
      X_Q <- if (!misspec_Q) {
        dat[, c("A","W1","W2","W3","W2sq","sinW3")]
      } else {
        dat[, c("A","W1","W3")]
      }
      
      X_g <- if (!misspec_g) {
        dat[, c("W1","W2","W3","W2W3","logW2")]
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
      
      QAW <- pmin(pmax(Qfit$SL.predict, eps), 1 - eps)
      
      dat1 <- dat; dat1$A <- 1
      dat0 <- dat; dat0$A <- 0
      
      X1_Q <- if (!misspec_Q) {
        dat1[, c("A","W1","W2","W3","W2sq","sinW3")]
      } else {
        dat1[, c("A","W1","W3")]
      }
      
      X0_Q <- if (!misspec_Q) {
        dat0[, c("A","W1","W2","W3","W2sq","sinW3")]
      } else {
        dat0[, c("A","W1","W3")]
      }
      
      Q1W <- pmin(pmax(predict(Qfit, newdata = X1_Q)$pred, eps), 1 - eps)
      Q0W <- pmin(pmax(predict(Qfit, newdata = X0_Q)$pred, eps), 1 - eps)
      
      gfit <- SuperLearner(
        Y = dat$A,
        X = X_g,
        SL.library = c("SL.glm", "SL.randomForest"),
        family = binomial(),
        cvControl = SL_cv_control
      )
      
      gW <- pmin(pmax(gfit$SL.predict, eps), 1 - eps)
      
      H <- dat$A / gW - (1 - dat$A) / (1 - gW)
      
      eps_fit <- glm(
        Yk ~ -1 + offset(logit(QAW)) + H,
        family = binomial()
      )
      
      eps_hat <- coef(eps_fit)[1]
      if (is.na(eps_hat)) eps_hat <- 0
      
      Q1W_star <- expit(logit(Q1W) + eps_hat / gW)
      Q0W_star <- expit(logit(Q0W) - eps_hat / (1 - gW))
      
      mean(Q1W_star - Q0W_star)
      
    }, error = function(e) NA_real_)
  }
  
  if (all(is.na(est))) return(NA_real_)
  
  return(-mean(est, na.rm = TRUE))
  
}


  
  tmle_cont_param_est <- function(dat,
                                  misspec_Q = FALSE,
                                  misspec_g = FALSE) {
    
    psi <- tryCatch({
      
      # feature 생성
      dat$W2sq  <- dat$W2^2
      dat$sinW3 <- sin(dat$W3)
      dat$W2W3  <- dat$W2 * dat$W3
      dat$logW2 <- log(abs(dat$W2) + 1)
      
      
      
      Yc <- (dat$Y - 1)/(K - 1)
      Yc <- pmin(pmax(Yc, 0), 1)
      
      # ---------------- Q model ----------------
      Qfit <- if (!misspec_Q) {
        lm(Yc ~ A + W1 + W2 + W2sq + W3 + sinW3, data = dat)
      } else {
        lm(Yc ~ A + W1 + W3, data = dat)
      }
      
      QAW <- predict(Qfit)
      QAW <- pmin(pmax(QAW, eps), 1 - eps)
      
      dat1 <- dat; dat1$A <- 1
      dat0 <- dat; dat0$A <- 0
      
      Q1W <- predict(Qfit, newdata = dat1)
      Q0W <- predict(Qfit, newdata = dat0)
      
      Q1W <- pmin(pmax(Q1W, eps), 1 - eps)
      Q0W <- pmin(pmax(Q0W, eps), 1 - eps)
      
      # ---------------- g model ----------------
      gfit <- if (!misspec_g) {
        glm(A ~ W1 + W2 + W3 + W2W3 + logW2,
            data = dat, family = binomial())
      } else {
        glm(A ~ W1 + W3,
            data = dat, family = binomial())
      }
      
      gW <- predict(gfit, type = "response")
      gW <- pmin(pmax(gW, eps), 1 - eps)
      
      # ---------------- clever covariate ----------------
      H <- dat$A / gW - (1 - dat$A) / (1 - gW)
      
      # ---------------- targeting ----------------
      eps_fit <- lm(Yc ~ -1 + offset(QAW) + H)
      eps_hat <- coef(eps_fit)[1]
      if (is.na(eps_hat)) eps_hat <- 0
      
      Q1W_star <- Q1W + eps_hat / gW
      Q0W_star <- Q0W - eps_hat / (1 - gW)
      
      mean(Q1W_star - Q0W_star)
      
    }, error = function(e) NA_real_)
    
    return(psi)
  }
  



  
  tmle_cont_SL_est <- function(dat,
                               misspec_Q = FALSE,
                               misspec_g = FALSE) {
    
    dat$W2sq  <- dat$W2^2
    dat$sinW3 <- sin(dat$W3)
    dat$W2W3  <- dat$W2 * dat$W3
    dat$logW2 <- log(abs(dat$W2) + 1)
    
    Yc <- (dat$Y - 1) / (K - 1)
    Yc <- pmin(pmax(Yc, 0), 1)
    
    X_Q <- if (!misspec_Q) {
      dat[, c("A","W1","W2","W3","W2sq","sinW3")]
    } else {
      dat[, c("A","W1","W3")]
    }
    
    X_g <- if (!misspec_g) {
      dat[, c("W1","W2","W3","W2W3","logW2")]
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
      QAW <- pmin(pmax(QAW, eps), 1 - eps)
      
      dat1 <- dat; dat1$A <- 1
      dat0 <- dat; dat0$A <- 0
      
      X1_Q <- if (!misspec_Q) {
        dat1[, c("A","W1","W2","W3","W2sq","sinW3")]
      } else {
        dat1[, c("A","W1","W3")]
      }
      
      X0_Q <- if (!misspec_Q) {
        dat0[, c("A","W1","W2","W3","W2sq","sinW3")]
      } else {
        dat0[, c("A","W1","W3")]
      }
      
      Q1W <- predict(Qfit, newdata = X1_Q)$pred
      Q0W <- predict(Qfit, newdata = X0_Q)$pred
      
      Q1W <- pmin(pmax(Q1W, eps), 1 - eps)
      Q0W <- pmin(pmax(Q0W, eps), 1 - eps)
      
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
      if (is.na(eps_hat)) eps_hat <- 0
      
      Q1W_star <- Q1W + eps_hat / gW
      Q0W_star <- Q0W - eps_hat / (1 - gW)
      
      mean(Q1W_star - Q0W_star)
      
    }, error = function(e) NA_real_)
    
    return(psi)
  }
  