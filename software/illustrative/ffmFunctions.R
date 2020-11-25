
create_ffm_model <- function(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
			     tau_h = 15, xi = 20, tau_h2 = NA, gamma = NA,
			     delta = NA, kappa = 1, q_g = 0, q_h = 0) {
  # Create an object of class "ffm"
  model <- list(p_0 = p_0, k_g = k_g, k_h = k_h, tau_g = tau_g, tau_h = tau_h,
                xi = xi, tau_h2 = tau_h2, gamma = gamma, delta = delta,
                kappa = kappa, q_g = q_g, q_h = q_h)
  class(model) <- "ffm"
  model
}


print.ffm <- function(mod, ...) {
  with(mod, {
    cat("\n------- Your Fitness Fatigue Model ---------\n")
    use_hill <- !is.na(gamma) & !is.na(delta)
    use_initial <- q_g + q_h > 0
    use_vdr <- !is.na(tau_h2)
    cat("--- Hill Transform:", use_hill, ", VDR:", use_vdr,
  ", Initial Fitness & Fatigue:", use_initial, "---\n\n")

    w_i <- ifelse(use_hill, "w_i_star", "w_i")
    w_i_h <- ifelse(use_vdr, "k^i_h2", w_i)

    cat("p_n =", p_0, "+", k_g, "* \\sum_{i = 1}^{n - 1}",
        w_i, "* \\exp^{-(n - i) /", tau_g, "}")
    cat("\n          -", k_h , "* \\sum_{i = 1}^{n - 1}",
        w_i_h, "* \\exp^{-(n - i) /", tau_h, "}")
    if(use_initial) {
        cat("\n          +", q_g , "* \\exp ^ {-n  /", tau_g, "} + ")
        cat(q_h , "* \\exp ^ {-n  /", tau_h, "}")
    }
    cat("\n          + eta_n\n")
    cat("where\n\n")
    if (use_hill) {
        cat("w_i_star =", kappa, "* w_i ^", gamma, "/ (", delta, "^",
      gamma, "+ w_i ^", gamma, ")")
        cat("\n\nand\n\n")
    }
    if (use_vdr) {
      cat("k^i_h2 =", "\\sum_{j = 1}^i", w_i, "* \\exp^{-(i - j) /", tau_h2, "}")
      cat("\n\nand\n\n")
    }
    cat("eta_n ~ N(", 0, ", ", xi = xi, "^ 2)\n")
  })
}


initialize_ffm_from_data <- function(df, tau_g_seq = c(100, 60, 30, 20),
                                     tau_h_seq = c(1, 5, 10, 15),
                                     tau_h2_seq = NA, delta_seq = NA,
                                     gamma_seq = NA, kappa = 1) {

  start_df <- expand.grid(tau_g = tau_g_seq, tau_h = tau_h_seq,
                          tau_h2 = tau_h2_seq, delta = delta_seq,
                          gamma = gamma_seq)
  start_df$p_0 <- NA
  start_df$k_g <- NA
  start_df$k_h <- NA
  start_df$xi <- NA

  y <- df$y
  w <- df$w
  n <- nrow(df)

  w_raw <- w
  for (i in 1:nrow(start_df)) {
    tau_g <- start_df[i, "tau_g"]
    tau_h <- start_df[i, "tau_h"]
    tau_h2 <- start_df[i, "tau_h2"]
    delta <- start_df[i, "delta"]
    gamma <- start_df[i, "gamma"]

    if (!is.na(delta) & !is.na(gamma)) {  # Hill Transform
       w <- hill_transform(w_raw, kappa, gamma, delta)
    }
    dummy_fitness <- sapply(1:n, function(i) convolve_training(w[1:i], tau_g))
    w_fatigue <- w
    if (!is.na(tau_h2)) { # VDR
        w_fatigue <- sapply(1:n, function(i) ewma_training(w[1:i], tau_h2))
    }
    dummy_fatigue <- sapply(1:n, function(i) convolve_training(w_fatigue[1:i],
                                                               tau_h))
    dummy_ffm <- lm(y ~ dummy_fitness + dummy_fatigue)
    start_df[i, "p_0"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "xi"] <- sigma(dummy_ffm)
  }
  print(start_df)
  cat("The best combination------\n")
  best_start <- start_df[which.min(start_df$xi), ]
  print(best_start)
  with(best_start, {
    ffm_model <- create_ffm_model(p_0, k_g, k_h, tau_g, tau_h, xi,
          tau_h2 = tau_h2, gamma = gamma,
          delta = delta, q_g = 0, q_h = 0)
    ffm_model
  })
}


make_predictions <- function(ffm, w) {
  with(ffm, {
    N <- length(w)
    initial_fitness_effects <- q_g * exp(-(1:N) / tau_g)
    initial_fatigue_effects <- q_h * exp(-(1:N) / tau_h)

    if (!is.na(delta) & !is.na(gamma)) {
       w <- hill_transform(w, kappa, gamma, delta) 
    }
    fitness <- (initial_fitness_effects
                + sapply(1:N, function(i) convolve_training(w[1:i], tau_g)))

    # VDR filter on fatigue (optional) -------------------------
    w_fatigue <- w
    if (!is.na(tau_h2)) {
        w_fatigue <- sapply(1:N, function(i) ewma_training(w[1:i], tau_h2))
    }
    fatigue <- (initial_fatigue_effects
                + sapply(1:N, function(i) convolve_training(w_fatigue[1:i],
                                                            tau_h)))
    y_hat <- p_0 + k_g * fitness - k_h * fatigue
    data.frame(y_hat = y_hat, w_ffm = w, fitness_hat = fitness,
               fatigue_hat = fatigue)
  })
}


convolve_tau_grad <- function(w, tau) {
    T <- length(w)
    if (T <= 1) return(0)

    yesterday_to_first_day <- (T - 1):1
    first_day_to_yesterday <- 1:(T - 1)
    
    sum(w[first_day_to_yesterday]
        * (yesterday_to_first_day / tau ^ 2)
        * exp(-yesterday_to_first_day / tau))
}


extract_params <- function(ffm, tune_initial = FALSE, tune_vdr = FALSE,
			   tune_hill = FALSE) {
  # theta is variable-length based on particular model
  with(ffm, {
    theta <- c(p_0, k_g, k_h, tau_g, tau_h, xi)
    if (tune_initial) {
      theta <- c(theta, q_g, q_h)
    }
    if (tune_hill) {
      theta <- c(theta, delta, gamma)
    }
    if(tune_vdr) {
      theta <- c(theta, tau_h2)
    }
    theta
  })
}


update <- function(ffm, theta, tune_initial = FALSE, tune_vdr = FALSE,
		   tune_hill = FALSE) {
  ffm$p_0 <- theta[1]
  ffm$k_g <- theta[2]
  ffm$k_h <- theta[3]
  ffm$tau_g <- theta[4]
  ffm$tau_h <- theta[5]
  ffm$xi <- theta[6]
  offset <- 7
  if (tune_initial) {
    ffm$q_g <- theta[offset]
    ffm$q_h <- theta[offset + 1]
    offset <- offset + 2
  }
  if (tune_hill) {
    ffm$delta <- theta[offset]
    ffm$gamma <-  theta[offset + 1]
    offset <- offset + 2
  }
  if (tune_vdr) {
     ffm$tau_h2 <- theta[offset]  # Fully-loaded model
  }
  ffm
}


maximize_likelihood <- function(ffm, df, reps = 1,
                                tune_initial=FALSE, tune_vdr=FALSE,
                                tune_hill=FALSE, maxit = 10000, tol = 1E-12) {
  theta <- extract_params(ffm, tune_initial, tune_vdr, tune_hill)
  w <- df$w
  y <- df$y
  N <- length(w)

  get_negloglike <- function(theta) {
    mod <- update(ffm, theta, tune_initial, tune_vdr, tune_hill)
    pred_df <- make_predictions(mod, w)
    -1.0 * sum(dnorm(y, mean = pred_df$y_hat, sd = mod$xi, log=TRUE))
  }

  lower_basic <- c(theta[1] / 2, # Not below 1/2 of initial intercept
                   0, 0,  # k_g and k_h may fall to zero, not below
                   1, 1,  # 1-period minimum for time-constants
                   theta[6] / 2) # Not below 1/2 of initial error sd

  upper_basic <- c(theta[1] * 2,  # Not above 2 times initial intercept
                   theta[2:3] * 10,  # k_g and k_h may reach 10x starting guess
                   N, N,  # Largest time constant allowed is dataset size
                   theta[6] * 2)  # Not above 2 times initial error sd

  parscale <- theta[1:6]
  offset <- 7
  if (tune_initial) {
    lower_basic <- c(lower_basic, 0, 0)
    upper_basic <- c(upper_basic, y[1] * 10, y[1] * 10)
    parscale <- c(parscale, 1, 1)
    pos <- offset:(offset + 1)
    theta[pos] <- c(y[1], y[1] / 2)  # initial values that aren't zero
    cat("\n\nTuning Initial parameters (q_g, q_h) with lower bound:",
	lower_basic[pos], "\n  and upper bound:", upper_basic[pos],
	"\n  and starting values:", theta[pos], "\n")
    offset <- offset + 2
  }
  if (tune_hill) {
    lower_basic <- c(lower_basic, .1, .1)
    upper_basic <- c(upper_basic, 100, 20)
    parscale <- c(parscale, .5, .5)
    pos <- offset:(offset + 1)
    cat("\n\nTuning Hill parameters (delta, gamma) with lower bound:",
	lower_basic[pos], "\n  and upper bound:", upper_basic[pos],
	"\n  and starting values:", theta[pos], "\n")
    offset <- offset + 2
  }
  if (tune_vdr) {
    lower_basic <- c(lower_basic, 1)
    upper_basic <- c(upper_basic, N)
    parscale <- c(parscale, .5)
    pos <- offset
    cat("\n\nTuning VDR parameter tau_h2 with lower bound:", lower_basic[pos],
	"\n  and upper bound:", upper_basic[pos], "\n  and starting value:",
	theta[pos], "\n")
  }
  if (sum(is.na(theta)) > 0) {
    stop("Missing parameter values. Ensure that starting values are set.")
  }
  cat("\nStarting L-BFGS-B optimization via optim.\n Depending")
  cat(" on the model, this could take 30 seconds or 30 minutes...\n\n")
  res <- optim(theta, get_negloglike, method = "L-BFGS-B",
               lower = lower_basic, upper = upper_basic,
               control = list(maxit = maxit, factr = tol, parscale = parscale))
  update(ffm, res$par, tune_initial, tune_vdr, tune_hill)
}

increase_likelihood_by_gradient <- function(ffm, df, reps = 5) {
  theta <- extract_params(ffm)

  w <- df$w
  y <- df$y
  N <- length(w)

  pred <- make_predictions(ffm, w)
  resid <- y - pred$y_hat
  for (rep in 1:reps) {
    df_dp_0 <- c(0, rep(1, N - 1))
    df_dk_g <- sapply(1:N, function(n) convolve_training(w[1:n], ffm$tau_g))
    df_dk_h <- -sapply(1:N, function(n) convolve_training(w[1:n], ffm$tau_h))
    df_dtau_g <- ffm$k_g * sapply(1:N,
                                  function(n) convolve_tau_grad(w[1:n],
                                                                ffm$tau_g))
    df_dtau_h <- -ffm$k_h * sapply(1:N,
                                   function(n) convolve_tau_grad(w[1:n],
                                                                 ffm$tau_h))
    dsse_dp_0 <- -2 * sum(resid * df_dp_0)
    dsse_dk_g <- -2 * sum(resid * df_dk_g)
    dsse_dk_h <- -2 * sum(resid * df_dk_h)
    dsse_tau_g <- -2 * sum(resid * df_dtau_g)
    dsse_tau_h <- -2 * sum(resid * df_dtau_h)

    lambda <- .001
    theta[1] <- theta[1] - 150 * lambda * (1 / N) * dsse_dp_0 
    theta[2] <- theta[2] - lambda / 8000 * (1 / N) * dsse_dk_g 
    theta[3] <- theta[3] - lambda / 8000 * (1 / N) * dsse_dk_h 
    theta[4] <- theta[4] -  lambda * (1 / N) * dsse_tau_g 
    theta[5] <- theta[5] - lambda * (1 / N) * dsse_tau_h

    ffm <- update(ffm, theta)
    pred <- make_predictions(ffm, w)
    resid <- y - pred$y_hat
    theta[6] <- sqrt((1 / (N - 5)) * sum(resid ^ 2))
    cat("Rep", rep, ", Parameters:", theta[1:5], "RMSE:", theta[6], "\n")
    plot(pred$y_hat ~ y,
	 main = paste("Pred vs Observed, R-squared:", cor(pred$y_hat, y)^2))
  }
  ffm
}


hill_transform <- function(w, kappa, gamma, delta) {
  # w: vector (or scalar) of training implulses
  kappa * w ^ gamma / (w ^ gamma +  delta ^ gamma)
}


convolve_training <- function(w, tau) {
  T <- length(w)
  if (T <= 1) return(0)

  yesterday_to_first_day <- (T - 1):1
  first_day_to_yesterday <- 1:(T - 1)
  sum(w[first_day_to_yesterday] * exp(-yesterday_to_first_day / tau))
}


ewma_training <- function(w, tau) {
  T <- length(w)
  if (T <= 1) return(0)

  today_to_first_day <- T:1
  first_day_to_today <- 1:T
  sum(w[first_day_to_today] * exp(-today_to_first_day / tau))
}


simulate.ffm <- function(ffm_model, w) {
  # Get predictions, add performance measurmemet error 
  pred_df <- make_predictions(ffm_model, w)
  y <- pred_df$y_hat + rnorm(length(w), sd = ffm_model$xi)
  data.frame(t = 1:length(w), w=w, y=y, y_hat = pred_df$y_hat,
             w_ffm = pred_df$w_ffm,
             fitness = pred_df$fitness, fatigue = pred_df$fatigue)
}
