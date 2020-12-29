print("Version 16")

create_ffm_model <- function(p_star = 400, k_g = .1, k_h = .3, tau_g = 50,
                             tau_h = 15, sigma = 20, tau_h2 = NA, gamma = NA,
                             delta = NA, kappa = 100, q_g = 0, q_h = 0) {
  # Create an object of class "ffm"
  model <- list(p_star = p_star, k_g = k_g, k_h = k_h, tau_g = tau_g, tau_h = tau_h,
                sigma = sigma, tau_h2 = tau_h2, gamma = gamma, delta = delta,
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

    cat("\\hat{p}_n = ", p_star, " + ", k_g, " * \\sum_{i = 1}^{n - 1} ",
        w_i, " * \\exp(-(n - i) / ", tau_g, ")", sep = "")
    cat("\n          - ", k_h , " * \\sum_{i = 1}^{n - 1} ",
        w_i_h, " * \\exp(-(n - i) / ", tau_h, ")", sep = "")
    if(use_initial) {
      cat("\n          + ", q_g , " * \\exp(-n / ", tau_g, ") + ", sep = "")
      cat(q_h , " * \\exp(-n  / ", tau_h, ")", sep = "")
    }
    cat("\n          + \\epsilon_n\n")
    cat("where\n\n")
    if (use_hill) {
      cat("w_i_star = ", kappa, " * w_i ^ ", gamma, " / (", delta, " ^ ",
          gamma, " + w_i ^ ", gamma, ")", sep = "")
      cat("\n\nand\n\n")
    }
    if (use_vdr) {
      cat("k^i_h2 = ", "\\sum_{j = 1}^i ", w_i,
          " * \\exp(-(i - j) / ", tau_h2, ")", sep="")
      cat("\n\nand\n\n")
    }
    cat("\\epsilon_n ~ N(", 0, ", ", sigma = sigma, " ^ 2)\n", sep = "")
  })
}


initialize_ffm_from_data <- function(df, tau_g_seq = c(100, 60, 30, 20),
                                     tau_h_seq = c(1, 5, 10, 15),
                                     estimate_initial = FALSE,
                                     tau_h2_seq = NA, delta_seq = NA,
                                     gamma_seq = NA, kappa = 100) {

  start_df <- expand.grid(tau_g = tau_g_seq, tau_h = tau_h_seq,
                          tau_h2 = tau_h2_seq, delta = delta_seq,
                          gamma = gamma_seq)
  start_df$p_star <- NA
  start_df$k_g <- NA
  start_df$k_h <- NA
  start_df$sigma <- NA

  p <- df$p
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
    if (!is.na(tau_h2)) {  # VDR
        w_fatigue <- sapply(1:n, function(i) ewma_training(w[1:i], tau_h2))
    }
    dummy_fatigue <- sapply(1:n, function(i) convolve_training(w_fatigue[1:i],
                                                               tau_h))
    dummy_ffm <- lm(p ~ dummy_fitness + dummy_fatigue)
    start_df[i, "p_star"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "sigma"] <- sigma(dummy_ffm)
                            
    start_df[i, "fitness_0"] <- 0
    start_df[i, "fatigue_0"] <- 0
                            
    if (estimate_initial) {                
      burn_in <- floor(.1 * nrow(df))  # Prepare to discard first 10% of data
      time <- burn_in:n
      trend_fit <- lm(dummy_fitness[burn_in:n] ~ time)
      start_df[i, "fitness_0"] <- max(0, predict(trend_fit,
                                                 newdata = data.frame(time = 0)))
      trend_fat <- lm(dummy_fatigue[burn_in:n] ~ time)
      start_df[i, "fatigue_0"] <- max(0, predict(trend_fat,
                                                 newdata = data.frame(time = 0)))
    }
  }
  cat("The best combination------\n")
  best_start <- start_df[which.min(start_df$sigma), ]
  print(best_start)
  with(best_start, {
    ffm_model <- create_ffm_model(p_star = p_star, k_g = k_g, k_h = k_h,
                                  tau_g = tau_g, tau_h = tau_h, sigma = sigma,
                                  tau_h2 = tau_h2, gamma = gamma,
                                  delta = delta, q_g = fitness_0, q_h = fatigue_0)
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
    p_hat <- p_star + k_g * fitness - k_h * fatigue
    data.frame(p_hat = p_hat, w_ffm = w, g_hat = fitness, h_hat = fatigue)
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
    theta <- c(p_star, k_g, k_h, tau_g, tau_h, sigma)
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
  ffm$p_star <- theta[1]
  ffm$k_g <- theta[2]
  ffm$k_h <- theta[3]
  ffm$tau_g <- theta[4]
  ffm$tau_h <- theta[5]
  ffm$sigma <- theta[6]
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


maximize_likelihood <- function(ffm, df, tune_initial=FALSE, tune_vdr=FALSE,
                                tune_hill=FALSE, maxit = 10000, factr = 10) {
  theta <- extract_params(ffm, tune_initial, tune_vdr, tune_hill)
  w <- df$w
  p <- df$p
  N <- length(w)

  get_negloglike <- function(theta) {
    mod <- update(ffm, theta, tune_initial, tune_vdr, tune_hill)
    pred_df <- make_predictions(mod, w)
    -1.0 * sum(dnorm(p, mean = pred_df$p_hat, sd = mod$sigma, log=TRUE))
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
    upper_basic <- c(upper_basic, p[1] * 10, p[1] * 10)
    parscale <- c(parscale, 1, 1)
    pos <- offset:(offset + 1)
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
  cat("\nStarting L-BFGS-B optimization via optim.\n")
  cat("Depending on the model, this could take a few minutes...\n")
      cat("Factor of machine tolerance for relative convergence:", factr, "\n")
  res <- optim(theta, get_negloglike, method = "L-BFGS-B",
               lower = lower_basic, upper = upper_basic,
               control = list(maxit = maxit, factr = factr, parscale = parscale))
  update(ffm, res$par, tune_initial, tune_vdr, tune_hill)
}


get_gradient_sse <- function(ffm, w, resid) {
    
  N <- length(w)

  # Derivative with respect to the Expectation function
  df_dp_star <- c(0, rep(1, N - 1))
  df_dk_g <- sapply(1:N, function(n) convolve_training(w[1:n], ffm$tau_g))
  df_dk_h <- -sapply(1:N, function(n) convolve_training(w[1:n], ffm$tau_h))
  df_dtau_g <- ffm$k_g * sapply(1:N,
                                function(n) convolve_tau_grad(w[1:n],
                                                                ffm$tau_g))
  df_dtau_h <- -ffm$k_h * sapply(1:N,
                                 function(n) convolve_tau_grad(w[1:n],
                                                               ffm$tau_h))
                                 
  # derivative with respect to the sum of squares error function                            
  dsse_dp_star <- -2 * sum(resid * df_dp_star)
  dsse_dk_g <- -2 * sum(resid * df_dk_g)
  dsse_dk_h <- -2 * sum(resid * df_dk_h)
  dsse_dtau_g <- -2 * sum(resid * df_dtau_g)
  dsse_dtau_h <- -2 * sum(resid * df_dtau_h)

  c(dsse_dp_star, dsse_dk_g, dsse_dk_h, dsse_dtau_g, dsse_dtau_h) 
}

                                   
increase_likelihood_by_gradient <- function(ffm, df, reps = 5, thin = 1,
                                            lambda = .001,
                                            parscale = c(1 / 100, 1, 1,
                                                         1 / 10, 1 / 10)) {
  theta <- extract_params(ffm)
  if (length(theta) > 6) {
    stop("Gradient Decent only available for 5-param FFM")
  }

  w <- df$w
  p <- df$p
  N <- length(w)

  pred <- make_predictions(ffm, w)
  resid <- p - pred$p_hat    
    
  for (rep in 1:reps) {
      
    grad_sse <- get_gradient_sse(ffm, w, resid)
    norm_sse_grad <- grad_sse / sqrt(sum(grad_sse ^ 2))
    scaled_sse_grad <- norm_sse_grad / parscale

    theta[1:5] <- theta[1:5] - lambda * scaled_sse_grad

    ffm <- update(ffm, theta)                                   
    pred <- make_predictions(ffm, w)
    resid <- p - pred$p_hat
    theta[6] <- sqrt((1 / (N - 5)) * sum(resid ^ 2))
    if (rep %% thin == 0) {                               
        cat("Rep", rep, ", Parameters:", theta[1:5], "RMSE:", theta[6], "\n")

    }
  }
  cat("\nRaw gradient is", grad_sse,
      "\nNormalized gradient is", norm_sse_grad,
      "\nNormalized gradient / parscale is", scaled_sse_grad,
      "\nparscale is", parscale, "\nlambda is", lambda, "\n\n")
  ffm
}


hill_transform <- function(w, kappa, gamma, delta) {
  # w: vector (or scalar) of training implulses
  kappa * w ^ gamma / (w ^ gamma +  delta ^ gamma)
}


get_hill_transformed_training <- function(ffm, w) {
  hill_transform(w, ffm$kappa, ffm$gamma, ffm$delta)
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
  # Get predictions, add performance measurement error 
  pred_df <- make_predictions(ffm_model, w)
  p <- pred_df$p_hat + rnorm(length(w), sd = ffm_model$sigma)
  data.frame(t = 1:length(w), w=w, p=p, p_hat = pred_df$p_hat,
             w_ffm = pred_df$w_ffm, g = pred_df$g, h = pred_df$h)
}
