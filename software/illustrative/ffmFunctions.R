
create_ffm_model <- function(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                             xi = 20, tau_h2 = NA, gamma = NA, delta = NA,
                             q_g = 0, q_h = 0) {

  model <- list(p_0 = p_0, k_g = k_g, k_h = k_h, tau_g = tau_g, tau_h = tau_h,
                xi = xi, tau_h2 = tau_h2, gamma = gamma, delta = delta,
                q_g = q_g, q_h = q_h)
  class(model) <- "ffm"
  model
}


print.ffm <- function(mod, ...) {
  with(mod, {
    cat("\n------- Your Fitness Fatigue Model ---------\n")
    use_hill <- !is.na(gamma) & !is.na(delta)
    use_initial <- q_g + q_h > 0
    use_vdr <- !is.na(tau_h2)
    cat("--- Hill Transform:", use_hill, ", VDR:", use_vdr, ", Initial Fitness & Fatigue:",
	use_initial, "---\n\n")

    w_i <- ifelse(use_hill, "w_i_star", "w_i")
    w_i_h <- ifelse(use_vdr, "k^i_h2", w_i)

    cat("p_n =", p_0, "+", k_g, "* \\sum_{i = 1}^{n - 1}", w_i, "* \\exp^{-(n - i) /", tau_g, "}")
    cat("\n          +", k_h , "* \\sum_{i = 1}^{n - 1}", w_i_h, "* \\exp^{-(n - i) /", tau_h, "}")
    if(use_initial) {
        cat("\n          +", q_h , "* \\exp ^ {-n  /", tau_h, "} + ")
        cat(q_g , "* \\exp ^ {-n  /", tau_g, "}")
    }
    cat("\n          + eta_n\n")
    cat("where\n\n")
    if (use_hill) {
        cat("w_i_star = w_i ^", gamma, "/ (", delta, "^", gamma, "+ w_i ^", gamma, ")")
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
			             tau_h_seq = c(1, 5, 10, 15), tau_h2_seq = NA,
                                     initialize_starting = FALSE) {

  start_df <- expand.grid(tau_g = tau_g_seq, tau_h = tau_h_seq)
  start_df$p_0 <- NA
  start_df$xi <- NA
  start_df$k_g <- NA
  start_df$k_h <- NA
  start_df$fitness_0 <- NA
  start_df$fatigue_0 <- NA
  # TODO: most of this is identical to Kalman
  for (i in 1:nrow(start_df)) {
    tau_g <- start_df[i, "tau_g"]
    tau_h <- start_df[i, "tau_h"]

    dummy_fitness <- sapply(1:nrow(df), function(n) convolve_training(df$w[1:n], tau_g))
    dummy_fatigue <- sapply(1:nrow(df), function(n) convolve_training(df$w[1:n], tau_h))
    dummy_ffm <- lm(df$y ~ dummy_fitness + dummy_fatigue)

    # Quick estimate of starting values
    time <- 1:nrow(df)
    trend_fit <- lm(dummy_fitness ~ time)
    start_df[i, "fitness_0"] <- predict(trend_fit, newdata = data.frame(time = 0))
    trend_fat <- lm(dummy_fatigue ~ time)
    start_df[i, "fatigue_0"] <- predict(trend_fat, newdata = data.frame(time = 0))

    fit_resid <- dummy_fitness[2:nrow(df)] - exp(-1 / tau_g) * dummy_fitness[1:(nrow(df) - 1)]
    fat_resid <- dummy_fatigue[2:nrow(df)] - exp(-1 / tau_h) * dummy_fatigue[1:(nrow(df) - 1)]

    start_df[i, "p_0"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "xi"] <- sigma(dummy_ffm)

    start_df[i, "sigma_g"] <- sd(fit_resid)
    start_df[i, "sigma_h"] <- sd(fat_resid)
  }

  best_start <- start_df[which.min(start_df$xi), ]

  ffm_model <- with(best_start,
                    create_ffm_model(p_0, k_g, k_h, tau_g, tau_h, xi))
  ffm_model
}

extract_params <- function(ffm) {
  with(ffm, {
    c(p_0,
      k_g,
      k_h,
      tau_g,
      tau_h,
      xi)
  })
}

update <- function(ffm, theta) {
  ffm$p_0 <- theta[1]
  ffm$k_g <- theta[2]
  ffm$k_h <- theta[3]
  ffm$tau_g <- theta[4]
  ffm$tau_h <- theta[5]
  ffm$xi <- theta[6]

  ffm
}

make_predictions <- function(ffm, w) {
  # Assumes w is 1-1 with days TODO: what if there are NAs?
  with(ffm, {
    N <- length(w)
    for (i in 1:N) {
    initial_fitness_effects <- q_h * exp(-(1:N) / tau_g)
    initial_fatigue_effects <- q_g * exp(-(1:N) / tau_h)

    fitness <- (initial_fitness_effects
              + sapply(1:N, function(i) convolve_training(w[1:i], tau_g)))

    # VDR filter on fatigue (optional) -------------------------
    w_fatigue <- w
    if (!is.na(tau_h2)) {
        w_fatigue <- sapply(1:N, function(i) ewma_training(w[1:i], tau_h2))
    }
    fatigue <- (initial_fatigue_effects
                + sapply(1:N, function(i) convolve_training(w_fatigue[1:i], tau_h)))

   }
    y_hat <- p_0 + k_g * fitness - k_h * fatigue
    list(y_hat=y_hat, fitness_hat = fitness, fatigue_hat = fatigue)
  })
}


increase_likelihood <- function(ffm, df, reps = 5, tol = 1E-12) {
  theta <- extract_params(ffm)

  cost_fn1 <- function(theta) {
      #theta <- c(theta1, theta2, theta3)
      mod <- update(ffm, theta)
      pred <- make_predictions(mod, df)
      -1.0 * sum(dnorm(df$y, mean = pred$y_hat, sd = mod$xi, log=TRUE))
  }
  res <- optim(theta, cost_fn1, method = "BFGS",
               control = list(maxit = 10000, reltol=tol, parscale = theta))
  theta1 <- res$par
  update(ffm, theta1)
}


hill_transform <- function(w, kappa, gamma, delta) {
  # w: vector (or scalar) of training implulses
  # kappa, gamma, delta: Hill function parameters
  kappa * w ^ gamma / (w ^ gamma +  delta ^ gamma)
}

# hill_transform(c(5, 6), 10000, 1, 10000) gets you back to where you were
# Note that's setting kappa = delta.

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

  #T, load_spec, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
  #			 delta = NA, gamma = NA, 
  #			 tau_h2 = NA,
  #			 fitness_0 = 0, fatigue_0 = 0, seed = 0) {

  with(ffm_model, {
    # Hill function transformation of training impulse (optional) ------ 
    w_raw <- w
    N <- length(w)
    if (!is.na(delta) & !is.na(gamma)) {
       w <- (w ^ gamma / (delta ^ gamma + w ^ gamma))
    }

    fitness <- (q_g
    	      + sapply(1:N, function(t) convolve_training(w[1:t], tau_g)))

    # VDR filter on fatigue (optional) -------------------------
    w_fatigue <- w
    if (!is.na(tau_h2)) {
      w_fatigue <- sapply(1:N, function(t) ewma_training(w[1:t], tau_h2))
    }
 
    fatigue <- (q_h
    	      + sapply(1:N, function(t) convolve_training(w_fatigue[1:t], tau_h)))
    
    y <- p_0 + k_g * fitness - k_h * fatigue + rnorm(N, 0, xi)
    
    data.frame(t=1:N, w_raw, w, y)
    })
}



initialize_ffm_from_data <- function(df, tau_g_seq = c(100, 60, 30, 20),
                                        tau_h_seq = c(1, 5, 10, 15),
                                        tau_h2_seq = NA) {

  start_df <- expand.grid(tau_g = tau_g_seq, tau_h = tau_h_seq)
  start_df$p_0 <- NA
  start_df$xi <- NA
  start_df$k_g <- NA
  start_df$k_h <- NA
  start_df$sigma_g <- NA
  start_df$sigma_h <- NA
  start_df$fitness_0 <- NA
  start_df$fatigue_0 <- NA
  
  for (i in 1:nrow(start_df)) {
    tau_g <- start_df[i, "tau_g"]
    tau_h <- start_df[i, "tau_h"]
  
    dummy_fitness <- sapply(1:nrow(df), function(n) convolve_training(df$w[1:n], tau_g))
    dummy_fatigue <- sapply(1:nrow(df), function(n) convolve_training(df$w[1:n], tau_h))
    dummy_ffm <- lm(df$y ~ dummy_fitness + dummy_fatigue)
  
    # Quick estimate of starting values
    time <- 1:nrow(df)
    trend_fit <- lm(dummy_fitness ~ time)
    start_df[i, "fitness_0"] <- predict(trend_fit, newdata = data.frame(time = 0))
    trend_fat <- lm(dummy_fatigue ~ time)
    start_df[i, "fatigue_0"] <- predict(trend_fat, newdata = data.frame(time = 0))
  
    fit_resid <- dummy_fitness[2:nrow(df)] - exp(-1 / tau_g) * dummy_fitness[1:(nrow(df) - 1)]
    fat_resid <- dummy_fatigue[2:nrow(df)] - exp(-1 / tau_h) * dummy_fatigue[1:(nrow(df) - 1)]
  
    start_df[i, "p_0"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "xi"] <- sigma(dummy_ffm)
  
    start_df[i, "sigma_g"] <- sd(fit_resid)
    start_df[i, "sigma_h"] <- sd(fat_resid)
  }
  
  best_start <- start_df[which.min(start_df$xi), ]
  
  
  ffm_model <- with(best_start,
                       create_ffm_model(p_0, k_g, k_h, tau_g, tau_h, xi))
  ffm_model
}

