
create_ffm_model <- function(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                             xi = 20, tau_h2 = NA, gamma = NA, delta = NA,
                             q_g = NA, q_h = NA) {

  model <- list(p_0 = p_0, k_g = k_g, k_h = k_h, tau_g = tau_g, tau_h = tau_h,
                xi = xi, tau_h2 = tau_h2, gamma = gamma, delta = delta,
                q_g = q_g, q_h = q_h)
  class(model) <- "ffm"
  model
}


ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                        xi = 20, tau_h2 = 2, gamma = 2, delta = 1.5,
                        q_g = 300, q_h = 500)

print.ffm <- function(mod, ...) {
  with(mod, {
    cat("\n------- Your Fitness Fatigue Model ---------\n")
    use_hill <- !is.na(gamma) & !is.na(delta)
    use_initial <- !is.na(q_g) & !is.na(q_h) 
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

print(ffm)


get_E_perf <- function(w, p_0, k_g, k_h, tau_g, tau_h,
		       tau_h2=NA, q_h=0, q_g=0) {
  T <- length(w)
  initial_fitness_effects <- q_h * exp(-(1:T) / tau_g)
  initial_fatigue_effects <- q_g * exp(-(1:T) / tau_h)
  fitness <- (initial_fitness_effects
            + sapply(1:T, function(t) convolve_training(w[1:t], tau_g)))

  # VDR filter on fatigue (optional) -------------------------
  w_fatigue <- w
  if (!is.na(tau_h2)) {
      w_fatigue <- sapply(1:T, function(t) ewma_training(w[1:t], tau_h2))
  }
  fatigue <- (initial_fatigue_effects
              + sapply(1:T, function(t) convolve_training(w_fatigue[1:t], tau_h)))

  p_0 + k_g * fitness - k_h * fatigue
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

simulate_ffm <- function(T, load_spec, p_0, k_g, k_h, tau_g, tau_h, sigma_e,
			 delta = NA, gamma = NA, 
			 tau_h2 = NA,
			 fitness_0 = 0, fatigue_0 = 0, seed = 0) {
  set.seed(seed)
 
  w <- create_training_impulse(T, load_spec) 

  # Hill function transformation of training impulse (optional) ------ 
  w_raw <- w
  if (!is.na(delta) & !is.na(gamma)) {
     w <- (w ^ gamma / (delta ^ gamma + w ^ gamma))
  }

  fitness <- (fitness_0
  	      + sapply(1:T, function(t) convolve_training(w[1:t], tau_g)))

  # VDR filter on fatigue (optional) -------------------------
  w_fatigue <- w
  if (!is.na(tau_h2)) {
    w_fatigue <- sapply(1:T, function(t) ewma_training(w[1:t], tau_h2))
  }
 
  fatigue <- (fatigue_0
  	      + sapply(1:T, function(t) convolve_training(w_fatigue[1:t], tau_h)))
  
  E_perf <- p_0 + k_g * fitness - k_h * fatigue
  perf <- E_perf + rnorm(T, 0, sigma_e)
  
  data.frame(t=1:T, w_raw, w, perf)
}



initialize_kalman_from_data <- function(df, tau_g_seq = c(100, 60, 30, 20),
                                        tau_h_seq = c(1, 5, 10, 15)) {

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
  
  
  kalman_model <- with(best_start,
                       create_kalman_model(p_0, k_g, k_h, tau_g, tau_h, xi,
                                           sigma_g, sigma_h, rho_gh = 0,
                                           initial_g = fitness_0, initial_h = fatigue_0,
                                           initial_sd_g = .5 * fitness_0, initial_sd_h = .5 * fatigue_0,
                                           initial_rho_gh =0))
  kalman_model
}


