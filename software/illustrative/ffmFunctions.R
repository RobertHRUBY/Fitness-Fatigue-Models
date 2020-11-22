
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
        cat("w_i_star = w_i ^", gamma, "/ (", delta, "^",
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
                                        tau_h2_seq = NA) {

  start_df <- expand.grid(tau_g = tau_g_seq, tau_h = tau_h_seq,
                          tau_h2 =tau_h2_seq)
  start_df$p_0 <- NA
  start_df$xi <- NA
  start_df$k_g <- NA
  start_df$k_h <- NA

  y <- df$y
  w <- df$w
  n <- nrow(df)

  for (i in 1:nrow(start_df)) {
    tau_g <- start_df[i, "tau_g"]
    tau_h <- start_df[i, "tau_h"]
    tau_h2 <- start_df[i, "tau_h2"]
  
    dummy_fitness <- sapply(1:n,
 													  function(i) convolve_training(w[1:i], tau_g))
    w_fatigue <- w
    if (!is.na(tau_h2)) {
        w_fatigue <- sapply(1:n, function(i) ewma_training(w[1:i], tau_h2))
    }
    dummy_fatigue <- sapply(1:n,
													  function(i) convolve_training(w_fatigue[1:i],
                                                          tau_h))
    dummy_ffm <- lm(y ~ dummy_fitness + dummy_fatigue)
    start_df[i, "p_0"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "xi"] <- sigma(dummy_ffm)
 }
  print(start_df)
  best_start <- start_df[which.min(start_df$xi), ]
  print(best_start)
	with(best_start, {
   
    ffm_model <- create_ffm_model(p_0, k_g, k_h, tau_g, tau_h, xi,
                                  tau_h2 = tau_h2)
	  					  		             
    ffm_model
  })
}


make_predictions <- function(ffm, w) {
  #  TODO: what if there are NAs?
  with(ffm, {
    N <- length(w)
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
                + sapply(1:N, function(i) convolve_training(w_fatigue[1:i],
                                                            tau_h)))

    y_hat <- p_0 + k_g * fitness - k_h * fatigue
    list(y_hat=y_hat, fitness_hat = fitness, fatigue_hat = fatigue)
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

extract_params <- function(ffm) {
  with(ffm, {
    c(p_0,
      k_g,
      k_h,
      tau_g,
      tau_h,
      xi,
			q_g,
			q_h,
      tau_h2
		)
  })
}


update <- function(ffm, theta) {
  ffm$p_0 <- theta[1]
  ffm$k_g <- theta[2]
  ffm$k_h <- theta[3]
  ffm$tau_g <- theta[4]
  ffm$tau_h <- theta[5]
  ffm$xi <- theta[6]
  ffm$q_g <- theta[7]
  ffm$q_h <- theta[8]
  ffm$tau_h2 <- theta[9] # Could be NA
  ffm
}

increase_likelihood <- function(ffm, df, reps = 1,
                                tune_starting=FALSE, tune_vdr=FALSE, tune_hill=FALSE, 
                                tol = 1E-12) {
  theta <- extract_params(ffm)
	theta1 <- theta[1:6]
  theta2 <- theta[7:8]  # initial values
  theta3 <- theta[9]  # initial values

  w <- df$w
  y <- df$y
  N <- length(w)

  # TODO: consider cost_function generator!
  cost_fn1 <- function(theta1) {
      theta <- c(theta1, theta2, theta3)
      mod <- update(ffm, theta)
      pred_df <- make_predictions(mod, w)
      -1.0 * sum(dnorm(y, mean = pred_df$y_hat, sd = mod$xi, log=TRUE))
  }

  cost_fn2 <- function(theta2) {
      theta <- c(theta1, theta2, theta3)
      mod <- update(ffm, theta)
      pred_df <- make_predictions(mod, w)
      -1.0 * sum(dnorm(y, mean = pred_df$y_hat, sd = mod$xi, log=TRUE))
  }

  cost_fn3 <- function(theta3) {
      theta <- c(theta1, theta2, theta3)
      mod <- update(ffm, theta)
      pred_df <- make_predictions(mod, w)
      -1.0 * sum(dnorm(y, mean = pred_df$y_hat, sd = mod$xi, log=TRUE))
  }

  for (rep in 1:reps) {
    res <- optim(theta1, cost_fn1, method = "L-BFGS-B",
								 lower = c(theta[1] / 5, 0, 0, theta[4] / 5, theta[5] / 5,
													 theta[6] / 5),
								 upper = theta * 5, # assume coming in w/ something reasonable
                 control = list(maxit = 10000, factr=tol, parscale = theta1))
    cat("Classic FFM Parameters:",
        "\np_0:", res$par[1], "\nk_g:", res$par[2], "\nk_h:", res$par[3],
        "\ntau_g:", res$par[4], "\ntau_h:", res$par[5], "\nxi:", res$par[6],
        "\nwith likelihood:", res$value, "\n\n")
    theta1 <- res$par
    
    if (tune_starting) {
      res <- optim(theta2, cost_fn2, method = "L-BFGS-B",
		  	 					  lower = c(0, 0),
		  						  upper = theta[1] * 5 * c(1, 1),
                    control = list(maxit = 10000, factr=tol, parscale = c(1, 1)))
      cat("Starting Values:",
          "\nq_g:", res$par[1], "\nq_h:", res$par[2], "\n",
          "\nwith likelihood:", res$value, "\n\n")
      theta2 <- res$par
    }

    if (tune_vdr) {
      res <- optim(theta3, cost_fn3, method = "L-BFGS-B",
		  	 					 lower = c(0),
		  						 upper = theta[4] * 5,
                   control = list(maxit = 10000, factr=tol))
      cat("VDR tau_h2:", res$par[1],
          "\nwith likelihood:", res$value, "\n\n")
      theta3 <- res$par
    }
	}

	theta <- c(theta1, theta2, theta3)
  update(ffm, theta)
}


increase_likelihood_by_gradient <- function(ffm, df, reps = 5, tol = 1E-12) {
  theta <- extract_params(ffm)

  w <- df$w
  y <- df$y
  N <- length(w)

  #ffm <- update(ffm, c(400, .1, .3, 50, 6, 20))
  ffm <- update(ffm, c(380, .05, .2, 45, 8, 10))
  pred <- make_predictions(ffm, w)
  resid <- y - pred$y_hat
  theta <- extract_params(ffm)
  reps <- 500
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
    cat(theta, "\n")
    plot(pred$y_hat ~ y, main = paste("R-squared:", cor(pred$y_hat, y)^2))
  }
  theta
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
  # Get predictions, add error 

  with(ffm_model, {
    # Hill function transformation of training impulse (optional) ------ 
    w_raw <- w
    N <- length(w)
    if (!is.na(delta) & !is.na(gamma)) {
       w <- (w ^ gamma / (delta ^ gamma + w ^ gamma))
    }
    pred_df <- make_predictions(ffm_model, w)
    y <- pred_df$y_hat + rnorm(length(w), sd = ffm_model$xi)
    data.frame(t=1:N, w_raw, w, y, y_hat = pred_df$y_hat,
               fitness = pred_df$fitness, fatigue = pred_df$fatigue)
  })
}
  #  fitness <- (q_g
  #  	      + sapply(1:N, function(t) convolve_training(w[1:t], tau_g)))

  #  # VDR filter on fatigue (optional) -------------------------
  #  w_fatigue <- w
  #  if (!is.na(tau_h2)) {
  #    w_fatigue <- sapply(1:N, function(t) ewma_training(w[1:t], tau_h2))
  #  }
 
  #  fatigue <- (q_h
  #  	      + sapply(1:N, function(t) convolve_training(w_fatigue[1:t], tau_h)))
  #  
  #  y <- p_0 + k_g * fitness - k_h * fatigue + rnorm(N, 0, xi)
  #  
  #  data.frame(t=1:N, w_raw, w, y)
  #  })


