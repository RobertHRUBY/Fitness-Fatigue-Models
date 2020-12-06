library(MASS)

create_kalman_model <- function(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                                xi = 20,
                                sigma_g = 8, sigma_h = 4, rho_gh = -.3,
                                initial_g = 35, initial_h = 20,
                                initial_sd_g = 10, initial_sd_h = 5,
                                initial_rho_gh = 0) {
  # Transition matrix
  A <- matrix(c(exp(-1 / tau_g), 0, 0, exp(-1 / tau_h)), ncol=2)

  # State intercept - each row is the fitness and fatigue effect to the next day's workout
  B <- matrix(c(exp(-1 / tau_g), exp(-1 / tau_h)), ncol=1)

  # Measurement matrix
  C <- matrix(c(k_g, -k_h), ncol=2)

  # Variances
  Q <- matrix(c(sigma_g ^ 2, rep(rho_gh * sigma_g * sigma_h, 2),
                sigma_h ^ 2), ncol=2)
  xi <- xi  # aka "R" matrix

  # prior distribution of fitness and fatigue
  x_0 <- c(initial_g, initial_h)
  M_0 <- matrix(c(initial_sd_g ^ 2,
                 rep(initial_rho_gh * initial_sd_g * initial_sd_h, 2),
                 initial_sd_h ^ 2), ncol=2)

  model <- list(A = A, B = B, C = C, Q = Q, xi = xi, x_0 = x_0, M_0 = M_0, p_0 = p_0)
  class(model) <- "kalmanfilter"
  model
}

filter <- function(kalman_model, df) {

  if (length(setdiff(c('w', 'y'), names(df))) > 0 ) {
    stop("df must have column 'w' (impulse) and 'y' (performance)")
  }
  # Data set extractions --
  T <- nrow(df)
  w <- df$w
  y <- df$y

  # Setting up structures ----------------------------------
  loglike <- numeric(T)
  X <- matrix(rep(NA, 2 * T), ncol=2)  # State vectors as rows
  M <- matrix(rep(NA, 4 * T), ncol=4) # Vectorized state vcovs as rows

  with(kalman_model, {
    # The Kalman updating equations
    for (n in 1:T) {
      # A priori mean and variance of state --
      if (n == 1) {
        z_n <- x_0
        P_n <- M_0
      } else {
        z_n <- A %*% X[n - 1, ] + B * w[n - 1]
        P_n <- Q + A %*% matrix(M[n - 1, ], ncol = 2) %*% t(A)
      }

      # Likelihood of perf measurement n --
      S_n <- xi ^ 2 + C %*% P_n %*% t(C)  # pre-fit residual covariance
      e_n <- y[n] - (p_0 + C %*% z_n)
      loglike[n] <- dnorm(e_n, mean = 0, sd = sqrt(S_n), log=TRUE)

      # A posteriori mean and variance of state --
      K_n <- P_n %*% t(C) %*% (1 / S_n)  # Kalman Gain
      X[n, ] <- z_n + K_n %*% e_n
      M[n, ] <- as.vector((diag(2) - K_n %*% C) %*% P_n)
    }
    df$y_hat <- p_0 + X %*% t(C)  # Filtered predictions of performance
    list(df = df, X = X, M = M, loglike = loglike)
  })
}

extract_and_transform_params <- function(kalman_model) {
  with(kalman_model, {
    k_g <- C[1, 1]
    k_h <- -1 * C[1, 2]
    tau_g <- -1 / log(A[1, 1])
    tau_h <- -1 / log(A[2, 2])
    sigma_g <- sqrt(Q[1, 1])
    sigma_h <- sqrt(Q[2, 2])
    rho_gh <- Q[1, 2] / (sigma_g * sigma_h)

  c(p_0,
    k_g,
    k_h,
    tau_g,
    tau_h,
    xi,
    sigma_g,
    sigma_h,
    rho_gh)
  })
}


update <- function(kalman_model, theta) {
    mod <- kalman_model

    mod$p_0 <- theta[1]
    mod$C[1, 1] <- theta[2]
    mod$C[1, 2] <- -1 * theta[3]
    mod$A[1, 1] <- exp(-1 / theta[4])
    mod$A[2, 2] <- exp(-1 / theta[5])
    mod$B[1, 1] <- exp(-1 / theta[4])
    mod$B[2, 1] <- exp(-1 / theta[5])
    mod$xi <- theta[6]
    mod$Q[1, 1] <- theta[7] ^ 2
    mod$Q[2, 2] <- theta[8] ^ 2
    mod$Q[1, 2] <- theta[7] * theta[8] * theta[9]
    mod$Q[2, 1] <- theta[7] * theta[8] * theta[9]

  mod
}


convolve_training <- function(w, tau) {
  T <- length(w)
  if (T <= 1) return(0)

  yesterday_to_first_day <- (T - 1):1
  first_day_to_yesterday <- 1:(T - 1)
  sum(w[first_day_to_yesterday] * exp(-yesterday_to_first_day / tau))
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
    start_df[i, "fitness_0"] <- predict(trend_fit, newdata = data.frame(time = 1))
    trend_fat <- lm(dummy_fatigue ~ time)
    start_df[i, "fatigue_0"] <- predict(trend_fat, newdata = data.frame(time = 1))
  
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
                                           initial_sd_g = .5 * fitness_0,
					   initial_sd_h = .5 * fatigue_0,
                                           initial_rho_gh =0))
  kalman_model
}


increase_likelihood <- function(kalman_model, df, reps = 5) {
  theta <- extract_and_transform_params(kalman_model)
  theta1 <- theta[1:6]
  theta2 <- theta[7:8]
  theta3 <- theta[9]

   cost_fn1 <- function(theta1) {
       theta <- c(theta1, theta2, theta3)
       mod <- update(kalman_model, theta)
       filtered <- filter(mod, df)
       -1.0 * sum(filtered$loglike)
   }
   
   cost_fn2 <- function(theta2) {
       theta <- c(theta1, theta2, theta3)
       mod <- update(kalman_model, theta)
       filtered <- filter(mod, df)
       -1.0 * sum(filtered$loglike)
   }
   
   cost_fn3 <- function(theta3) {
       theta <- c(theta1, theta2, theta3)
       mod <- update(kalman_model, theta)
       filtered <- filter(mod, df)
       -1.0 * sum(filtered$loglike)
   }
  
  tol <- 1E-14
  for (rep in 1:reps) {
    cat("\nIterative Maximum Likelihood, reptition", rep, "------\n")
    res <- optim(theta1, cost_fn1, method = "BFGS",
                 control = list(maxit = 10000, reltol=tol,
                 parscale = theta1))
    theta1 <- res$par
  
    cat("Classic FFM Parameters:",
        "\np_0:", res$par[1], "\nk_g:", res$par[2], "\nk_h:", res$par[3],
        "\ntau_g:", res$par[4], "\ntau_h:", res$par[5], "\nxi:", res$par[6],
        "\nwith likelihood:", res$value, "\n\n")
  
    res <- optim(theta2, cost_fn2, method = "L-BFGS-B",
               lower=c(0, 0),
               upper=c(kalman_model$xi * 10, kalman_model$xi * 10),
                 control = list(maxit = 10000, factr=tol))
    theta2 <- res$par
  
    cat("Q variances (sigma_g, sigma_h):", res$par, "with likelihood: ", res$value, "\n")
  
    res <- optim(theta3, cost_fn3, method = "L-BFGS-B",
               lower=-.999,
               upper=.999,
                 control = list(maxit = 10000, factr=tol))
    theta3 <- res$par
  
    cat("\nQ correlation:", res$par, "with likelihood: ", res$value, "\n")
  }

  theta <- c(theta1, theta2, theta3)
  update(kalman_model, theta) 
}


simulate.kalmanfilter <- function(kalman_model, w) {
  # Setting up structures ----------------------------------
  T <- length(w)
  y <- numeric(T)
  X <- matrix(rep(NA, 2 * T), ncol=2)  # State matrix w/ fitness and fatigue

  with(kalman_model, {
    for (n in 1:T) {
      # A priori mean and variance of state --
      if (n == 1) {
        # simulate unconditional: x_0
        X[n, ] <- x_0
      } else {
        # simulate conditional: x_n | x_(n - 1)
        X[n, ] <- mvrnorm(1, A %*% X[n - 1, ] + B * w[n - 1], Q)
      }
      # Simulate conditional: y_n | x_n
      y[n] <- rnorm(1, p_0 + C %*% X[n, ], xi)
    }
    data.frame(t = 1:T, w, y, true_fitness = X[, 1], true_fatigue = X[, 2])
  })
}


print.kalmanfilter <- function(mod, ...) {
   with(mod, {
     cat("\n------- Your Kalman Filter Model -----\n")
     cat("------- State Model ------------------\n")
     mat_op_print("x_n = [", A=A, "] * ", "x_(n-1) + [", B=B, "] * w_(n-1) + v_n")
     cat("where\n")
     mat_op_print("v_n ~ N(", c(0, 0), ", ", Q = Q, ")")
     cat("and\n")
     mat_op_print("x_0 =[", x_0, "], Var(x_0) = [", M_0=M_0, "]")
     cat("\n------- Meansurement Model ------------\n")
     mat_op_print("y_n =", p_0, "+ [", C=C, "] * x_n + eta_n")
     cat("\nwhere\n")
     mat_op_print("eta_n ~ N(", 0, ", ", xi = xi, "^ 2)")
   })
}


mat_op_print <- function(..., width = 0) {
  # From @Stibu at StackOverflow
  # https://stackoverflow.com/questions/39419622/printing-matrices-and-vectors-side-by-side

  # get arguments
  args <- list(...)
  chars <- sapply(args, is.character)

  # auxilliary function to create character of n spaces
  spaces <- function(n) paste(rep(" ", n), collapse = "")

  # convert vectors to row matrix
  vecs <- sapply(args, is.vector)
  args[vecs & !chars] <- lapply(args[vecs & !chars], function(v) matrix(v, ncol = 1))

  # convert all non-characters to character with format
  args[!chars] <- lapply(args[!chars], format, width = width)

  # print names as the first line, if present
  arg_names <- names(args)
  if (!is.null(arg_names)) {
    get_title <- function(x, name) {
      if (is.matrix(x)) {
        paste0(name, spaces(sum(nchar(x[1, ])) + ncol(x) - 1 - nchar(name)))
      } else {
        spaces(nchar(x))
      }
    }
  cat(mapply(get_title, args, arg_names), "\n")
  }

  # auxiliary function to create the lines
  get_line <- function(x, n) {
    if (is.matrix(x)) {
      if (nrow(x) < n) {
       spaces(sum(nchar(x[1, ])) + ncol(x) - 1)
      } else {
        paste(x[n, ], collapse = " ")
      }
    } else if (n == 1) {
      x
    } else {
      spaces(nchar(x))
    }
  }

  # print as many lines as needed for the matrix with most rows
  N <- max(sapply(args[!chars], nrow))
  for (n in 1:N) {
    cat(sapply(args, get_line, n), "\n")
  }
}
