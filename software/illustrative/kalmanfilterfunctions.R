print("Version 15")
library(MASS)


create_kalman_model <- function(p_star = 400, k_g = .1, k_h = .3, tau_g = 50,
                                tau_h = 15, xi = 20,
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

  model <- list(A = A, B = B, C = C, Q = Q, xi = xi, x_0 = x_0,
                M_0 = M_0, p_star = p_star)
  class(model) <- "kalmanfilter"
  model
}


filter <- function(kalman_model, df) {

  if (length(setdiff(c('w', 'p'), names(df))) > 0 ) {
    stop("df must have column 'w' (impulse) and 'p' (performance)")
  }
  # Data set extractions --
  T <- nrow(df)
  w <- df$w
  p <- df$p

  # Setting up structures ----------------------------------
  loglike <- numeric(T)
  X <- matrix(rep(NA, 2 * T), ncol=2)  # a posteriori state estimates as rows
  M <- matrix(rep(NA, 4 * T), ncol=4) # Vectorized state vcovs as rows
  Z <- matrix(rep(NA, 2 * T), ncol=2)  # a priori state estimates as rows

  with(kalman_model, {
    # The Kalman updating equations
    for (n in 1:T) {
      # A priori mean and variance of state --
      if (n == 1) {
        z_n <- A %*% x_0 + B * mean(w)
        P_n <- Q + A %*% M_0 %*% t(A)
      } else {
        z_n <- A %*% X[n - 1, ] + B * w[n - 1]
        P_n <- Q + A %*% matrix(M[n - 1, ], ncol = 2) %*% t(A)
      }

      # Likelihood of perf measurement n --
      S_n <- xi ^ 2 + C %*% P_n %*% t(C)  # pre-fit residual covariance
      e_n <- p[n] - (p_star + C %*% z_n)
      loglike[n] <- dnorm(e_n, mean = 0, sd = sqrt(S_n), log=TRUE)

      # A posteriori mean and variance of state --
      K_n <- P_n %*% t(C) %*% (1 / S_n)  # Kalman Gain
      X[n, ] <- z_n + K_n %*% e_n
      M[n, ] <- as.vector((diag(2) - K_n %*% C) %*% P_n)
      Z[n, ] <- z_n
    }
    p_hat <- p_star + X %*% t(C)  # Filtered predictions of performance
    list(p_hat = p_hat, g_hat = X[, 1], h_hat = X[, 2], M = M,
         Z = Z, loglike = loglike)
  })
}


extract_params <- function(kalman_model) {
  with(kalman_model, {
    k_g <- C[1, 1]
    k_h <- -1 * C[1, 2]
    tau_g <- -1 / log(A[1, 1])
    tau_h <- -1 / log(A[2, 2])
    sigma_g <- sqrt(Q[1, 1])
    sigma_h <- sqrt(Q[2, 2])
    rho_gh <- Q[1, 2] / (sigma_g * sigma_h)

  c(p_star,
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

    mod$p_star <- theta[1]
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
  start_df$p_star <- NA
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
  
    dummy_fitness <- sapply(1:nrow(df),
                            function(n) convolve_training(df$w[1:n], tau_g))
    dummy_fatigue <- sapply(1:nrow(df),
                            function(n) convolve_training(df$w[1:n], tau_h))
    dummy_ffm <- lm(df$p ~ dummy_fitness + dummy_fatigue)
  
    # Quick estimate of starting values
    burn_in <- floor(.1 * nrow(df))  # Prepare to discard first 10% of data
    n <- nrow(df)
    time <- burn_in:n
    trend_fit <- lm(dummy_fitness[burn_in:n] ~ time)
    start_df[i, "fitness_0"] <- max(0, predict(trend_fit,
                                               newdata = data.frame(time = 0)))
    trend_fat <- lm(dummy_fatigue[burn_in:n] ~ time)
    start_df[i, "fatigue_0"] <- max(0, predict(trend_fat,
                                               newdata = data.frame(time = 0)))
  
    fit_resid <- (dummy_fitness[2:nrow(df)]
                  - exp(-1 / tau_g) * dummy_fitness[1:(nrow(df) - 1)])
    fat_resid <- (dummy_fatigue[2:nrow(df)]
                  - exp(-1 / tau_h) * dummy_fatigue[1:(nrow(df) - 1)])
  
    start_df[i, "p_star"] <- abs(as.numeric(coef(dummy_ffm)[1]))
    start_df[i, "k_g"] <- abs(as.numeric(coef(dummy_ffm)[2]))
    start_df[i, "k_h"] <- abs(as.numeric(coef(dummy_ffm)[3]))
    start_df[i, "xi"] <- sigma(dummy_ffm)
  
    start_df[i, "sigma_g"] <- sd(fit_resid)
    start_df[i, "sigma_h"] <- sd(fat_resid)
  }
  
  best_start <- start_df[which.min(start_df$xi), ]
  kalman_model <- with(best_start,
                       create_kalman_model(p_star, k_g, k_h, tau_g, tau_h, xi,
                                           sigma_g, sigma_h, rho_gh = 0,
                                           initial_g = fitness_0, initial_h = fatigue_0,
                                           initial_sd_g = .5 * fitness_0,
                                           initial_sd_h = .5 * fatigue_0,
                                           initial_rho_gh = 0))
  kalman_model
}


maximize_likelihood <- function(kalman_model, df, factr = 10) {
  # One-shot likelihood maximization with L-BFGS-B
  theta <- extract_params(kalman_model)

  get_negloglike <- function(theta) {
    mod <- update(kalman_model, theta)
    filtered <- filter(mod, df)
    -sum(filtered$loglike)
  }
    
  lower <- c(theta[1] / 2, # Not below 1/2 of initial intercept
             0, 0,  # k_g and k_h may fall to zero, not below
             1, 1,  # 1-period minimum for time-constants
             theta[6] / 2, # Not below 1/2 of initial error sd
             1E-6, # state variances can get close to zero
             1E-6,
             -.999) # state correlation can get close to -1
    
  upper <- c(theta[1] * 2,  # Not above 2 times initial intercept
             theta[2:3] * 10,  # k_g and k_h may reach 10x starting guess
             nrow(df), nrow(df),  # Largest time constant allowed is dataset size
             theta[6] * 2,  # Not above 2 times initial error sd
             theta[6] * 10, # Q[1, 1] / xi <= 10
             theta[6] * 10, # Q[2, 2] / xi <= 10
             .999)  # state correlation can get close to +1
    
  cat("\nStarting L-BFGS-B optimization with factr", factr, "\n")
  cat("\nStarting -log likelihood is", get_negloglike(theta), "\n")
  cat("Minimizing. This might take a few minutes...\n\n")

  res <- optim(theta, get_negloglike, method = "L-BFGS-B",
               control = list(maxit = 10000, factr = factr, parscale = upper))
    
  cat("L-BFGS-B procedure finished with message:\n", res$message, "\n\n")
  cat("The final value of -log likelihood is", res$value, "\n")

  update(kalman_model, res$par)
}
                            

simulate.kalmanfilter <- function(kalman_model, w) {
  # Setting up structures ----------------------------------
  T <- length(w)
  p <- numeric(T)
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
      # Simulate conditional: p_n | x_n
      p[n] <- rnorm(1, p_star + C %*% X[n, ], xi)
    }
    data.frame(t = 1:T, w, p, g = X[, 1], h = X[, 2])
  })
}


print.kalmanfilter <- function(mod, rnd = 4) {
   with(mod, {
     cat("\n------- Your Kalman Filter Model -----\n")
     cat("------- State Model ------------------\n")
     mat_op_print("x_n = [", A=round(A, rnd), "] * ",
                  "x_{n-1} + [", B=round(B, rnd), "] * w_{n-1} + v_n")
     cat("with state error\n")
     mat_op_print("v_n ~ N(", c(0, 0), ", ", Q = round(Q, rnd), ")")
     cat("\nThe filter is initialized by: \n")
     mat_op_print("x_0 =[", x_0, "], Var(x_0) = [",
                  M_0 = round(M_0, rnd), "]")
     cat("\n------- Measurement Model ------------\n")
     mat_op_print("p_n =", round(p_star, rnd), "+ [",
                  C = round(C, rnd), "] * x_n + eta_n")
     cat("\nwhere\n")
     mat_op_print("eta_n ~ N(", 0, ", ", xi = round(xi, rnd), "^ 2)")
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
