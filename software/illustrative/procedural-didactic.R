source("ffmFunctions.R")

# The Training Plan -----------------------------------------------------------
w <- c(c(0:100), rep(0, 20), c(0:100), rep(0, 200))
## All simulations will use these training impulses!

# Basic FFM -------------------------------------------------------------------
ffm_basic <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
                              tau_h = 15, xi = 20)

df <- simulate(ffm_basic, w)

# Predictions with true parameters
pred_true_df <- make_predictions(ffm_basic, w)
plot(df$y)
points(pred_true_df$y_hat, col = 'blue')

# Estimating basic model, first get starting values from data set
ffm <- initialize_ffm_from_data(df)

# Just for demonstration - Get a feel for Gradient Descent with basic case
ffm_gd <- increase_likelihood_by_gradient(ffm, df, reps=300)
print(ffm_gd)

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df)
print(ffm_ml)

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

# Estimate initial values -----------------------------------------------------
ffm_add_initial <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
                                    tau_h = 15, xi = 20, q_g = 1500, q_h = 500)

df <- simulate(ffm_add_initial, w)
head(df, 5)  # Look for fitness and fatigue to be around their starting values

# See the initial values at work
plot(df$fitness)
plot(df$fatigue)

ffm <- initialize_ffm_from_data(df)

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE)
print(ffm_ml)

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')


# Estimate VDR parameter and initial values -----------------------------------
ffm_vdr <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .03, tau_g = 50,
                            tau_h = 15, xi = 20,
                            tau_h2 = 3, 
                            q_g = 300, q_h = 500)
print(ffm_vdr)
df <- simulate(ffm_vdr, w)
head(df, 5)

# Fatigue can get pretty large with tau_h and tau_h2
plot(df$fitness, main = "Fitness with VDR")
plot(df$fatigue, main = "Fatigue with VDR")

# Predictions with true parameters
pred_true_df <- make_predictions(ffm_vdr, w)
plot(df$y)
points(pred_true_df$y_hat, col = 'blue')

# Specify tau_h2_seq for data-driven VDR starting values
ffm <- initialize_ffm_from_data(df, tau_h2_seq = c(1, 2, 5, 10, 15))

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE, tune_vdr = TRUE)
print(ffm_ml)

# Predictions with estimated model - OK, this was a bit artificial!!
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

# Estimate Hill coefficients -----------------------------------------------
ffm_hill <- create_ffm_model(p_0 = 400, k_g = 4, k_h = 5, tau_g = 50,
                             tau_h = 15, xi = 20, gamma = 2, delta = 10)

# Look at Hill transformed training impulses
w_hill <- get_hill_transformed_training(ffm_hill, w)
plot(w_hill ~ w, main = "Simulation-specified Hill Transformation")

df <- simulate(ffm_hill, w)
head(df) 

# Predictions with true model
pred_df <- make_predictions(ffm_hill, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

# Specify delta_seq and gamma_seq for data-driven hill starting values
ffm <- initialize_ffm_from_data(df, delta_seq = c(.3, 1, 1.5, 5, 20),
                                gamma_seq = c(.3, 1, 2, 5, 20))

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_hill = TRUE)
print(ffm_ml)

# Hill transformation analysis
w_hill_ml <- get_hill_transformed_training(ffm_ml, w)
plot(w_hill ~ w, 
     main = "Hill transformation - True (black) & Est (blue )")
points(w_hill_ml ~ w, col = 'blue')


# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')


# Not the estimation we'd like to see for Hill. Let's try closer starting vals
# (Better) one-shot maximum likelihood using L-BFGS-B
# TODO: get delta and gamma to be in the right order always
ffm_close <- update(ffm, c(380, 3, 6, 55, 12, 25, 8, 1.5), tune_hill = TRUE)
ffm_ml2 <- maximize_likelihood(ffm_close, df, tune_initial = TRUE,
                               tune_hill = TRUE)
print(ffm_ml2)

# Examining Hill fit
w_hill_ml2 <- get_hill_transformed_training(ffm_ml2, w)
plot(w_hill_ml2 ~ w,
     main = "Hill transformation Again - True (black) & Est (blue )")

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml2, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

# The works: VDR, Hill, and initial values ------------------------------------
ffm_the_works <- create_ffm_model(p_0 = 400, k_g = 3, k_h = .3, tau_g = 50,
                                  tau_h = 10, xi = 10, tau_h2 = 5,
                                  gamma = .5, delta = 5,
                                  q_g = 30, q_h = 10)
print(ffm_the_works)

w_hill_the_works <- get_hill_transformed_training(ffm_the_works, w)
plot(w_hill_the_works ~ w, main = "Hill transformation")

df <- simulate(ffm_the_works, w)

# Predictions with estimated model
plot(df$fitness)
plot(df$fatigue)
pred_df <- make_predictions(ffm_the_works, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

ffm <- initialize_ffm_from_data(df, tau_h2_seq = c(1, 2, 5, 10),
                                delta_seq = c(.3, 1, 1.5,  5, 10),
                                gamma_seq = c(.3, 1, 3, 5, 10))

ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE,
                              tune_vdr = TRUE, tune_hill = TRUE)
print(ffm_ml)

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

w_hill_ml3 <- get_hill_transformed_training(ffm_ml, w)
plot(w_hill_ml3 ~ w,
     main = "Hill transformation Again - True (black) & Est (blue )")

# ---------------------------------------------------------------------
# Kalman Filter -------------------------------------------------------
# ---------------------------------------------------------------------
source("kalmanFunctions.R")
kalman_model <- create_kalman_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
                                    tau_h = 15, xi = 15,
                                    sigma_g = 20, sigma_h = 10, rho_gh = .2,
                                    initial_g = 1500, initial_h = 500,
                                    initial_sd_g = 500, initial_sd_h = 250,
                                    initial_rho_gh = 0)
print(kalman_model)
w <- rep(c(seq(10, 50), rep(20, 14)), 5)
w <- c(w, rep(0, 100), w)  #  Adding long rest!
plot(w, main = "Training impulses for this demonstration", xlab = "time")

set.seed(134323)
df <- simulate(kalman_model, w)
head(df)

# Note that state error can lead to fitness and fatigue dropping below 0
plot(df$true_fitness, main = "True fitness with state error")
plot(df$true_fatigue, main = "True fatigue with state error")

filtered <- filter(kalman_model, df)
names(filtered)

# Predictions with True Parameters
plot(df$y, xlab = "time",
     main = "Predictions with Kalman Filter, True Parameters")
points(filtered$df$y_hat, col = 'blue')

kf_mod <- initialize_kalman_from_data(df)
print(kf_mod)

kf_mod <- increase_likelihood(kf_mod, df, reps = 10)
print(kf_mod)

filtered <- filter(kf_mod, df)

plot(filtered$X[, 1], main = "Estimated fitness")
plot(filtered$X[, 2], main = "Estimated fatigue")

df$pred <- filtered$df$y_hat

plot(y ~ t, data = df, main = "Observed and Kalman Filter predictions")
points(filtered$df$y_hat, col = 'blue')
