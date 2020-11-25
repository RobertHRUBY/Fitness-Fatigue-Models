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

# Just for demonstration - only works with Basic FFM, scaled for example
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
                                    tau_h = 15, xi = 20, q_g = 40, q_h = 100)

df <- simulate(ffm_add_initial, w)
head(df, 5)  # Look for fitness and fatigue to be around their starting values

ffm <- initialize_ffm_from_data(df)

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE)
print(ffm_ml)

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')


# Estimate VDR parameter and initial values -----------------------------------
ffm_vdr <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
                            tau_h = 15, xi = 20,
                            tau_h2 = 8, 
                            q_g = 300, q_h = 500)
print(ffm_vdr)
df <- simulate(ffm_vdr, w)
head(df, 5)

# Specify tau_h2_seq for data-driven VDR starting values
ffm <- initialize_ffm_from_data(df, tau_h2_seq = c(1, 2, 3))

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE, tune_vdr = TRUE)
print(ffm_ml)


# Estimate Hill coefficients and initial values
ffm_hill <- create_ffm_model(p_0 = 400, k_g = 1, k_h = 3, tau_g = 50,
                             tau_h = 15, xi = 20, gamma = 2, delta = 1.5,
                             q_g = 40, q_h = 100)

df <- simulate(ffm_hill, w)
head(df)  # w_ffm is the Hill transformed impulses

# Predictions with true model
pred_df <- make_predictions(ffm_hill, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')

# Specify delta_seq and gamma_seq for data-driven hill starting values
ffm <- initialize_ffm_from_data(df, delta_seq = c(.3, 1, 1.5, 5, 20),
                                gamma_seq = c(.3, 1, 2, 5, 20))

# One-shot maximum likelihood using L-BFGS-B
ffm_ml <- maximize_likelihood(ffm, df, tune_initial = TRUE, tune_hill = TRUE)
print(ffm_ml)

# Hill transformation analysis
plot(df$w_ffm ~ df$w,
     main = "Hill transformation - True (black) & Est (blue )")

# Need to standardize hill function - gamma and delta swap positions
w_hill_ml <- hill_transform(w, ffm$kappa, ffm$gamma, ffm$delta)
points(w_hill_ml ~ w, col = 'blue')

# Something has gone wrong with the estimation. Let's try better starting vals

# (Better) one-shot maximum likelihood using L-BFGS-B
ffm_close <- update(ffm, c(380, .5, 4, 60, 10, 25, 50, 110, 3, 1.5))
ffm_ml2 <- maximize_likelihood(ffm_close, df, tune_initial = TRUE,
                               tune_hill = TRUE)
print(ffm_ml2)
w_hill_ml2 <- hill_transform(w, ffm_ml2$kappa, ffm_ml2$gamma, ffm_ml2$delta)
plot(w_hill_ml2 ~ w, main = 'Another go at Hill')

# Predictions with estimated model
pred_df <- make_predictions(ffm_ml, w)
plot(df$y)
points(pred_df$y_hat, col = 'red')


# The works: VDR, Hill, and initial values ------------------------------------
ffm_the_works <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
                                  tau_h = 15, xi = 10, tau_h2 = 10,
                                  gamma = 3, delta = 5,
                                  q_g = 300, q_h = 100)
print(ffm_the_works)

df <- simulate(ffm_the_works, w)
ffm <- initialize_ffm_from_data(df, tau_h2_seq = c(2, 5, 10, 30),
                                delta_seq = c(.3, 1, 1.5,  5, 10),
                                gamma_seq = c(.3, 1, 3, 5, 10))

ffm <- maximize_likelihood(hill_ffm, df, reps = 5)

pred_df <- make_predictions(hill_ffm, w)



