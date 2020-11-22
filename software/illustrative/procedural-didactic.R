source("ffmFunctions.R")

# Basic FFM ---------------------------------------------
basic_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
			      tau_h = 15, xi = 20)

w <- c(c(0:100), rep(0, 20), c(0:100), rep(0, 200))
df <- simulate(basic_ffm, w)

ffm <- initialize_ffm_from_data(df)

ffm_orig <- ffm
ffm <- increase_likelihood(ffm, df, reps = 1)

print(ffm)

pred <- make_predictions(ffm, df$w)$y_hat
plot(df$y)
points(pred, col = 'red')


# Add initial values -------------------------------------
ffm_add_initial <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50,
				    tau_h = 15, xi = 20, q_g = 40, q_h = 100)

df <- simulate(ffm_add_initial, w)

ffm <- initialize_ffm_from_data(df)

ffm_orig <- ffm
ffm <- increase_likelihood(ffm, df, reps = 1)
print(ffm)

# Add VDR
vdr_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                            xi = 20, tau_h2 = 2, 
                            q_g = 300, q_h = 500)
print(vdr_ffm)
df <- simulate(ffm_add_initial, w)

ffm <- initialize_ffm_from_data(df, tau_h2_seq = c(1, 2, 3))

ffm_orig <- ffm
ffm <- increase_likelihood(ffm, df, reps = 1)
print(ffm)




# Add Hill Transform

hill_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                             xi = 20, gamma = 2, delta = 1.5,
                             q_g = 300, q_h = 500)
print(hill_ffm)


# Not yet implemented ----------------------------

vdr_ffm <- initialize_ffm_from_data(df, tau_g_seq, tau_h_seq, tau_h2_seq)

vdr_ffm <- increase_likelihood(vdr_ffm, df, reps = 5)

pred <- predict(vdr_ffm)

