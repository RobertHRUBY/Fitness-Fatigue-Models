source("didactic/ffmFunctions.R")

basic_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                              xi = 20)

w <- c(c(0:100), rep(0, 20), c(0:100))
df <- simulate(basic_ffm, w)

ffm <- initialize_ffm_from_data(df)

make_predictions(ffm, w) # you don't need y to make predictions, unlike KF!

increase_likelihood(ffm, df)

ffm_orig <- ffm
ffm <- increase_likelihood(ffm, df, reps = 10)

predict(ffm, df)

print(basic_ffm)


ffm_add_initial <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                                    xi = 20, q_g = 300, q_h = 500)
print(ffm_add_initial)


hill_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                             xi = 20, gamma = 2, delta = 1.5,
                             q_g = 300, q_h = 500)
print(hill_ffm)


vdr_ffm <- create_ffm_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                            xi = 20, tau_h2 = 2, gamma = 2, delta = 1.5,
                            q_g = 300, q_h = 500)
print(vdr_ffm)

# Not yet implemented ----------------------------

vdr_ffm <- initialize_ffm_from_data(df, tau_g_seq, tau_h_seq, tau_h2_seq)

vdr_ffm <- increase_likelihood(vdr_ffm, df, reps = 5)

pred <- predict(vdr_ffm)

