# ------------------------------------------------------------------------------
# Example inputs for functions contained in the file
# ------------------------------------------------------------------------------

# Source in the required functions
source("ffm_simulation.R")
source("standard_ffm_objective.R")
source("fitness_delay_ffm_objective.R")
source("vdr_ffm_objective.R")
source("cross_validation.R")


# Develop some synthetic data via simulation to use for the examples

synthetic_pars <- c(100, 1.1, 25, 1.3, 5, 10, 5) # c(p*, kg, Tg, kh, Th, qg, qh)
training_loads <- loads <- data.frame("day" = 0:100, 
                                      "load" = c(0, rep(c(1, 1.2, 0.5, 1.8, 2, 0.25, 0.7, 0.9, 0, 0.5, 
                                                 1, 0.8, 1.2, 1.3, 0.9, 0, 0, 2, 1.1, 0.5), 5) ) )
performances <- 

# pars <- c(100, 1, 25, 1.2, 5)  # Parameters in order (p*, k_g, Tau_g, k_h, Tau_h)

# Training loads (inputs)
# loads <- data.frame("day" = c(0,1,2,3,4,5), "load" = c(100, 100, 100, 100, 100))

# Measured performances (model target)
# perfVals <- data.frame("day" = c(3,4), "performance" = c(500, 200))