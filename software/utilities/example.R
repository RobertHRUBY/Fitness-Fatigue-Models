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

  # Generate
  synthetic_pars <- c(100, 1.1, 25, 1.3, 5) # c(p*, kg, Tg, kh, Th)
  training_loads <- loads <- data.frame("day" = 0:100, 
                                        "load" = c(0, rep(c(1, 1.2, 0.5, 1.8, 2, 0.25, 0.7, 0.9, 0, 0.5, 
                                                   1, 0.8, 1.2, 1.3, 0.9, 0, 0, 2, 1.1, 0.5), 5) ) )
  performances <- standardPredict(pars = synthetic_pars,
                                  loads = training_loads,
                                  initialPars = c(10,5))
  performances <- performances[,1:2]
  performances <- performances[seq(1, 100, 3),]

  # Plot
  plot(performances$performance, type = "p", col = "blue", ylab = "Performance [a.u]",
       xlab = "Day", main = "Synthetic data for the example", cex.main = 0.75)
  
  # Remove synthetic parameters to avoid any confusion
  rm(synthetic_pars)
  
# Example: Fitting the various models to the synthetic data via different algorithms
#          and approaches (e.g. NLS or MLE)

  # Package dependencies
  require(optimx)
  require(GA)
  require(pso)
  require(cmaes)
  require(DEoptim)
  
  # Standard model (fitting the model to synthetic data via nonlinear least-squares)
  
    # Set up some box constraints for the examples c(p*, kg, Tg, kh, Th, qg, qh)
    box_constraints <- data.frame("lower" = c(50, 0.1, 1, 0.1, 1, 0, 0),
                                  "upper" = c(150, 3, 50, 3, 50, 20, 20))
    
    # Set up some starting parameter values for the derivative-based methods
    starting_guess <- c(97, 1.3, 30, 1.6, 8, 3, 1) # c(p*, kg, Tg, kh, Th, qg, qh)
  
    # Algorithm 1: L-BFGS-B (quasi-Newton) - Requires starting values (First-order)
    # We also request to include initial components (and have specified them in box_constraints)
    # and par
    fittedModel1 <- optimx(par = starting_guess,
                           fn = standardObjectiveSS,
                           method = "L-BFGS-B",
                           lower = box_constraints$lower,
                           upper = box_constraints$upper,
                           control = list(maxit = 100),
                           perfVals = performances,
                           loads = training_loads,
                           # We have included initial conditions in par and lower/upper
                           initial = TRUE)
    
    # Algorithm 2: Genetic algorithm - doesn't require starting values
    fittedModel2 <- GA::ga(type = "real-valued",
                           fitness = standardObjectiveSS,
                           perfVals = performances,
                           loads = training_loads,
                           lower = box_constraints$lower,
                           upper = box_constraints$upper,
                           maxiter = 100,
                           monitor = TRUE,
                           optim = TRUE,
                           maximise = TRUE, # Note this returns -1 * RSS as GA maximises
                           initial = TRUE
                           ) 
      
      
      GA(par = starting_guess,
                           fn = standardObjectiveSS,
                           method = "L-BFGS-B",
                           lower = box_constraints$lower,
                           upper = box_constraints$upper,
                           control = list(maxit = 100),
                           perfVals = performances,
                           loads = training_loads,
                           # We have included initial conditions in par and lower/upper
                           initial = TRUE)
  
  
  # Standard model (Maximum Likelihood Estimation)

# pars <- c(100, 1, 25, 1.2, 5)  # Parameters in order (p*, k_g, Tau_g, k_h, Tau_h)

# Training loads (inputs)
# loads <- data.frame("day" = c(0,1,2,3,4,5), "load" = c(100, 100, 100, 100, 100))

# Measured performances (model target)
# perfVals <- data.frame("day" = c(3,4), "performance" = c(500, 200))