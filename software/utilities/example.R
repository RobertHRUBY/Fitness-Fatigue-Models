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
  
  # -----------------------------------------------------------------------------------
  # Standard model (fitting the model to synthetic data via nonlinear least-squares)
  # -----------------------------------------------------------------------------------
  
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
      
      # Compute Performance
      fittedModel1_perf <- standardPredict(pars = as.numeric(fittedModel1[1:5]), loads = training_loads,
                                           initialPars = as.numeric(fittedModel1[6:7]))
    
      # Algorithm 2: Genetic algorithm - doesn't require starting values
      fittedModel2 <- GA::ga(type = "real-valued",
                             fitness = standardObjectiveSS,
                             perfVals = performances,
                             loads = training_loads,
                             lower = box_constraints$lower,
                             upper = box_constraints$upper,
                             maxiter = 100,
                             monitor = TRUE,
                             optim = TRUE,    # Include local search (BFGS) at stochastic intervals
                             maximise = TRUE, # We pass this to fn to get back -1 * RSS as GA maximizes by default
                             initial = TRUE
                             )
    
      # Compute performance (Simulate model)
      fittedModel2_perf <- standardPredict(pars = as.numeric(fittedModel2@solution[1:5]),
                                           loads = training_loads,
                                           initialPars = as.numeric(fittedModel2@solution[6:7]))
      
  # -----------------------------------------------------------------------------------  
  # Standard model (Maximum Likelihood Estimation)
  # -----------------------------------------------------------------------------------   
      
      # Set up some box constraints for the examples c(p*, kg, Tg, kh, Th, sigma, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 0.1, 1, 0, 0, 0),
                                    "upper" = c(150, 3, 50, 3, 50, 5, 20, 20))
      
      # Set up some starting parameter values for the derivative-based methods
      starting_guess <- c(97, 1.3, 30, 1.6, 8, 1, 3, 1) # c(p*, kg, Tg, kh, Th, sigma, qg, qh)
      
      fittedModel3 <- cmaes::cma_es(par = starting_guess,
                                    fn = standardObjectiveLL,  # Log likelihood function (Negative by default)
                                    lower = box_constraints$lower,
                                    upper = box_constraints$upper,
                                    control = list(maxit = 100),
                                    perfVals = performances,
                                    loads = training_loads,
                                    initial = TRUE)
      
      # Compute Performance
      fittedModel3_perf <- standardPredict(pars = fittedModel3$par[1:5],
                                           loads = training_loads,
                                           initialPars = fittedModel3$par[7:8])
      
      # Repeating with the GA algorithm (which again maximises by default so we pass maximise = TRUE to fn)
      fittedModel4 <- GA::ga(type = "real-valued",
                             fitness = standardObjectiveLL,
                             perfVals = performances,
                             loads = training_loads,
                             lower = box_constraints$lower,
                             upper = box_constraints$upper,
                             maxiter = 100,
                             monitor = TRUE,
                             optim = TRUE,    # Include local search (BFGS) at stochastic intervals
                             maximise = TRUE, # We pass this to fn to get back normal LL as GA maximizes by default
                             initial = TRUE
      )
      
      # Compute performance (Simulate model)
      fittedModel4_perf <- standardPredict(pars = as.numeric(fittedModel4@solution[1:5]),
                                           loads = training_loads,
                                           initialPars = as.numeric(fittedModel4@solution[7:8]))
      
  # -----------------------------------------------------------------------------------
  # Fitness-delay model: NLS
  # -----------------------------------------------------------------------------------

      # Set up some box constraints for the examples c(p*, kg, Tg, Tg2, kh, Th, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 1, 0.1, 1, 0, 0),
                                    "upper" = c(150, 3, 50, 20, 3, 50, 20, 20))
      
      # Set up parameter structure for PSO algorithm
      par_structure <- c(NA, NA, NA, NA, NA, NA, NA, NA) # c(p*, kg, Tg, Tg2, kh, Th, qg, qh)
      
      # Particle swarm optimisation
      fittedModel5 <- psoptim(par = par_structure,
                              fn = fitnessDelayObjectiveSS,
                              lower = box_constraints$lower,
                              upper = box_constraints$upper,
                              control = list(maxit = 100),
                              initial = TRUE,  # Find initial component values
                              loads = training_loads,
                              perfVals = performances)
      
      # Compute performance (Simulate model)
      fittedModel5_perf <- fitnessDelayPredict(pars = fittedModel5$par[1:6],
                                               loads = training_loads,
                                               initialPars = fittedModel5$par[7:8])
      
  # -----------------------------------------------------------------------------------
  # Fitness-delay model: Maximum likelihood estimation
  # -----------------------------------------------------------------------------------
      
      # Set up some box constraints for the examples c(p*, kg, Tg, Tg2, kh, Th, sigma, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 1, 0.1, 1, 0, 0, 0),
                                    "upper" = c(150, 3, 50, 20, 3, 50, 5, 20, 20))
      
      # Differential evolution algorithm
      fittedModel6 <- DEoptim(fn = fitnessDelayObjectiveLL,
                              lower = box_constraints$lower,
                              upper = box_constraints$upper,
                              initial = TRUE,
                              loads = training_loads,
                              perfVals = performances)

      # Compute performance (Simulate model)
      fittedModel6_perf <- fitnessDelayPredict(pars = as.numeric(fittedModel6$optim$bestmem[1:6]),
                                                                 loads = training_loads,
                                                                 initialPars = as.numeric(fittedModel6$optim$bestmem[8:9]))
    
  # -----------------------------------------------------------------------------------
  # VDR Model: Nonlinear least-squares
  # -----------------------------------------------------------------------------------
      
      # Set up some box constraints for the examples c(p*, kg, Tg, kh, Th, Th2, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 0.1, 1, 1, 0, 0),
                                    "upper" = c(150, 3, 50, 3, 50, 10, 20, 20))
      
      # Genetic algorithm
      fittedModel7 <- GA::ga(type = "real-valued",
                             fitness = vdrObjectiveSS,
                             perfVals = performances,
                             loads = training_loads,
                             lower = box_constraints$lower,
                             upper = box_constraints$upper,
                             maxiter = 100,
                             monitor = TRUE,
                             optim = TRUE,    # Include local search (BFGS) at stochastic intervals
                             maximise = TRUE, # We pass this to fn to get back -1 * RSS as GA maximizes by default
                             initial = TRUE
      )
      
      # Compute performance (Simulate model)
      fittedModel7_perf <- vdrPredict(pars = as.numeric(fittedModel7@solution[1:6]),
                                      loads = training_loads,
                                      initialPars = as.numeric(fittedModel7@solution[7:8]))
    
  # -----------------------------------------------------------------------------------
  # VDR Model: Maximum likelihood estimation
  # -----------------------------------------------------------------------------------
      
      # Set up some box constraints for the examples c(p*, kg, Tg, kh, Th, Th2, sigma, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 0.1, 1, 1, 0, 0, 0),
                                    "upper" = c(150, 3, 50, 3, 50, 10, 5, 20, 20))
      
      # Genetic algorithm
      fittedModel8 <- GA::ga(type = "real-valued",
                             fitness = vdrObjectiveLL,
                             perfVals = performances,
                             loads = training_loads,
                             lower = box_constraints$lower,
                             upper = box_constraints$upper,
                             maxiter = 100,
                             monitor = TRUE,
                             optim = TRUE,    # Include local search (BFGS) at stochastic intervals
                             maximise = TRUE, # We pass this to fn to get back normal LL as GA maximizes by default
                             initial = TRUE
      )
      
      # Compute performance (Simulate model)
      fittedModel8_perf <- vdrPredict(pars = as.numeric(fittedModel8@solution[1:6]),
                                      loads = training_loads,
                                      initialPars = as.numeric(fittedModel8@solution[8:9]))
    
  # -----------------------------------------------------------------------------------
  # VDR Model: Cross-validation (Train-test) via multistart L-BFGS-B algorithm
  # -----------------------------------------------------------------------------------
    
      # Package dependencies
      require(doSNOW)
      require(foreach)
      require(RcppAlgos)
      require(parallel)
      require(caret)
      
      # Set up some box constraints for the examples c(p*, kg, Tg, kh, Th, Th2, sigma, qg, qh)
      box_constraints <- data.frame("lower" = c(50, 0.1, 1, 0.1, 1, 1, 0, 0, 0),
                                    "upper" = c(150, 3, 50, 3, 50, 10, 5, 20, 20))
      
      # Set up proper input data structure for this function
      input_data <- loads
      input_data$performances <- rep(NA, length(loads$load))
      input_data[performances$day + 1, "performances"] <- performances$performance
      input_data$block <- c(rep(1,51), rep(2,50))
      
      # Run the function
      fittedModel9_perf <- expandingWindow_CV(dat = input_data,
                                              bounds = box_constraints,
                                              initial = TRUE)
      