turnerModel = function(inputData,
                       constraints,
                       doTrace = FALSE,
                       useEvolution = FALSE,
                       initialWindow = NULL,
                       testHorizon = NULL,
                       expandRate = NULL
                       ){
  
  # (1) Check function dependencies
  
  require(caret)        # Handy time-interval slicer function for CV
  if (useEvolution == TRUE){
  require(DEoptim)      # Differential evolution optimiser
  }
  require(truncnorm)    # To generate random initial population member(s) for
                        # the optimisation routine with two heuristics used
  require(deSolve)      # Numerical methods differential equation solver
  
  # (2) Function input validation checks
  
  # Set default expanding window cross-validation parameters (if not passed)
  # If initialWindow specified, make user also supply testHorizon and expandRate
  if(!is.null(initialWindow) && (is.null(testHorizon) || is.null(expandRate))){
    stop(paste0("Please also supply testHorizon and expandRate arguments with",
                " initialWindow"))
  }
  
  # If no initialWindow specified, or just testHorizon or expandRate arguments
  # supplied. Just use following default(s)
  if(is.null(initialWindow)){
    initialWindow <- round(length(inputData$days) * 0.60, 0)   # 60% of data
    testHorizon <- round(length(inputData$days) * 0.2, 0)      # 20% of data
    expandRate <- round(length(inputData$days) * 0.04, 0)      # 4% of data
    print("No initialWindow argument supplied", quote = FALSE)
    print("Defaults used for initialWindow, testHorizon and expandRate",
          quote = FALSE)
    print("-----------------------------------------------------------",
          quote = FALSE)
    print(paste0("initialWindow = ", initialWindow, " days | (60%)"),
          quote = FALSE)
    print(paste0("testHorizon = ", testHorizon, " days | (20%)"),
          quote = FALSE)
    print(paste0("expandRate = ", expandRate, " days | (4%)"),
          quote = FALSE)
    print("-----------------------------------------------------------",
          quote = FALSE)
    print("Check these are appropriate for your implementation",
          quote = FALSE)
  }
  
  # Check that the input data has the right colnames
  colnames(inputData) <- c("days", "performances", "loads")
  
  
  # (3) Develop calibration function
  
  turnerCalibrate = function(sliceIntervals, currentSlice){
    
    # Model computation function (i.e. given known parameters)
    
    turnerCompute = function(parmsAndICS){
      # TODO
    } # End of turnerCompute
    
    
    # Mean-squared error (objective) function (i.e. used in optimization)
    
    turnerMSE = function(parmsAndICS){
      
      # Function called by numerical ODE solver
      turnerSolve = function(t, y, parmsAndICS){
        r = c()
        r[1] = (parmsAndICS[1]*currentLoad) - 
          ((1/parmsAndICS[3]) * y["G"]^(parmsAndICS[5]))
        r[2] = (parmsAndICS[2]*currentLoad) - 
          ((1/parmsAndICS[4]) * y["H"]^(parmsAndICS[6]))
        return(list(r))
      } # end of turnerSolve
      
      # Solve model
      for (j in 1:length(compData$days)){
        
        currentLoad = compData$loads[j]
        
        if (j == 1){
          stateInit = c(G = parmsAndICS[8],     # Fitness
                        H = parmsAndICS[9])     # Fatigue
        } else{
          # Initialize based on previous value
          stateInit = c(G = compData$G[j-1],    # Fitness 
                        H = compData$H[j-1])    # Fatigue
        }
        
        t = 0:1
        out = ode(y = stateInit, times = t, func = turnerSolve, 
                  parms = parmsAndICS)
        
        if (j == 1){
          compData$G[j] <- unname(out[1,2])
          compData$H[j] <- unname(out[1,3])
        } else{
          compData$G[j] <- unname(out[2,2])
          compData$H[j] <- unname(out[2,3])
        }
        
        compData$pHat[j] <- parmsAndICS[7] + compData$G[j] - compData$H[j]
        
      } # End of solve loop
      
      # Wrap in cost function (Mean squared error)
      compSubset <- subset(compData, !is.na(compData$p) == TRUE)
      MSE <- mean((compSubset$p - compSubset$pHat)^2)
      
      # Return MSE value
      return(MSE)
    } # End of turnerMSE function
    
    # Subset user input into training and testing data for current slice
    
    trainingData = inputData[sliceIntervals$train[[currentSlice]],]
    testingData = inputData[sliceIntervals$test[[currentSlice]],]
    
    # Create data structure for the solver
    
    compData = data.frame("days" = 0:length(trainingData$days),
                          "G" = c(rep(0, length(trainingData$days)+1)),
                          "H" = c(rep(0, length(trainingData$days)+1)),
                          "p" = c(0, trainingData$performances),
                          "pHat" = c(rep(0, length(trainingData$days)+1)),
                          "loads" = c(0, trainingData$loads))
    
    # Fit the model (Two strategies made available)
    
    # Use Differential evolution (Slow)
    
    if (useEvolution == TRUE){
      
      NPop = 300
      
      #Create initial population matrix for parameters and initial conditions
      #(kg,kh,Tg,Th,alpha,beta,p0,g0,h0)
      popInit = matrix(data = NA, nrow = NPop, ncol = 9)
      for (k in 1:9){
        set.seed(100)
        popInit[,k] = rtruncnorm(n = NPop, a = constraints$lower[k],
                   b = constraints$upper[k], mean = constraints$upper[k]/2,
                   sd = constraints$upper[k]/5)
      }
      
      # Filter such that kh > kg and Tg > Th in initial population (Heuristic)
      for (l in 1:NPop){
        # If kg > kh then swap
        if (popInit[l,1] > popInit[l,2]){
          tempVal = popInit[l,1]
          popInit[l,1] = popInit[l,2]
          popInit[l,2] = tempVal
        }
        # If Tg < Th then swap
        if (popInit[l,3] < popInit[l,4]){
          tempVal = popInit[l,3]
          popInit[l,3] = popInit[l,4]
          popInit[l,4] = tempVal
        }
      }
      if (exists(tempVal)){
        rm(tempVal)
      }
      

      # Call DE optimiser
      
      fittedModel = DEoptim(fn = turnerMSE,
                            lower = constraints$lower,
                            upper = constraints$upper,
                            DEoptim.control(strategy = 6,
                                            NP = NPop, #Population members
                                            trace = doTrace, # Trace output
                                            CR = 0.5, # Crossover probability
                                            F = 0.8, # Differential weighting
                                            itermax = 100, # Max iterations
                                            initialpop = popInit
                                            )
                            )
      
      # Extract fitted values
      
      
    } # End of DE (Evolutionary) fitting method
    
    # If using limited memory modification of bounded BFGS (quasi-Newton)
    # (Default)
    if (useEvolution == FALSE){
      
      # Generate some guesses towards the starting values
      startingValues <- numeric(length = 9)
      for (m in 1:9){
        set.seed(100)
        startingValues[m] = rtruncnorm(1, a = constraints$lower[m],
                                       b = constraints$upper[m],
                                       mean = constraints$upper[m]/2,
                                       sd = constraints$upper[m]/10)
      }
      
      # Check conditions on starting values kg < kh and Tg > Th hold (Heuristic)
      if (startingValues[1] > startingValues[2]){
        tempVal = startingValues[1]
        startingValues[1] = startingValues[2]
        startingValues[2] = tempVal
      }
      if (startingValues[3] < startingValues[4]){
        tempVal = startingValues[3]
        startingValues[3] = startingValues[4]
        startingValues[4] = tempVal
      }
      if (exists(tempVal)){
        rm(tempVal)
      }
      
      # Tracing? or no? (default off)
      if (doTrace == TRUE){
        doTrace = 6
      } else{
        doTrace = 0
      }
      
      # Call the optimiser
      
      fittedModel = optim(fn = turnerMSE,
                          par = startingValues,
                          method = "L-BFGS-B",
                          lower = constraints$lower,
                          upper = constraints$upper,
                          control = list(
                            trace = doTrace,
                            maxit = 200
                                        )
                          )
      
      # Extract fitted values
      
    } # End of BFGS Fitting Method
    
    # Compute modeled values, forecast errors
    
      # R-squared (Train)
    
      # RMSE (Train)
    
      # RMSE (Test)
    
      # MAPE (Test) - This is the metric for selecting the 'best' parameter set
    
    # Return results for current slice
    return(fittedModel)
  }
  
  # Split and isolate data into expanding windows (Cross-validation split)
  sliceIntervals = createTimeSlices(inputData$days,
                                     initialWindow = initialWindow,
                                     horizon = testHorizon,
                                     fixedWindow = FALSE,
                                     skip = expandRate)
  
  # Implement model (loop over the slices by calling the turnerCalibrate fn
  sliceModels <- list()
  for (i in 1:length(sliceIntervals$train)){
    sliceModels[[i]] = turnerCalibrate(sliceIntervals, currentSlice = i)
  }
  
  # Tabulate results (print to console) and generate plots (best set, all sets)
  
  # Return fitted model to user
  return(sliceModels)
}

# Vector order is : k_g, k_h, T_g, T_h, alpha, beta, p0, g0, h0
constraints <- data.frame("lower" = c(0.1,0.1,1,1,0.5,0.5,25,5,5),
                          "upper" = c(10,10,50,50,5,5,200,100,100))
