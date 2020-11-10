turnerModel = function(inputData,
                       constraints,
                       doTrace = FALSE,
                       initialWindow = NULL,
                       testHorizon = NULL,
                       expandRate = NULL,
                       doParallel = FALSE,   # Do not parallise by default
                       ){
  
  # (1) Check function dependencies (packages)
  
  require(caret)        # Handy time-interval slicer function for CV
  
  require(GA)           # Genetic Evolution Optimiser
  
  require(truncnorm)    # To generate random initial population member(s) for
                        # the optimisation routine with two heuristics used
  
  require(deSolve)      # Numerical methods differential equation solver
  
  # Required for parallelisation of GA
  if (doParallel = TRUE){
  require(parallel)
  require(foreach)
  require(iterators)
  require(doParallel)
  require(doRNG)        # allows to seed GA search to facilitate replication
  }
  
  # (2) Function input validation checks
  
  # Put a check on the number of observed measurements in the dataset
  if (sum(!is.na(inputData$performances)) < 40){
    print("Number of observed measurements for fitting the model is small",
          quote = FALSE)
    print("Results likely to be poor. Are you sure you want to continue?",
          quote = FALSE)
    invisible(readline(prompt = "Press [enter] to continue"))
  }
  
  # Set default expanding window cross-validation parameters (if not passed)
  # If initialWindow specified, make user also supply testHorizon and expandRate
  if(!is.null(initialWindow) && (is.null(testHorizon) || is.null(expandRate))){
    stop(paste0("Please also supply testHorizon and expandRate arguments with",
                " initialWindow"))
  }
  
  # If no initialWindow specified, or just testHorizon or expandRate arguments
  # supplied. Just use following default(s)
  if(is.null(initialWindow)){
    initialWindow = round(length(inputData$days) * 0.60, 0)   # 60% of data
    testHorizon = round(length(inputData$days) * 0.2, 0)      # 20% of data
    expandRate = round(length(inputData$days) * 0.04, 0)      # 4% of data
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
  
  turnerCalibrate = function(sliceIntervals, currentSlice, inputData){
    
    # Model computation function (i.e. given known parameters)
    
    turnerCompute = function(parmsAndICS, inputData){
      
      compData = data.frame("days" = 0:length(inputData$days),
                            "G" = c(rep(0, length(inputData$days)+1)),
                            "H" = c(rep(0, length(inputData$days)+1)),
                            "p" = c(NA, inputData$performances),
                            "pHat" = c(rep(0, length(inputData$days)+1)),
                            "loads" = c(0, inputData$loads)
                            )
      
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
          compData$G[j] = unname(out[1,2])
          compData$H[j] = unname(out[1,3])
        } else{
          compData$G[j] = unname(out[2,2])
          compData$H[j] = unname(out[2,3])
        }
        
        compData$pHat[j] = parmsAndICS[7] + compData$G[j] - compData$H[j]
        
      } # End of solve loop
      
      return(compData)
      
    } # End of turnerCompute
    
    # Mean-squared error (objective) function (i.e. used in optimization)
    
    turnerMSE = function(parmsAndICS){
      
      # Create data structure for the solver
      
      compData = data.frame("days" = 0:length(trainingData$days),
                            "G" = c(rep(0, length(trainingData$days)+1)),
                            "H" = c(rep(0, length(trainingData$days)+1)),
                            "p" = c(NA, trainingData$performances),
                            "pHat" = c(rep(0, length(trainingData$days)+1)),
                            "loads" = c(0, trainingData$loads))
      
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
          compData$G[j] = unname(out[1,2])
          compData$H[j] = unname(out[1,3])
        } else{
          compData$G[j] = unname(out[2,2])
          compData$H[j] = unname(out[2,3])
        }
        
        compData$pHat[j] = parmsAndICS[7] + compData$G[j] - compData$H[j]
        
      } # End of solve loop
      
      # Wrap in cost function (Mean squared error)
      compSubset = subset(compData, !is.na(compData$p) == TRUE)
      MSE = mean((compSubset$p - compSubset$pHat)^2)
      
      # Return MSE value (note minus due to minimization in GA)
      return(-MSE)
    } # End of turnerMSE function
    
    # Subset user input into training and testing data for current slice
    
    trainingData = inputData[sliceIntervals$train[[currentSlice]],]
    testingData = inputData[sliceIntervals$test[[currentSlice]],]
    
    # Fit the model (Genetic evolution strategy)
    
    # Original paper uses: tournament selection, BLX-alpha crossover,
    #                      Gaussian mutation, external penalties to enforce
    #                      constraints (not included here)
    
    require(GA)
    fittedModel = ga(type = "real-valued", 
                     fitness = turnerMSE,
                     lower = constraints$lower,
                     upper = constraints$upper,
                     maxiter = 500,
                     monitor = doTrace, # monitor the optimization
                     popSize = 400,
                     elitism = base::max(1, round(popSize*0.05)),
                     selection = gareal_tourSelection, # Tournament selection
                     crossover = gareal_blxCrossover,  # BLX (blend crossover)
                     mutation = gareal_rsMutation,     # Random around solution
                     run = 25, # Halt if no. consecutive gens w/out improvement
                     parallel = doParallel, # Snow (windows), multicore (MacOS)
                     seed = 12345 # Seed for replication later
                     )
    
    # Reference: https://www.jstatsoft.org/article/view/v053i04
    
    # Extract fitted values (model parameters, results etc)
    parmsAndICS = c()
    
    # Compute modeled values, forecast errors
    
      # Compute modeled performance values and isolate for train/test
    
      fittedPerf = turnerCompute(parmsAndICS = fittedPars,
                                 inputData = inputData)
      
      fittedPerfTrain = subset(fittedPerf[1:length(trainingData$days)+1,], 
                               !is.na(
                                 fittedPerf[1:length(trainingData$days)+1,"p"]
                                      ) == TRUE)
      
      testHead = head(sliceIntervals$test[[currentSlice]],n=1)+1
      testTail = tail(sliceIntervals$test[[currentSlice]],n=1)+1
      
      fittedPerfTest = subset(fittedPerf[testHead:testTail,], 
                              !is.na(fittedPerf[testHead:testTail,"p"]) == TRUE)
    
      # Fit and error statistic functions
      
        # R-squared function
        RSQfunc = function(x,y){
          rsq = 1 - ( (sum((y-x)^2))  / (sum((y-mean(y))^2))) 
          return(round(rsq*100,3))
        }
      
        # RMSE function
        RMSEfunc = function(x,y,n){
          z = (x - y)^2
          return(sqrt(sum(z)/n))
        }
          
        # MAPE function
        MAPEfunc = function(x,y,n){
          return(mean(abs((x-y)/x))*100)
        }
      
      # Training set statistics
        
      RSQtrain = RSQfunc(x = fittedPerfTrain$pHat, y = fittedPerfTrain$p)
      RMSEtrain = RMSEfunc(x = fittedPerfTrain$pHat, y = fittedPerfTrain$p,
                           n = length(fittedPerfTrain$days))
      
      # Test set statistics
      
      RMSEtest = RMSEfunc(x = fittedPerfTest$pHat, y = fittedPerfTest$p,
                          n = length(fittedPerfTest$days))
      MAPEtest = MAPEfunc(x = fittedPerfTest$p, y= fittedPerfTest$pHat,
                          n = length(fittedPerfTest$days))  
        
      # Compile results
      fittedStats = data.frame("RSQtrain" = RSQtrain, "RMSEtrain" = RMSEtrain,
                             "RMSEtest" = RMSEtest, "MAPEtest" = MAPEtest)
    
    # Object to return
      returnObject = list("fittedModel" = fittedModel,
                          "fittedPerf" = fittedPerf,
                          "fittedStats" = fittedStats)
    
    # Return results for current slice
    return(returnObject)
  
  } # End of turnerCalibrate() function
  
  
  # (4) Split and isolate data into expanding windows (Cross-validation split)
  sliceIntervals = createTimeSlices(inputData$days,
                                     initialWindow = initialWindow,
                                     horizon = testHorizon,
                                     fixedWindow = FALSE,
                                     skip = expandRate)
  
  nIntervals = length(sliceIntervals$train)
  
  # (5) Implement model (loop over the slices by calling the turnerCalibrate fn
  sliceModels = list()
  for (i in 1:nIntervals){
    currentSlice = i
    print(paste0("Training model ~ Slice ", currentSlice, " ..."), quote =FALSE)
    sliceModels[[i]] = turnerCalibrate(sliceIntervals, currentSlice, inputData)
    print(paste0("Slice ", currentSlice, " estimation complete"), quote = FALSE)
  }
  
  # (6) Tabulate results (print to console) and generate plots 
  
  # Best set found (by lowest MAPE_test)
  MAPEvals = c()
  for (i in 1:nIntervals){
    MAPEvals[i] = sliceModels[[i]]$fittedStats[1,4]
  }
  lowestMAPE = which.min(MAPEvals)
  bestModel = sliceModels[[lowestMAPE]]
  
  # Summary statistics
  collateMatrix = matrix(data = NA, nrow = nIntervals, ncol = 14)
  for (i in 1:nIntervals){
    collateMatrix[i,1:10] = as.numeric(sliceModels[[i]]$fittedModel[1,1:10])
    collateMatrix[i,11:14] = as.numeric(sliceModels[[i]]$fittedStats[1,1:4])
  }
  colnames(sliceSummary) = c("k_g", "k_h", "tau_g", "tau_h", "alpha", "beta",
                             "p0", "g0", "h0", "MSE", "RSQtrain", "RMSEtrain",
                             "RMSEtest", "MAPEtest")
  sliceSummary = apply(collateMatrix, 1, summary)
  
  # Plot best set
  
  # Plot all sets (interval/region plot)
  
  # Print results to console
  
  # Return fitted models to user
  returnObject = list("bestModel" = bestModel,
                      "sliceSummary" = collateMatrix,
                      "sliceStats" = sliceSummary,
                      "sliceModels" = sliceModels)
  
  return(returnObject)
}