turnerModel = function(inputData,
                       constraints,
                       doTrace = FALSE,
                       initialWindow = NULL,
                       testHorizon = NULL,
                       expandRate = NULL,
                       doParallel = FALSE   # Not parallel by default
                       ){
  
  # (1) Check function dependencies (packages)
  
  require(caret)        # Handy time-interval slicer function for CV
  
  require(GA)           # Genetic Evolution Optimiser
  
  require(truncnorm)    # To generate random initial population member(s) for
                        # the optimisation routine with two heuristics used
  
  require(deSolve)      # Numerical methods differential equation solver
  
  # Packages required for parallelisation of GA
  if (doParallel == TRUE){
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
                     run = 50, # Halt if no. consecutive gens w/out improvement
                     parallel = doParallel, # Snow (windows), multicore (MacOS)
                     seed = 12345 # Seed for replication later
                     )
    
    # Reference: https://www.jstatsoft.org/article/view/v053i04
    # Reference: https://cran.r-project.org/web/packages/GA/GA.pdf
    # Reference: https://cran.r-project.org/web/packages/GA/vignettes/GA.html
    
    # Extract fitted values (model parameters, results etc)
    fittedPars = as.data.frame(t(as.numeric(fittedModel@solution[1,])))
    fittedPars[1,10] = abs(as.numeric(fittedModel@fitnessValue))
    colnames(fittedPars) = c("kg","kh","Tg","Th","alpha","beta","p0","g0","h0",
                             "MSE")
    
    # Compute modeled values, forecast errors
    
      # Compute modeled performance values and isolate for train/test
    
      fittedPerf = turnerCompute(parmsAndICS = as.numeric(fittedPars[1,1:9]),
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
      fittedPars[1,"RSQTrain"] = RSQtrain
      fittedPars[1,"RMSETrain"] = RMSEtrain
      fittedPars[1,"RMSETest"] = RMSEtest
      fittedPars[1,"MAPETest"] = MAPEtest
    
    # Object to return
      returnObject = list("fittedPars" = fittedPars,
                          "fittedPerf" = fittedPerf,
                          "optimModel" = fittedModel)
    
    # Return results for current slice
    return(returnObject)
  
  } # End of turnerCalibrate() function
  
  
  # (4) Slice data into expanding windows (Cross-validation split)
  sliceIntervals = createTimeSlices(inputData$days,
                                     initialWindow = initialWindow,
                                     horizon = testHorizon,
                                     fixedWindow = FALSE,
                                     skip = expandRate)
  
  nIntervals = length(sliceIntervals$train)
  
  # (5) Implement model - loop over the slices by calling turnerCalibrate()
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
    MAPEvals[i] = sliceModels[[i]]$fittedPars$MAPETest
  }
  
  lowestMAPE = which.min(MAPEvals)
  bestModel = sliceModels[[lowestMAPE]]$fittedPars
  bestModelp = sliceModels[[lowestMAPE]]$fittedPerf
  
  PerfVals = matrix(data = NA, nrow = length(bestModelp$days), ncol = nIntervals)
  for (i in 1:nIntervals){
    PerfVals[,i] = sliceModels[[i]]$fittedPerf$pHat
  }
  
  # Summary statistics
  collateMatrix = matrix(data = NA, nrow = nIntervals, ncol = 14)
  for (i in 1:nIntervals){
    collateMatrix[i,1:14] = as.numeric(sliceModels[[i]]$fittedPars)
  }
  sliceStats = apply(collateMatrix, 2, summary)
  colnames(sliceStats) = c("kg", "kh", "Tg", "Th", "alpha", "beta",
                             "p0", "g0", "h0", "MSE", "RSQtrain", "RMSEtrain",
                             "RMSEtest", "MAPEtest")
  colnames(collateMatrix) = c("kg", "kh", "Tg", "Th", "alpha", "beta",
                              "p0", "g0", "h0", "MSE", "RSQtrain", "RMSEtrain",
                              "RMSEtest", "MAPEtest")
  
  # Plot best set
  plot(bestModelp$days, bestModelp$loads, type = "h",
       ylim = c(0,
                max(c(bestModelp$G, bestModelp$H, bestModelp$pHat,
                      bestModelp$p, bestModelp$loads),na.rm=TRUE)+10),
       col = "grey85", ylab = "Arbitrary units (a.u)",
       xlab = "Day", main = "Key results: Best set found (MAPE_test)")
  points(bestModelp$days, bestModelp$p, pch = 10, col = "purple")
  lines(bestModelp$days, bestModelp$pHat, col = "black")
  lines(bestModelp$days, bestModelp$G, col = "blue")
  lines(bestModelp$days, bestModelp$H, col = "red")
  legend("topleft", c("Training load",
                      "Measured performance",
                      "Modelled performance",
                      "Fitness trace",
                      "Fatigue trace"),
         fill = c("grey85", "purple", "black", "blue", "red"), cex = 0.7)

  # Plot all sets (interval/region plot)
  dailymin = apply(PerfVals, 1, min)
  dailymax = apply(PerfVals, 1, max)
  nDays = length(bestModelp$days)
  plot(bestModelp$days, bestModelp$loads, type = "h", col = "grey85",
       ylim = c(0,
                max(c(),na.rm = TRUE)+10),
       xlab = "day",
       ylab = "Arbitrary units (a.u)",
       main = "All train/test sets (slices/windows)")
  polygon(c(rev(nDays),nDays),c(rev(dailymax), dailymin),col="orange", 
          border="black", lty = 2)
  points(bestModelp$days, bestModelp$p, col = "purple", pch = 20)
  legend("topright", c("Training load",
                       "Measured perf",
                       paste0("Modelled perf (all slices, n=",nIntervals,")")),
         fill = c("grey85", "purple", "orange"), cex = 0.75)
  
  # Print results to console
  print("Slice Parameter Sets:", quote = FALSE)
  print("------------------------------------", quote = FALSE)
  print(collateMatrix)
  print("", quote = FALSE)
  print("Summary Statistics (across slices)")
  print("------------------------------------", quote = FALSE)
  print(sliceStats)
  print("", quote = FALSE)
  print("Best Set Found", quote = FALSE)
  print("------------------------------------", quote = FALSE)
  print(bestModel)
  
  # Return fitted models to user
  returnObject = list("bestModel" = bestModel,
                      "bestModelPerf" = bestModelp,
                      "sliceSummary" = collateMatrix,
                      "sliceStats" = sliceStats,
                      "sliceModels" = sliceModels)
  
  return(returnObject)
}