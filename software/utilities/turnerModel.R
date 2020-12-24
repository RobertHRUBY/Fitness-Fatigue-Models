# Version 1.0
# Documentation: github.com/bsh2/fitness-fatigue-models/software/utilities/

turnerModel = function(inputData,
                       constraints,
                       doTrace = TRUE,
                       initialWindow = NULL,
                       testHorizon = NULL,
                       expandRate = NULL,
                       doParallel = FALSE,
                       maxIt = 1000,
                       popSize = 120,
                       gaSelection = 'gareal_tourSelection',
                       gaCrossover = 'gareal_blxCrossover',
                       gaMutation = 'gareal_rsMutation',
                       gaElitism = 7.5
                       ){
  
  # Basic reference info, parameter order through code file:
  # c(kg, kh, Tg, Th, alpha, beta, p*, g0, h0)
  
  # Dependencies (main)
  require(caret)
  require(GA)
  require(deSolve)
  
  # Dependencies (parallel implementation)
  if (doParallel == TRUE){
    require(parallel)
    require(foreach)
    require(iterators)
    require(doParallel)
    require(doRNG)
  }

# ------------------------------------------------------------------------------
# BASIC INPUT VALIDATION AND LOAD DEPENDENCIES
# ------------------------------------------------------------------------------

  # Column names of input data
  if (dim(inputData)[2] != 3){
    stop("Are you sure the input data is of 3 column form in order from L to R:
         days, performances, loads?")
  }
  colnames(inputData) <- c("days", "performances", "loads")
  
  # Box constraints
  if (!all(constraints$lower > 0) || !all(constraints$upper > 0)){
    stop("Bounds must be non-zero non-negative real values")
  }
  
  if (!all(constraints$lower <= constraints$upper)){
    stop("Bounds incorrectly specified. Check values")
  }
  
  if (constraints$lower[5] < 0.3 || constraints$lower[6] < 0.3){
    stop("lower bounds on alpha,beta are too low. Will cause
         local search to violate bounds and crash")
  }
  
  if (dim(constraints)[1] != 9){
    stop("Box constraints of incorrect dimension")
  }

  # Load series
  if (all(is.na(inputData$loads))){
    stop("Loads cannot contain NA values. Use 0 to indicate no training")
  }
  
  # Time point column (days)
  if (all(is.na(inputData$days))){
    stop("Days column in input cannot contain NA values. Use sequential vals")
  }
  
  # Number of performance measurements in the data set
  if (sum(!is.na(inputData$performances)) <= 40){
    print("Number of observed performance values for fitting model is small",
          quote = FALSE)
    print("Results are likely to be poor. Do you want to continue?", 
          quote = FALSE)
    invisible(readline(prompt = "Press [enter] to continue"))
  }
  
  # If user supplies initialWindow but not testHorizon or expandRate
  if(!is.null(initialWindow) && (is.null(testHorizon)||is.null(expandRate))){
    stop("If supplying initialWindow, testHorizon and expandRate must
                   also be supplied")
  }
  
  # If no expanding window arguments specified, use following defaults
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
  } else{
    # Or convert supplied values to 'days'
    initialWindow = round(length(inputData$days) * initialWindow/100, 0)
    testHorizon = round(length(inputData$days) * testHorizon/100, 0)
    expandRate = round(length(inputData$days) * expandRate/100, 0)
  }

# ------------------------------------------------------------------------------
# DEFINE FUNCTIONS USED IN THE PROCESS
# ------------------------------------------------------------------------------

  # Model Computation Function
  computeModel = function(parms, loadSeries){
    # Format data into correct format
    compData = data.frame("days" = 0:length(loadSeries),
                          "G" = c(rep(0, length(loadSeries)+1)),
                          "H" = c(rep(0, length(loadSeries)+1)),
                          "pHat" = c(rep(0, length(loadSeries)+1)),
                          "loads" = c(0, loadSeries)
    )
    # Function called by numerical ODE solver
    turnerSolve = function(t, y, parms){
      r = c()
      r[1] = (parms[1]*currentLoad) - 
        ((1/parms[3]) * y["G"]^(parms[5]))
      r[2] = (parms[2]*currentLoad) - 
        ((1/parms[4]) * y["H"]^(parms[6]))
      return(list(r))
    } # end of turnerSolve
    # Solve model
    for (j in 1:length(compData$days)){
      currentLoad = compData$loads[j]
      if (j == 1){
        stateInit = c(G = parms[8],     # Fitness
                      H = parms[9])     # Fatigue
      } else{
        # Initialize based on previous value
        stateInit = c(G = compData$G[j-1],    # Fitness 
                      H = compData$H[j-1])    # Fatigue
      }
      t = 0:1
      out = ode(y = stateInit, times = t, func = turnerSolve, 
                parms = parms)
      if (j == 1){
        compData$G[j] = unname(out[1,2])
        compData$H[j] = unname(out[1,3])
      } else{
        compData$G[j] = unname(out[2,2])
        compData$H[j] = unname(out[2,3])
      }
      compData$pHat[j] = parms[7] + compData$G[j] - compData$H[j]
    } # End of solve loop
    return(compData)
  } # End of turnerCompute
  
  # Global Loss Function
  objectiveFn = function(parmsAndICS){
    
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
    RSS = sum((compSubset$p - compSubset$pHat)^2)
    
    # Return RSS value (note minus due to minimization in GA)
    return(-RSS)
  } # End of turner RSS fitness function
  
  # Cross-validation fitting function
  crossValidate = function(objectiveFn,
                           trainingData,
                           testingData){
    
    sliceModel = ga(type = "real-valued",
                    fitness = objectiveFn,
                    lower = constraints$lower,
                    upper = constraints$upper,
                    maxiter = maxIt,
                    monitor = doTrace,
                    popSize = popSize,
                    optim = TRUE,
                    optimArgs = list(method = "L-BFGS-B",
                                     control = list(maxit = 1500)
                    ),
                    elitism = gaElitism,
                    selection = gaSelection,
                    crossover = gaCrossover,
                    mutation = gaMutation,     # Random
                    run = 150, # Halt value
                    parallel = doParallel, # multi-platform
                    seed = 12345 # Seed for replication later
    ) # End of GA call
    
    # Optimization related plotting (See GA package documentation)
    # https://cran.r-project.org/web/packages/GA/GA.pdf
    plot(sliceModel)
    
    # Extract values and create summary object (primarySummary)
    slicePars = as.numeric(sliceModel@solution[1,])
    sliceSummary = summary(sliceModel)
    
    # Compute model predicted performance (primaryPerformances) - Vector
    slicePerformances = computeModel(loadSeries = inputData$loads, 
                                       parms = slicePars)
    missingLength = length(slicePerformances$days) -
      length(c(NA,trainingData$performances,testingData$performances))
    
    slicePerformances$measured = c(NA,
                                   trainingData$performances,
                                   testingData$performances,
                                   rep(NA,missingLength))
    slicePerformances$data = c('NA', 
                               rep("training", length(trainingData$performances)),
                               rep("testing", length(testingData$performances)),
                               rep('NA',missingLength))
    
    trainingpHat = slicePerformances[c(
      slicePerformances$data == "training" & 
        !is.na(slicePerformances$measured)),"pHat"]
    
    trainingMeasured = slicePerformances[c(
      slicePerformances$data == "training" & 
        !is.na(slicePerformances$measured)),"measured"]
    
    testingpHat = slicePerformances[c(
      slicePerformances$data == "testing" & 
        !is.na(slicePerformances$measured)),"pHat"]
    
    testingMeasured = slicePerformances[c(
      slicePerformances$data == "testing" & 
        !is.na(slicePerformances$measured)),"measured"]
    
    # Fit (Training Data) - RSQ, RMSE, MAPE
    RSQtrain = RSQfunc(predicted = trainingpHat, 
                       actual = trainingMeasured)
    
    RMSEtrain = RMSEfunc(predicted = trainingpHat,
                         actual = trainingMeasured,
                         days = length(trainingMeasured))
    
    MAPEtrain = MAPEfunc(predicted = trainingpHat,
                         actual = trainingMeasured)
    
    # Predictions (Testing Data) - RMSE, MAPE
    RMSEtest = RMSEfunc(predicted = testingpHat,
                        actual = testingMeasured,
                        days = length(testingMeasured))
    MAPEtest= MAPEfunc(predicted = testingpHat,
                         actual = testingMeasured)
    
    sliceStats = data.frame("RSQtrain" = RSQtrain,
                            "RMSEtrain" = RMSEtrain,
                            "MAPEtrain" = MAPEtrain,
                            "RMSEtest" = RMSEtest,
                            "MAPEtest" = MAPEtest)
    
    returnObject = list("sliceSummary" = sliceSummary,
                        "slicePars" = slicePars,
                        "sliceStats" = sliceStats,
                        "slicePerf" = slicePerformances,
                        "optim" = sliceModel)
    
  return(returnObject)
  }
  
  # Model metrics functions
  
  # RSQ (R-squared)
  RSQfunc = function(predicted,actual){
    rsq = 1 - ( (sum((actual-predicted)^2))  / 
                  (sum((actual-mean(actual))^2))
    ) 
    return(round(rsq*100,3))
  }
  
  # RMSE (Root mean squared error)
  RMSEfunc = function(predicted,actual,days){
    z = (predicted - actual)^2
    return(sqrt(sum(z)/days))
  }
  
  # MAPE (Mean absolute percentage error)
  MAPEfunc = function(actual,predicted){
    return(mean(abs((actual-predicted)/actual))*100)
  }
  
# ------------------------------------------------------------------------------
# FIRST OPERATION: Fitting to all available data
# ------------------------------------------------------------------------------
  
  trainingData = inputData
  
  # Fit model
  primaryModel = ga(type = "real-valued",
                    fitness = objectiveFn,
                    lower = constraints$lower,
                    upper = constraints$upper,
                    maxiter = maxIt,
                    monitor = doTrace,
                    popSize = popSize,
                    optim = TRUE,
                    optimArgs = list(method = "L-BFGS-B",
                                     control = list(maxit = 1500)
                    ),
                    elitism = gaElitism,
                    selection = gaSelection,
                    crossover = gaCrossover,
                    mutation = gaMutation,     # Random
                    run = 150, # Halt value
                    parallel = doParallel, # multi-platform
                    seed = 12345 # Seed for replication later
  ) # End of GA call
  
  # Optimization related plotting (See GA package documentation)
  # https://cran.r-project.org/web/packages/GA/GA.pdf
  plot(primaryModel)
  
  # Extract values and create summary object (primarySummary)
  primaryPars = as.numeric(primaryModel@solution[1,])
  primarySummary = summary(primaryModel)
  
  # Compute model predicted performance (primaryPerformances) - Vector
  primaryPerformances = computeModel(loadSeries = inputData$loads, 
                                     parms = primaryPars)
  primaryPerformances$measured = c(NA,inputData$performances)
  
  # Plot model predicted performance
  ylimP =  max(c(primaryPerformances$pHat, inputData$loads,
                 primaryPerformances$measured), na.rm = TRUE)
  plot(primaryPerformances$days, primaryPerformances$loads, type = "h", 
       ylim = c(0,ylimP*1.05),
       col = "grey50", ylab = "Arbitrary units (a.u)",
       xlab = "Day", main = "Fitted model (primary set)")
  lines(primaryPerformances$days, primaryPerformances$pHat, lty = 1, col = "red", lwd = 2)
  points(primaryPerformances$days, primaryPerformances$measured, pch = 18, col = "black")
  legend(1, ylimP*1.05,
         c("training load", "fitted model", "observed data"),
         pch = c(NA,NA,18), lty = c(1,1,NA), 
         lwd = c(1,2,NA),
         col = c("grey50","red","black"),
         text.col = c("black","red","black"), cex = .8,
         x.intersp = 2,
         y.intersp = 1.5, bty = "n")
  
  # Compute model statistics (primaryStats) - RSQ, RMSE, MAPE
  RSQPrimary = RSQfunc(
    predicted = primaryPerformances$pHat[!is.na(primaryPerformances$measured)],
    actual = primaryPerformances$measured[!is.na(primaryPerformances$measured)])
  RMSEPrimary = RMSEfunc(
    predicted = primaryPerformances$pHat[!is.na(primaryPerformances$measured)],
    actual = primaryPerformances$measured[!is.na(primaryPerformances$measured)], 
    days = length(primaryPerformances$measured[!is.na(primaryPerformances$measured)]))
  MAPEPrimary = MAPEfunc(
    actual = primaryPerformances$measured[!is.na(primaryPerformances$measured)],
    predicted = primaryPerformances$pHat[!is.na(primaryPerformances$measured)])
  
  primaryStats = data.frame("RSQ" = RSQPrimary,
                            "RMSE" = RMSEPrimary,
                            "MAPE" = MAPEPrimary)
  
  rm(trainingData)
  
# ------------------------------------------------------------------------------ 
# SECOND OPERATION: Cross validating using expanding-window
# ------------------------------------------------------------------------------
  
  # Create the expanding-window slices
  
  slices = createTimeSlices(inputData$days,
                            initialWindow = initialWindow,
                            horizon = testHorizon,
                            fixedWindow = FALSE,
                            skip = expandRate)
  
  nSlices = length(slices$train)
  sliceModels <- list()
  cvMetrics = matrix(data = NA, ncol = 5, nrow = nSlices)
  cvPerf = matrix(data = NA, ncol = nSlices+1, nrow = length(inputData$days)+1)
  cvPerf[,1] = 0:length(inputData$days)
  cvParms = matrix(data = NA, nrow = nSlices, ncol = 9)
  sliceNames = c()
  for (i in 1:nSlices){
    
    print(paste0("Training model | Slice ", i), quote = FALSE)
    
    currentSlice = i
    
    # Subset input data into training and validation data for current slice
    trainingData = inputData[slices$train[[currentSlice]],]
    testingData = inputData[slices$test[[currentSlice]],]
    
    # Main function call
    sliceModels[[i]] = crossValidate(objectiveFn, trainingData, testingData)
    
    # Extract data
    cvMetrics[i,] = as.numeric(sliceModels[[i]]$sliceStats[1,])
    cvPerf[,i+1] = as.numeric(sliceModels[[i]]$slicePerf$pHat)
    cvParms[i,] = as.numeric(sliceModels[[i]]$slicePars)
    sliceNames[i] = paste0("slice_",i)
  }
  
  cvSummary = apply(cvMetrics, 2, summary)
  
  colnames(cvPerf) = c("days",sliceNames)
  colnames(cvParms) = c("kg","kh","Tg","Th","alpha","beta","p0","g0","h0")
  colnames(cvSummary) = c("RSQtrain","RMSEtrain","MAPEtrain","RMSEtest",
                          "MAPEtest")
  rownames(cvParms) = sliceNames
  rownames(cvMetrics) = sliceNames
  
  # CV plot
  dailymin = apply(cvPerf[,2:nSlices], 1, min, na.rm = TRUE)
  dailymax = apply(cvPerf[,2:nSlices], 1, max, na.rm = TRUE)
  ylimP = max(c(dailymax,primaryPerformances$pHat,
                primaryPerformances$measured,
                primaryPerformances$loads), na.rm = TRUE)
  nDays = length(primaryPerformances$days)-1
  plot(primaryPerformances$days, primaryPerformances$loads, 
       type = "h", col = "grey50",
       ylim = c(0,ylimP*1.15),
       xlab = "Day",
       ylab = "Arbitrary units (a.u)",
       main = "Fitted set with CV estimates")
  polygon(c(rev(0:nDays),0:nDays),c(rev(dailymax), dailymin),col="orange", 
          border="grey30", lty = 2, lwd = 0.5)
  lines(primaryPerformances$days, primaryPerformances$pHat, 
        lty = 1, col = "red", lwd = 2)
  points(inputData$days, inputData$performances, pch = 18, 
         col = "black", cex = 0.8)
  legend(1, ylimP*1.14,
         c("training load", "cv models", "fitted model", "observed data"),
         pch = c(NA,NA,NA,20), lty = c(1,1,1,NA), 
         lwd = c(1,4,2,NA),
         col = c("grey50","orange","red","black"),
         text.col = c("grey50","orange","red","black"), cex = .8,
         x.intersp = 2,
         y.intersp = 1.2, bty = "n")
  
# ------------------------------------------------------------------------------
# THIRD OPERATION: Results tabulation, plotting, printing to console
# ------------------------------------------------------------------------------  
  
  # Print output
  
  print("Process completed: Printing main summary information", quote = FALSE)
  print("------------------------------------------------------------------",
        quote = FALSE)
  print("Optimisation summary (Main Set):", quote = FALSE)
  print("", quote = FALSE)
  print(primarySummary)
  print("", quote = FALSE)
  print("Model fit metrics (Main set):", quote = FALSE)
  print(primaryStats)
  print("", quote = FALSE)
  print("------------------------------------------------------------------",
        quote = FALSE)
  print("", quote = FALSE)
  print("Printing cross-validation information")
  print("------------------------------------------------------------------",
        quote = FALSE)
  print("Summary statistics across expanding windows:", quote = FALSE)
  print(cvSummary)
  print("", quote = FALSE)
  print("Model metrics for each expanding window", quote = FALSE)
  print(cvMetrics)
  print("", quote = FALSE)
  print("Fitted parameter values across expanding windows:", quote = FALSE)
  print(cvParms)
  print("", quote = FALSE)
  print("------------------------------------------------------------------",
        quote = FALSE)
  print("COMPLETE. Returning object", quote = FALSE)
  
  mainSet = list("summary" = primarySummary,
                 "pars" = primaryPars,
                 "stats" = primaryStats,
                 "predictions" = primaryPerformances,
                 "optim" = primaryModel)
  
  crossValidation = list("summary" = cvSummary,
                         "metrics" = cvMetrics,
                         "parameters" = cvParms,
                         "predictions" = cvPerf,
                         "raw" = sliceModels)
  
  returnObject = list("main" = mainSet,
                      "cv" = crossValidation)
  
return(returnObject)
}# End of turnerModel