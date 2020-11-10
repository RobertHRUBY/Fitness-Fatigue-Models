standardModel <- function(inputData, 
                           constraints = NULL, 
                           method = 'bfgs', 
                           startValues = NULL, 
                           initialWindow = NULL,
                           testHorizon = NULL, 
                           expandRate = NULL,
                           doTrace = FALSE){
  
  # Check function dependencies
  # ----------------------------------------------------------------------------
  if (!"caret" %in% rownames(installed.packages())){
    install.packages("caret")} else{
      library("caret")
    }
  if (!"ModelMetrics" %in% rownames(installed.packages()))
    {install.packages("ModelMetrics")} else{
      library("ModelMetrics")
    }
  if (method == "de" && !"DEoptim" %in% rownames(installed.packages())){
    install.packages("DEoptim")} else{
      library("DEoptim")
    }

  # Function input validation
  # ----------------------------------------------------------------------------
  if (sum(!is.na(inputData$performances)) < 40){
    print("Number of observed measurements for fitting the model is small",
          quote = FALSE)
    print("Results likely to be poor. Are you sure you want to continue?",
          quote = FALSE)
    invisible(readline(prompt = "Press [enter] to continue"))
  }
  
  if(missing(inputData)){
    stop("No input data supplied. Example provided in the documentation")
  }
  
  # Make sure inputData has the correct columnames
  colnames(inputData) <- c("days", "performances", "loads")
  
  # Check if box-constraints are supplied, if not set to default values
  if(is.null(constraints)){
    stop("No box constraints supplied for the optimisation. Please supply.")
  }
  
  # Check availability of starting values for BFGS algorithm
  if(method == 'bfgs' && is.null(startValues)){
    stop("No starting values supplied for BFGS. Supply or try: method = 'de'")
  }
  
  # Check suitability of starting values
  if(method == 'bfgs' && !is.null(startValues)){
    for (i in 1:5){
      if ((startValues[i] <= constraints$lower[i]) || 
          (startValues[i] >= constraints$upper[i])){
        stop("Starting value(s) on or out of bounds of the constraints")
      }
    }
  }
  
  # If initialWindow specified, make user also supply testHorizon and expandRate
  if(!is.null(initialWindow) && (is.null(testHorizon) || is.null(expandRate))){
    stop(paste0("Please also supply testHorizon and expandRate arguments with",
         " initialWindow"))
  }
  
  # If no initialWindow specified, or just testHorizon or expandRate arguments
  # supplied. Use following default(s)
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
  
  # The primary calibration function
  # ----------------------------------------------------------------------------
  calibrateModel <- function(timeSlices, currentSlice){
    
    objectiveFn <- function(par){
      # Initialize the vector containing the squared residuals
      squaredResiduals <- numeric(length(measuredPerformance))
      for (i in 1:length(measuredPerformance)){
        inputSubset <- trainingData[1:measureIndex[i], ]
        modelledPerformance <- par[1] + sum( inputSubset$loads *
          ( (par[2] * exp( - (measureIndex[i] - inputSubset$days) / par[3])) -
            (par[4] * exp( - (measureIndex[i] - inputSubset$days) / par[5])) 
          )
        )
        squaredResiduals[i] <- (modelledPerformance - 
                                  measuredPerformance[i])^2  
      }
      return(mean(squaredResiduals))
    } # End of objective/cost function
    
    computePerformance <- function(fittedPars, nDays){
      calibratedPerformance <- numeric(nDays)
      for (i in 1:nDays){
        tempSubset <- inputData[1:inputData$days[i], c("days", "loads")]
        calibratedPerformance[i] <- fittedPars[1] + sum ( tempSubset$loads *
          ( (fittedPars[2] * exp( - (i - tempSubset$days) / fittedPars[3])) -
            (fittedPars[4] * exp( - (i - tempSubset$days) / fittedPars[5])) 
          )
        )
      }
      return(calibratedPerformance)
    } # End of computePerformance function
    
    # Subset input data into training and testing data for current slice
    trainingData <- inputData[timeSlices$train[[currentSlice]],]
    testingData <- inputData[timeSlices$test[[currentSlice]],]
    
    # Isolate the measurement index and associated performance values
    # for the training data
    measureIndex <- subset(trainingData$days, 
                           is.na(trainingData$performances) == FALSE)
    
    measuredPerformance <- subset(trainingData$performances, 
                                  is.na(trainingData$performances) == FALSE)
    
    # Method: Broyden-Fletcher-Goldfarb-Shanno (Limited memory modification)
    # (Default)
    if (method == 'bfgs'){
      
      if(doTrace == TRUE){
        traceRef <- 6
      } else{
        traceRef <- 0
      }
      
      # Fit the model
      calibratedModel <- optim(par = startValues,
                               fn = objectiveFn,
                               lower = constraints$lower,
                               upper = constraints$upper,
                               method = "L-BFGS-B",
                               control = list(trace = traceRef,
                                              maxit = 10000
                                              # TODO: Parscale
                               )
      )
      
      # Extract values
      calibratedModel <- unlist(calibratedModel)
      fittedPars <- as.numeric(calibratedModel[1:5])
      calibratedModel <- as.data.frame(t(calibratedModel))
      colnames(calibratedModel) <- c("p0","k_g","tau_g","k_h","tau_h","MSE_objective",
                                     "counts_fn","counts_gn","convcode",
                                     "convergence_message")
      
    } # End of BFGS method
    
    # Method: Differential evolution
    if (method == 'de'){
      
      # Fit the model
      calibratedModel <- DEoptim(fn = objectiveFn,
                                 lower = constraints$lower,
                                 upper = constraints$upper,
                                 DEoptim.control(
                                   strategy = 6,
                                   NP = 300, #N population members (>10*n_pars)
                                   trace = doTrace,
                                   CR = 0.9,
                                   F = 0.9,
                                   itermax = 250
                                   # TODO: Parallelise the code
                                 )
      )
      
      # Extract values
      fittedPars <- as.numeric(calibratedModel$optim$bestmem)
      calibratedModel <- data.frame("p0" = fittedPars[1],
                                   "k_g" = fittedPars[2],
                                   "tau_g" = fittedPars[3],
                                   "k_h" = fittedPars[4],
                                   "tau_h" = fittedPars[5],
                                   "MSE_objective" = as.numeric(
                                     calibratedModel$optim$bestval),
                                   "nfeval" = as.numeric(
                                     calibratedModel$optim$nfeval),
                                   "iter" = as.numeric(
                                     calibratedModel$optim$iter))
    } # End of DEoptim method
    
    # Compute performance
    nDays <- tail(inputData$days, n = 1)
    slicePerformance <- computePerformance(fittedPars, nDays)
  
    # RMSE (Training set)
    RMSETrain <- rmse(measuredPerformance, slicePerformance[measureIndex])
    
    # R-squared (Training set)
    rsq_func <- function(x,y){
      rsq <- 1 - ( (sum((y-x)^2))  / (sum((y-mean(y))^2))) 
      return(round(rsq*100,3))
    }
    
    # RSQ (Training set)
    RSQTrain <- rsq_func(x = slicePerformance[measureIndex],
                         y = measuredPerformance)
      
    # Forecast errors
    # Isolate non-NA measurements within the testing data
    isolateSlice <- slicePerformance[head(testingData$days, n = 1):
                                       tail(testingData$days, n = 1)]
    isolateTests <- subset(testingData$performances, 
                           !is.na(testingData$performances))
    isolateSlice <- subset(isolateSlice, !is.na(testingData$performances))
    
    # Mean Absolute Percentage Error
    MAPETest <- mean(
      abs(
      (isolateTests - isolateSlice) /
        isolateTests)) * 100
    
    # Root Mean Squared Error
    RMSETest <- rmse(isolateTests, isolateSlice)
    
    # Collate statistics
    predError <- data.frame(RMSETrain, RMSETest, MAPETest, RSQTrain)
    
    # Put the slice into a list and return
    slice <- list("calibration" = calibratedModel,
                  "performance" = slicePerformance,
                  "pred_error" = predError)
    
  return(slice)
  } # End of calibrateModel function

  # Split data into expanding windows, and fit models
  # ----------------------------------------------------------------------------
  
  # Split data into expanding windows (appropriate train/test sets)
  timeSlices <- createTimeSlices(inputData$days,
                                  initialWindow = initialWindow,
                                  horizon = testHorizon,
                                  fixedWindow = FALSE,
                                  skip = expandRate)

  # Fit the expanding window
  model <- list()
  for(i in 1:length(timeSlices$train)){
    model[[i]] <- calibrateModel(timeSlices, currentSlice = i)
    sliceVals <- c(as.numeric(model[[i]]$calibration[1:6]),
                   as.numeric(model[[i]]$pred_error$RSQTrain),
                   as.numeric(model[[i]]$pred_error$RMSETrain),
                   as.numeric(model[[i]]$pred_error$RMSETest),
                   as.numeric(model[[i]]$pred_error$MAPETest))
    slicePerf <- c(as.numeric(model[[i]]$performance))
    if (i == 1){
      allSlices <- data.frame(sliceVals)
      allPerformances <- data.frame(slicePerf)
    } else{
      allSlices <- cbind(allSlices, sliceVals)
      allPerformances <- cbind(allPerformances, slicePerf)
    }
    
  }
  
  rownames(allSlices) <- c("p_0","k_g","T_g","k_h","T_h","MSE_fnval",
                           "RSQ_Train", "RMSE_Train", "RMSE_test", "MAPE_test")
  colnames(allSlices) <- paste0("slice",1:length(timeSlices$train))
  colnames(allPerformances) <- paste0("slice",1:length(timeSlices$train))

  # Set with lowest MAPE (Test set)
  bestMAPE <- as.numeric(which.min(allSlices[10,]))
  bestMAPEp <- allPerformances[,bestMAPE]
  bestMAPE <- data.frame("p0" = allSlices[1,bestMAPE],
                            "k_g" = allSlices[2,bestMAPE],
                            "T_g" = allSlices[3,bestMAPE],
                            "k_h" = allSlices[4,bestMAPE],
                            "T_h" = allSlices[5,bestMAPE],
                            "MSE" = allSlices[6,bestMAPE],
                            "RSQ" = allSlices[7,bestMAPE],
                            "RMSE_train" = allSlices[8,bestMAPE],
                            "RMSE_test" = allSlices[9,bestMAPE],
                            "MAPE_test" = allSlices[10,bestMAPE])
    
  # Tabulate results, print to terminal
  print("COMPLETE: TABULATING MODEL",
        quote = FALSE)
  print("---------------------------------------------------------------------",
        quote = FALSE)
  print("SUMMARY STATISTICS (ALL SLICES):", quote = FALSE)
  print("", quote = FALSE)
  print(apply(allSlices,1,summary)[,1:6])
  print("", quote = FALSE)
  print(apply(allSlices,1,summary)[,7:10])
  print("", quote = FALSE)
  print("BEST PARAMETERS RECOVERED (TRAIN AND TEST):", quote = FALSE)
  print("", quote = FALSE)
  print(bestMAPE[1:6])
  print("", quote = FALSE)
  print(bestMAPE[7:10])
  print("", quote = FALSE)
  print("---------------------------------------------------------------------",
        quote = FALSE)
  print("PRINTING SUMMARY PLOTS: SEE CONSOLE",
        quote = FALSE)
  
  # Plot the training load distribution and measured performance values
  plot(inputData$days, inputData$loads, type = "h", 
       ylim = c(0,
                max(c(inputData$loads,inputData$performances,bestMAPEp), na.rm = TRUE) + 10),
       col = "grey75",
       ylab = "Arbitrary units (a.u)", xlab = "Day",
       main = "Key results: Best set found by cross-validation")
  points(inputData$days, inputData$performances, pch = 10, col = "red")
  lines(inputData$days, bestMAPEp, col = "blue")
  legend("topleft", c("Training load (a.u)",
                      "Measured performance", 
                      paste0("Fitted set (MAPE): (",
                             signif(as.numeric(bestMAPE[1]),2),
                             ", ",
                             signif(as.numeric(bestMAPE[2]),2),
                             ", ",
                             signif(as.numeric(bestMAPE[3]),2),
                             ", ",
                             signif(as.numeric(bestMAPE[4]),2),
                             ", ",
                             signif(as.numeric(bestMAPE[5]),2),
                             ")")),
         fill = c("grey75","red","blue"), cex = 0.7)
  
  # Plot region of uncertainty from all sets
  dailymin <- apply(allPerformances, 1, min)
  dailymax <- apply(allPerformances, 1, max)
  nDays <- 1:tail(inputData$days, n = 1)
  plot(nDays, inputData$performances, type = "n",
       ylim = c(min(c(dailymin,inputData$performances), na.rm = TRUE) - 10,
                max(c(dailymax,inputData$performances), na.rm = TRUE) + 10),
       ylab = "Performance Forecast (a.u)",
       xlab = "Day",
       main = "Model performance across all slices"
       )
  polygon(c(rev(nDays),nDays),c(rev(dailymax), dailymin),col="grey85", 
          border="red", lty = 2)
  points(nDays, inputData$performances, col = "blue", pch = 20)
  legend("bottomright", c(paste0("All slices (n=",length(timeSlices$train),")"), 
                          "Measured Data"),
         fill = c("red","blue"), cex = 0.75)

  return(list("bestMem" = bestMAPE,
              "summary" = apply(allSlices,1,summary),
              "allSlices" = allSlices,
              "slicePerformance" = allPerformances))
} # End of main function
