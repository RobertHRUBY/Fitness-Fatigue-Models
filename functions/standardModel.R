standardModel = function(inputData,
                        constraints,
                        method = 'bfgs',
                        startingValues = NULL,
                        doTrace = FALSE,
                        initialComponent = FALSE,
                        initialWindow = NULL,
                        testHorizon = NULL,
                        expandRate = NULL,
                        doParallel = FALSE){
  
  # ------------------------------------------------------------------------------
  # BASIC INPUT VALIDATION AND LOAD DEPENDENCIES
  # ------------------------------------------------------------------------------
  
  # Global function dependencies
  
  require(caret)
  require(stats)
  
  # Individual function dependencies
  
  if (method == "ga"){
    require(GA)
  }
  if (method == "ga" && "doParallel" == TRUE){
    require(parallel)
    require(foreach)
    require(iterators)
    require(doParallel)
    require(doRNG)
  }
  
  # Validation checks on function call, and subsequently establish conditions
  
  # Column names of input data
  colnames(inputData) <- c("days", "performances", "loads")
  
  # Box constraints
  if (!all(constraints$lower > 0) || !all(constraints$upper > 0)){
    stop("Bounds must be non-zero non-negative real values")
  }
  
  if (!all(constraints$lower <= constraints$upper)){
    stop("Bounds incorrectly specified. Check values")
  }
  
  if (!isTRUE(initialComponent) && dim(constraints)[1] != 5){
    stop("Box constraints of incorrect dimension")
  }
  
  if (isTRUE(initialComponent) && dim(constraints)[1] != 7){
    stop("Box constraints of incorrect dimension")
  }
  
  # Starting values
  
  # Basic model
  if (initialComponent == FALSE && method == "bfgs"){
    # If user supplied starting Values
    if (!is.null(startingValues)){
      # Check that starting value vector length is correct
      if (length(startingValues) != 5){
        stop("Incorrect parameter vector supplied for startingValues")
      }
      # Check that starting value vector is within bounds
      for (i in 1:5){
        if (startingValues[i] <= constraints$lower[i] ||
            startingValues[i] >= constraints$upper[i]){
          stop("Starting values not within bounds (cannot be on bounds)")
        }
      }
    }
    # If user has not supplied starting values, generate them (p*,kg,Tg,kh,Th)
    # from random sampling from truncated normal distribution with mean
    # at middle of constraints
    if (is.null(startingValues)){
      stop("Please supply starting values in order (p*,k_g,tau_g,k_h,tau_h")
    }
  }
  
  # With initial component
  if (initialComponent == TRUE && method == "bfgs"){
    # If user supplied starting Values
    if (!is.null(startingValues)){
      # Check that starting value vector length is correct
      if (length(startingValues) != 7){
        stop("Incorrect parameter vector supplied for startingValues")
      }
      # Check that starting value vector is within bounds
      for (i in 1:7){
        if (startingValues[i] <= constraints$lower[i] ||
            startingValues[i] >= constraints$upper[i]){
          stop("Starting values not within bounds (cannot be on bounds)")
        }
      }
    }
    # If user has not supplied starting values, generate them (p*,K,Tau,q)
    if (is.null(startingValues)){
      stop("Please supply starting values in order 
           (p*,k_g,tau_g,k_h,tau_h,q_g,q_h")
    }
  }
  
  # Load series
  if (all(is.na(inputData$loads))){
    stop("Loads cannot contain NA values. Use 0 to indicate no training")
  }
  
  # Time point column (days)
  if (all(is.na(inputData$days))){
    stop("Days column in input cannot contain NA values. Use sequential vals")
  }
  
  # Method
  if (method != "bfgs" && method != "ga"){
    stop("Method supplied incorrectly. Options are 'bfgs' or 'ga'")
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
  
  # Optimisation tracing
  if (isTRUE(doTrace) && method == "bfgs"){
    doTrace == 6                              # Optim control argument
    # if left as true / false value that is argument ready to supply to GA
  }
  
  # ------------------------------------------------------------------------------
  # DEFINE FUNCTIONS USED IN THE PROCESS
  # ------------------------------------------------------------------------------
  
  # Note:
  # While some of these functions are 'common', it adds more complexity to have
  # common file dependencies than to have everything self contained. This
  # unfortunately leads to more repetition of basic code, but I prefer this in
  # some sense as it reduces user issues.
  
  # Model computation
  
  computeModel = function(loads, pars){
    
    p = numeric(length = length(loads))
    s = 1:length(loads)
    df0 = data.frame(s, "ws" = loads)
    
    # With 2 initial components
    if (initialComponent == TRUE){
      for (n in 1:length(s)){
        df1 <- df0[1:s[n], ]
        
        p[n] <- pars[1] + 
          pars[2] * sum( df1$ws * exp(- (n - df1$s) / pars[3]) ) -
          pars[4] * sum( df1$ws * exp(- (n - df1$s) / pars[5]) ) +
          pars[6]*(exp (- (i/pars[3]))) -
          pars[7]*(exp (- (i/pars[5])))
      }
    }
    
    # Without initial component
    if (initialComponent == FALSE){
      for (n in 1:length(s)){
        df1 <- df0[1:s[n], ]
        p[n] <- pars[1] + 
          pars[2] * sum( df1$ws * exp(- (n - df1$s) / pars[3]) ) -
          pars[4] * sum( df1$ws * exp(- (n - df1$s) / pars[5]) )
      }
    }
    
    return(p)  
    
  } # End of computeModel function
  
  
  # ----------------------------------------------------------------------------
  # Select appropriate global objective function
  # ----------------------------------------------------------------------------
  
  # With initial component
  # --------------------------------------------------------------------------
  if (initialComponent == TRUE){
    # Note to self, order of pars is c(p0,kg,Tg,kh,Th,qg,qh)
    objectiveFn = function(par){
      # Initialize the vector containing the squared residuals
      squaredResiduals = numeric(length(measuredPerformance))
      for (i in 1:length(measuredPerformance)){
        inputSubset = trainingData[1:measureIndex[i], ]
        modelledPerformance = par[1] + 
          par[2]*(sum(inputSubset$loads * 
                        exp(-(measureIndex[i]-inputSubset$days)/par[3]))) -
          par[4]*(sum(inputSubset$loads * 
                        exp(-(measureIndex[i]-inputSubset$days)/par[5]))) +
          par[6]*(exp (- (measureIndex[i]/par[3]))) -
          par[7]*(exp (- (measureIndex[i]/par[5])))
        squaredResiduals[i] = (modelledPerformance - measuredPerformance[i])^2  
      }
      if (method == "bfgs"){
        return(mean(squaredResiduals))
      }
      if (method == "ga"){
        return(- mean(squaredResiduals))
      }
    } # End of objective/cost function
  }
  
  # Without initial component
  # --------------------------------------------------------------------------
  if (initialComponent == FALSE){
    # Note to self, order of pars is c(p0,kg,Tg,kh,Th)
    objectiveFn = function(par){
      # Initialize the vector containing the squared residuals
      squaredResiduals = numeric(length(measuredPerformance))
      for (i in 1:length(measuredPerformance)){
        inputSubset = trainingData[1:measureIndex[i], ]
        modelledPerformance <- par[1] + 
          par[2]*(sum(inputSubset$loads * 
                        exp(-(measureIndex[i]-inputSubset$days)/par[3]))) -
          par[4]*(sum(inputSubset$loads * 
                        exp(-(measureIndex[i]-inputSubset$days)/par[5])))
        squaredResiduals[i] = (modelledPerformance - measuredPerformance[i])^2  
      }
      if (method == "bfgs"){
        return(mean(squaredResiduals))
      }
      if (method == "ga"){
        return(- mean(squaredResiduals))
      }
    } # End of objective/cost function
  }
  
  
  # ----------------------------------------------------------------------------
  # Cross validation fitting function
  # ----------------------------------------------------------------------------
  
  crossValidate = function(slices, currentSlice){
    
    # Fit the model
    if (method == "bfgs"){
      
      # Call optimiser
      sliceModel = optim(par = startingValues,
                         fn = objectiveFn,
                         lower = constraints$lower,
                         upper = constraints$upper,
                         method = "L-BFGS-B",
                         control = list(trace = doTrace,
                                        maxit = 10000)
                         # TODO: Make use of parscale at some point
      )
      
      # Extract values and summary object
      sliceModel = unlist(sliceModel)
      if (initialComponent == TRUE){
        slicePars = as.numeric(sliceModel[1:7])
        sliceSummary = as.data.frame(t(sliceModel))
        colnames(sliceSummary) = c("p0","k_g","Tau_g","k_h","Tau_h","q_g","q_h",
                                   "MSE","counts_fn","counts_gn",
                                   "convcode","convergence")
      }
      if (initialComponent == FALSE){
        slicePars = as.numeric(sliceModel[1:5])
        sliceSummary = as.data.frame(t(sliceModel))
        colnames(sliceSummary) = c("p0","k_g","Tau_g","k_h","Tau_h",
                                   "MSE","counts_fn","counts_gn",
                                   "convcode","convergence")
      }
    }
    
    if (method == "ga"){
      
      # Call optimiser
      sliceModel = ga(type = "real-valued",
                      fitness = objectiveFn,
                      lower = constraints$lower,
                      upper = constraints$upper,
                      maxiter = 10000,
                      monitor = doTrace,
                      popSize = 120,
                      optim = TRUE,
                      optimArgs = list(method = "L-BFGS-B",
                                       poptim = 0.1,
                                       pressel = 0.5,
                                       control = list(maxit = 1500)
                      ),
                      elitism = 5,
                      selection = gareal_tourSelection, # Tournament
                      crossover = gareal_blxCrossover,  # BLX (blend)
                      mutation = gareal_rsMutation,     # Random
                      run = 150, # Halt value
                      parallel = doParallel, # multi-platform
                      seed = 12345 # Seed for replication later
      ) # End of GA call
      
      # Extract values and summary object
      slicePars = as.numeric(sliceModel@solution[1,])
      sliceSummary = summary(primaryModel)
      
    }
    
    # Compute model performance values (i.e. predictions) over whole time series
    
    slicePerf = computeModel(loads = inputData$loads,
                             pars = slicePars)
    
    # Compute model metrics (Train + Test blocks) - RSQ, RMSE, MAPE
    # For the training interval
    RSQtrain = RSQfunc(predicted = slicePerf[measureIndex],
                       actual = measuredPerformance)
    RMSEtrain = RMSEfunc(predicted = slicePerf[measureIndex],
                         actual = measuredPerformance,
                         days = length(measuredPerformance))
    MAPEtrain = MAPEfunc(actual = measuredPerformance,
                         predicted = slicePerf[measureIndex])
    
    # For the testing interval
    RMSEtest = RMSEfunc(predicted = slicePerf[testingIndex],
                        actual = measuredPerformanceTest, 
                        days = length(measuredPerformanceTest))
    MAPEtest = MAPEfunc(actual = measuredPerformanceTest,
                        predicted = slicePerf[testingIndex])
    
    # Compile results
    sliceStats = data.frame("RSQtrain" = RSQtrain,
                            "RMSEtrain" = RMSEtrain,
                            "MAPEtrain" = MAPEtrain,
                            "RMSEtest" = RMSEtest,
                            "MAPEtest" = MAPEtest)
    
    # Return object
    returnObject = list("sliceSummary" = sliceSummary,
                        "slicePars" = slicePars,
                        "sliceStats" = sliceStats,
                        "slicePerf" = slicePerf,
                        "optim" = sliceModel)
    
    return(returnObject)
  } # End of crossValidate function
  
  # --------------------------------------------------------------------------
  # Model Metrics functions
  # --------------------------------------------------------------------------
  
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
  measureIndex = subset(inputData$days, 
                        !is.na(inputData$performances))
  measuredPerformance = subset(inputData$performances, 
                               !is.na(inputData$performances))
  
  if (method == "ga"){
    
    # Fit model
    primaryModel = ga(type = "real-valued",
                      fitness = objectiveFn,
                      lower = constraints$lower,
                      upper = constraints$upper,
                      maxiter = 10000,
                      monitor = doTrace,
                      popSize = 120,
                      optim = TRUE,
                      optimArgs = list(method = "L-BFGS-B",
                                       poptim = 0.1,
                                       pressel = 0.5,
                                       control = list(maxit = 1500)
                      ),
                      elitism = 5,
                      selection = gareal_tourSelection, # Tournament
                      crossover = gareal_blxCrossover,  # BLX (blend)
                      mutation = gareal_rsMutation,     # Random
                      run = 150, # Halt value
                      parallel = doParallel, # multi-platform
                      seed = 12345 # Seed for replication later
    ) # End of GA call
    
    # Optimization related plotting (See GA package documentation)
    # https://cran.r-project.org/web/packages/GA/GA.pdf
    plot(primaryModel)
    # TODO: Explore plotting functionality further
    
    # Extract values and create summary object (primarySummary)
    primaryPars = as.numeric(primaryModel@solution[1,])
    primarySummary = summary(primaryModel)
  }
  
  if (method == "bfgs"){
    
    # Fit model
    primaryModel = optim(par = startingValues,
                         fn = objectiveFn,
                         lower = constraints$lower,
                         upper = constraints$upper,
                         method = "L-BFGS-B",
                         control = list(trace = doTrace,
                                        maxit = 10000)
                         # TODO: Make use of parscale at some point
    )
    
    # Extract values and create summary object (primarySummary)
    primaryModel = unlist(primaryModel)
    if (initialComponent == TRUE){
      primaryPars = as.numeric(primaryModel[1:7])
      primarySummary = as.data.frame(t(primaryModel))
      primarySummary[,c(-9,-10,-11,-12)] = 
        round(as.numeric(primarySummary[,c(-9,-10,-11,-12)]),3)
      colnames(primarySummary) = c("p0","k_g","Tau_g","k_h","Tau_h","q_g","q_h",
                                   "MSE","counts_fn",
                                   "counts_gn","convcode","convergence")
    }
    if (initialComponent == FALSE){
      primaryPars = as.numeric(primaryModel[1:5])
      primarySummary = as.data.frame(t(primaryModel))
      primarySummary[,c(-7,-8,-9,-10)] = 
        round(as.numeric(primarySummary[,c(-7,-8,-9,-10)]),3)
      colnames(primarySummary) = c("p0","k_g","Tau_g","k_h","Tau_h",
                                   "MSE","counts_fn",
                                   "counts_gn","convcode","convergence")
    }
    
  }
  
  # Compute model predicted performance (primaryPerformances) - Vector
  primaryPerformances = computeModel(loads = inputData$loads, 
                                     pars = primaryPars)
  
  # Plot model predicted performance
  ylimP =  max(c(primaryPerformances, inputData$loads,
                 inputData$performances), na.rm = TRUE)
  plot(inputData$days, inputData$loads, type = "h", 
       ylim = c(0,ylimP*1.05),
       col = "grey50", ylab = "Arbitrary units (a.u)",
       xlab = "Day", main = "Fitted model (primary set)")
  lines(inputData$days, primaryPerformances, lty = 1, col = "red", lwd = 2)
  points(inputData$days, inputData$performances, pch = 18, col = "black")
  legend(1, ylimP*1.025,
         c("training load", "fitted model", "observed data"),
         pch = c(NA,NA,18), lty = c(1,1,NA), 
         lwd = c(1,2,NA),
         col = c("grey50","red","black"),
         text.col = c("black","red","black"), cex = .8,
         x.intersp = 2,
         y.intersp = 1.5, bty = "n")
  
  # Compute model statistics (primaryStats) - RSQ, RMSE, MAPE
  RSQPrimary = RSQfunc(predicted = primaryPerformances[measureIndex],
                       actual = measuredPerformance)
  RMSEPrimary = RMSEfunc(predicted = primaryPerformances[measureIndex],
                         actual = measuredPerformance, 
                         days = length(measuredPerformance))
  MAPEPrimary = MAPEfunc(actual = primaryPerformances[measureIndex],
                         predicted = measuredPerformance)
  
  primaryStats = data.frame("RSQ" = RSQPrimary,
                            "RMSE" = RMSEPrimary,
                            "MAPE" = MAPEPrimary)
  
  rm(measureIndex, measuredPerformance, trainingData)
  
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
  
  # Fit the expanding window slices
  
  sliceModels <- list()
  cvMetrics = matrix(data = NA, ncol = 5, nrow = nSlices)
  cvPerf = matrix(data = NA, ncol = nSlices, nrow = length(inputData$days))
  for (i in 1:nSlices){
    
    print(paste0("Training model | Slice ", i), quote = FALSE)
    currentSlice = i
    # Subset input data into training and validation data for current slice
    trainingData = inputData[slices$train[[currentSlice]],]
    testingData = inputData[slices$test[[currentSlice]],]
    # Isolate the measurement index and associated performance values
    measureIndex = subset(trainingData$days, 
                          !is.na(trainingData$performances))
    testingIndex = subset(testingData$days,
                          !is.na(testingData$performances))
    # Measured performance values over the training interval
    measuredPerformance = subset(trainingData$performances, 
                                 !is.na(trainingData$performances))
    measuredPerformanceTest = subset(testingData$performances, 
                                     !is.na(testingData$performances))
    
    sliceModels[[i]] = crossValidate(slices, currentSlice)
    
    cvMetrics[i,] = as.numeric(sliceModels[[i]]$sliceStats[1,])
    cvPerf[,i] = as.numeric(sliceModels[[i]]$slicePerf)
  }
  
  # Compute results
  
  # Averages across all the slices
  colnames(cvMetrics) = c("RSQtrain","RMSEtrain","MAPEtrain","RMSEtest",
                          "MAPEtest")
  cvSummary = apply(cvMetrics, 2, summary)
  sliceNames = c()
  for (i in 1:nSlices){
    sliceNames[i] = paste0("slice_",i)
  }
  rownames(cvMetrics) = sliceNames
  colnames(cvPerf) = sliceNames
  
  # Model summary (parameter values + optim ref) - Loop depending on IC
  if (initialComponent == TRUE){
    cvParms = matrix(data = NA, nrow = nSlices, ncol = 7)
    for (i in 1:nSlices){
      cvParms[i,] = as.numeric(sliceModels[[i]]$slicePars)
    }
    colnames(cvParms) = c("p0","k_g","Tau_g","k_h","Tau_h","q_g","q_h")
    rownames(cvParms) = sliceNames
  }
  if (initialComponent == FALSE){
    cvParms = matrix(data = NA, nrow = nSlices, ncol = 5)
    for (i in 1:nSlices){
      cvParms[i,] = as.numeric(sliceModels[[i]]$slicePars)
    }
    colnames(cvParms) = c("p0","k_g","Tau_g","k_h","Tau_h")
    rownames(cvParms) = sliceNames
  }
  
  # Plot cross-validation interval
  
  dailymin = apply(cvPerf, 1, min)
  dailymax = apply(cvPerf, 1, max)
  ylimP = max(c(dailymax,inputData$performances,
                inputData$loads), na.rm = TRUE)
  nDays = length(inputData$days)
  plot(inputData$days, inputData$loads, type = "h", col = "grey50",
       ylim = c(0,ylimP*1.05),
       xlab = "Day",
       ylab = "Arbitrary units (a.u)",
       main = "Fitted set with CV estimates")
  polygon(c(rev(1:nDays),1:nDays),c(rev(dailymax), dailymin),col="orange", 
          border="grey30", lty = 2, lwd = 0.5)
  lines(inputData$days, primaryPerformances, lty = 1, col = "red", lwd = 2)
  points(inputData$days, inputData$performances, pch = 18, 
         col = "black", cex = 0.8)
  legend(1, ylimP*1.025,
         c("training load", "cv models", "fitted model", "observed data"),
         pch = c(NA,NA,NA,20), lty = c(1,1,1,NA), 
         lwd = c(1,4,2,NA),
         col = c("grey50","orange","red","black"),
         text.col = c("grey50","orange","red","black"), cex = .8,
         x.intersp = 2,
         y.intersp = 1.5, bty = "n")
  
  
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
  
  # Structure object to return to user
  
  mainSet = list("summary" = primarySummary,
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
} # End of basicModel() function