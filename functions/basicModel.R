basicModel = function(inputData,
                      constraints,
                      method = 'bfgs',
                      startingValues = NULL,
                      doTrace = FALSE,
                      initialComponent = FALSE,
                      initialWindow = NULL,
                      testHorizon = NULL,
                      expandRate = NULL,
                      doParallel = FALSE){
  
  # Global function dependencies
  
    require(caret)
  
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
  
    if (!isTRUE(initialComponent) && dim(constraints)[1] != 3){
      stop("Box constraints of incorrect dimension")
    }
  
    if (isTRUE(initialComponent) && dim(constraints)[1] != 4){
      stop("Box constraints of incorrect dimension")
    }
    
    # Starting values
  
      # Basic model
      if (initialComponent == FALSE){
        # If user supplied starting Values
        if (!is.null(startingValues)){
          # Check that starting value vector length is correct
          if (length(startingValues) != 3){
            stop("Incorrect parameter vector supplied for startingValues")
          }
          # Check that starting value vector is within bounds
          for (i in 1:3){
            if (startingValues[i] <= constraints$lower[i] ||
                startingValues[i] >= constraints$upper[i]){
              stop("Starting values not within bounds (cannot be on bounds)")
            }
          }
        }
        # If user has not supplied starting values, generate them (p*,K,Tau)
        if (is.null(startingValues)){
          # TODO 
        }
      }
  
      # With initial component
      if (initialComponent == TRUE){
        # If user supplied starting Values
        if (!is.null(startingValues)){
          # Check that starting value vector length is correct
          if (length(startingValues) != 4){
            stop("Incorrect parameter vector supplied for startingValues")
          }
          # Check that starting value vector is within bounds
           for (i in 1:4){
            if (startingValues[i] <= constraints$lower[i] ||
                startingValues[i] >= constraints$upper[i]){
              stop("Starting values not within bounds (cannot be on bounds)")
            }
          }
        }
        # If user has not supplied starting values, generate them (p*,K,Tau,q)
        if (is.null(startingValues)){
          # TODO
        }
      }
  
    # Load series
    if (!all(is.na(inputData$loads))){
      stop("Loads cannot contain NA values. Use 0 to indicate no training")
    }
  
    # Time point column (days)
    if (!all(is.na(inputData$days))){
      stop("Days column in input cannot contain NA values. Use sequential vals")
    }
    
    # Method
    if (method != "bfgs" || method != "ga"){
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
    }
  
  # Calibration function
    
    calibrateModel = function(slices, currentSlice){
      
      # If including the initial component
      if (initialComponent == TRUE){
      
        # Note to self, order of pars is c(p0,K,T,q)
        objectiveFn = function(par){
          # Initialize the vector containing the squared residuals
          squaredResiduals = numeric(length(measuredPerformance))
          for (i in 1:length(measuredPerformance)){
            inputSubset = trainingData[1:measureIndex[i], ]
            modelledPerformance = par[1] + 
                                   par[4]*(exp (- (measureIndex[i]/par[3]))) +
                                   par[2]*(sum(inputSubset$loads *
                                               exp( - (measureIndex[i] - 
                                                       inputSubset$days
                                                       ) / 
                                                    par[3]
                                                   )
                                               )
                                           )
            squaredResiduals[i] = (modelledPerformance - 
                                      measuredPerformance[i]
                                    )^2  
          }
          if (method == "bfgs"){
            return(mean(squaredResiduals))
          }
          if (method == "ga"){
            return(- mean(squaredResiduals))
          }
        } # End of objective/cost function
        
        # Performance computation function
        # TODO

      } # End of if initialComponent == TRUE
      
      # If not including the initial component
      if (initialComponent == FALSE){
        
        # Note to self, order of pars is c(p0,K,T,q)
        objectiveFn = function(par){
          # Initialize the vector containing the squared residuals
          squaredResiduals = numeric(length(measuredPerformance))
          for (i in 1:length(measuredPerformance)){
            inputSubset = trainingData[1:measureIndex[i], ]
            modelledPerformance <- par[1] + 
                                   par[2]*(sum(inputSubset$loads *
                                                exp( - (measureIndex[i] - 
                                                          inputSubset$days
                                                ) / 
                                                  par[3]
                                                )
                                              )
                                           )
            squaredResiduals[i] = (modelledPerformance - 
                                      measuredPerformance[i]
            )^2  
          }
          if (method == "bfgs"){
            return(mean(squaredResiduals))
          }
          if (method == "ga"){
            return(- mean(squaredResiduals))
          }
        } # End of objective/cost function
        
        # Performance computation function
        # TODO
        
      } # End of if initialComponent == FALSE
      
      # Subset input data into training and testing data for current slice
      trainingData = inputData[slices$train[[currentSlice]],]
      testingData = inputData[slices$test[[currentSlice]],]
      
      # Isolate the measurement index and associated performance values
      
      # Days on which measurements were taken over the training interval
      measureIndex = subset(trainingData$days, 
                             !is.na(trainingData$performances))
      
      # Measured performance values over the training interval
      measuredPerformance = subset(trainingData$performances, 
                                    !is.na(trainingData$performances))
      
      # Measured performance values over the testing interval
      measuredPerformanceTest = subset(testingData$performances, 
                                       !is.na(trainingData$performances))
      
      # Fit the model
      
      if (method = "bfgs"){
        require(stats)
        
        fittedModel = optim(par = startingValues,
                            fn = objectiveFn,
                            lower = constraints$lower,
                            upper = constraints$upper,
                            method = "L-BFGS-B",
                            control = list(trace = doTrace,
                                            maxit = 10000)
                                            # TODO: Make use of parscale
                            )
        
        # Extract and order values
        fittedModel = unlist(fittedModel)
        if (initialComponent == TRUE){
          fittedPars = as.numeric(fittedModel[1:4])
          fittedModel = as.data.frame(t(fittedModel))
          colnames(fittedModel) = c("p0","K","T","q","MSE","counts_fn",
                                    "counts_gn","convcode","convergence_message")
        }
        if (initialComponent == FALSE){
          fittedPars = as.numeric(fittedModel[1:3])
          fittedModel = as.data.frame(t(fittedModel))
          colnames(fittedModel) = c("p0","K","T","MSE","counts_fn",
                                    "counts_gn","convcode","convergence_message")
        }
        
      } # End of BFGS Method
      
      if (method = "ga"){
        require(GA)
        # Includes stochastic local search
        fittedModel = ga(type = "real-valued",
                         fitness = objectiveFn,
                         lower = constraints$lower,
                         upper = constraints$upper,
                         maxiter = 10000,
                         monitor = doTrace,
                         popSize = 50,
                         optim = TRUE,
                         optimArgs = list(method = "L-BFGS-B",
                                          poptim = 0.1,
                                          pressel = 0.5,
                                          control = list(maxit = 1500)
                                          ),
                         elitism = 5,
                         selection = gareal_tourSelection, # Tournament selection
                         crossover = gareal_blxCrossover,  # BLX (blend crossover)
                         mutation = gareal_rsMutation,     # Random around solution
                         run = 150, # Halt if no. consecutive gens w/out improvement
                         parallel = doParallel, # Snow (windows), multicore (MacOS)
                         seed = 12345 # Seed for replication later
                         )
        
        # Extract values
        
      }
      
      # Compute performance values across full time series
      nDays = tail(inputData$days, n = 1)
      fittedPerf = computePerformance(fittedPars, nDays)
      
      # Compute prediction errors and descriptive statistics (RSQ, RMSE, MAPE)
      
        # Isolate the performance values just for the days that observed values
        # exist in the testing interval
        fittedPerfTest = fittedPerf[head(testingData$days, n = 1):
                                      tail(testingData$days, n = 1)]
        fittedPerfTest = subset(fittedPerfTest, 
                                !is.na(testingData$performances))
      
        # Functions to compute modeling statistics
          
          # R-squared function
          RSQfunc = function(predicted,actual){
            rsq = 1 - ( (sum((actual-predicted)^2))  / 
                        (sum((actual-mean(actual))^2))
                      ) 
            return(round(rsq*100,3))
          }
          
          # RMSE function
          RMSEfunc = function(predicted,actual,days){
            z = (predicted - actual)^2
            return(sqrt(sum(z)/days))
          }
          
          # MAPE function
          MAPEfunc = function(actual,predicted){
            return(mean(abs((actual-predicted)/actual))*100)
          }
        
        # Compute descriptive statistics
          
          # For the training interval
          RSQTrain = RSQfunc(predicted = fittedPerf[measureIndex],
                             actual = measuredPerformance)
          RMSETrain = RMSEfunc(predicted = fittedPerf[measureIndex],
                               actual = measuredPerformance)
          
          # For the testing interval
          RMSETest = RMSEfunc(predicted = fittedPerfTest,
                              actual = measuredPerformanceTest)
          MAPETest = MAPEfunc(actual = measuredPerformanceTest,
                              predicted = fittedPerfTest)
          
          # Compile results
          fittedPars[1, "RSQTrain"] = RSQTrain
          fittedPars[2, "RMSETrain"] = RMSETrain
          fittedPars[3, "RMSETest"] = RMSETest
          fittedPars[4, "MAPETest"] = MAPETest
      
      # Create function object to return
      returnObject = list("fittedPars" = fittedPars,
                          "fittedPerf" = fittedPerf,
                          "fittedModel" = fittedModel)
    
    return(returnObject)
    } # End of calibration function
    
  # Create expanding-window slices
  
    slices = createTimeSlices(inputData$days,
                            initialWindow = initialWindow,
                            horizon = testHorizon,
                            fixedWindow = FALSE,
                            skip = expandRate)
  
    nSlices = length(slice$train)
  
  # Fit the slices
  
    sliceModels <- list()
    for (i in 1:nSlices){
      print(paste0("Training model | Slice ", i), quote = FALSE)
      sliceModels[[i]] = calibrateModel(slices, currentSlice = i)
    }
  
  # Tabulate results, print these to console, and generate plots
  
    # Collate all performance values across slices for region plot
    
    # Best set found (by lowest MAPE_test, between slices)
    
    # Summary statistics
    
    # Plot best set
    
    # Plot all slices (region plot)
    
    # Print results to console (all slices, summary statistics, best set)
  
  # Structure object to return
    
    mainObject = list()
  
return(mainObject)  
} # End of basicModel() function