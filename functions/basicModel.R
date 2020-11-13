basicModel = function(inputData,
                      constraints,
                      method = 'bfgs',
                      startingValues = NULL,
                      doTrace = FALSE,
                      initialComponent = FALSE,
                      initialWindow = NULL,
                      testHorizon = NULL,
                      expandRate = NULL){
  
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
    
    calibrateModel = function(){
      
      # Loss function
      if (initialComponent == TRUE){
        
      }
      
      # Loss function
      if (initialComponent == FALSE){
        
      }
      
      # Subset input data into training and testing data for current slice
      
      # Isolate the measurement index and associated performance values
      
      # Fit the model
      
      if (method = "bfgs"){
        require(stats)
        
        # Extract values, compute performance values
        if (initialComponent == TRUE){
          
        }
        
        if (initialComponent == FALSE){
          
        }
        
        
      }
      
      if (method = "ga"){
        require(GA)
        
        # Extract values, compute performance values
        if (initialComponent == TRUE){
          
        }
        
        if (initialComponent == FALSE){
          
        }
        
      }
      
      # Compute prediction errors and descriptive statistics (RSQ, RMSE, MAPE)
    
      # Create function object to return
      calibrationObject = list()
    
    return(calibrationObject)
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