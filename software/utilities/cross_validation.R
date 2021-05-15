

# Fit the VDR model via MLE using the L-BFGS-B algorithm started from multiple
# random starting points (parallelised train-test splits)

expandingWindow_CV <- function(dat,
                               bounds,
                               initialWindow = 60,
                               testHorizon = 20,
                               expandRate = 4,
                               nStarts = 10,
                               cores = NULL,
                               initial = FALSE){
  
  source("vdr_ffm_objective.R")
  source("ffm_simulation.R")
  
  # Ancillary functions
  # ----------------------------------------------------------------------------
  generate_splits <- function(initialWindow, testHorizon, expandRate, dat){
    # Convert supplied arguments for CV into 'days'
    initialWindow = round(length(dat$day) * initialWindow/100, 0)
    testHorizon = round(length(dat$day) * testHorizon/100, 0)
    expandRate = round(length(dat$day) * expandRate/100, 0)
    # Create splits
    splits <- createTimeSlices(dat$day,
                               initialWindow = initialWindow,
                               horizon = testHorizon,
                               fixedWindow = FALSE,
                               skip = expandRate)
    return(splits)
  }
  
  mape <- function(measured, predicted){
    return(mean(abs((measured - predicted)/measured))*100)
  }
  
  create_grid <- function(bounds, nStarts, initial){
    
    if (initial == FALSE){
      set.seed(101)
      parmat <- data.frame(
        "p0" = comboSample(seq(bounds$lower[1] + 1, bounds$upper[1] - 1, 2.5), 
                           m = 1, n = nStarts),
        "kg" = comboSample(seq(bounds$lower[2] + 0.1, bounds$upper[2] - 0.1, 0.1), 
                           m = 1, n = nStarts),
        "Tg" = comboSample(seq(bounds$lower[3] + 1, bounds$upper[3] - 1, 1.5), 
                           m = 1, n = nStarts),
        "kh" = comboSample(seq(bounds$lower[4] + 0.1, bounds$upper[4] - 0.1, 0.1), 
                           m = 1, n = nStarts),
        "Th" = comboSample(seq(bounds$lower[5] + 1, bounds$upper[5] - 1, 1.5), 
                           m = 1, n = nStarts),
        "Th2" = comboSample(seq(bounds$lower[6] + 0.1, bounds$upper[6] - 0.1, 0.1), 
                            m = 1, n = nStarts),
        "sigma" = comboSample(seq(bounds$lower[7] + 0.1, bounds$upper[7] - 0.1, 0.1), 
                              m = 1, n = nStarts)
      )  # We also add or subtract a little to get away from starting on the bounds
    }
    
    if (initial == TRUE){
      set.seed(101)
      parmat <- data.frame(
        "p0" = comboSample(seq(bounds$lower[1] + 1, bounds$upper[1] - 1, 2.5), 
                           m = 1, n = nStarts),
        "kg" = comboSample(seq(bounds$lower[2] + 0.1, bounds$upper[2] - 0.1, 0.1), 
                           m = 1, n = nStarts),
        "Tg" = comboSample(seq(bounds$lower[3] + 1, bounds$upper[3] - 1, 1.5), 
                           m = 1, n = nStarts),
        "kh" = comboSample(seq(bounds$lower[4] + 0.1, bounds$upper[4] - 0.1, 0.1), 
                           m = 1, n = nStarts),
        "Th" = comboSample(seq(bounds$lower[5] + 1, bounds$upper[5] - 1, 1.5), 
                           m = 1, n = nStarts),
        "Th2" = comboSample(seq(bounds$lower[6] + 0.1, bounds$upper[6] - 0.1, 0.1), 
                            m = 1, n = nStarts),
        "sigma" = comboSample(seq(bounds$lower[7] + 0.1, bounds$upper[7] - 0.1, 0.1), 
                              m = 1, n = nStarts),
        "qg" = comboSample(seq(bounds$lower[8] + 0.1, bounds$upper[8] - 0.1, 0.1), 
                           m = 1, n = nStarts),
        "qh"= comboSample(seq(bounds$lower[9] + 0.1, bounds$upper[9] - 0.1, 0.1), 
                          m = 1, n = nStarts)
      )  # We also add or subtract a little to get away from starting on the bounds
    }
    return(as.matrix(parmat))
  }
  
  train_test <- function(dat, parmat, bounds, main, splits = NA, 
                         currentSplit = NA, initial){
    
    if(main == FALSE){
      # If we are training-testing on splits vs. training on the whole of block 1
      training_data <- dat[splits$train[[currentSplit]], ]
      testing_data <- dat[splits$test[[currentSplit]], ]}
    
    if(main == TRUE){
      # If training on the whole block 1 and testing on block 2
      training_data <- dat[dat$block == 1, ]
      testing_data <- dat[dat$block == 2, ]
    }
    
    # Isolate a vector of days on which measurements exist for train and test data
    measure_idx_train <- subset(training_data$day, !is.na(training_data$performance))
    measure_idx_test <- subset(testing_data$day, !is.na(testing_data$performance))
    
    # Isolate a vector of measurements in for train and test data
    measurements_train <- subset(training_data$performance, !is.na(training_data$performance))
    measurements_test <- subset(testing_data$performance, !is.na(testing_data$performance))
    
    # Put data in required format for objective function vdrObjective()
    dat_temp <- data.frame("day" = measure_idx_train, 
                           "performance" = measurements_train)
    
    load_temp <- training_data[, c("day", "load")]
    
    # Fitting iterations
    fittedModel <- optimx::multistart(parmat, 
                                      fn = vdrObjectiveLL, 
                                      lower = bounds$lower, 
                                      upper = bounds$upper,
                                      method = "L-BFGS-B",
                                      control = list(maxit = 500,
                                                     trace = FALSE),
                                      loads = load_temp,
                                      perfVals = dat_temp,
                                      initial = initial)
    
    # Compute predicted performance for the entire time-series (blocks 1 + 2)
    temp_predictions <- sapply(1:dim(parmat)[1], 
                               function(i) vdrPredict(pars = as.numeric(fittedModel[i, 1:6]), 
                                                      loads = dat[,c("day", "load")],
                                                      initialPars = as.numeric(fittedModel[i, 8:9]))$performance)
    
    # Extract predictions at required days to evaluate model performance
    predictions_training <- temp_predictions[measure_idx_train, ]
    predictions_testing <- temp_predictions[measure_idx_test, ]
    
    # Compute metrics (MAPE)
    mape_train <- sapply(1:dim(predictions_training)[2],
                         function(i) mape(measured = measurements_train, 
                                          predicted = predictions_training[,i]))
    mape_test <- sapply(1:dim(predictions_testing)[2],
                        function(i) mape(measured = measurements_test,
                                         predicted = predictions_testing[,i]))
    
    # Collect these metrics and calculate average split statistics
    fittedModel <- cbind(fittedModel, mape_train, mape_test)
    
    stats <- c("mean_mape_train" = mean(mape_train), 
               "mean_mape_test" = mean(mape_test),
               "sd_mape_train" =  sd(mape_train),
               "sd_mape_test" = sd(mape_test))
    
    # Develop output
    output <- list("fittedModel" = fittedModel, "predictions" = temp_predictions,
                   "stats" = stats)
    
    return(output)
  }
  
  # ----------------------------------------------------------------------------
  
  
  # Generate splits (note split vectors gives you an index position vs. a 'day', think t-1!)
  splits <- generate_splits(initialWindow, testHorizon, expandRate, dat[dat$block == 1, ])
  nSplits <- length(splits$train)   # Number of splits
  
  # Create an array of random starting values over the bounds
  parmat <- create_grid(bounds, nStarts, initial = initial)
  
  # Iterate over the splits (train-test)
  if (is.null(cores)){
    cores <- detectCores(logical = TRUE)
  } # If cores not specified by user
  cl <- makeCluster(cores, type = "SOCK")              # Make cluster
  registerDoSNOW(cl)                                   # Register cluster
  fitted_splits <- foreach(i = 1:nSplits, .verbose = TRUE, .packages = c("optimx"),
                           .export = c("train_test", "vdrObjectiveLL", "vdrPredict",
                                       "mape")) %dopar%{
                                         train_test(dat, parmat, bounds, 
                                                    main = FALSE, splits = splits,
                                                    initial = initial,
                                                    currentSplit = i)}
  # By default results returned as a list
  stopCluster(cl)                       # Stop cluster
  names(fitted_splits) <- paste0("split_",1:nSplits) # Add names to the list for each split
  
  # Compute model performance across splits and add to the existing list object
  mape_train_across <- matrix(NA, nrow = nStarts, ncol = nSplits)
  mape_test_across <- matrix(NA, nrow = nStarts, ncol = nSplits)
  for (i in 1:nSplits){
    mape_train_across[,i] <- fitted_splits[[i]]$fittedModel$mape_train
    mape_test_across[,i] <- fitted_splits[[i]]$fittedModel$mape_test
  }
  fitted_splits$across_splits <- list("training_mape" = c("mean" = mean(mape_train_across),
                                                          "sd" = sd(mape_train_across)),
                                      "testing_mape" = c("mean" = mean(mape_test_across),
                                                         "sd" = sd(mape_test_across)))
  
  fitted_splits$starting_pars <- parmat
  
  # Fit the model to all block 1 data, and test on block 2 (Main train/test split)
  mainModel <- train_test(dat, parmat, bounds, main = TRUE, initial = initial)
  
  # Extract the best set by lowest -logLik value and by prediction on test set
  mainModel$bestSet$fit <- mainModel$fittedModel[mainModel$fittedModel$value == 
                                                   min(mainModel$fittedModel$value), ]
  mainModel$bestSet$test <- mainModel$fittedModel[mainModel$fittedModel$testing_mape == 
                                                    min(mainModel$fittedModel$mape_test), ]
  
  fitted_splits$main <- mainModel
  fitted_splits$splits <- splits
  fitted_splits$nSplits <- nSplits
  fitted_splits$nStarts <- nStarts
  
  # Output results
  return(fitted_splits)
}