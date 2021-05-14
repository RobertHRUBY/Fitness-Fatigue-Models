source(vdr_ffm_objective.R)
source(ffm_simulation.R)

expandingWindow_CV <- function(dat,
                               bounds,
                               model,
                               objective,
                               algorithm,
                               initialWindow = 60,
                               testHorizon = 20,
                               expandRate = 4,
                               nStarts = 10,
                               cores = NULL){
  
  
  
  
  
  
  
  
  
  # Generate splits (note split vectors gives you an index position vs. a 'day', think t-1!)
  splits <- generate_splits(initialWindow, testHorizon, expandRate, dat[dat$block == 1, ])
  nSplits <- length(splits$train)   # Number of splits
  
  # Train/Test on each split
  
  # Create an array of random starting values over the bounds
  parmat <- create_grid(bounds, nStarts)
  
  # Iterate over the splits
  if (is.null(cores)){
    cores <- detectCores(logical = TRUE)
  } # If cores not specified by user
  cl <- makeCluster(cores, type = "SOCK")              # Make cluster
  registerDoSNOW(cl)                                   # Register cluster
  fitted_splits <- foreach(i = 1:nSplits, .verbose = TRUE, .packages = c("optimx"),
                           .export = c("train_test", "vdrObjective", "vdrPredict",
                                       "mape", "kh2Compute", "convolveTraining")) %dopar%{
                                         train_test(dat, parmat, bounds, main = FALSE, splits = splits, currentSplit = i)}
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
  mainModel <- train_test(dat, parmat, bounds, main = TRUE)
  
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