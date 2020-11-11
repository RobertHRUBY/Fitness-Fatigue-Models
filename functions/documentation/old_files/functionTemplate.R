functionTemplate = function(){
  
  # (1) Check function dependencies (packages)
  
  # (2) Function input validation checks
  
  # (3) Develop the primary calibration function
  
    # (3.1) Define objective function
  
    # (3.2) Define model computation function
  
    # (3.3) Subset input to generate slice data
  
    # (3.4) Fit the model (Bounded Quasi-Newton method or Genetic Evolution)
  
    # (3.5) Extract fitted parameter values, compute model forecast values
  
    # (3.6) Compute R-squared*, RMSE and MAPE values for train* and test sets
    #       * only training set
  
    # (3.7) Return object to loop (optim data, performance values, statistics)
  
  # (4) Slice data into expanding windows
  
    # (4.1) Slices determined by initialWindow, testHorizon, expandRate
  
  # (5) Implement model (loop over primary calibration function)
  
    # (5.1) For slice 1:n {fit model} (Calls fn in section 3 of code)
  
  # (6) Tabulate results and print plots to console
  
    # (6.1) Identify best slice (set) found (by lowest MAPE_test)
  
    # (6.2) Compile summary statistics across all slices
  
    # (6.3) Plot the best slice (set)
  
    # (6.4) Plot all slices (sets) (region plot)
  
    # (6.5) Print results to console (summary statistics and best set)
  
  # (7) Return object to user (slices (pars,stats), performance values, summary)

}