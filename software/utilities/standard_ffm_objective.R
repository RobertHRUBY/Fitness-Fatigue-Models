# ------------------------------------------------------------------------------
# Objective function (Residual sum of squares) - Standard FFM
# ------------------------------------------------------------------------------
standardObjectiveSS <- function(pars, loads, perfVals, maximise = FALSE){
  
  nMeasurements <- length(perfVals$performance) # Number of performance measurements
  
  # Zeroed vector of length equal to number of performance measurements
  squaredResiduals <- numeric(length = nMeasurements)
  
  # For each performance measurement calculate (modeled - measured)^2 under pars
  for (n in 1:nMeasurements){
    
    dayT <- perfVals$day[n]              # Day of measured performance
    measured <- perfVals$performance[n]  # Measured performance value on dayT
    
    # Isolate the required load data to compute the model up to dayT (i.e. from t=0 to dayT - 1)
    # Note: 1:dayT rather than 1:(dayT - 1) as the first row in the loads array is w(0)=0
    inputSubset <- loads[1:dayT, ]
    
    # Compute modelled performance on dayT under pars // p\hat(dayT) = p* + g(dayT) - h(dayT)
    model <- pars[1] + 
      pars[2] * (sum(inputSubset$load * exp(-(dayT - inputSubset$day) / pars[3]) ) ) -
      pars[4] * (sum(inputSubset$load * exp(-(dayT - inputSubset$day) / pars[5]) ) )
    
    # Compute the squared residual value (model - measured)^2
    squaredResiduals[n] <- (model - measured)^2  
    
  } # Loop updates until iterated over all available measurements
  
  # Output
  if(maximise = FALSE){
    return(sum(squaredResiduals))
  }
  if(maximise = TRUE){
    return(-sum(squaredResiduals))
  }
}



