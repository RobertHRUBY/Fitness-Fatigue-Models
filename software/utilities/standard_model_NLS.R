# ------------------------------------------------------------------------------
# Example inputs for functions contained in the file
# ------------------------------------------------------------------------------

# pars <- c(100, 1, 25, 1.2, 5)  # Parameters in order (p*, k_g, Tau_g, k_h, Tau_h)

# Training loads (inputs)
# loads <- data.frame("day" = c(0,1,2,3,4,5), "load" = c(100, 100, 100, 100, 100))

# Measured performances (model target)
# perfVals <- data.frame("day" = c(3,4), "performance" = c(500, 200))

# ------------------------------------------------------------------------------
# Objective function (Residual sum of squares) - Standard FFM
# ------------------------------------------------------------------------------
standardObjective <- function(pars, loads, perfVals){
  
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
  return(sum(squaredResiduals))
}

# ------------------------------------------------------------------------------
# Prediction function (Residual sum of squares) - Standard FFM
# ------------------------------------------------------------------------------
standardPredict <- function(pars, loads, returnObject = "all"){
  
  # Returns
  #   if returnObject = "all" : dataframe with daily performance, fitness, fatigue
  #   if returnObject = "fitness" : vector of daily model fitness values only
  #   if returnObject = "fatigue" : vector of daily model fatigue values only
  #   if returnObject = "performance" : vector of daily model performance values only
  
  # Set up zeroed vectors of required length
  seriesLength <- tail(loads$day, 1)
  performance <- numeric(length = seriesLength)  # Model performance
  fitness <- numeric(length = seriesLength)      # Model fitness
  fatigue <- numeric(length = seriesLength)      # Model fatigue
  
  # Calculate model fitness g(t), fatigue h(t), and performance  p(t) for t = 1:seriesLength
  for (t in 1:seriesLength){
    
    # Isolate the required load data for calculating p(t) (i.e. loads from day 0 to day t-1)
    inputSubset <- loads[loads$day < t, ]
    
    # Compute g(t), h(t), p(t) for current t
    fitness[t] <- pars[2] * sum( inputSubset$load * exp(- (t - inputSubset$day) / pars[3]) )
    fatigue[t] <- pars[4] * sum( inputSubset$load * exp(- (t - inputSubset$day) / pars[5]) )
    performance[t] <- pars[1] + fitness[t] - fatigue[t]
    
  } # Loop index updates (t <- t+1) until t = seriesLength
  
  # Output
  if (returnObject == "performance"){return(performance)}
  if (returnObject == "fitness"){return(fitness)}
  if (returnObject == "fatigue"){return(fatigue)}
  if (returnObject == "all"){
    return(data.frame("day" = 1:seriesLength, "fitness" = fitness, "fatigue" = fatigue,
                      "performance" = performance))
  }
  
} # End function (closing bracket)