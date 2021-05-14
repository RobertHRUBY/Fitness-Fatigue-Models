# ------------------------------------------------------------------------------
# Prediction function - Standard FFM
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

