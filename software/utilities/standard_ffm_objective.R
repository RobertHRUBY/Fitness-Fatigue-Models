# ------------------------------------------------------------------------------
# Objective function (Residual sum of squares) - Standard FFM
# ------------------------------------------------------------------------------
standardObjectiveSS <- function(pars, loads, perfVals, initial = FALSE,
                                maximise = FALSE){
  
  # INPUT NOTES:
  # ----------------------------------------------------
  #         [1] [2] [3] [4] [5] [6] [7]
  # Pars: c(p*, kg, Tg, kh, Th)          initial = FALSE
  # Pars: c(p*, kg, Tg, kh, Th, qg, qh)  initial = TRUE
  # ----------------------------------------------------
  
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
    
    if (initial == TRUE){
      initFitness <- pars[6] * exp(-(dayT) / pars[3])
      initFatigue <- pars[7] * exp(-(dayT) / pars[5])
    } else{
      initFitness <- 0
      initFatigue <- 0
    }
    
    # Compute modelled performance on dayT under pars // p\hat(dayT) = p* + g(dayT) - h(dayT)
    model <- pars[1] + initFitness - initFatigue +
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
    return(-1 * sum(squaredResiduals))
  }
}

# ------------------------------------------------------------------------------
# Objective function (Log likelihood) - Standard FFM
# ------------------------------------------------------------------------------

standardObjectiveLL <- function(pars, loads, perfVals, initial = FALSE, 
                              maximise = FALSE){
  
  # INPUT NOTES:
  # ---------------------------------------------------------------------------
  #         [1] [2] [3] [4] [5] [6]    [7] [8]
  # Pars: c(p*, kg, Tg, kh, Th, sigma)                  initial = FALSE
  # Pars: c(p*, kg, Tg, kh, Th, sigma, qg, qh)          initial = TRUE
  # ---------------------------------------------------------------------------
  
  # Ancillary function (required)
  # ----------------------------------------------------------------------------
  convolveTraining <- function(loads, tau){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * exp(-(dayt:1 / tau))))
  }
  # ----------------------------------------------------------------------------
  
  finalMeasurement <- tail(perfVals$day, 1)
  
  if (initial == TRUE){
    initFitness <- pars[7] * exp(-(1:finalMeasurement) / pars[3])
    initFatigue <- pars[8] * exp(-(1:finalMeasurement) / pars[5])
  }
  
  # Compute modeled performance from t=1 to t=finalMeasurement
  modFitness <- pars[2] * sapply(1:finalMeasurement, function(t) convolveTraining(loads$load[1:t], pars[3]))
  modFatigue <- pars[4] * sapply(1:finalMeasurement, function(t) convolveTraining(loads$load[1:t], pars[5]))
  
  if (initial == FALSE){
    modPerformance <- pars[1] + modFitness - modFatigue
  }
  
  if (initial == TRUE){
    modPerformance <- pars[1] + initFitness - initFatigue + modFitness - modFatigue
  }
  
  # Extract modeled performance values on days where measurement exists
  modPerformance <- modPerformance[perfVals$day]
  
  # Compute errors
  errors <- perfVals$performance - modPerformance
  
  if (maximise = FALSE){
    return(-1.0 * sum(dnorm(errors, mean = 0, sd = pars[6], log = TRUE)))
  }
  if (maximise = TRUE){
    return(sum(dnorm(errors, mean = 0, sd = pars[6], log = TRUE)))
  }
  
}
