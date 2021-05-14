# ------------------------------------------------------------------------------
# Objective function (Residual sum of squares) - VDR FFM
# ------------------------------------------------------------------------------
vdrObjective <- function(pars, loads, perfVals, initial = FALSE, maximise = FALSE){
  
  # INPUT NOTES:
  # ----------------------------------------------------------
  #         [1] [2] [3] [4] [5] [6]  [7] [8]
  # Pars: c(p*, kg, Tg, kh, Th, Th2)          initial = FALSE
  # Pars: c(p*, kg, Tg, kh, Th, Th2, qg, qh)  initial = TRUE
  # ----------------------------------------------------------
  
  nMeasurements <- length(perfVals$performance)  # Number of performance measurements
  
  # Zeroed vector of length equal to number of performance measurements
  squaredResiduals <- numeric(length = nMeasurements)
  
  # For each performance measurement calculate (modelled - measured)^2 under pars
  for (n in 1:nMeasurements){
    
    dayT <- perfVals$day[n]                # Day of measured performance
    measured <- perfVals$performance[n]    # Measured performance value on dayT
    
    # Isolate the required load data to compute the model up to dayT (i.e., from t=0 to dayT - 1)
    inputSubset <- loads[1:dayT, ]
    
    # Initial components
    if (initial == TRUE){
      initFitness <- pars[7] * exp(-(dayT) / pars[3])
      initFatigue <- pars[8] * exp(-(dayT) / pars[6])
    } else{
      initFitness <- 0
      initFatigue <- 0
    }
    
    # Set up a zeroed vector to hold the variable gain term values kh2(i) for i=0 to dayT - 1
    kh2 <- numeric(length = dayT)  # Variable gain term vector
    
    # Calculate the variable gain term kh2(i) for i=0,1,2,...,dayT-1 (Recursive)
    for (i in 1:dayT){
      kh2[i] <- sum( inputSubset$load[1:i] * exp( -((inputSubset$day[i]-inputSubset$day[1:i])/pars[6])))
    }
    
    # Compute modelled performance on dayT under pars // p\hat(dayT) = p* + g(dayT) - h(dayT)
    model <- pars[1] + initFitness - initFatigue +
      pars[2] * (sum(inputSubset$load * exp(-(dayT - inputSubset$day) / pars[3]))) - 
      pars[4] * (sum(kh2 * inputSubset$load * exp(-(dayT - inputSubset$day) / pars[5])))
    
    # Compute the squared residual value (model - measured)^2
    squaredResiduals[n] <- (model - measured)^2
    
  }
  
  # Output
  if(maximise = FALSE){
    return(sum(squaredResiduals))
  }
  if(maximise = TRUE){
    return(-sum(squaredResiduals))
  }
}

# ------------------------------------------------------------------------------
# Objective function (Log likelihood) - VDR FFM
# ------------------------------------------------------------------------------

vdrObjective <- function(pars, loads, perfVals, initial = FALSE, maximise = FALSE){
  
  # INPUT NOTES:
  # -----------------------------------------------------------------
  #         [1] [2] [3] [4] [5] [6]  [7]    [8] [9]
  # Pars: c(p*, kg, Tg, kh, Th, Th2, sigma)          initial = FALSE
  # Pars: c(p*, kg, Tg, kh, Th, Th2, sigma, qg, qh)  initial = TRUE
  # -----------------------------------------------------------------
  
  # Ancillary functions (required)
  # ----------------------------------------------------------------------------
  convolveTraining <- function(loads, tau){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * exp(-(dayt:1 / tau))))
  }
  
  kh2Compute <- function(loads, tau){
    day_i <- length(loads)
    return(sum(loads[1:day_i] * exp(-((day_i-1):0 / tau))))
  }
  # ----------------------------------------------------------------------------
  
  finalMeasurement <- tail(perfVals$day, 1)
  
  if (initial == TRUE){
    initFitness <- pars[8] * exp(-(1:finalMeasurement) / pars[3])
    initFatigue <- pars[9] * exp(-(1:finalMeasurement) / pars[5])
  }
  
  # Compute modeled performance from t=1 to t=finalMeasurement
  modFitness <- pars[2] * sapply(1:finalMeasurement, function(t) convolveTraining(loads$load[1:t], pars[3]))
  kh2 <- sapply(1:finalMeasurement, function(i) kh2Compute(loads$load[1:i], pars[6]))
  modFatigue <- pars[4] * sapply(1:finalMeasurement, function(t) convolveTraining(kh2[1:t], pars[5]))
  
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
    return(-1.0 * sum(dnorm(errors, mean = 0, sd = pars[7], log = TRUE)))
  }
  if (maximise = TRUE){
    return(sum(dnorm(errors, mean = 0, sd = pars[7], log = TRUE)))
  }

}


