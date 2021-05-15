# ------------------------------------------------------------------------------
# Prediction function - Standard FFM
# ------------------------------------------------------------------------------
standardPredict <- function(pars, loads, initialPars = c(0,0)){
  
  # Parameters supplied as: pars <- c(p*, k_g, Tau_g, k_h, Tau_h)
  
  # Ancillary function (required)
  # ----------------------------------------------------------------------------
  convolveTraining <- function(loads, tau){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * exp(-(dayt:1 / tau))))
  }
  # ----------------------------------------------------------------------------
  
  # Length of the training load series (final day)
  T <- tail(loads$day, 1)
  
  # If initial parameters q_g and q_h are supplied (otherwise evaluates to 0)
  initialFitness <- initialPars[1] * exp(-(1:T) / pars[3])
  initialFatigue <- initialPars[2] * exp(-(1:T) / pars[4])
  
  # Calculate the fitness and fatigue effects (Utilizing sapply function)
  fitness <- pars[2] * 
    base::sapply(1:T, function(t) convolveTraining(loads$load[1:t], 
                                                   pars[3]))
  
  fatigue <- pars[4] * 
    base::sapply(1:T, function(t) convolveTraining(loads$load[1:t], 
                                                   pars[5]))
  
  performance <- pars[1] + initialFitness - initialFatigue + fitness - fatigue
  
  # Return model predicted performance, fitness, and fatigue
  return(data.frame(day = 1:T,
                    performance = performance, 
                    fitness = fitness, 
                    fatigue = fatigue, 
                    load = loads$load[2:(T + 1)]))
}

# ------------------------------------------------------------------------------
# Prediction function - Fitness-delay FFM
# ------------------------------------------------------------------------------
fitnessDelayPredict <- function(pars, loads, initialPars = c(0,0)){
  
  # Parameters supplied as: pars <- c(p*, k_g, Tau_g, Tau_g2, k_h, Tau_h)
  
  # Ancillary function (required)
  # ----------------------------------------------------------------------------
  convolveTrainingDelay <- function(loads, tau1, tau2){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    delay <- exp(-(dayt:1 / tau1)) - exp(-(dayt:1 / tau2)) 
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * delay))
  }
  convolveTraining <- function(loads, tau){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * exp(-(dayt:1 / tau))))
  }
  # ----------------------------------------------------------------------------
  
  # Length of the training load series (final day)
  T <- tail(loads$day, 1)
  
  # If initial parameters q_g and q_h are supplied (otherwise evaluates to 0)
  initialFitness <- initialPars[1] * exp(-(1:T) / pars[3])
  initialFatigue <- initialPars[2] * exp(-(1:T) / pars[6])
  
  # Calculate the fitness and fatigue effects (Utilizing sapply function)
  fitness <- pars[2] * 
    base::sapply(1:T, function(t) convolveTrainingDelay(loads$load[1:t], 
                                                   pars[3], pars[4]))
  
  fatigue <- pars[5] * 
    base::sapply(1:T, function(t) convolveTraining(loads$load[1:t], 
                                                   pars[6]))
  
  performance <- pars[1] + initialFitness - initialFatigue + fitness - fatigue
  
  # Return model predicted performance, fitness, and fatigue
  return(data.frame(day = 1:T,
                    performance = performance, 
                    fitness = fitness, 
                    fatigue = fatigue, 
                    load = loads$load[2:(T + 1)]))
}

# ------------------------------------------------------------------------------
# Prediction function - VDR FFM
# ------------------------------------------------------------------------------

vdrPredict <- function(pars, loads, initialPars = c(0,0)){
  
  # Parameters supplied as: pars <- c(p*, k_g, Tau_g, k_h, Tau_h, Tau_h2)
  
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
  
  # Length of the training load series supplied = final day (T) for which to compute p\hat(t)
  T <- tail(loads$day, 1)
  
  # If initial parameters q_g and q_h are supplied (otherwise evaluates to 0)
  initialFitness <- initialPars[1] * exp(-(1:T) / pars[3])
  initialFatigue <- initialPars[2] * exp(-(1:T) / pars[5])
  
  # Compute modeled fitness
  fitness <- pars[2] * 
    base::sapply(1:T, function(i) convolveTraining(loads$load[1:i], pars[3]))
  
  # For each i=1:T, compute k_h_2(i) = sum_(j=0)^(i){w(j)e^(-(i-j)/tau_h_2)} 
  # s.t. k_h_2(t) = sum(k_h_2(i)) for i = 1,2,...,(t-1)
  kh2 <- base::sapply(1:T, function(i) kh2Compute(loads$load[1:i], pars[6]))
  
  # Compute modeled fatigue
  fatigue <- pars[4] *
    base::sapply(1:T, function(i) convolveTraining(kh2[1:i], pars[5]))
  
  # Compute modeled performance
  performance <- pars[1] + initialFitness - initialFatigue + fitness - fatigue
  
  # Return model predicted performance, fitness, and fatigue
  return(data.frame(day = 1:T,
                    performance = performance, 
                    fitness = fitness, 
                    fatigue = fatigue,
                    kh2 = kh2,
                    load = loads$load[2:(T + 1)]))
}