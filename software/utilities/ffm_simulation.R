# ------------------------------------------------------------------------------
# Prediction function - Standard FFM
# ------------------------------------------------------------------------------
standardPredict <- function(pars, loads, initialPars = c(0,0)){
  
  # Parameters supplied as: pars <- c(p*, k_g, Tau_g, k_h, Tau_h)
  
  convolveTraining <- function(loads, tau){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * exp(-(dayt:1 / tau))))
  }
  
  # Length of the training load series (final day)
  T <- tail(loads$day, 1)
  
  # If initial parameters q_g and q_h are supplied (otherwise evaluates to 0)
  initialFitness <- initialPars[1] * exp(-(1:T) / pars[3])
  initialFatigue <- initialPars[2] * exp(-(1:T) / pars[4])
  
  # Calculate the fitness and fatigue effects (Utilizing sapply function)
  fitness <- pars[2] * 
    base::sapply(1:T, function(t) convolveTraining(loads$load[1:t], 
                                                   pars[3]))
  
  fatigue <- standardPars[4] * 
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
standardPredict <- function(pars, loads, initialPars = c(0,0)){
  
  # Parameters supplied as: pars <- c(p*, k_g, Tau_g, Tau_g2, k_h, Tau_h)
  
  convolveTraining <- function(loads, tau1, tau2){
    
    # Value of t relevant to (eq 6.9) 
    dayt <- length(loads)
    delay <- exp(-(dayt:1 / tau1)) - exp(-(dayt:1 / tau2)) 
    
    # Note that loads[1:dayt] will yield c(w(0), w(1), ... , w(t-1))
    return(sum(loads[1:dayt] * delay))
  }
  
  # Length of the training load series (final day)
  T <- tail(loads$day, 1)
  
  # If initial parameters q_g and q_h are supplied (otherwise evaluates to 0)
  initialFitness <- initialPars[1] * exp(-(1:T) / pars[3])
  initialFatigue <- initialPars[2] * exp(-(1:T) / pars[6])
  
  # Calculate the fitness and fatigue effects (Utilizing sapply function)
  fitness <- pars[2] * 
    base::sapply(1:T, function(t) convolveTraining(loads$load[1:t], 
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
