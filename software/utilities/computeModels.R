# Version 1.0
# Documentation: github.com/bsh2/fitness-fatigue-models/software/utilities/

computeModels = function(model = NULL,
                         parms = NULL,
                         loadSeries = NULL){

# Models include: Quick guide
  
  
  #   standardModel(parms, loadSeries) - model = "standard"
  #       parms: c(p*,kg,Tg,kh,Th)
  #       loadSeries: single vector of positive real values of time-series length
  
  #   standardModelIC(parms, loadSeries) - model = "standardIC"
  #       parms: c(p*,kg,Tg,kh,Th,qg,qh)
  #       loadSeries: single vector of positive real values of time-series length
  
  #   turnerModel(parms, loadSeries) - model = "turner"
  #       parms: c(kg, kh, Tg, Th, alpha, beta, p*, g0, h0)
  #       loadSeries: single vector of positive real values of time-series length
  
  #   banisterModel(parms, loadSeries) - model = "banister"
  #       parms: c(kg, kh, Tg, Th, p*, g0, h0)
  #       loadSeries: single vector of positive real values of time-series length
  
  #   calvertModel(parms, loadSeries) - model = "calvert"
  #       parms:
  #       loadSeries: single vector of positive real values of time-series length
  
  #   calvertModelIC(parms, loadSeries) - model = "calvert"
  #       parms:
  #       loadSeries: single vector of positive real values of time-series length
  
# EXPERIMENTAL
#   basicModel(parms, loadSeries) - model = "basic"
#       parms: c(p*, K, T)
#       loadSeries: single vector of positive real values of time-series length

#   basicModelIC(parms, loadSeries) - model = "basicIC"
#       parms: c(p*, K, T, q)
#       loadSeries: single vector of positive real values of time-series length

# Input validation
  
  if (is.null(model)){
    stop("Supply the name of the model you want to compute")
  }
  
  if (is.null(loadSeries)){
    stop("No training load series supplied")
  }
  
  if (is.null(parms)){
    stop("No definite parameter values supplied")
  }
  
# Model functions
  
  # Basic Model (No initial component)
  basicModel = function(parms, loadSeries){
    p = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
    for (n in 1:length(s)){
      df1 <- df0[1:s[n], ]
      p[n] <- parms[1] + parms[2]*sum(df1$ws * exp(-(n-df1$s) / parms[3]))
    }
  return(p)
  }
  
  # Basic Model (With single initial component)
  basicModelIC = function(parms, loadSeries){
    p = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
    for (n in 1:length(s)){
      df1 <- df0[1:s[n], ]
      p[n] <- parms[1] + parms[4]*(exp (- (n/parms[3]))) +
        parms[2]* sum(df1$ws * exp(-(n-df1$s) / parms[3]))
    }
    return(p)
  }
    
  # Standard Model (No initial component)
  standardModel = function(parms, loadSeries){
    p = numeric(length = length(loadSeries))
    fitness = numeric(length = length(loadSeries))
    fatigue = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
      for (n in 1:length(s)){
        df1 <- df0[1:s[n], ]
        fitness[n] = parms[2] * sum( df1$ws * exp(- (n - df1$s) / parms[3]) )
        fatigue[n] = parms[4] * sum( df1$ws * exp(- (n - df1$s) / parms[5]) )
        p[n] = parms[1] + fitness[n] - fatigue[n]
      }
    return(data.frame("fitness" = fitness, "fatigue" = fatigue,
                      "performance" = p))  
  } # End of computeModel function
  
  # Standard Model (With two initial components)
  standardModelIC = function(parms, loadSeries){
    p = numeric(length = length(loadSeries))
    fitness = numeric(length = length(loadSeries))
    fatigue = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
    for (n in 1:length(s)){
      df1 <- df0[1:s[n], ]
      fitness[n] = parms[2] * sum( df1$ws * exp(- (n - df1$s) / parms[3]) ) +
        parms[6]*(exp (- (n/parms[3])))
      fatigue[n] =  parms[4] * sum( df1$ws * exp(- (n - df1$s) / parms[5]) ) + 
        parms[7]*(exp (- (n/parms[5])))
      p[n] <- parms[1] + fitness[n] - fatigue[n]
    }
    return(data.frame("fitness" = fitness, "fatigue" = fatigue,
                      "performance" = p))  
  } # End of computeModel function

  # Turner's model
  turnerCompute = function(parms, loadSeries){
    # Format data into correct format
    compData = data.frame("days" = 0:length(loadSeries),
                          "G" = c(rep(0, length(loadSeries)+1)),
                          "H" = c(rep(0, length(loadSeries)+1)),
                          "pHat" = c(rep(0, length(loadSeries)+1)),
                          "loads" = c(0, loadSeries)
    )
    # Function called by numerical ODE solver
    turnerSolve = function(t, y, parms){
      r = c()
      r[1] = (parms[1]*currentLoad) - 
        ((1/parms[3]) * y["G"]^(parms[5]))
      r[2] = (parms[2]*currentLoad) - 
        ((1/parms[4]) * y["H"]^(parms[6]))
      return(list(r))
    } # end of turnerSolve
    # Solve model
    for (j in 1:length(compData$days)){
      currentLoad = compData$loads[j]
      if (j == 1){
        stateInit = c(G = parms[8],     # Fitness
                      H = parms[9])     # Fatigue
      } else{
        # Initialize based on previous value
        stateInit = c(G = compData$G[j-1],    # Fitness 
                      H = compData$H[j-1])    # Fatigue
      }
      t = 0:1
      out = ode(y = stateInit, times = t, func = turnerSolve, 
                parms = parms)
      if (j == 1){
        compData$G[j] = unname(out[1,2])
        compData$H[j] = unname(out[1,3])
      } else{
        compData$G[j] = unname(out[2,2])
        compData$H[j] = unname(out[2,3])
      }
      compData$pHat[j] = parms[7] + compData$G[j] - compData$H[j]
    } # End of solve loop
    return(compData)
  } # End of turnerCompute
  
  # Banister's model (Original system)
  # Model Computation Function
  banisterCompute = function(parms, loadSeries){
    # Format data into correct format
    compData = data.frame("days" = 0:length(loadSeries),
                          "G" = c(rep(0, length(loadSeries)+1)),
                          "H" = c(rep(0, length(loadSeries)+1)),
                          "pHat" = c(rep(0, length(loadSeries)+1)),
                          "loads" = c(0, loadSeries)
    )
    # Function called by numerical ODE solver
    banisterSolve = function(t, y, parms){
      r = c()
      r[1] = (parms[1]*currentLoad) - 
        ((1/parms[3]) * y["G"])
      r[2] = (parms[2]*currentLoad) - 
        ((1/parms[4]) * y["H"])
      return(list(r))
    } # end of banisterSolve
    # Solve model
    for (j in 1:length(compData$days)){
      currentLoad = compData$loads[j]
      if (j == 1){
        stateInit = c(G = parms[6],     # Fitness
                      H = parms[7])     # Fatigue
      } else{
        # Initialize based on previous value
        stateInit = c(G = compData$G[j-1],    # Fitness 
                      H = compData$H[j-1])    # Fatigue
      }
      t = 0:1
      out = ode(y = stateInit, times = t, func = banisterSolve, 
                parms = parms)
      if (j == 1){
        compData$G[j] = unname(out[1,2])
        compData$H[j] = unname(out[1,3])
      } else{
        compData$G[j] = unname(out[2,2])
        compData$H[j] = unname(out[2,3])
      }
      compData$pHat[j] = parms[5] + compData$G[j] - compData$H[j]
    } # End of solve loop
    return(compData)
  } # End of banisterCompute
  
  # Calvert's model (Fitness-delay)
  calvertModel = function(parms, loadSeries){
    
    p = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
      for (n in 1:length(s)){
        df1 <- df0[1:s[n], ]
        p[n] <- parms[1] + 
          parms[2] * sum( df1$ws * (exp(- (n - df1$s) / parms[3]) - 
                                     exp(- (n - df1$s) / parms[4]))) -
          parms[5] * sum( df1$ws * exp(- (n - df1$s) / parms[6]) )
      }
    return(p)  
  }
  
  # Calvert's model (Fitness-delay)
  calvertModelIC = function(parms, loadSeries){
    
    p = numeric(length = length(loadSeries))
    s = 1:length(loadSeries)
    df0 = data.frame(s, "ws" = loadSeries)
    for (n in 1:length(s)){
      df1 <- df0[1:s[n], ]
      
      p[n] <- parms[1] + 
        parms[2] * sum( df1$ws * (exp(- (n - df1$s) / parms[3]) - 
                                   exp(- (n - df1$s) / parms[4]))) -
        parms[5] * sum( df1$ws * exp(- (n - df1$s) / parms[6]) ) +
        parms[7]*(exp (- (n/parms[3]))) -
        parms[8]*(exp (- (n/parms[6])))
    }
    return(p)  
  }
  
# Call appropriate function
  
  # Basic Model
    if (model == "basic"){
      if (length(parms) != 3){
        stop("Incorrect parameters supplied. Please supply a vector of 
             c(p*,K,T)")
      }
      computedModel = basicModel(parms, loadSeries)
    }
    # Initial conditions
    if (model == "basicIC"){
      if (length(parms) != 4){
        stop("Incorrect parameters supplied. Please supply a vector of 
               c(p*,K,T,q)")
      }
      computedModel = basicModelIC(parms, loadSeries)
    }
  
  # Standard Model
    if (model == "standard"){
      if (length(parms) != 5){
        stop("Incorrect parameters supplied. Please supply a vector of 
             c(p*,kg,Tg,kh,Th)")
      }
      computedModel = standardModel(parms, loadSeries)
    }
    # Initial conditions
    if (model == "standardIC"){
      if (length(parms) != 7){
        stop("Incorrect parameters supplied. Please supply a vector of 
             c(p*,kg,Tg,kh,Th,qg,qh)")
      }
      computedModel = standardModelIC(parms, loadSeries)
    }
  
  # Turner model
    if (model == "turner"){
      if (length(parms) != 9){
        stop("Incorrect parameters supplied. Please supply a vector of 
             c(kg,kh,Tg,Th,alpha,beta,p0,g0,h0)")
      }
      require(deSolve)
      computedModel = turnerCompute(parms, loadSeries)
    }
  
  # Banister model
    if (model == "banister"){
      if (length(parms) != 7){
        stop("Incorrect parameters supplied. Please supply a vector of 
             c(kg,kh,Tg,Th,p0,g0,h0)")
      }
      require(deSolve)
      computedModel = banisterCompute(parms, loadSeries)
    }
  
  # Calvert model (no initial component)
    if (model == "calvert"){
      if (length(parms) != 6){
        stop("Incorrect parameters supplied. Please supply a vector of 
               c(p*,kg,Tg1,Tg2,kh,Th)")
      }
      computedModel = calvertModel(parms, loadSeries)
    }
    
  # Calvert model (with initial component)
    if (model == "calvertIC"){
      if (length(parms) != 8){
        stop("Incorrect parameters supplied. Please supply a vector of 
               c(p*,kg,Tg1,Tg2,kh,Th,qg,qh)")
      }
      computedModel = calvertModelIC(parms, loadSeries)
    }
  
  # Return computed model of choice
  return(computedModel)
}