computeModels = function(model = NULL,
                         parms = NULL,
                         loadSeries = NULL){
  
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
  
  
  if (model == "turner"){
    
    if (length(parms) != 9){
      stop("Incorrect parameters supplied. Please supply a vector of 
           c(kg,kh,Tg,Th,alpha,beta,p0,g0,h0")
    }
    
    library(deSolve)
    
    computedModel = turnerCompute(parms, loadSeries)
    
  }
  
  # Return computed model of choice
  return(computedModel)
}