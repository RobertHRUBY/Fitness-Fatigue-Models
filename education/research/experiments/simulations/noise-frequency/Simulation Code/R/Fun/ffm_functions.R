# ===================================(3.4.1)====================================
# Functions: Fitness-Fatigue Model specific functions
# ==============================================================================

# Function to compute the FFM given model parameters and training load (wt) data

ffm.compute <- function(p0, s, k1, k2, tau1, tau2, wt){
  model_performance <- numeric(length(s))
  df_temp <- data.frame(s, wt)
  for (b in 1:length(s)) {
    df_temp2 <- df_temp[1:s[b], ]
    model_performance[b] <- p0 +
      k1 * (sum((exp(- ((df_temp2$s[b]) - df_temp2$s) / tau1) * df_temp2$wt))) -
        k2 * (sum((exp( - ((df_temp2$s[b]) - df_temp2$s) / tau2) * 
          df_temp2$wt)))
  }
  return(model_performance)
}

# Fit the fitness fatigue model to true data with error

ffm.fit <- function(x){
  ffm.ss <- function(par){
    squared_diff <- numeric(length(x[!is.na(x)]))
    temporary_data <- data.frame(s2, "wt" = wt_b1)
    temporary_performance <- x[!is.na(x)]
    measure_days <- freq
    for (l in 1:length(squared_diff)){
      loop_dat <- temporary_data[1:measure_days[l],]
        squared_diff[l] <- ( ( p0 + 
          par[1]*(sum(exp(-(measure_days[l] - loop_dat$s2) / par[2]) * 
            loop_dat$wt)) - 
              par[3]*(sum(exp(-(measure_days[l] - loop_dat$s2) / par[4]) * 
                loop_dat$wt))) - 
                  (temporary_performance[l]) )^2
    }
    return(sum(squared_diff))
  }
  parameters <- data.frame(optimx(par = optim_start, fn = ffm.ss, 
    lower = c(0.001,2,0.002,1), upper = c(10, 80, 20, 60),
    method = "L-BFGS-B", control = list(maxit = 10^5)))
  return(unlist(parameters))
}
  
# Compute FFM predictions with Preload (Ludwig) values

ffm.predictions <- function(parameter_matrix){
  wt1 <- wt_b1
  wt2 <- wt_b2
  # Set up preload objects
  preload_1 <- vector(mode = "numeric", length = nSims)
  preload_2 <- vector(mode = "numeric", length = nSims)
  # Functions to compute preload
  Preload <- function(tau_param){
    pload_vals <- numeric(length(s2))
    df_temp <- data.frame(s2, wt1)
    for (h in 1:length(s2)){
      df_temp2 <- df_temp[1:s2[h],]
      pload_vals[h] <- sum((exp(- ((df_temp2$s2[h]) - df_temp2$s2) / 
        tau_param) * df_temp2$wt1))
    }
    pload_end <- pload_vals[length(s2)]
    return(pload_end)
  }
  # Calculate Preload Values for each parameter column
  for (r in 1:nSims){
    preload_1[r] <- Preload(parameter_matrix["T1", r])
    preload_2[r] <- Preload(parameter_matrix["T2", r])
  }
  preload_vals <- data.frame(preload_1, preload_2)
  predicted_performance <- matrix(nrow = length(s2), ncol = nSims)
  # Function to compute FFM with the preload values
  ComputeWithPreload <- function(pars, pr1, pr2){
    model_performance <- numeric(length(s2))
    df_temp <- data.frame(s2,wt2)
    for (s in 1:length(s2)) {
      df_temp2 <- df_temp[1:s2[s], ]
      model_performance[s] <- p0 +
        pars[1] * ((pr1 * exp(-s / pars[2])) +
          sum((exp(- ((df_temp2$s2[s]) - df_temp2$s2) / pars[2]) * 
            df_temp2$wt2))) -  pars[3] * ((pr2 * exp(-s / pars[4])) +
              sum((exp( - ((df_temp2$s2[s]) - df_temp2$s2) / pars[4]) * 
                df_temp2$wt2)))
    }
    return(model_performance)
  }
  for (w in 1:nSims){
    parameters_temp <- c(parameter_matrix[1:4,w])
    pload_temp1 <- preload_1[w]
    pload_temp2 <- preload_2[w]
    predicted_performance[,w] <- ComputeWithPreload(parameters_temp,
      pload_temp1, pload_temp2)
  }
  return(predicted_performance)  
}



