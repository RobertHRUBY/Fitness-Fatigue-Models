ffm4 <- function(p0, s, k1, tau1, k2, tau2, ws){
  # ----------------------------------------------------------------------------
  # PURPOSE
  #   Generate model performance over a range of days 1:s (Compute model values)
  #
  # ARGS:
  #   p0:        Initial performance (vector of length 1)
  #   s :        Vector of consecutive days (1:n), length n
  #   par:       Vector of length 1 for k1,tau1,k2,tau2 (model parameters)
  #   ws:        Vector of training load for days 1:length(s)
  #
  # RETURNS:
  #    Vector of length s containing model performance values
  # ----------------------------------------------------------------------------
  p <- numeric(length(s))
  df0 <- data.frame(s,ws)
  for (n in 1:length(s)){
    df1 <- df0[1:s[n], ]
    p[n] <- p0 + 
      k1 * sum( df1$ws * exp(- (n - df1$s) / tau1) ) -
      k2 * sum( df1$ws * exp(- (n - df1$s) / tau2) )
  }
  return(p)
}

ffm4_forecast <- function(p0,s1,s2,k1,tau1,k2,tau2,ws1,ws2){
  # ----------------------------------------------------------------------------
  # PURPOSE
  #   Forecasting for hold-back (second block) data using Ludwig's pre-load
  #   method, to avoid having to compute model over both blocks to obtain
  #   appropriate predictions.
  #
  # ARGS:
  #   p0:        Initial performance at start of first block (vector of length 1)
  #   s1:        Vector of consecutive days (1:n), length n, in first block
  #   s2:        Vector of consecutive days (1:n), length n, in second block
  #   par(s):    Vector of length 1 for k1,tau1,k2,tau2 (model parameters)
  #              NB: These are the fitted (estimated) model parameters found
  #   ws1:       Vector of training load for days 1:length(s1), in first block
  #   ws2:       Vector of training load for days 1:length(s2), in second block
  #
  # RETURNS:
  #    Vector of length s2 containing model performance values for 'hold back'
  #    data
  # ----------------------------------------------------------------------------
  
  # Part 1: Nested Preload Functions
  pr1 <- function(){
    fitness_values <- numeric( length(s1) )
    df_temp <- data.frame(s1, ws1)
    for (i in 1:length(s1) ){
      df_temp_2 <- df_temp[1:s1[i], ]
      fitness_values[i] <- sum((exp(- ((df_temp_2$s1[i]) - df_temp_2$s1) / tau1) * 
                           df_temp_2$ws1))
    }
    return(fitness_values[length(s1)])
  }
  pr2 <- function(){
    fatigue_values <- numeric( length(s1) )
    df_temp <- data.frame(s1, ws1)
    for (i in 1:length(s1) ){
      df_temp_2 <- df_temp[1:s1[i], ]
      fatigue_values[i] <- sum((exp(- ((df_temp_2$s1[i]) - df_temp_2$s1) / tau2) * 
                            df_temp_2$ws1))
    }
    return(fatigue_values[length(s1)])
  }
  
  # Part 2: Compute Model Performance with Preload (Forecast second block)
  model_forecast <- numeric(length(s2))
  preload_fitness <- pr1()   # Compute Fitness Preload
  preload_fatigue <- pr2()   # Compute Fatigue Preload
  df_a <- data.frame(s2,ws2)
  for (i in 1:length(s2) ) {
    df_b <- df_a[1:s2[i], ]
    model_forecast[i] <- p0 +
      k1 * ((preload_fitness * exp(-i / tau1)) + 
              sum((exp(- ((df_b$s2[i]) - df_b$s2) / tau1) * df_b$ws2))) -
      k2 * ((preload_fatigue * exp(-i / tau2)) + 
              sum((exp( - ((df_b$s2[i]) - df_b$s2) / tau2) * df_b$ws2)))
  }
  return(model_forecast)
}