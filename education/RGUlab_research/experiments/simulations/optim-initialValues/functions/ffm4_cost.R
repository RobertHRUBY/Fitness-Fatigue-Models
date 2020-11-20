# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Cost functions for the 4 Parameter FFM
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# Residual Sum of Squares (RSS)
ffm4_ss <- function(par){
  # Used for the cost function of L-BFGS-B and Nelder-Mead
  differences <- numeric(length(p_noise_a))
  df0 <- data.frame(s1, ws1)
  for (n in 1:length(differences)){
    df1 <- df0[1:measure_index[n], ]
    differences[n] <- ( 
      (p0 +
         par[1] * sum( df1$ws1 * exp( - (measure_index[n] - df1$s1) / par[2] ) ) -
         par[3] * sum( df1$ws1 * exp( - (measure_index[n] - df1$s1) / par[4] ) )
      ) -
        p_noise_a[n]
    )^2
  }
  return(sum(differences))
}

# Mean-Squared Error (MSE)
ffm4_mse <- function(par){
  # Used for the cost function of DE,CMA-ES & Grid-Search
  differences <- numeric(length(p_noise_a))
  df0 <- data.frame(s1, ws1)
  for (n in 1:length(differences)){
    df1 <- df0[1:measure_index[n], ]
    differences[n] <- ( 
      (p0 +
         par[1] * sum( df1$ws1 * exp( - (measure_index[n] - df1$s1) / par[2] ) ) -
         par[3] * sum( df1$ws1 * exp( - (measure_index[n] - df1$s1) / par[4] ) )
      ) -
        p_noise_a[n]
    )^2
  }
  return(mean(differences))
}