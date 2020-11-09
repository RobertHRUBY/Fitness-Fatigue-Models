# CMA-ES Optimisation (Covariance Matrix Adaptation Evolution Strategy)
call_cmaes_fit <- function(i){
  temp <- cma_es(par = as.numeric(par_start[i,]), fn = ffm4_mse, 
                 lower = lower_bound, upper = upper_bound,
                 control = list(maxit = 100*(4^2))
  )
  return(temp)
}