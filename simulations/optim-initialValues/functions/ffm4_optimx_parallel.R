call_lbfgsb_fit <- function(i){
    parameters_found <- data.frame(optimx(
      par <- as.numeric(par_start[i,]), # Initial Parameters
      fn = ffm4_mse,
      method = c("L-BFGS-B"),
      lower = lower_bound,
      upper = upper_bound,
      hessian = TRUE,
      control = list(
        maxit = 10^8,
        trace = 0) 
    ))
    return(parameters_found)
}
