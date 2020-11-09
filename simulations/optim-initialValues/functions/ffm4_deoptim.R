# Differential Evolution Optimisation Function

de_fit_mse <- function(...){
  DEoptim(
  ffm4_mse, 
  lower = lower_bound, 
  upper = upper_bound, 
  DEoptim.control(
    VTR = precision, # Search stop when cost reaches noise^2
    strategy = 2, 
    NP = 300, 
    itermax = 1000, # maximum number of iterations
    CR = 0.9, F = 0.9, 
    trace = TRUE
    )  
  )
}

