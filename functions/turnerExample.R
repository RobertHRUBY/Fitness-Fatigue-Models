source("turnerModel.R")

# Create simulated data (with and without noise)



# Box constraints on the optimisation

# Vector order is : k_g, k_h, T_g, T_h, alpha, beta, p0, g0, h0
constraints = data.frame("lower" = c(0.1,0.1,1,1,0.5,0.5,25,5,5),
                         "upper" = c(10,10,50,50,5,5,200,100,100))

test <- turnerModel(inputData, constraints, doTrace = TRUE, useEvolution = FALSE)
