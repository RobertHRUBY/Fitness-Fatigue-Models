# ===================================(3.4.1)====================================
# Script: Main program script handling the simulation of specified scenarios
# ==============================================================================

# Begin outer loop (the loop controlling the Athlete-TRIMP grouping)
for (athlete_index in 1:6){
# Begin simulation of all 6 Athlete-TRIMP combinations
  
  # Set up simulation settings  
  source('R/Scripts/outer_control.R')
  
  # Initialise objects used to store simulation data which will come out of each
  # iteration of the inner loop
  
  scenario_params <- list()
  prediction_vals <- list()
  errors_abs <- list()
  errors_signed <- list()
  non_convergence <- c()
  
  # Begin inner loop (scenario loop - indexed by sce_index)
  for (sce_index in 1:35){
    
    # Athlete control statements (refresh the true data each loop)
    if (athlete_index == 1){
      true_block1 <- true_int_1[1:56]     # True Perf, TRIMPS-1 (Block 1)
    }
    if (athlete_index == 2){
      true_block1 <- true_int_2[1:56]     # True Perf, TRIMPS-2 (Block 1)
    }
    if (athlete_index == 3 | athlete_index == 5){
      true_block1 <- true_adv_1[1:56]     # True Perf, TRIMPS-1 (Block 1)
    }
    if (athlete_index == 4 | athlete_index == 6){
      true_block1 <- true_adv_2[1:56]     # True Perf, TRIMPS-2 (Block 1)
    }
    
    # Set Measurement Frequency (Cut down true data to required size)
    kPT <- seq(1, 56, by = frequencies[sce_index])
    true_block1[c( - kPT)] <- NA
    true_block1[c( - kPT)] <- NA
    kPT <- head(c(kPT, s2 * NA), length(s2))
    freq <- seq(1, length(s2), by = frequencies[sce_index])
    
    # Generate matrix of performance values
    perf_matrix <- matrix(nrow = length(s2), ncol = nSims)
    set.seed(100)
    for (k in 1:nSims){
      perf_matrix[,k] <- true_block1 + rnorm(length(s2), 0, errors[sce_index])
    }
    
    # Set up Cluster
    # **************************************************************************
    source("R/Scripts/vars_list.R")   # List of Variables to pass to each worker
    numCores <- detectCores(logical = FALSE)
    cl <- makeCluster(numCores)
    clusterExport(cl, varslist, envir = .GlobalEnv)
    clusterEvalQ(cl, library(optimx))
    # **************************************************************************
    # Fit the model
    scenario_params[[sce_index]] <- parApply(cl, perf_matrix, 2, ffm.fit)
    rownames(scenario_params[[sce_index]]) <- c("k1", "T1", "k2", "T2", "value", 
      "fevals", "gevals", "niter", "convcode", "kkt1", "kkt2", "xtimes")
    # Stop the cluster
    # **************************************************************************
    stopCluster(cl)
    # **************************************************************************
    
    # Analyse Parameter Convergence
      # Output value sum() will be an integer, representing the number of
      # parameter sets where convergence to a global minima was not achieved
    non_convergence[sce_index] <- 
      sum(scenario_params[[sce_index]]["convcode",] != 0)
    
    # Generate Prediction Values
    prediction_vals[[sce_index]] <- ffm.predictions(scenario_params[[sce_index]])
    
    # Print update message
    print(paste0("    Simulation ", sce_index, "/35 Completed"))
    
  } # End inner loop (scenario loop)
  
  # ----------------------------------------------------------------------------
  # Analysis of Simulation Results
  # ----------------------------------------------------------------------------
  
  # Transform prediction values into prediction errors (Signed and absolute)
  
  errors_abs <- TransformPred(prediction_vals, true_block2, signed = FALSE)
  errors_signed <- TransformPred(prediction_vals, true_block2, signed = TRUE)
  
  # Calculate Average Daily Prediction Error (68% Spread and Median), DPE_s/m
  # //Absolute Error Values

  DPE_vals <- ComputeDPE(errors_abs)
  
  # Test for Heteroscedasticity by checking signed error values over chunks

  TChunks <- TimeChunks(errors_signed)
  TChunks <- do.call(rbind.data.frame, TChunks)
  TChunks <- add_column(TChunks, "Simulation" = rep(1:35, each = 4),
    "Time_chunk" = rep(1:4, 35), .before = 1)
  
  # Create scatterplots of DPE_s across the simulations by error and freq
  
  DPE_spread <- DPE_vals[, c(1,2,3,6)]
  DPE_spread[,3] <- rep(1:7, each = 5)
  DPE_plot <- PlotScatters(DPE_spread)
  
  # Compute multiple regression model
  
  DPE_regression <- buildregression(DPE_spread)
  
  # Compute the correlation coef for model parameters
  coef_k1k2 <- as.numeric(CorrelatePars(scenario_params, pars = c('k1','k2')))
  coef_k1T1 <- as.numeric(CorrelatePars(scenario_params, pars = c('k1','T1')))
  coef_k1T2 <- as.numeric(CorrelatePars(scenario_params, pars = c('k1','T2')))
  coef_k2T1 <- as.numeric(CorrelatePars(scenario_params, pars = c('k2','T1')))
  coef_k2T2 <- as.numeric(CorrelatePars(scenario_params, pars = c('k2','T2')))
  coef_T1T2 <- as.numeric(CorrelatePars(scenario_params, pars = c('T1','T2')))
  coef_T1k1k2 <- as.numeric(CorrelatePars2(scenario_params, 'T1'))
  coef_T2k1k2 <- as.numeric(CorrelatePars2(scenario_params, 'T2'))
  
  par_coefs <- data.frame(coef_k1k2, coef_k1T1, coef_k1T2, coef_k2T1, coef_k2T2,
    coef_T1T2, coef_T1k1k2, coef_T2k1k2)
  colnames(par_coefs) <- c("k1-k2", 'k1-T1', 'k1-T2', 'k2-T1', 'k2-T1',
    'T1-T2','T1-k1/k2', 'T2-k1/k2')
  
 # Plot Parameter Distributions
  parameter_isolation <- subsetParameterList(scenario_params)

  p1_k1 <- plotParameters(parameter_isolation[[1]], plabel = "k1")
  p2_k2 <- plotParameters(parameter_isolation[[2]], plabel = "k2")
  p3_T1 <- plotParameters(parameter_isolation[[3]], plabel = "T1")
  p4_T2 <- plotParameters(parameter_isolation[[4]], plabel = "T2")
  
  # Save Everything to Appropriate Output Directory
  source("R/Scripts/save_data.R")
  
  # Clear data to ensure it doesn't 'leak' into other simulations and as a test
  # to check that isn't happening (i.e. it should run without error as it
  # regenerates these objects based on control statements at the start of each
  # outer loop iteration)
  rm(coef_k1k2, coef_k1T1, coef_k1T2, coef_k2T1, coef_k2T2, coef_T1T2,
     coef_T1k1k2, coef_T2k1k2, par_coefs, DPE_regression, DPE_plot, DPE_spread,
       TChunks, errors_abs, errors_signed, non_convergence, prediction_vals,
          scenario_params, perf_matrix, true_block1, true_block2, kPT, freq,
            p0, optim_start, errors, wt_b1, wt_b2, parameter_isolation, p1_k1,
              p2_k2, p3_T1, p4_T2)
  
} # End outer loop (athlete loop)

