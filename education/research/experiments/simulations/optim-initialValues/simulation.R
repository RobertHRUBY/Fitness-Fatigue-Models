# github.com/bsh2 ~ Simulation Experiment Code File ~ 27 March 2020

# ============================================================================== 
# Precursory information
# ==============================================================================

# Deployment Instructions

  #   Step 1: Install any required packages
  #   Step 2: Set wd() to source file location
  #   setwd("C:/Data/Deployment")
  #   Step 3: Check adjustable conditions are set-up correctly
  #   Step 4: 'Source' this code file

# ============================================================================== 
# Set adjustable simulation conditions
# ==============================================================================

  # SD of Gaussian Distribution used to create 'noisy' values from true + error
  noise             <- 20                   
  
  # SD of Truncated Gaussian Distribution used to derive starting values
  # spread across the entire bounds
  k_noise           <- 3
  T_noise           <- 15
  
  # Vector of days
  s1                <- 1:100        # Tuning block
  s2                <- 1:100        # Validation block
  s                 <- 1:200        # Both Blocks
  
  # Parameter indicating stopping criteria for DE
  precision         <- noise^(1.5)
  
  # Bounds on the parameter space for the grid search and fitting methods
  lower_bound       <- c(0.01, 2,  0.01, 2)
  upper_bound       <- c(10,   40, 10,   40)
  
  # For methods that require starting values, we iteratively fit to following n
  num_starting_sets <- c(1, 5, 10, 25, 50, 100, 250, 1000, 10000)
  
  # Measurement frequency corresponding to every X days where X is as follows
  measurement_freqs <- c(1,2,3,5)
  
  # Number of levels for each parameter for grid search
  grid_levels       <- 114                # Total sets = grid_levels^4
  
  # Bias towards fixed notation in output (i.e. decimals)
  options(scipen = 999)
  
  # Simple check to make sure grid is split into equal intervals
  if ((grid_levels)^4 %% 6 != 0){
    print("Number of grid levels not equally divisible into 6 grids")
    print("Select a different grid_levels or alter the division of the grid")
    print("Program Stopping")
    stop()
  }
  
# ============================================================================== 
# Load Required Packages
# ==============================================================================

  library(optimx)               # L-BFGS-B
  library(cmaes)                # CMA-ES
  library(DEoptim)              # DE
  library(truncnorm)            # Constructing starting values

  # Parallel Backend
  library(foreach)
  library(doParallel)
  library(parallel)
  library(snow)
  
  # Analysis & Plotting
  library(scatterplot3d)
  library(lattice)
  library(latticeExtra)
  library(cluster)
  library(factoextra)
  
# ============================================================================== 
# Source simulation /functions (fitting, computation, analysis)
# ==============================================================================

  source("functions/ffm4_computation.R")        # Model computation & Forecast
  source("functions/ffm4_cost.R")               # RSS and MSE Cost Functions
  source("functions/ffm4_optimx_parallel.R")    # L-BFGS-B Implementation
  source("functions/ffm4_deoptim.R")            # DE Implementation
  source("functions/ffm4_cmaes_parallel.R")     # CMA-ES Implementation
  source("functions/ffm4_grid_search.R")        # Custom Grid Search
  source("functions/ffm4_numerical_analysis.R") # R^2 and RMSE Functions
  
# ============================================================================== 
# Import and Set Up Primary Output Directory
# ==============================================================================

  cases <- read.csv("input/cases.csv")
  loads <- read.csv("input/training_loads.csv")
  dir.create(path = "out")
  
# ============================================================================== 
# Main Simulation Body
# ==============================================================================

  # Begin Console Logging
  sink(file = paste0("out/console_output.log"), append=TRUE, split=TRUE)

# ------------------------------------------------------------------------------
# Outer Loop (12 Cases - 4 Unique Patterns with 3 Different Rates of Change)
# ------------------------------------------------------------------------------

  case <- 1
  while (case < 13){
  
  # ----------------------------------------------------------------------------
  # Create case specific output directory
  # ----------------------------------------------------------------------------

    # Create Output Directory For Case
    dir.create(path = paste0("out/case_",case))

  # ----------------------------------------------------------------------------
  # Inner Loop 1: Measurement Frequency (Every 1,2,3 or 5 days)
  # ----------------------------------------------------------------------------
    
    # Initialise counter
    inner_loop_1 <- 1
    
    while (inner_loop_1 < length(measurement_freqs) + 1){
      
      # Set up measurement frequency condition
      freq_index <- measurement_freqs[inner_loop_1]
      print(paste0("Inner Loop 1 Start: Freq = ",freq_index))
      
      # Create output subdirectories for the frequency condition
      source("scripts/inner_loop_1_directories.R", print.eval = TRUE)
      
      # Extract (TL values, True Pars, p(0)), generate (true, noisy perf) and 
      # save iteration data
      source("scripts/iteration_data.R", print.eval = TRUE)
      
      # Plot case specific TL series and performance values
      source("scripts/plot_case_data.R", print.eval = TRUE)
      
      # Run Grid Search
      grid_start_time <- Sys.time()   # Start Timer
      grid_search <- grid_fit_mse()   # Calls Exhaustive Procedure
      
      # Analyse Grid Search
      source("scripts/grid_search_analysis.R", print.eval = TRUE)

      # Model Fitting via Differential Evolution (DE)
      de_runtime_start <- Sys.time()  # Start Timer
      de_optimisation <- de_fit_mse() # Calls Fitting Procedure
      de_runtime_end <- Sys.time()    # End Timer
      
      # DE Analysis
      source("scripts/de_analysis.R", print.eval = TRUE)

      # ------------------------------------------------------------------------
      # Inner Loop 2 (Number of Starting Parameter Sets)
      # ------------------------------------------------------------------------
      
      # Initialise counter
      inner_loop_2 <- 1
      
      while (inner_loop_2 < length(num_starting_sets) + 1){
      
        # Set up starting set condition
          n_starting_sets <- num_starting_sets[inner_loop_2]
          print(paste0("Inner Loop 2 Start: N-Starting Sets = ",n_starting_sets))
        
        # Create output subdirectories for the starting set condition
          source("scripts/inner_loop_2_directories.R", print.eval = TRUE)
        
        # Starting Sets Method 1: Truncated Normal Distribution
        # ----------------------------------------------------------------------
          
          # Source Starting Values
          method_counter <- 1
          source("scripts/truncated_sets.R", print.eval = TRUE)
          
          # L-BFGS-B Fitting and Analysis
          print(paste0("Fitting Parameters::Truncated Normal Dist::L-BFSG-B"))
          source("scripts/call_lbfgsb.R", print.eval = TRUE)
          source("scripts/lbfgsb_analysis.R", print.eval = TRUE)
          
          # CMA-ES Fitting and Analysis
          print(paste0("Fitting Parameters::Truncated Normal Dist::CMA-ES"))
          source("scripts/call_cmaes.R", print.eval = TRUE)
          source("scripts/cmaes_analysis.R", print.eval = TRUE)
          
          # Clean Up Starting Values Ready for Next Starting Set Method
          rm(par_start, par_start_k1, par_start_k2, par_start_T1, par_start_T2)
          gc()
          
        # Starting Sets Method 2: 'Good' Solutions from the Grid Search
        # ----------------------------------------------------------------------

          # Source Starting Values
          method_counter <- 2
          source("scripts/good_sets.R", print.eval = TRUE)
          
          # L-BFGS-B Fitting and Analysis
          print(paste0("Fitting Parameters::Good Dist::L-BFSG-B"))
          source("scripts/call_lbfgsb.R", print.eval = TRUE)
          source("scripts/lbfgsb_analysis.R", print.eval = TRUE)
          
          # CMA-ES Fitting and Analysis
          print(paste0("Fitting Parameters::Good Dist::CMA-ES"))
          source("scripts/call_cmaes.R", print.eval = TRUE)
          source("scripts/cmaes_analysis.R", print.eval = TRUE)
          
          # Clean Up Starting Values Ready for Next Starting Set Method
          rm(par_start)
          gc()
        
        # Starting Sets Method 3: 'Bad' Solutions from the Grid Search
        # ----------------------------------------------------------------------
          
          # Source Starting Values
          method_counter <- 3
          source("scripts/bad_sets.R", print.eval = TRUE)
          
          # L-BFGS-B Fitting and Analysis
          print(paste0("Fitting Parameters::Bad Dist::L-BFGS-B"))
          source("scripts/call_lbfgsb.R", print.eval = TRUE)
          source("scripts/lbfgsb_analysis.R", print.eval = TRUE)
          
          # CMA-ES Fitting and Analysis
          print(paste0("Fitting Parameters::Bad Dist::CMA-ES"))
          source("scripts/call_cmaes.R", print.eval = TRUE)
          source("scripts/cmaes_analysis.R", print.eval = TRUE)
          
          # Clean Up Starting Values Ready for Next Starting Set Method
          rm(par_start)
          gc()
          
        # Starting Sets Method 4: Edges of the bounds
        # ----------------------------------------------------------------------  
        
          # Source Starting Values
          method_counter <- 4 
          source("scripts/edge_sets.R", print.eval = TRUE)
          
          # L-BFGS-B Fitting and Analysis
          print(paste0("Fitting Parameters::Edge Dist::L-BFGS-B"))
          source("scripts/call_lbfgsb.R", print.eval = TRUE)
          source("scripts/lbfgsb_analysis.R", print.eval = TRUE)
          
          # CMA-ES Fitting and Analysis
          print(paste0("Fitting Parameters::Edge Dist::CMA-ES"))
          source("scripts/call_cmaes.R", print.eval = TRUE)
          source("scripts/cmaes_analysis.R", print.eval = TRUE)
          
          # Clean Up Starting Values Ready for Next Starting Set Method
          rm(par_start, par_start_k1, par_start_k2, par_start_T1, par_start_T2)
          gc()
      
        # Starting Sets Method 5: Middle of the bounds
        # ----------------------------------------------------------------------
          
          method_counter <- 5
          # Source Starting Values
          source("scripts/middle_sets.R", print.eval = TRUE)
          # TODO (Set Up Starting Values)
          
          # L-BFGS-B Fitting and Analysis
          print(paste0("Fitting Parameters::Middle Dist::L-BFGS-B"))
          source("scripts/call_lbfgsb.R", print.eval = TRUE)
          source("scripts/lbfgsb_analysis.R", print.eval = TRUE)
          
          # CMA-ES Fitting and Analysis
          print(paste0("Fitting Parameters::Middle Dist::CMA-ES"))
          source("scripts/call_cmaes.R", print.eval = TRUE)
          source("scripts/cmaes_analysis.R", print.eval = TRUE)
          
          # Clean Up Starting Values Ready for Next Starting Set Method
          rm(par_start, par_start_k1, par_start_k2, par_start_T1, par_start_T2)
          gc()
        
      # ------------------------------------------------------------------------
      # Iterate Inner Loop 2 (Number of Starting Sets)
      # ------------------------------------------------------------------------
      
      print("Updating Inner Loop 2::Number of Starting Sets")
      # Remove inner loop 2 objects
      rm(n_starting_sets, numCores, cl)
      gc()
              
      inner_loop_2 <- inner_loop_2 + 1
      } # Closing bracket of inner loop 2
      
  # ------------------------------------------------------------------------------
  # Iterate Inner Loop 1 (Measurement Frequency)
  # ------------------------------------------------------------------------------
    
    # Remove inner loop 1 objects  
    rm(p_noise_all, p_noise_a, measure_index, p_true_a, p_true_b, p_true_alt_a, 
       p_true_alt_b, p_true_all, p_true_alt, par_true, p0, ws_alt_all, ws_alt, 
       ws1, ws2, ws_all, freq_index, grid_best_50k, grid_worst_50k)
    gc()
      
    print("Updating Inner Loop 1::Measurement Frequency Loop")
    inner_loop_1 <- inner_loop_1 + 1
    } # Closing bracket of inner loop 1
    
# ------------------------------------------------------------------------------
# Iterate Outer Loop
# ------------------------------------------------------------------------------
  
  # Clear Memory Dumps
  gc()
      
  case <- case + 1
  } # Closing bracket of outer loop

sink() # Stop Logging Console