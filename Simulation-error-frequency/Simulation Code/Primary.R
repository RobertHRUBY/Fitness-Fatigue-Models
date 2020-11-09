# ===================================(v3)=======================================
 
# Fitness-Fatigue Model Simulation Experiment (Measure error and test frequency)
#
# Authors       Ben Stephens Hemingway
# Email         b.stephens-hemingway[at]rgu.[ac.uk] (Correspondence)
# GitHub        @bsh2
# Institution   Robert Gordon University, Aberdeen
# Version       3
# Built Using   R v.3.4.4
# License       MIT
# Development   June 2017-Present (Ongoing)

# ==============================================================================

# ------------------------------------------------------------------------------
# Begin Program
# ------------------------------------------------------------------------------
  print("Initialising Fitness Fatigue Model Simulation...")

  mainStart <- Sys.time()    # Record experiment start time
  options(scipen = 999)      # Heavy bias toward output in fixed notation

# ------------------------------------------------------------------------------
# Load required packages
# ------------------------------------------------------------------------------
  
  print("Loading Packages...")
  
  library(stats)
  library(ggplot2)
  library(ggthemes)
  library(optimx)
  library(reshape2)
  library(openxlsx)
  library(tibble)
  library(car)
  library(parallel)

# ------------------------------------------------------------------------------
# Load custom functions
# ------------------------------------------------------------------------------
  
  print("Loading Custom Functions...")
  
  source("R/Fun/multiplot.R")           # Grid-like plot function
  source("R/Fun/ffm_functions.R")       # FFM related functions
  source("R/Fun/summary_statistics.R")  # Functions for calculating summary stat

# ------------------------------------------------------------------------------
# Build output directories
# ------------------------------------------------------------------------------
  
  print("Building Output Directories...")
  source("R/Scripts/directories.R")   # Build simulation specific Directories

# ------------------------------------------------------------------------------
# Set universal experiment conditions and variables
# ------------------------------------------------------------------------------
  
  nSims <- 10000                 # Number of simulations per scenario
  s1 <- as.numeric(1:112)       # Vector of days over 16 weeks
  s2 <- as.numeric(1:56)        # Vector of days over 8 weeks
  print(paste0("Number of Simulations Assigned Per Scenario: ", nSims))
  cat(nSims, file = "Out/number_iterations.txt")
  
# ------------------------------------------------------------------------------
# Load TRIMP values for all 6 athlete-TRIMP groupings
# ------------------------------------------------------------------------------
  
  print("Loading TRIMP data...")
  trimps_1_int <- readRDS("Data/Trimps/trimps_1_int.Rda")
  trimps_2_int <- readRDS("Data/Trimps/trimps_2_int.Rda")
  trimps_1_adv <- readRDS("Data/Trimps/trimps_1_adv.Rda")
  trimps_2_adv <- readRDS("Data/Trimps/trimps_2_adv.Rda")

# ------------------------------------------------------------------------------
# Plot TRIMP distributions and save to output directory
# ------------------------------------------------------------------------------
  
  source("R/Scripts/plot_trimps.R")

# ------------------------------------------------------------------------------
# Set athlete-specific Parameters
# ------------------------------------------------------------------------------
  
# Intermediate Athlete
  ath_int <- c(4500, 0.501, 18, 1.002, 5)       # p(0), k1, T1, k2, T2
  optim_int <- c(0.501, 18, 1.002, 5)           # k1, T1, k2, T2
# Advanced Athlete
  ath_adv <- c(5250, 4.501, 8, 5.002, 7)        # p(0), k1, T1, k2, T2
  optim_adv <- c(4.501, 8, 5.002, 7)            # k1, T1, k2, T2

# ------------------------------------------------------------------------------
# Compute True Performance for both athletes & TRIMPS-1/2 over the 16 weeks
# ------------------------------------------------------------------------------
# NB: ffm.compute arguments (p0, s, k1, k2, tau1, tau2, wt)
  
# Intermediate Athlete: TRIMPS-1
  true_int_1 <- ffm.compute(ath_int[1], s1, ath_int[2], ath_int[4], ath_int[3],
    ath_int[5], trimps_1_int)
# Intermediate Athlete: TRIMPS-2
  true_int_2 <- ffm.compute(ath_int[1], s1, ath_int[2], ath_int[4], ath_int[3],
    ath_int[5], trimps_2_int)
# Advanced Athlete: TRIMPS-1
  true_adv_1 <- ffm.compute(ath_adv[1], s1, ath_adv[2], ath_adv[4], ath_adv[3],
    ath_adv[5], trimps_1_adv)
# Advanced Athlete: TRIMPS-2
  true_adv_2 <- ffm.compute(ath_adv[1], s1, ath_adv[2], ath_adv[4], ath_adv[3],
    ath_adv[5], trimps_2_adv)

# ------------------------------------------------------------------------------
# Plot True Performance for both athletes & TRIMPS-1/2 over the 16 weeks
# ------------------------------------------------------------------------------

  source("R/Scripts/plot_true.R")
  saveRDS(true_int_1, "Out/True/int_perf_T1.Rda")
  saveRDS(true_int_2, "Out/True/int_perf_T2.Rda")
  saveRDS(true_adv_1, "Out/True/adv_perf_T1.Rda")
  saveRDS(true_adv_2, "Out/True/adv_perf_T2.Rda")

# ------------------------------------------------------------------------------
# Generate measurement frequency and measurement error vectors
# ------------------------------------------------------------------------------
  
  # Reference Table
  #                                           Error Quantity
  #     -----------  --------------------  --------------------
  #     Error State   Scenarios            INT (W)      ADV (W)
  #     -----------  --------------------  -------     --------
  #     2% CV        1,6,11,16,21,26,31     90W          105W
  #     4% CV        2,7,12,17,22,27,32     180W         210W  
  #     6% CV        3,8,13,18,23,28,33     270W         315W
  #     8% CV        4,9,14,19,24,29,34     360W         420W
  #     10% CV       5,10,15,20,25,30,35    450W         525W
  
  print("Preparing for lift off...")
  
  frequencies <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5), 
   rep(7,5))
  errors_int <- c(rep(c(90,180,270,360,450), 7))
  errors_adv <- c(rep(c(105,210,315,420,525), 7))

# ------------------------------------------------------------------------------
# Run the simulations
# ------------------------------------------------------------------------------
  
  print("Loading simulations...")
  source('R/Scripts/run_simulations.R')

# ------------------------------------------------------------------------------
# Print Final Message
# ------------------------------------------------------------------------------

  mainEnd <- Sys.time()       # Record experiment end time
  total_time <- mainEnd - mainStart
  print("Mission Complete: Simulations Complete")
  print(paste0("Deployment Duration: ", total_time))
  cat(total_time, file = "Out/runtime.txt")
