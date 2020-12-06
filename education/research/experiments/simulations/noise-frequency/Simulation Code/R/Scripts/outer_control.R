# ===================================(3.4.1)====================================
# Script: Outer simulation loop control statements
# ==============================================================================

if (athlete_index == 1){
  # Intermediate Athlete | TRIMPS-1
  errors <- errors_int
  optim_start <- optim_int
  true_block2 <- true_int_1[57:112] # True Perf, TRIMPS-1 (Block 2)
  p0 <- ath_int[1]
  wt_b1 <- trimps_1_int[1:56]
  wt_b2 <- trimps_1_int[57:112]
  print("---------------------------------------------------------")
  print("Beginning Intermediate | TRIMPS-1 Simulations...")
  print("---------------------------------------------------------")
}

if (athlete_index == 2){
  # Intermediate Athlete | TRIMPS-2
  errors <- errors_int
  optim_start <- optim_int
  true_block2 <- true_int_2[57:112] # True Perf, TRIMPS-2 (Block 2)
  p0 <- ath_int[1]
  wt_b1 <- trimps_2_int[1:56]
  wt_b2 <- trimps_2_int[57:112]
  print("---------------------------------------------------------")
  print("Beginning Intermediate | TRIMPS-2 Simulations...")
  print("---------------------------------------------------------")
}

if (athlete_index == 3){
  # Advanced Athlete | TRIMPS-1
  errors <- errors_adv
  optim_start <- optim_adv
  true_block2 <- true_adv_1[57:112]
  p0 <- ath_adv[1]
  wt_b1 <- trimps_1_adv[1:56]
  wt_b2 <- trimps_1_adv[57:112]
  print("---------------------------------------------------------")
  print("Beginning Advanced | TRIMPS-1 Simulations...")
  print("---------------------------------------------------------")
}

if (athlete_index == 4){
  # Advanced Athlete | TRIMPS-2
  errors <- errors_adv
  optim_start <- optim_adv
  true_block2 <- true_adv_2[57:112]
  p0 <- ath_adv[1]
  wt_b1 <- trimps_2_adv[1:56]
  wt_b2 <- trimps_2_adv[57:112]
  print("---------------------------------------------------------")
  print("Beginning Advanced | TRIMPS-2 Simulations...")
  print("---------------------------------------------------------")
}

if (athlete_index == 5){
  # Advanced Athlete with Intermediate Error | TRIMPS-1
  errors <- errors_int
  optim_start <- optim_adv
  true_block2 <- true_adv_1[57:112]
  p0 <- ath_adv[1]
  wt_b1 <- trimps_1_adv[1:56]
  wt_b2 <- trimps_1_adv[57:112]
  print("---------------------------------------------------------")
  print("Beginning Advanced (Int Error) | TRIMPS-1 Simulations...")
  print("---------------------------------------------------------")
}

if (athlete_index == 6){
  # Advanced Athlete with Intermediate Error | TRIMPS-2
  errors <- errors_int
  optim_start <- optim_adv
  true_block2 <- true_adv_2[57:112]
  p0 <- ath_adv[1]
  wt_b1 <- trimps_2_adv[1:56]
  wt_b2 <- trimps_2_adv[57:112]
  print("---------------------------------------------------------")
  print("Beginning Advanced (Int Error) | TRIMPS-2 Simulations...")
  print("---------------------------------------------------------")
}