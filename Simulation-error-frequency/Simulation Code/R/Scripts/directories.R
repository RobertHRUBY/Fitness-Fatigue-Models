# ===================================(3.4.1)====================================
# Script: Generate required output directories
# ==============================================================================

  dir.create(path = "Out")            # Build primary output directory
  dir.create("Out/TRIMPS")            # TRIMP Plots
  dir.create("Out/True")              # True performance plots and values
  
# Simulation Output for Intermediate Athlete
  
  dir.create("Out/Int")      
  dir.create("Out/Int/T1")
  dir.create("Out/Int/T2")
  
# Simulation Output for Advanced Athlete
  
  dir.create("Out/Adv")
  dir.create("Out/Adv/T1")
  dir.create("Out/Adv/T2")
  
# Simulation Output for Advanced Athlete with Intermediate Errors Applied
  
  dir.create("Out/Adv_I")
  dir.create("Out/Adv_I/T1")
  dir.create("Out/Adv_I/T2")
  
