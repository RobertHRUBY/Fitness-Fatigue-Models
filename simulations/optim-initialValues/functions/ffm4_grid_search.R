# Code file : ffm4_gridsearch.R
# Multicore (Parallel Core) Grid Search Function

# Grid Search
grid_fit_mse <- function(...){
  # Set up the evaluation grid
  par1 <- seq(lower_bound[1], upper_bound[1], length.out = grid_levels)
  par2<- seq(lower_bound[2], upper_bound[2], length.out = grid_levels)
  par3 <- seq(lower_bound[3], upper_bound[3], length.out = grid_levels)
  par4 <- seq(lower_bound[4], upper_bound[4], length.out = grid_levels)
  grid <- expand.grid(par1,par2,par3,par4)
  print("Primary GRID established")
  print(paste0("4 Parameters: ",grid_levels^4," Evaluations"))
  print("Dividing grid into 6 segments to conserve memory")
  rm(par1,par2,par3,par4)
  gc()
  
  # Split Grid into Sub-Sections
  
  division <- length(grid[,1])/6
  
  grid_1 <- grid[1:division,]
  saveRDS(grid_1, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_1.Rda"))
  print("Grid 1 Generated and saved")
  rm(grid_1)
  gc()
  
  grid_2 <- grid[(division+1):(division*2),]
  saveRDS(grid_2, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_2.Rda"))
  print("Grid 2 Generated and saved")
  rm(grid_2)
  gc()
  
  grid_3 <- grid[((division*2)+1):(division*3),]
  saveRDS(grid_3, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_3.Rda"))
  print("Grid 3 Generated and saved")
  rm(grid_3)
  gc()
  
  grid_4 <- grid[((division*3)+1):(division*4),]
  saveRDS(grid_4, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_4.Rda"))
  print("Grid 4 Generated and saved")
  rm(grid_4)
  gc()
  
  grid_5 <- grid[((division*4)+1):(division*5),]
  saveRDS(grid_5, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_5.Rda"))
  print("Grid 5 Generated and saved")
  rm(grid_5)
  gc()
  
  grid_6 <- grid[((division*5)+1):(division*6),]
  saveRDS(grid_6, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_6.Rda"))
  print("Grid 6 Generated and saved")
  rm(grid_6,grid)
  gc()
  
  # EVALUATE GRID 1
  print("Evaluating GRID 1")
  grid_1 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_1.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_1[,5] <- parApply(cl, grid_1, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_1, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_1.Rda"))
  # Collect Garbage
  rm(grid_1)
  gc()
  print("GRID 1/6 EVALUATED")
  
  # EVALUATE GRID 2
  print("Evaluating GRID 2")
  grid_2 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_2.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_2[,5] <- parApply(cl, grid_2, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_2, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_2.Rda"))
  # Collect Garbage
  rm(grid_2)
  gc()
  print("GRID 2/6 EVALUATED")
  
  # EVALUATE GRID 3
  print("Evaluating GRID 3")
  grid_3 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_3.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_3[,5] <- parApply(cl, grid_3, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_3, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_3.Rda"))
  # Collect Garbage
  rm(grid_3)
  gc()
  print("GRID 3/6 EVALUATED")
  
  # EVALUATE GRID 4
  print("Evaluating GRID 4")
  grid_4 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_4.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_4[,5] <- parApply(cl, grid_4, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_4, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_4.Rda"))
  # Collect Garbage
  rm(grid_4)
  gc()
  print("GRID 4/6 EVALUATED")
  
  # EVALUATE GRID 5
  print("Evaluating GRID 5")
  grid_5 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_5.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_5[,5] <- parApply(cl, grid_5, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_5, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_5.Rda"))
  # Collect Garbage
  rm(grid_5)
  gc()
  print("GRID 5/6 EVALUATED")
  
  # EVALUATE GRID 6
  print("Evaluating GRID 6")
  grid_6 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_6.Rda"))
  cl = makeCluster(c(rep("localhost",detectCores(logical = FALSE))),
                   type = "SOCK")
  clusterExport(cl, c("p_noise_a","s1","ws1","measure_index","p0"))
  # Evaluate Grid
  grid_6[,5] <- parApply(cl, grid_6, 1, ffm4_mse)
  # Stop Cluster and Clean Up Garbage
  stopCluster(cl)
  # Save Grid out
  saveRDS(grid_6, file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_6.Rda"))
  # Collect Garbage
  gc()
  print("GRID 6/6 EVALUATED: ALL GRIDS EVALUATED")
  
  # Compile results across grids
  print("Importing Grids from Storage to Combine")
  grid_1 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_1.Rda"))
  grid_2 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_2.Rda")) 
  grid_3 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_3.Rda"))
  grid_4 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_4.Rda"))
  grid_5 <- readRDS(file = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/raw/grid_5.Rda"))
  
  print("All grids imported, combining rows")
  grid <- rbind(grid_1,grid_2,grid_3,grid_4,grid_5,grid_6)
  rm(grid_1,grid_2,grid_3,grid_4,grid_5,grid_6)
  gc()
  
  colnames(grid) <- c("k1","T1","k2","T2","fn_val")
  print("Returning the grid")
  return(grid)
}

# Computation for use in parApply function
ffm4_apply <- function(pars,p0,s,ws){
  p <- numeric(length(s))
  df0 <- data.frame(s,ws)
  for (n in 1:length(s)){
    df1 <- df0[1:s[n], ]
    p[n] <- p0 + 
      pars[1] * sum( df1$ws * exp(- (n - df1$s) / pars[2]) ) -
      pars[3] * sum( df1$ws * exp(- (n - df1$s) / pars[4]) )
  }
  return(p)
}