cmaes_start_time <- Sys.time()
numCores <- detectCores(logical = FALSE)
cl <- makeCluster(numCores)
registerDoParallel(cl)
cmaes_optimisation <- foreach(i = 1:length(par_start[,1]),
                              .packages = "cmaes") %dopar% {
                                                    call_cmaes_fit(i)
                                                  }
stopCluster(cl)
cmaes_end_time <- Sys.time()