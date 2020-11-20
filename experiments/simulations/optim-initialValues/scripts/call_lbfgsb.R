lbfgsb_start_time <- Sys.time()
numCores <- detectCores(logical = FALSE)
cl <- makeCluster(numCores)
registerDoParallel(cl)
lbfgsb_optimisation <- foreach(i = 1:length(par_start[,1]),
                            .combine = "rbind", .packages = "optimx") %dopar% {
                                            call_lbfgsb_fit(i)
                              }
stopCluster(cl)
lbfgsb_end_time <- Sys.time()