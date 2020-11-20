# Starting Parameters become equivalent n from best values of grid
grid_worst_50k <- read.csv("input/worst_50k_sets.csv")
par_start <- grid_worst_50k[2:n_starting_sets,]

write.csv(par_start, 
          file = paste0("out/case_",case,"/freq_",
                        freq_index,"/opt/",n_starting_sets,
                        "_sets/init/bad/par_start.csv"))
par_start <- par_start[,2:5]
rownames(par_start) <- c()