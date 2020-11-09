# Starting Parameters become equivalent n from best values of grid
          
par_start <- grid_best_50k[1:n_starting_sets,]

write.csv(par_start, 
          file = paste0("out/case_",case,"/freq_",
                        freq_index,"/opt/",n_starting_sets,
                        "_sets/init/good/par_start.csv"))
par_start <- par_start[,2:5]
rownames(par_start) <- c()