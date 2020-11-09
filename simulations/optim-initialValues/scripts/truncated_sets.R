# Generate required number of starting sets for Truncated Normal Method

        # (Drawn pseudorandomly from a Truncated Normal Distribution)
  
          # Note: We set the mean of each truncated normal distribution to the 
          #       middle point between the parameter bounds (i.e. the middle of 
          #       viable parameter range) which means that parameters towards
          #       the edge of the bounds are weighted less in random selection
        
          set.seed(101)
          par_start_k1 <-rtruncnorm(n = n_starting_sets, a = lower_bound[1], 
                                    b = upper_bound[1], mean = 5, sd = k_noise)
          set.seed(202)
          par_start_T1 <-rtruncnorm(n = n_starting_sets, a = lower_bound[2], 
                                    b = upper_bound[2], mean = 19, sd = T_noise)
          set.seed(303)
          par_start_k2 <-rtruncnorm(n = n_starting_sets, a = lower_bound[3], 
                                    b = upper_bound[3], mean = 5, sd = k_noise)
          set.seed(404)
          par_start_T2 <-rtruncnorm(n = n_starting_sets, a = lower_bound[4], 
                                    b = upper_bound[4], mean = 19, sd = T_noise)
        
        # Filter Starting Sets such that k2>k1 and T1>T2

          # To do this we simply check
          #    if k1 > k2, then k1<=>k2 (swap)
          #    if T1 < T2, then T1<=>T2
    
          k1_temp <- c(length = n_starting_sets)
          k2_temp <- c(length = n_starting_sets)
          T1_temp <- c(length = n_starting_sets)
          T2_temp <- c(length = n_starting_sets)
          for (i in 1:n_starting_sets){
            if (par_start_k1[i] > par_start_k2[i]){
              k1_temp[i] <- par_start_k2[i]
              k2_temp[i] <- par_start_k1[i]
            } else {
              k1_temp[i] <- par_start_k1[i]
              k2_temp[i] <- par_start_k2[i]
            }
            if (par_start_T1[i] < par_start_T2[i]){
              T1_temp[i] <- par_start_T2[i]
              T2_temp[i] <- par_start_T1[i]
            } else {
              T1_temp[i] <- par_start_T1[i]
              T2_temp[i] <- par_start_T2[i]
            }
          }
          par_start_k1 <- unname(k1_temp)
          par_start_k2 <- unname(k2_temp)
          par_start_T1 <- unname(T1_temp)
          par_start_T2 <- unname(T2_temp)
          rm(k1_temp,k2_temp,T1_temp,T2_temp)
        
        # Save starting set values
          par_start <- cbind(par_start_k1, 
                             par_start_T1, 
                             par_start_k2, 
                             par_start_T2)
          
          write.csv(par_start, file = paste0("out/case_",case,"/freq_",
                                            freq_index,"/opt/",n_starting_sets,
                                            "_sets/init/norm/par_start.csv"))
                
        # Plot starting set values
if (n_starting_sets > 1){
                                                        # Histogram [Multi-Plot]
          pdf(paste0("out/case_",case,"/freq_",
                     freq_index,"/opt/",n_starting_sets,
                     "_sets/init/norm/histograms.pdf"), 
              width = 10, height = 8)
          par(mfrow=c(2,2))
          hist(par_start[ ,1], main = expression('k'[1] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,3], main = expression('k'[2] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,2], main = expression(tau[1] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,4], main = expression(tau[2] * ' parameter'), 
               xlab = "Starting Values")
          dev.off()
          
          setEPS(height = 8, width = 10)
          postscript(paste0("out/case_",case,"/freq_",
                     freq_index,"/opt/",n_starting_sets,
                     "_sets/init/norm/histograms.eps"))
          par(mfrow=c(2,2))
          hist(par_start[ ,1], main = expression('k'[1] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,3], main = expression('k'[2] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,2], main = expression(tau[1] * ' parameter'), 
               xlab = "Starting Values")
          hist(par_start[ ,4], main = expression(tau[2] * ' parameter'), 
               xlab = "Starting Values")
          dev.off()
          
                                                            # Boxplot Multi-Plot
          pdf(paste0("out/case_",case,"/freq_",
                     freq_index,"/opt/",n_starting_sets,
                     "_sets/init/norm/boxplots.pdf"), 
              width = 10, height = 8)
          par(mfrow=c(2,2))
          boxplot(par_start[ ,1], main = expression('k'[1] * ' parameter'))
          boxplot(par_start[ ,3], main = expression('k'[2] * ' parameter'))
          boxplot(par_start[ ,2], main = expression(tau[1] * ' parameter'))
          boxplot(par_start[ ,4], main = expression(tau[2] * ' parameter'))
          dev.off()
          
          setEPS(height = 8, width = 10)
          postscript(paste0("out/case_",case,"/freq_",
                     freq_index,"/opt/",n_starting_sets,
                     "_sets/init/norm/boxplots.eps"))
          par(mfrow=c(2,2))
          boxplot(par_start[ ,1], main = expression('k'[1] * ' parameter'))
          boxplot(par_start[ ,3], main = expression('k'[2] * ' parameter'))
          boxplot(par_start[ ,2], main = expression(tau[1] * ' parameter'))
          boxplot(par_start[ ,4], main = expression(tau[2] * ' parameter'))
          dev.off()
  
}