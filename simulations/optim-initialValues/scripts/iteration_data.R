# Extract case specific values for Primary & Alt TL, True Pars, and p(0)
  ws_all <- loads[ ,cases$true_load[case]]
  ws1 <- ws_all[1:100]
  ws2 <- ws_all[101:200]
  ws_alt <- loads[101:200, cases$alt_load[case]]
  ws_alt_all <- c(ws1, ws_alt)
  p0 <- cases$p0[case]
  par_true <- cases[case,13:16]
      
# Generate 'true' performance values for the case
  p_true_all <- ffm4(p0, s, par_true$k1, par_true$tau1, par_true$k2, 
                     par_true$tau2, ws_all)
  p_true_alt <- ffm4(p0, s, par_true$k1, par_true$tau1, par_true$k2, 
                     par_true$tau2, ws_alt_all)
  p_true_a   <- p_true_all[1:100]
  p_true_b   <- p_true_all[101:200]
  p_true_alt_a <- p_true_alt[1:100]
  p_true_alt_b <- p_true_alt[101:200]
        
# Generate 'noisy' block a performance values for the case
  set.seed(800)
  p_noise_a <- rnorm(s1, p_true_a, noise)

# Subset 'noisy' performance values based on measurement freq
  measure_index <- seq(1, length(s1), by = freq_index)
  p_noise_a <- p_noise_a[c(measure_index)]
  p_noise_all <- rep(NA,length(s))      
  p_noise_all[measure_index] <- p_noise_a
  
# Save Performance Values
  write.csv(data.frame("day" = s, "true_perf" = p_true_all,
                       "true_alt_perf" = p_true_alt,
                       "noisy_perf" = p_noise_all),
            file = paste0("out/case_",case,"/freq_",freq_index,
                          "/case_data/performance_values.csv"))