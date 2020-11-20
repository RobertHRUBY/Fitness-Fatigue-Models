# Method Label
if (method_counter == 1){
  method_label <- "norm"
}
if (method_counter == 2){
  method_label <- "good"
}
if (method_counter == 3){
  method_label <- "bad"
}
if (method_counter == 4){
  method_label <- "edge"
}
if (method_counter == 5){
  method_label <- "mid"
}

print(paste0("Analysing Parameters Found - CMA-ES Method: ", method_label))

# Compute Runtime and Save
cat(difftime(cmaes_end_time, cmaes_start_time), 
    file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,
                  "/cmaes_runtime.txt"))

# Save Object
saveRDS(cmaes_optimisation,
        file = paste0("out/case_",case,"/freq_",freq_index,
          "/opt/",n_starting_sets,"_sets/raw/",method_label,"/cmaes.Rda"))

# Reduce object to single dataframe

if (n_starting_sets == 1){
  cmaes_pars <- cmaes_optimisation[[1]]$par
  cmaes_value <- cmaes_optimisation[[1]]$value
  cmaes_output <- data.frame(rbind(cmaes_pars),cmaes_value, rbind(par_start))
  colnames(cmaes_output) <- c("k1","T1","k2","T2","fn_val",
                            "k1_start","T1_start","k2_start","T2_start")
}

if (n_starting_sets > 1){
cmaes_output <- data.frame(Reduce(rbind,cmaes_optimisation))
cmaes_output <- cbind(Reduce(rbind,cmaes_output$par),Reduce(rbind,cmaes_output$value))
cmaes_output <- as.data.frame(cmaes_output)
cmaes_output <- cbind(cmaes_output,par_start)
colnames(cmaes_output) <- c("k1","T1","k2","T2","fn_val",
                            "k1_start","T1_start","k2_start","T2_start")
}

if (n_starting_sets > 1){
  cmaes_ordered <- cmaes_output[order(cmaes_output[,5]),]
  write.csv(cmaes_output,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/cmaes_ordered.csv"))
}


# Compute Model Performance for each parameter set, both primary and alt loads

performance_found <- as.data.frame(apply(cmaes_output[,1:4,drop = F], 1, ffm4_apply, p0, s, ws_all))
alternate_performance_2 <- as.data.frame(apply(cmaes_output[,1:4,drop = F], 1, ffm4_apply, p0, s, ws_alt_all))
alternate_performance <- alternate_performance_2[101:200,]

# Save Performance Values
saveRDS(performance_found,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/perf/",method_label,"/cmaes_primary.Rda"))
saveRDS(alternate_performance_2,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/perf/",method_label,"/cmaes_alt.Rda"))

# Compute Modelling Data (R^2, RMSE, & parameter ratio's)

    # RMSE (BLock A - Model Fitted v. 'Noisy' Performances)
    cmaes_output[,10] <- apply(performance_found[1:100,,drop = F], 2, 
                        rmse_func, y = p_noise_a, n = length(measure_index),
                        t = measure_index)
    # RMSE (Block A - Model Fitted v. 'True' Performances)
    cmaes_output[,11] <- apply(performance_found[1:100,,drop = F], 2,
                        rmse_func, y = p_true_a, n = length(s1), t=s1)
    # R^2 and Adjusted R^2 (Block A - Model Fitted v. 'Noisy' Performances)
    cmaes_output[,12] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[1,]
    cmaes_output[,13] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[2,]
    # R^2 and Adjusted R^2 (Block A - Model Fitted v. 'True' Performances)
    cmaes_output[,14] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[1,]
    cmaes_output[,15] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[2,]
    # RMSE (BLock B - Model Forecast v. 'True' Performances)
    cmaes_output[,16] <- apply(performance_found[101:200,,drop = F], 2, 
                        rmse_func, y = p_true_b, n = length(s2),
                        t = s2)
    # RMSE (BLock B - Model Alt Forecast v. 'Alt True' Performances)
    cmaes_output[,17] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rmse_func, y = p_true_alt_b, n = length(s2),
                        t = s2)
    # R^2 and Adjusted R^2 (Block B - Model Forecast v. 'True' Perfs)
    cmaes_output[,18] <- apply(performance_found[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[1,]
    cmaes_output[,19] <- apply(performance_found[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[2,]
    # R^2 and Adjusted R^2 (Block B - Model Alt Forecast v. 'Alt True' Perfs)
    cmaes_output[,20] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[1,]
    cmaes_output[,21] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[2,]
    # T1/T2 Ratio
    cmaes_output[,22] <- as.numeric(cmaes_output[,2]/cmaes_output[,4])
    # K2/K1 Ratio
    cmaes_output[,23] <- as.numeric(cmaes_output[,3]/cmaes_output[,1])
    # T1>T2
    cmaes_output[,24] <- as.logical(cmaes_output[,2] > cmaes_output[,4])
    # K2>K1
    cmaes_output[,25] <- as.logical(cmaes_output[,3] > cmaes_output[,1])
    
    # RMSE
    cmaes_output[,26] <- sqrt(cmaes_output$fn_val)

    colnames(cmaes_output) <- c("k1","T1","k2","T2","fn_val",
                                   "k1_start","T1_start","k2_start","T2_start",
                                    "RMSE_A_Fitted_Noisy",
                                    "RMSE_A_Fitted_True", "RSQ_A_Fitted_Noisy",
                                    "RSQ_ADJ_A_Fitted_Noisy", "RSQ_A_Fitted_True",
                                    "RSQ_ADJ_A_FITTED_TRUE", "RMSE_B_Forecast_True",
                                    "RMSE_B_AltForecast_AltTrue",
                                    "RSQ_B_Forecast_True","RSQ_ADJ_B_Forecast_True",
                                    "RSQ_B_AltForecast_AltTrue",
                                    "RSQ_ADJ_B_AltForecast_AltTrue", "T1/T2","k2/k1",
                                    "T1 mthan T2", 
                                    "k2 mthan k1","RMSE")

# Save Parameter Set
write.csv(cmaes_output,
          file = paste0("out/case_",case,"/freq_",freq_index,
          "/opt/",n_starting_sets,"_sets/pars/",method_label,"/cmaes_params.csv"))

if (n_starting_sets > 1){
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/cmaes_mse_vs_fit.eps"))
  plot(cmaes_output$fn_val, cmaes_output$RMSE_A_Fitted_True, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (Fitted vs. True)")
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/cmaes_mse_vs_forecast.eps"))
  plot(cmaes_output$fn_val, cmaes_output$RMSE_B_Forecast_True, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (True vs. Forecast)")
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/cmaes_mse_vs_altforecast.eps"))
  plot(cmaes_output$fn_val, cmaes_output$RMSE_B_AltForecast_AltTrue, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (True Alt vs. Forecast Alt)")
  dev.off()
  
}

if (n_starting_sets > 1){
# Modelling Summary Statistics
cmaes_model_summary <- data.frame(
    "RMSE_A_Fitted_Noisy" = summary_fun(cmaes_output$RMSE_A_Fitted_Noisy),
    "RMSE_A_Fitted_True"  = summary_fun(cmaes_output$RMSE_A_Fitted_True),
    "RSQ_A_Fitted_Noisy" = summary_fun(cmaes_output$RSQ_A_Fitted_Noisy),
    "RSQ_A_Fitted_True" = summary_fun(cmaes_output$RSQ_A_Fitted_True),
    "RSQ_ADJ_A_Fitted_Noisy" = summary_fun(cmaes_output$RSQ_ADJ_A_Fitted_Noisy),
    "RSQ_ADJ_A_Fitted_True" = summary_fun(cmaes_output$RSQ_ADJ_A_FITTED_TRUE),
    "RMSE_B_Forecast_True" = summary_fun(cmaes_output$RMSE_B_Forecast_True),
    "RMSE_B_AltForecast_AltTrue" = summary_fun(cmaes_output$RMSE_B_AltForecast_AltTrue),
    "RSQ_B_Forecast_True" = summary_fun(cmaes_output$RSQ_B_Forecast_True),
    "RSQ_ADJ_B_Forecast_True" = summary_fun(cmaes_output$RSQ_ADJ_B_Forecast_True),
    "RSQ_B_AltForecast_AltTrue" = summary_fun(cmaes_output$RSQ_B_AltForecast_AltTrue),
    "RSQ_ADJ_B_AltForecast_AltTrue" = summary_fun(cmaes_output$RSQ_ADJ_B_AltForecast_AltTrue)
  )

write.csv(cmaes_model_summary,
          file = paste0("out/case_",case,"/freq_",freq_index,
                        "/opt/",n_starting_sets,"_sets/pars/",method_label,
                        "/cmaes_model_summary.csv"))
}

if (n_starting_sets > 1){
cmaes_parameter_summary <- data.frame(
    "k1" = summary_fun(cmaes_output$k1),
    "k2" = summary_fun(cmaes_output$k2),
    "T1" = summary_fun(cmaes_output$T1),
    "T2" = summary_fun(cmaes_output$T2),
    "fn_val" = summary_fun(cmaes_output$fn_val),
    "T1/T2" = summary_fun(unlist(cmaes_output$`T1/T2`)),
    "k2/k1" = summary_fun(unlist(cmaes_output$`k2/k1`))
  )

write.csv(cmaes_parameter_summary,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/cmaes_summary_stats.csv"))
}


# Parameter Correlations
if (n_starting_sets > 10){
cmaes_cors <- as.data.frame(t(cbind(
    unlist(cor.test(unique(cmaes_output[,c("k1","k2")])[,"k1"], unique(cmaes_output[,c("k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("k1","T1")])[,"k1"], unique(cmaes_output[,c("k1","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("k1","T2")])[,"k1"], unique(cmaes_output[,c("k1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("k2","T1")])[,"k2"], unique(cmaes_output[,c("k2","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("k2","T2")])[,"k2"], unique(cmaes_output[,c("k2","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("T1","T2")])[,"T1"], unique(cmaes_output[,c("T1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(cmaes_output[,c("T1","k1","k2")])[,"T1"],unique(cmaes_output[,c("T1","k1","k2")])[,"k1"]/unique(cmaes_output[,c("T1","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)],
    unlist(cor.test(unique(cmaes_output[,c("T2","k1","k2")])[,"T2"],unique(cmaes_output[,c("T2","k1","k2")])[,"k1"]/unique(cmaes_output[,c("T2","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)]
    )
  ), stringsAsFactors = FALSE)  

rownames(cmaes_cors) <- c("k1_k2","k1_T1","k1_T2","k2_T1","k2_T2",
                            "T1_T2","T1_k1/k2", "T2_k1/k2")
colnames(cmaes_cors) <- c("test_statistic_value", 
                            "degrees_of_freedom",
                            "p_value",
                            "association",
                            "null_value",
                            "alternative_hypothesis",
                            "method_of_association",
                            "names_of_data",
                            "confidence_interval_1",
                            "confidence_interval_2")

write.csv(cmaes_cors,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/cmaes_correlations.csv"))
          
# Quantiles for parameter values, cost function values, RMSE and R^2 values
  
  quantile_probs <- c(0,0.025,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.975,1)
  cmaes_quantiles <- apply(cmaes_output[,c(c(1:5),c(10:23))], 2, quantile, probs = quantile_probs)
  
  write.csv(cmaes_quantiles,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/cmaes_quantiles.csv"))  

} # End of if > 10 statement
  
print(paste0("Analysis Complete - CMA-ES | Method: ", method_label))

  
# Remove objects depending on whats been created in the script
if (n_starting_sets >= 1){
  #rm(cmaes_optimisation, cmaes_end_time, cmaes_start_time)
  rm(performance_found, alternate_performance,
     alternate_performance_2,
     min_perf,min_set,min_perf_alt, method_label, cmaes_output)
}
if (n_starting_sets > 1){
  rm(cmaes_model_summary, cmaes_parameter_summary, cmaes_ordered)
}
if (n_starting_sets > 10){
  rm(cmaes_cors,k1k2_CI,k1T1_CI,k1T2_CI,k2T1_CI,k2T2_CI,T1T2_CI,T1k1k2_CI,
     T2k1k2_CI, cmaes_quantiles, quantile_probs)
}
  
gc()