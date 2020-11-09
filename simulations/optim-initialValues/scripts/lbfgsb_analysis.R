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

print(paste0("Analysing Parameters Found - L-BFGS-B | Method: ", method_label))

# Compute Runtime and Save
cat(difftime(lbfgsb_end_time, lbfgsb_start_time), 
    file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,
                  "/lbfgsb_runtime.txt"))

# Save out Object and DataFrame
saveRDS(lbfgsb_optimisation,
        file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb.Rda"))
write.csv(lbfgsb_optimisation,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb.csv"))

if (n_starting_sets > 1){
  lbfgsb_ordered <- lbfgsb_optimisation[order(lbfgsb_optimisation[,5]),]
  write.csv(lbfgsb_ordered,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb_ordered.csv"))
}


# Compute Model Performance for each parameter set, both primary and alt loads
lbfgsb_optimisation <- cbind(lbfgsb_optimisation,par_start)
rownames(lbfgsb_optimisation) <- c(1:length(par_start[,1]))
colnames(lbfgsb_optimisation) <- c("k1","T1","k2","T2","fn_val","fevals",
                                   "gevals","niter","convcode","kkt1","kkt2",
                                   "xtime","k1_start","T1_start","k2_start",
                                   "T2_start")

performance_found <- as.data.frame(apply(lbfgsb_optimisation[,1:4,drop = F], 1, ffm4_apply, p0, s, ws_all))
alternate_performance_2 <- as.data.frame(apply(lbfgsb_optimisation[,1:4,drop = F], 1, ffm4_apply, p0, s, ws_alt_all))
alternate_performance <- alternate_performance_2[101:200,]

# Save Performance Values
saveRDS(performance_found,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/perf/",method_label,"/lbfgsb_primary.Rda"))
saveRDS(alternate_performance_2,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/perf/",method_label,"/lbfgsb_alt.Rda"))

# Compute Modelling Data (R^2, RMSE, & parameter ratio's)

# RMSE (BLock A - Model Fitted v. 'Noisy' Performances)
    lbfgsb_optimisation[,17] <- apply(performance_found[1:100,,drop = F], 2, 
                        rmse_func, y = p_noise_a, n = length(measure_index),
                        t = measure_index)
# RMSE (Block A - Model Fitted v. 'True' Performances)
    lbfgsb_optimisation[,18] <- apply(performance_found[1:100,,drop = F], 2,
                        rmse_func, y = p_true_a, n = length(s1), t=s1)
# R^2 and Adjusted R^2 (Block A - Model Fitted v. 'Noisy' Performances)
    lbfgsb_optimisation[,19] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[1,]
    lbfgsb_optimisation[,20] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[2,]
# R^2 and Adjusted R^2 (Block A - Model Fitted v. 'True' Performances)
    lbfgsb_optimisation[,21] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[1,]
    lbfgsb_optimisation[,22] <- apply(performance_found[1:100,,drop = F], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[2,]
# RMSE (BLock B - Model Forecast v. 'True' Performances)
    lbfgsb_optimisation[,23] <- apply(performance_found[101:200,,drop = F], 2, 
                        rmse_func, y = p_true_b, n = length(s2),
                        t = s2)
# RMSE (BLock B - Model Alt Forecast v. 'Alt True' Performances)
    lbfgsb_optimisation[,24] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rmse_func, y = p_true_alt_b, n = length(s2),
                        t = s2)
# R^2 and Adjusted R^2 (Block B - Model Forecast v. 'True' Perfs)
    lbfgsb_optimisation[,25] <- apply(performance_found[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[1,]
    lbfgsb_optimisation[,26] <- apply(performance_found[101:200,, drop = F], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[2,]
# R^2 and Adjusted R^2 (Block B - Model Alt Forecast v. 'Alt True' Perfs)
    lbfgsb_optimisation[,27] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[1,]
    lbfgsb_optimisation[,28] <- apply(alternate_performance_2[101:200,,drop = F], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[2,]
# T1/T2 Ratio
    lbfgsb_optimisation[,29] <- as.numeric(lbfgsb_optimisation[,2]/lbfgsb_optimisation[,4])
# K2/K1 Ratio
    lbfgsb_optimisation[,30] <- as.numeric(lbfgsb_optimisation[,3]/lbfgsb_optimisation[,1])
# T1>T2
    lbfgsb_optimisation[,31] <- as.logical(lbfgsb_optimisation[,2] > lbfgsb_optimisation[,4])
# K2>K1
    lbfgsb_optimisation[,32] <- as.logical(lbfgsb_optimisation[,3] > lbfgsb_optimisation[,1])
# RMSE
    lbfgsb_optimisation[,33] <- sqrt(lbfgsb_optimisation$fn_val)

colnames(lbfgsb_optimisation) <- c("k1","T1","k2","T2","fn_val","fevals",
                                   "gevals","niter","convcode","kkt1","kkt2",
                                   "xtime","k1_start","T1_start","k2_start",
                                   "T2_start","RMSE_A_Fitted_Noisy",
                                    "RMSE_A_Fitted_True", "RSQ_A_Fitted_Noisy",
                                    "RSQ_ADJ_A_Fitted_Noisy", "RSQ_A_Fitted_True",
                                    "RSQ_ADJ_A_FITTED_TRUE", "RMSE_B_Forecast_True",
                                    "RMSE_B_AltForecast_AltTrue",
                                    "RSQ_B_Forecast_True","RSQ_ADJ_B_Forecast_True",
                                    "RSQ_B_AltForecast_AltTrue",
                                    "RSQ_ADJ_B_AltForecast_AltTrue", "T1/T2","k2/k1",
                                    "T1 mthan T2", "k2 mthan k1", "RMSE")

# Save Parameter Set
write.csv(lbfgsb_optimisation,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/lbfgsb_params.csv"))

if (n_starting_sets > 1){
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb_mse_vs_fit.eps"))
  plot(lbfgsb_optimisation$fn_val, lbfgsb_optimisation$RMSE_A_Fitted_True, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (Fitted vs. True)")
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb_mse_vs_forecast.eps"))
  plot(lbfgsb_optimisation$fn_val, lbfgsb_optimisation$RMSE_B_Forecast_True, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (True vs. Forecast)")
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
  "/opt/",n_starting_sets,"_sets/raw/",method_label,"/lbfgsb_mse_vs_altforecast.eps"))
  plot(lbfgsb_optimisation$fn_val, lbfgsb_optimisation$RMSE_B_AltForecast_AltTrue, type = "p",
       pch = 18,
       xlab = "MSE",
       ylab = "RMSE (True Alt vs. Forecast Alt)")
  dev.off()
  
}

if (n_starting_sets > 1){
# Modelling Summary Statistics
lbfgsb_model_summary <- data.frame(
    "RMSE_A_Fitted_Noisy" = summary_fun(lbfgsb_optimisation$RMSE_A_Fitted_Noisy),
    "RMSE_A_Fitted_True"  = summary_fun(lbfgsb_optimisation$RMSE_A_Fitted_True),
    "RSQ_A_Fitted_Noisy" = summary_fun(lbfgsb_optimisation$RSQ_A_Fitted_Noisy),
    "RSQ_A_Fitted_True" = summary_fun(lbfgsb_optimisation$RSQ_A_Fitted_True),
    "RSQ_ADJ_A_Fitted_Noisy" = summary_fun(lbfgsb_optimisation$RSQ_ADJ_A_Fitted_Noisy),
    "RSQ_ADJ_A_Fitted_True" = summary_fun(lbfgsb_optimisation$RSQ_ADJ_A_FITTED_TRUE),
    "RMSE_B_Forecast_True" = summary_fun(lbfgsb_optimisation$RMSE_B_Forecast_True),
    "RMSE_B_AltForecast_AltTrue" = summary_fun(lbfgsb_optimisation$RMSE_B_AltForecast_AltTrue),
    "RSQ_B_Forecast_True" = summary_fun(lbfgsb_optimisation$RSQ_B_Forecast_True),
    "RSQ_ADJ_B_Forecast_True" = summary_fun(lbfgsb_optimisation$RSQ_ADJ_B_Forecast_True),
    "RSQ_B_AltForecast_AltTrue" = summary_fun(lbfgsb_optimisation$RSQ_B_AltForecast_AltTrue),
    "RSQ_ADJ_B_AltForecast_AltTrue" = summary_fun(lbfgsb_optimisation$RSQ_ADJ_B_AltForecast_AltTrue)
  )
write.csv(lbfgsb_model_summary,
          file = paste0("out/case_",case,"/freq_",freq_index,
                        "/opt/",n_starting_sets,"_sets/pars/",method_label,
                        "/lbfgsb_model_summary.csv"))
}

if (n_starting_sets > 1){
lbfgsb_parameter_summary <- data.frame(
    "k1" = summary_fun(lbfgsb_optimisation$k1),
    "k2" = summary_fun(lbfgsb_optimisation$k2),
    "T1" = summary_fun(lbfgsb_optimisation$T1),
    "T2" = summary_fun(lbfgsb_optimisation$T2),
    "fn_val" = summary_fun(lbfgsb_optimisation$fn_val),
    "T1/T2" = summary_fun(unlist(lbfgsb_optimisation$`T1/T2`)),
    "k2/k1" = summary_fun(unlist(lbfgsb_optimisation$`k2/k1`))
  )

write.csv(lbfgsb_parameter_summary,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/lbfgsb_summary_stats.csv"))
}

# Analyse parameter correlations (Pearson's and Scatter MultiPlot w. Regression)

if (n_starting_sets > 10){
lbfgsb_cors <- as.data.frame(t(cbind(
    unlist(cor.test(unique(lbfgsb_optimisation[,c("k1","k2")])[,"k1"], unique(lbfgsb_optimisation[,c("k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("k1","T1")])[,"k1"], unique(lbfgsb_optimisation[,c("k1","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("k1","T2")])[,"k1"], unique(lbfgsb_optimisation[,c("k1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("k2","T1")])[,"k2"], unique(lbfgsb_optimisation[,c("k2","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("k2","T2")])[,"k2"], unique(lbfgsb_optimisation[,c("k2","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("T1","T2")])[,"T1"], unique(lbfgsb_optimisation[,c("T1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(lbfgsb_optimisation[,c("T1","k1","k2")])[,"T1"],unique(lbfgsb_optimisation[,c("T1","k1","k2")])[,"k1"]/unique(lbfgsb_optimisation[,c("T1","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)],
    unlist(cor.test(unique(lbfgsb_optimisation[,c("T2","k1","k2")])[,"T2"],unique(lbfgsb_optimisation[,c("T2","k1","k2")])[,"k1"]/unique(lbfgsb_optimisation[,c("T2","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)]
    )
  ), stringsAsFactors = FALSE)          

 rownames(lbfgsb_cors) <- c("k1_k2","k1_T1","k1_T2","k2_T1","k2_T2",
                            "T1_T2","T1_k1/k2", "T2_k1/k2")
 colnames(lbfgsb_cors) <- c("test_statistic_value", 
                            "degrees_of_freedom",
                            "p_value",
                            "association",
                            "null_value",
                            "alternative_hypothesis",
                            "method_of_association",
                            "names_of_data",
                            "confidence_interval_1",
                            "confidence_interval_2")

write.csv(lbfgsb_cors,
          file = paste0("out/case_",case,"/freq_",freq_index,
                  "/opt/",n_starting_sets,"_sets/pars/",method_label,"/lbfgsb_correlations.csv"))
          
# Quantiles for parameter values, cost function values, RMSE and R^2 values
  
  quantile_probs <- c(0,0.025,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.975,1)
  lbfgsb_quantiles <- apply(lbfgsb_optimisation[,c(1,2,3,4,5,c(17:30))], 2, quantile, probs = quantile_probs)
  
  write.csv(lbfgsb_quantiles,
            file = paste0("out/case_",case,"/freq_",freq_index,
            "/opt/",n_starting_sets,"_sets/pars/",method_label,"/lbfgsb_quantiles.csv"))

} # End of if > 10 statement
  
print(paste0("Analysis Complete - L-BFGS-B | Method: ", method_label))
  
# Remove objects depending on whats been created in the script
if (n_starting_sets >= 1){
  rm(lbfgsb_optimisation, performance_found, alternate_performance,
     alternate_performance_2, lbfgsb_end_time, lbfgsb_start_time,
     min_perf,min_set,min_perf_alt, method_label)
}
if (n_starting_sets > 1){
  rm(lbfgsb_model_summary, lbfgsb_parameter_summary, lbfgsb_ordered)
}
if (n_starting_sets > 10){
  rm(lbfgsb_cors,k1k2_CI,k1T1_CI,k1T2_CI,k2T1_CI,k2T2_CI,T1T2_CI,T1k1k2_CI,
     T2k1k2_CI, lbfgsb_quantiles, quantile_probs)
}
  
gc()
