# Evaluate Runtime
cat(difftime(grid_end_time, grid_start_time), 
    file = paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/grid_runtime.txt"))

# Save the grid-search objects

saveRDS(grid_search, file = paste0("out/case_",case,"/freq_",freq_index,
                                   "/grid/raw/grid_all.Rda"))

# Retain the best 50,000 sets for use with additional optimisation run

grid_search <- grid_search[order(grid_search[ ,5]), ]
grid_best_50k <- grid_search[1:50000, ]
grid_worst_50k <- grid_search[(grid_levels^4 - 49999):(grid_levels^4),]
grid_worst_50k <- grid_worst_50k[order(grid_worst_50k[,5], decreasing = TRUE),]

# Evaluate quantiles on the cost_fn values across the grid

grid_quantiles <- quantile(grid_search[,5], 
                           probs = c(0.0001,0.00025, 0.0005,0.001,
                                     0.0025, 0.005, 0.01, 0.025, 0.05, 0.1,
                                     0.25, 0.5, 0.75, 0.90, 0.95, 0.975,
                                     0.990, 0.9925, 0.995, 1))

# Plot the changes in cost function values

  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_50k.eps"
  ))
  plot(
    x = 1:50000,
    y = grid_best_50k[, 5],
    type = "l",
    main = "Best 50k Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_1k.eps"
  ))
  plot(
    x = 1:1000,
    y = grid_best_50k[1:1000, 5],
    type = "l",
    main = "Best 1k Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_500.eps"
  ))
  plot(
    x = 1:500,
    y = grid_best_50k[1:500, 5],
    type = "l",
    main = "Best 500 Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_100.eps"
  ))
  plot(
    x = 1:100,
    y = grid_best_50k[1:100, 5],
    type = "l",
    main = "Best 100 Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_50.eps"
  ))
  plot(
    x = 1:50,
    y = grid_best_50k[1:50, 5],
    type = "l",
    main = "Best 50 Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()
  
  setEPS(width = 8, height = 8)
  postscript(paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/raw/best_20.eps"
  ))
  plot(
    x = 1:20,
    y = grid_best_50k[1:20, 5],
    type = "l",
    main = "Best 50 Parameter Sets",
    ylab = "Cost Function (MSE) Value",
    xlab = "Ordered Set Number"
  )
  dev.off()

# Plot the change in worst cost function values

setEPS(width = 8,height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/raw/worst_50k.eps"))
plot(x = 1:50000, y = grid_worst_50k[,5], type = "l",
     main = "Best 50k Parameter Sets", ylab = "Cost Function (MSE) Value",
     xlab = "Ordered Set Number")
dev.off()

# Remove the majority of grid-search values to save memory
rm(grid_search)
gc()

# Save the additional grid-search objects
write.csv(grid_quantiles, file = paste0("out/case_",case,"/freq_",freq_index,
                                   "/grid/raw/costfn_quantiles.csv"))
write.csv(grid_best_50k, file = paste0("out/case_",case,"/freq_",freq_index,
                                   "/grid/raw/best_50k_sets.csv"))
write.csv(grid_worst_50k, file = paste0("out/case_",case,"/freq_",freq_index,
                                   "/grid/raw/worst_50k_sets.csv"))

# Compute an arbitrary cost function value threshold
# Settling for 10% above the minimum value found
cost_fn_threshold <-
  grid_best_50k[1, 5] + ((grid_best_50k[1, 5] / 100) * 10)
cat(
  paste0(
    "Min Cost Fn Value Found: ",
    round(grid_best_50k[1, 5], 3),
    " | Threshold: ",
    round(cost_fn_threshold, 3),
    " (Retain sets above 10% Above Minimum Value)"
  ),
  file = paste0(
    "out/case_",
    case,
    "/freq_",
    freq_index,
    "/grid/threshold/threshold.txt"
  )
)

# Retain 'best fitting' sets and plot
  # Threshold: Up to 25% above the lowest cost fn value identified
grid_a <- grid_best_50k[grid_best_50k[,5] <= cost_fn_threshold, ]

# Compute model performance for each threshold set
grid_a_perf <-
  as.data.frame(apply(grid_a[, 1:4], 1, ffm4_apply, p0, s, ws_all))
grid_a_perf_alt <-
  as.data.frame(apply(grid_a[, 1:4], 1, ffm4_apply, p0, s, ws_alt_all))

# Save Performance Values
write.csv(grid_a_perf, file = paste0("out/case_",case,"/freq_",freq_index,
                                     "/grid/threshold/perf/primary.csv"))
write.csv(grid_a_perf_alt, paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/perf/alt.csv"))

# Numerical Analysis for Each Parameter Threshold Set
  
  # R^2 and RMSE (Fit and Forecast) - Primary & Alt Load Series

    # RMSE (BLock A - Model Fitted v. 'Noisy' Performances)
    grid_a[,6] <- apply(grid_a_perf[1:100,], 2, 
                        rmse_func, y = p_noise_a, n = length(measure_index),
                        t = measure_index)
    
    # RMSE (Block A - Model Fitted v. 'True' Performances)
    grid_a[,7] <- apply(grid_a_perf[1:100,], 2,
                        rmse_func, y = p_true_a, n = length(s1), t=s1)
    
    # R^2 and Adjusted R^2 (Block A - Model Fitted v. 'Noisy' Performances)
    grid_a[,8] <- apply(grid_a_perf[1:100,], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[1,]
    grid_a[,9] <- apply(grid_a_perf[1:100,], 2,
                        rsq_func, y = p_noise_a, n = length(measure_index), 
                        t = measure_index)[2,]

    # R^2 and Adjusted R^2 (Block A - Model Fitted v. 'True' Performances)
    grid_a[,10] <- apply(grid_a_perf[1:100,], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[1,]
    grid_a[,11] <- apply(grid_a_perf[1:100,], 2,
                        rsq_func, y = p_true_a, n = length(s1), 
                        t = s1)[2,]
    
    # RMSE (BLock B - Model Forecast v. 'True' Performances)
    grid_a[,12] <- apply(grid_a_perf[101:200,], 2, 
                        rmse_func, y = p_true_b, n = length(s2),
                        t = s2)
    
    # RMSE (BLock B - Model Alt Forecast v. 'Alt True' Performances)
    grid_a[,13] <- apply(grid_a_perf_alt[101:200,], 2, 
                        rmse_func, y = p_true_alt_b, n = length(s2),
                        t = s2)
    
    # R^2 and Adjusted R^2 (Block B - Model Forecast v. 'True' Perfs)
    grid_a[,14] <- apply(grid_a_perf[101:200,], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[1,]
    grid_a[,15] <- apply(grid_a_perf[101:200,], 2, 
                        rsq_func, y = p_true_b, n = length(s2),
                        t = s2)[2,]
    
    # R^2 and Adjusted R^2 (Block B - Model Alt Forecast v. 'Alt True' Perfs)
    grid_a[,16] <- apply(grid_a_perf_alt[101:200,], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[1,]
    grid_a[,17] <- apply(grid_a_perf_alt[101:200,], 2, 
                        rsq_func, y = p_true_alt_b, n = length(s2),
                        t = s2)[2,]
    
    # Ratio's k2/k1 and T1/T2
    grid_a[,18] <-as.numeric(grid_a[,2]/grid_a[,4])
    grid_a[,19] <- as.numeric(grid_a[,3]/grid_a[,1])
    
    # If T1>T2 and K2>K1
    grid_a[,20] <- as.logical(grid_a[,2]>grid_a[,4])
    grid_a[,21] <- as.logical(grid_a[,3]>grid_a[,1])
    
    # Parameter Set Column Names
    model_colnames <- c("k1","T1","k2","T2","fn_val","RMSE_A_Fitted_Noisy",
                        "RMSE_A_Fitted_True", "RSQ_A_Fitted_Noisy",
                        "RSQ_ADJ_A_Fitted_Noisy", "RSQ_A_Fitted_True",
                        "RSQ_ADJ_A_FITTED_TRUE", "RMSE_B_Forecast_True",
                        "RMSE_B_AltForecast_AltTrue",
                        "RSQ_B_Forecast_True","RSQ_ADJ_B_Forecast_True",
                        "RSQ_B_AltForecast_AltTrue",
                        "RSQ_ADJ_B_AltForecast_AltTrue", "T1/T2","k2/k1",
                        "T1 mthan T2", 
                        "k2 mthan k1")

    colnames(grid_a) <- model_colnames
    
    # Save Parameter Data for Each Threshold Set
    write.csv(grid_a, paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/pars/par_sets.csv"))
    
# Modelling Summary Data (mean,sd,median,iqr,min,max) of (R^2/RMSE)
  
  grid_a_model_summary <- data.frame(
    "RMSE_A_Fitted_Noisy" = summary_fun(grid_a$RMSE_A_Fitted_Noisy),
    "RMSE_A_Fitted_True"  = summary_fun(grid_a$RMSE_A_Fitted_True),
    "RSQ_A_Fitted_Noisy" = summary_fun(grid_a$RSQ_A_Fitted_Noisy),
    "RSQ_A_Fitted_True" = summary_fun(grid_a$RSQ_A_Fitted_True),
    "RSQ_ADJ_A_Fitted_Noisy" = summary_fun(grid_a$RSQ_ADJ_A_Fitted_Noisy),
    "RSQ_ADJ_A_Fitted_True" = summary_fun(grid_a$RSQ_ADJ_A_FITTED_TRUE),
    "RMSE_B_Forecast_True" = summary_fun(grid_a$RMSE_B_Forecast_True),
    "RMSE_B_AltForecast_AltTrue"=summary_fun(grid_a$RMSE_B_AltForecast_AltTrue),
    "RSQ_B_Forecast_True" = summary_fun(grid_a$RSQ_B_Forecast_True),
    "RSQ_ADJ_B_Forecast_True" = summary_fun(grid_a$RSQ_ADJ_B_Forecast_True),
    "RSQ_B_AltForecast_AltTrue" = summary_fun(grid_a$RSQ_B_AltForecast_AltTrue),
    "RSQ_ADJ_B_AltForecast_AltTrue" = 
      summary_fun(grid_a$RSQ_ADJ_B_AltForecast_AltTrue)
    )
  
  write.csv(grid_a_model_summary, paste0("out/case_",case,"/freq_",freq_index,
                                "/grid/threshold/stats/modelling_summary.csv"))

# Plot the change in cost function value for the threshold set
  # This plot will help after the grid search to identify whether there is a
  # specific group of parameters that can be considered 'near optimal'
  # Along with the clustering work
setEPS(width = 8,height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/threshold/pars/change_in_cost.eps"))
plot(x = 1:length(grid_a[,1]), y = grid_a[,5], type = "l",
     main = "Threshold Set", ylab = "Cost Function (MSE) Value",
     xlab = "Ordered Set Number")
dev.off()

# Plot change in numerical fit in relation to cost function value
setEPS(width = 8, height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/threshold/pars/cost_vs_fit_A.eps"))
plot(x=grid_a[,5],y=grid_a[,7], type = "l",
     main = "Model Fit vs. True (Tuning Block)", ylab = "RMSE (A.U)",
     xlab = "Cost Function Value (MSE)")
dev.off()

setEPS(width = 8, height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/threshold/pars/cost_vs_fit_B.eps"))
plot(x=grid_a[,5],y=grid_a[,12], type = "l",
     main = "Model Fit vs. True (Validation Block)", ylab = "RMSE (A.U)",
     xlab = "Cost Function Value (MSE)")
dev.off()

setEPS(width = 8, height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/threshold/pars/cost_vs_fit_B_Alt.eps"))
plot(x=grid_a[,5],y=grid_a[,13], type = "l",
     main = "Model Fit vs. True (Validation Block) - Alt TL",ylab ="RMSE (A.U)",
     xlab = "Cost Function Value (MSE)")
dev.off()

# Parameter Analysis for Each Parameter Threshold Set

  # Boxplots
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/boxplots.pdf"), width = 10, height = 8)
  par(mfrow=c(2,2))
  boxplot(grid_a$k1, xlab = expression('k'[1] * ' values'))
  boxplot(grid_a$k2, xlab = expression('k'[2] * ' values'))
  boxplot(grid_a$T1, xlab = expression(tau[1] * ' values'))
  boxplot(grid_a$T2, xlab = expression(tau[2] * ' values'))
  dev.off()
  
  setEPS(width = 10, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/boxplots.eps"))
   par(mfrow=c(2,2))
  boxplot(grid_a$k1, xlab = expression('k'[1] * ' values'))
  boxplot(grid_a$k2, xlab = expression('k'[2] * ' values'))
  boxplot(grid_a$T1, xlab = expression(tau[1] * ' values'))
  boxplot(grid_a$T2, xlab = expression(tau[2] * ' values'))
  dev.off()
  
  # Histograms
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/histograms.pdf"), width = 10, height = 8)
  par(mfrow=c(2,2))        
  hist(grid_a$k1, xlab = expression('k'[1] * ' values'), main = c())
  hist(grid_a$k2, xlab = expression('k'[2] * ' values'), main = c())
  hist(grid_a$T1, xlab = expression(tau[1] * ' values'), main = c())
  hist(grid_a$T2, xlab = expression(tau[2] * ' values'), main = c())
  dev.off()
  
  setEPS(width = 10, height = 8)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/histograms.eps"))
  par(mfrow=c(2,2))        
  hist(grid_a$k1, xlab = expression('k'[1] * ' values'), main = c())
  hist(grid_a$k2, xlab = expression('k'[2] * ' values'), main = c())
  hist(grid_a$T1, xlab = expression(tau[1] * ' values'), main = c())
  hist(grid_a$T2, xlab = expression(tau[2] * ' values'), main = c())
  dev.off()

  # Parameter Correlations and Multiplot Scatter
  
  grid_a_cors <- as.data.frame(t(cbind(
    unlist(cor.test(unique(grid_a[,c("k1","k2")])[,"k1"], unique(grid_a[,c("k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("k1","T1")])[,"k1"], unique(grid_a[,c("k1","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("k1","T2")])[,"k1"], unique(grid_a[,c("k1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("k2","T1")])[,"k2"], unique(grid_a[,c("k2","T1")])[,"T1"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("k2","T2")])[,"k2"], unique(grid_a[,c("k2","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("T1","T2")])[,"T1"], unique(grid_a[,c("T1","T2")])[,"T2"], method = "pearson"),recursive = FALSE),
    unlist(cor.test(unique(grid_a[,c("T1","k1","k2")])[,"T1"],unique(grid_a[,c("T1","k1","k2")])[,"k1"]/unique(grid_a[,c("T1","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)],
    unlist(cor.test(unique(grid_a[,c("T2","k1","k2")])[,"T2"],unique(grid_a[,c("T2","k1","k2")])[,"k1"]/unique(grid_a[,c("T2","k1","k2")])[,"k2"], method = "pearson"),recursive = FALSE)[c(1:8,10,11)]
    )
  ), stringsAsFactors = FALSE)
  
  correlation_rownames <- c("k1_k2","k1_T1","k1_T2","k2_T1","k2_T2",
                            "T1_T2","T1_k1/k2", "T2_k1/k2")
  correlation_colnames <- c("test_statistic_value", 
                            "degrees_of_freedom",
                            "p_value",
                            "association",
                            "null_value",
                            "alternative_hypothesis",
                            "method_of_association",
                            "names_of_data",
                            "confidence_interval_1",
                            "confidence_interval_2")

  rownames(grid_a_cors) <- correlation_rownames
  colnames(grid_a_cors) <- correlation_colnames
  
  write.csv(grid_a_cors, file = paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/correlations.csv"))
  
  # Multiplot Correlation Scatter with Regression Line and CI
  
    # Construct the confidence intervals required (25% threshold)
    # k1 - k2
    k1k2_CI <- predict(lm(unique(grid_a[,c("k1","k2")])[,"k2"] ~ unique(grid_a[,c("k1","k2")])[,"k1"]),
                  newdata = data.frame(x = unique(grid_a[,c("k1","k2")])[,"k1"]),
                  interval = "confidence", level = 0.99)[,]
    k1k2_CI <- as.data.frame(k1k2_CI)
    k1k2_CI$x <- as.numeric(unique(grid_a[,c("k1","k2")])[,"k1"])
    k1k2_CI <- unique(k1k2_CI)
    k1k2_CI <- k1k2_CI[order(k1k2_CI$x, decreasing = TRUE),]
    
    # k1 - T1
    k1T1_CI <- predict(lm(unique(grid_a[,c("k1","T1")])[,"T1"] ~ unique(grid_a[,c("k1","T1")])[,"k1"]),
                  newdata = data.frame(x = unique(grid_a[,c("k1","T1")])[,"k1"]),
                  interval = "confidence", level = 0.99)[,]
    k1T1_CI <- as.data.frame(k1T1_CI)
    k1T1_CI$x <- as.numeric(unique(grid_a[,c("k1","T1")])[,"k1"])
    k1T1_CI <- unique(k1T1_CI)
    k1T1_CI <- k1T1_CI[order(k1T1_CI$x, decreasing = TRUE),]
    
    # k1 - T2
    k1T2_CI <- predict(lm(unique(grid_a[,c("k1","T2")])[,"T2"] ~ unique(grid_a[,c("k1","T2")])[,"k1"]),
                  newdata = data.frame(x = unique(grid_a[,c("k1","T2")])[,"k1"]),
                  interval = "confidence", level = 0.99)[,]
    k1T2_CI <- as.data.frame(k1T2_CI)
    k1T2_CI$x <- as.numeric(unique(grid_a[,c("k1","T2")])[,"k1"])
    k1T2_CI <- unique(k1T2_CI)
    k1T2_CI <- k1T2_CI[order(k1T2_CI$x, decreasing = TRUE),]    
    
    # k2 - T1
    k2T1_CI <- predict(lm(unique(grid_a[,c("k2","T1")])[,"T1"] ~ unique(grid_a[,c("k2","T1")])[,"k2"]),
                  newdata = data.frame(x = unique(grid_a[,c("k2","T1")])[,"k2"]),
                  interval = "confidence", level = 0.99)[,]
    k2T1_CI <- as.data.frame(k2T1_CI)
    k2T1_CI$x <- as.numeric(unique(grid_a[,c("k2","T1")])[,"k2"])
    k2T1_CI <- unique(k2T1_CI)
    k2T1_CI <- k2T1_CI[order(k2T1_CI$x, decreasing = TRUE),]
    
    # k2 - T2
    k2T2_CI <- predict(lm(unique(grid_a[,c("k2","T2")])[,"T2"] ~ unique(grid_a[,c("k2","T2")])[,"k2"]),
                  newdata = data.frame(x = unique(grid_a[,c("k2","T2")])[,"k2"]),
                  interval = "confidence", level = 0.99)[,]
    k2T2_CI <- as.data.frame(k2T2_CI)
    k2T2_CI$x <- as.numeric(unique(grid_a[,c("k2","T2")])[,"k2"])
    k2T2_CI <- unique(k2T2_CI)
    k2T2_CI <- k2T2_CI[order(k2T2_CI$x, decreasing = TRUE),]
    
    # T1 - T2
    T1T2_CI <- predict(lm(unique(grid_a[,c("T1","T2")])[,"T2"] ~ unique(grid_a[,c("T1","T2")])[,"T1"]),
                  newdata = data.frame(x = unique(grid_a[,c("T1","T2")])[,"T1"]),
                  interval = "confidence", level = 0.99)[,]
    T1T2_CI <- as.data.frame(T1T2_CI)
    T1T2_CI$x <- as.numeric(unique(grid_a[,c("T1","T2")])[,"T1"])
    T1T2_CI <- unique(T1T2_CI)
    T1T2_CI <- T1T2_CI[order(T1T2_CI$x),]
    
    # T1 - k1/k2
    T1k1k2_CI <- predict(lm(unique(grid_a[,c("T1","k1","k2")])[,"k1"]/unique(grid_a[,c("T1","k1","k2")])[,"k2"] ~ unique(grid_a[,c("T1","k1","k2")])[,"T1"]),
                  newdata = data.frame(x = unique(grid_a[,c("T1","k1","k2")])[,"T1"]),
                  interval = "confidence", level = 0.99)[,]
    T1k1k2_CI <- as.data.frame(T1k1k2_CI)
    T1k1k2_CI$x <- as.numeric(unique(grid_a[,c("T1","k1","k2")])[,"T1"])
    T1k1k2_CI <- unique(T1k1k2_CI)
    T1k1k2_CI <- T1k1k2_CI[order(T1k1k2_CI$x),]
    
    # T2 - k1/k2
    T2k1k2_CI <- predict(lm(unique(grid_a[,c("T2","k1","k2")])[,"k1"]/unique(grid_a[,c("T2","k1","k2")])[,"k2"] ~ unique(grid_a[,c("T2","k1","k2")])[,"T2"]),
                  newdata = data.frame(x = unique(grid_a[,c("T2","k1","k2")])[,"T2"]),
                  interval = "confidence", level = 0.99)[,]
    T2k1k2_CI <- as.data.frame(T2k1k2_CI)
    T2k1k2_CI$x <- as.numeric(unique(grid_a[,c("T2","k1","k2")])[,"T2"])
    T2k1k2_CI <- unique(T2k1k2_CI)
    T2k1k2_CI <- T2k1k2_CI[order(T2k1k2_CI$x),]
    
  # Scatter Multi-Plot
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/correlations.pdf"), 
             width = 12, height = 12)
  par(mfrow=c(3,3))
  # K1 ~ K2
  plot(grid_a$k1, grid_a$k2, type = "n",  xlab = expression(k[1]),
       ylab = expression(k[2]), main = expression(k[1] * ' ~ ' * k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[1]),2),
                    " (2.s.f) [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1k2_CI$x),k1k2_CI$x),
          y = c(rev(k1k2_CI$upr), k1k2_CI$lwr), col = "grey85", border = NA)
  lines(k1k2_CI$x, k1k2_CI$fit, col = "blue")
  lines(k1k2_CI$x, k1k2_CI$lwr, col = "red", lty = 2)
  lines(k1k2_CI$x, k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$k2,0.4), pch = 20, col = "blue")
  # K1 ~ T1
  plot(grid_a$k1, grid_a$T1, type = "n",  xlab = expression(k[1]),
       ylab = expression(tau[1]), main = expression(k[1] * ' ~ ' * tau[1]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[2]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1T1_CI$x),k1T1_CI$x),
          y = c(rev(k1T1_CI$upr), k1T1_CI$lwr), col = "grey85", border = NA)
  lines(k1T1_CI$x, k1T1_CI$fit, col = "blue")
  lines(k1T1_CI$x, k1T1_CI$lwr, col = "red", lty = 2)
  lines(k1T1_CI$x, k1T1_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$T1,1), pch = 20, col = "blue")  
  # K1 ~ T2
  plot(grid_a$k1, grid_a$T2, type = "n",  xlab = expression(k[1]),
       ylab = expression(tau[2]), main = expression(k[1] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[3]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1T2_CI$x),k1T2_CI$x),
          y = c(rev(k1T2_CI$upr), k1T2_CI$lwr), col = "grey85", border = NA)
  lines(k1T1_CI$x, k1T2_CI$fit, col = "blue")
  lines(k1T1_CI$x, k1T2_CI$lwr, col = "red", lty = 2)
  lines(k1T1_CI$x, k1T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # K2 ~ T1
  plot(grid_a$k2, grid_a$T1, type = "n",  xlab = expression(k[2]),
       ylab = expression(tau[1]), main = expression(k[2] * ' ~ ' * tau[1]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[4]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k2T1_CI$x),k2T1_CI$x),
          y = c(rev(k2T1_CI$upr), k2T1_CI$lwr), col = "grey85", border = NA)
  lines(k2T1_CI$x, k2T1_CI$fit, col = "blue")
  lines(k2T1_CI$x, k2T1_CI$lwr, col = "red", lty = 2)
  lines(k2T1_CI$x, k2T1_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k2,0.4), jitter(grid_a$T1,1), pch = 20, col = "blue")  
  # K2 ~ T2
  plot(grid_a$k2, grid_a$T2, type = "n",  xlab = expression(k[2]),
       ylab = expression(tau[2]), main = expression(k[2] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[5]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k2T2_CI$x),k2T2_CI$x),
          y = c(rev(k2T2_CI$upr), k2T2_CI$lwr), col = "grey85", border = NA)
  lines(k2T2_CI$x, k2T2_CI$fit, col = "blue")
  lines(k2T2_CI$x, k2T2_CI$lwr, col = "red", lty = 2)
  lines(k2T2_CI$x, k2T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k2,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # T1 ~ T2
  plot(grid_a$T1, grid_a$T2, type = "n",  xlab = expression(tau[1]),
       ylab = expression(tau[2]), main = expression(tau[1] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[6]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T1T2_CI$x),T1T2_CI$x),
          y = c(rev(T1T2_CI$upr), T1T2_CI$lwr), col = "grey85", border = NA)
  lines(T1T2_CI$x, T1T2_CI$fit, col = "blue")
  lines(T1T2_CI$x, T1T2_CI$lwr, col = "red", lty = 2)
  lines(T1T2_CI$x, T1T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T1,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # T1 ~ k1/k2
  plot(grid_a$T1, (grid_a$k1/grid_a$k2), type = "n",  xlab = expression(tau[1]),
       ylab = expression(k[1] *'/'* k[2]), main = expression(tau[1] * ' ~ ' * k[1] *'/'* k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[7]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T1k1k2_CI$x),T1k1k2_CI$x),
          y = c(rev(T1k1k2_CI$upr), T1k1k2_CI$lwr), col = "grey85", border = NA)
  lines(T1k1k2_CI$x, T1k1k2_CI$fit, col = "blue")
  lines(T1k1k2_CI$x, T1k1k2_CI$lwr, col = "red", lty = 2)
  lines(T1k1k2_CI$x, T1k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T1,0.4), jitter(grid_a$k1/grid_a$k2,1), pch = 20, col = "blue")  
  # T2 ~ k1/k2
  plot(grid_a$T2, (grid_a$k1/grid_a$k2), type = "n",  xlab = expression(tau[2]),
       ylab = expression(k[1] *'/'* k[2]), main = expression(tau[2] * ' ~ ' * k[1] *'/'* k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[8]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T2k1k2_CI$x),T2k1k2_CI$x),
          y = c(rev(T2k1k2_CI$upr), T2k1k2_CI$lwr), col = "grey85", border = NA)
  lines(T2k1k2_CI$x, T2k1k2_CI$fit, col = "blue")
  lines(T2k1k2_CI$x, T2k1k2_CI$lwr, col = "red", lty = 2)
  lines(T2k1k2_CI$x, T2k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T2,0.4), jitter(grid_a$k1/grid_a$k2,1), pch = 20, col = "blue")  
  
  dev.off()
  
  setEPS(height = 12, width = 12)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/correlations.eps"))
    par(mfrow=c(3,3))
  # K1 ~ K2
  plot(grid_a$k1, grid_a$k2, type = "n",  xlab = expression(k[1]),
       ylab = expression(k[2]), main = expression(k[1] * ' ~ ' * k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[1]),2),
                    " (2.s.f) [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1k2_CI$x),k1k2_CI$x),
          y = c(rev(k1k2_CI$upr), k1k2_CI$lwr), col = "grey85", border = NA)
  lines(k1k2_CI$x, k1k2_CI$fit, col = "blue")
  lines(k1k2_CI$x, k1k2_CI$lwr, col = "red", lty = 2)
  lines(k1k2_CI$x, k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$k2,0.4), pch = 20, col = "blue")
  # K1 ~ T1
  plot(grid_a$k1, grid_a$T1, type = "n",  xlab = expression(k[1]),
       ylab = expression(tau[1]), main = expression(k[1] * ' ~ ' * tau[1]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[2]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1T1_CI$x),k1T1_CI$x),
          y = c(rev(k1T1_CI$upr), k1T1_CI$lwr), col = "grey85", border = NA)
  lines(k1T1_CI$x, k1T1_CI$fit, col = "blue")
  lines(k1T1_CI$x, k1T1_CI$lwr, col = "red", lty = 2)
  lines(k1T1_CI$x, k1T1_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$T1,1), pch = 20, col = "blue")  
  # K1 ~ T2
  plot(grid_a$k1, grid_a$T2, type = "n",  xlab = expression(k[1]),
       ylab = expression(tau[2]), main = expression(k[1] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[3]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k1T2_CI$x),k1T2_CI$x),
          y = c(rev(k1T2_CI$upr), k1T2_CI$lwr), col = "grey85", border = NA)
  lines(k1T1_CI$x, k1T2_CI$fit, col = "blue")
  lines(k1T1_CI$x, k1T2_CI$lwr, col = "red", lty = 2)
  lines(k1T1_CI$x, k1T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k1,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # K2 ~ T1
  plot(grid_a$k2, grid_a$T1, type = "n",  xlab = expression(k[2]),
       ylab = expression(tau[1]), main = expression(k[2] * ' ~ ' * tau[1]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[4]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k2T1_CI$x),k2T1_CI$x),
          y = c(rev(k2T1_CI$upr), k2T1_CI$lwr), col = "grey85", border = NA)
  lines(k2T1_CI$x, k2T1_CI$fit, col = "blue")
  lines(k2T1_CI$x, k2T1_CI$lwr, col = "red", lty = 2)
  lines(k2T1_CI$x, k2T1_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k2,0.4), jitter(grid_a$T1,1), pch = 20, col = "blue")  
  # K2 ~ T2
  plot(grid_a$k2, grid_a$T2, type = "n",  xlab = expression(k[2]),
       ylab = expression(tau[2]), main = expression(k[2] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[5]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(k2T2_CI$x),k2T2_CI$x),
          y = c(rev(k2T2_CI$upr), k2T2_CI$lwr), col = "grey85", border = NA)
  lines(k2T2_CI$x, k2T2_CI$fit, col = "blue")
  lines(k2T2_CI$x, k2T2_CI$lwr, col = "red", lty = 2)
  lines(k2T2_CI$x, k2T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$k2,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # T1 ~ T2
  plot(grid_a$T1, grid_a$T2, type = "n",  xlab = expression(tau[1]),
       ylab = expression(tau[2]), main = expression(tau[1] * ' ~ ' * tau[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[6]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T1T2_CI$x),T1T2_CI$x),
          y = c(rev(T1T2_CI$upr), T1T2_CI$lwr), col = "grey85", border = NA)
  lines(T1T2_CI$x, T1T2_CI$fit, col = "blue")
  lines(T1T2_CI$x, T1T2_CI$lwr, col = "red", lty = 2)
  lines(T1T2_CI$x, T1T2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T1,0.4), jitter(grid_a$T2,1), pch = 20, col = "blue")  
  # T1 ~ k1/k2
  plot(grid_a$T1, (grid_a$k1/grid_a$k2), type = "n",  xlab = expression(tau[1]),
       ylab = expression(k[1] *'/'* k[2]), main = expression(tau[1] * ' ~ ' * k[1] *'/'* k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[7]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T1k1k2_CI$x),T1k1k2_CI$x),
          y = c(rev(T1k1k2_CI$upr), T1k1k2_CI$lwr), col = "grey85", border = NA)
  lines(T1k1k2_CI$x, T1k1k2_CI$fit, col = "blue")
  lines(T1k1k2_CI$x, T1k1k2_CI$lwr, col = "red", lty = 2)
  lines(T1k1k2_CI$x, T1k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T1,0.4), jitter(grid_a$k1/grid_a$k2,1), pch = 20, col = "blue")  
  # T2 ~ k1/k2
  plot(grid_a$T2, (grid_a$k1/grid_a$k2), type = "n",  xlab = expression(tau[2]),
       ylab = expression(k[1] *'/'* k[2]), main = expression(tau[2] * ' ~ ' * k[1] *'/'* k[2]),
       sub = paste0("r = ",signif(as.numeric(grid_a_cors$association[8]),2),
                    " (2.s.f) | [Points jittered for plot clarity]"))
  polygon(x = c(rev(T2k1k2_CI$x),T2k1k2_CI$x),
          y = c(rev(T2k1k2_CI$upr), T2k1k2_CI$lwr), col = "grey85", border = NA)
  lines(T2k1k2_CI$x, T2k1k2_CI$fit, col = "blue")
  lines(T2k1k2_CI$x, T2k1k2_CI$lwr, col = "red", lty = 2)
  lines(T2k1k2_CI$x, T2k1k2_CI$upr, col = "red", lty = 2)
  points(jitter(grid_a$T2,0.4), jitter(grid_a$k1/grid_a$k2,1), pch = 20, col = "blue")  
  
  dev.off()
  
# Parameter Summary Statistic Data (parameters, cost_fn, rule, ratios)

  grid_a_parameter_summary <- data.frame(
    "k1" = summary_fun(grid_a$k1),
    "k2" = summary_fun(grid_a$k2),
    "T1" = summary_fun(grid_a$T1),
    "T2" = summary_fun(grid_a$T2),
    "fn_val" = summary_fun(grid_a$fn_val),
    "T1/T2" = summary_fun(unlist(grid_a$`T1/T2`)),
    "k2/k1" = summary_fun(unlist(grid_a$`k2/k1`))
  )
   
  write.csv(grid_a_parameter_summary, paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/stats/parameter_summary.csv"))
 
# Quantile Summary Data for Each Parameter Threshold Set
#   (Parameters, Cost Fn Values, R^2, RMSE)
  
  quantile_probs <- c(0,0.025,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.975,1)
  
  grid_a_quantiles <- apply(grid_a[,1:19], 2, quantile, probs = quantile_probs)
  
  write.csv(grid_a_quantiles, paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/stats/quantiles.csv"))

# Individual Plots

  for (i in 1:length(grid_a$k1)){
    pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/individual_plots/",i,".pdf"), 
      width = 10, height = 10)
    plot(s, p_true_all, type = "l", col = "blue",
         main = paste0("Threshold Set | Set ",i," | (",
                       round(grid_a[i,1],3),",",round(grid_a[i,2],3),",",
                       round(grid_a[i,3],3),",",
                       round(grid_a[i,4],3),")"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_noise_all,p_true_all,grid_a_perf[,i]),na.rm=TRUE)-50,
                  max(c(p_noise_all,p_true_all,grid_a_perf[,i]),na.rm=TRUE)+50)
         )
    points(s,p_noise_all,col="black",pch=18)
    lines(s,grid_a_perf[,i], col = "red", lty = 2)
    legend("bottomright", c("'True' Perf - Primary TL","'Noisy' Perf (Fitting Vals)","Fit/Forecast - Primary TL"),
           fill = c("blue","black","red"))
    dev.off()
    
    pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/individual_plots_alt/",i,".pdf"), 
      width = 10, height = 10)
    plot(s, p_true_alt, type = "l", col = "darkgreen",
         main = paste0("Threshold Set | Set ",i," | (",
                       round(grid_a[i,1],3),",",round(grid_a[i,2],3),",",
                       round(grid_a[i,3],3),",",
                       round(grid_a[i,4],3),")"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_noise_all,p_true_all,grid_a_perf[,i]),na.rm=TRUE)-50,
                  max(c(p_noise_all,p_true_all,grid_a_perf[,i]),na.rm=TRUE)+50)
         )
    lines(s,grid_a_perf_alt[,i], col = "red", lty = 2)
     points(s,p_noise_all, col = "black", pch = 18)
    legend("bottomright", c("'True' Perf - Alt TL","'Noisy' Perf (Fitting Vals)","Fit/Forecast - Alt TL"),
           fill = c("darkgreen","black","red"))
    dev.off()
  }
  
  # All lines plotted on a graph
   pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/primary_all_lines.pdf"), 
      width = 10, height = 10)
    plot(s, p_true_all, type = "n",
         main = paste0("Threshold Set | All Lines | Primary Performance"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_noise_all,p_true_all,apply(grid_a_perf, 1, min)),na.rm=TRUE)-50,
                  max(c(p_noise_all,p_true_all,apply(grid_a_perf, 1, max)),na.rm=TRUE)+50)
         )
    for (i in 1:length(grid_a$k1)){
      lines(s,grid_a_perf[,i], col = "red",lty=2)
    }
    lines(s,p_true_all, col = "blue")
    points(s,p_noise_all, col = "black", pch = 18)
    legend("bottomright", c("'True' Perf - Primary TL","Fit/Forecast - Primary TL","'Noisy' Perf (Fitting Vals)"),
           fill = c("blue","red","black"))
  dev.off()
  
  setEPS(height = 10, width = 10)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/primary_all_lines.eps"))
  plot(s, p_true_all, type = "n",
         main = paste0("Threshold Set | All Lines | Primary Performance"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_noise_all,p_true_all,apply(grid_a_perf, 1, min)),na.rm=TRUE)-50,
                  max(c(p_noise_all,p_true_all,apply(grid_a_perf, 1, max)),na.rm=TRUE)+50)
         )
    for (i in 1:length(grid_a$k1)){
      lines(s,grid_a_perf[,i], col = "red",lty=2)
    }
    lines(s,p_true_all, col = "blue")
    points(s,p_noise_all, col = "black", pch = 18)
    legend("bottomright", c("'True' Perf - Primary TL","Fit/Forecast - Primary TL","'Noisy' Perf (Fitting Vals)"),
           fill = c("blue","red","black"))
  dev.off()
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/alt_all_lines.pdf"), 
      width = 10, height = 10)
    plot(s, p_true_all, type = "n",
         main = paste0("Threshold Set | All Lines | Alt Performance"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_true_all,apply(grid_a_perf_alt, 1, min)),na.rm=TRUE)-50,
                  max(c(p_true_all,apply(grid_a_perf_alt, 1, max)),na.rm=TRUE)+50)
         )
    for (i in 1:length(grid_a$k1)){
      lines(s,grid_a_perf_alt[,i], col = "red",lty=2)
    }
    lines(s,p_true_alt, col = "darkgreen")
    points(s,p_noise_all, col = "black", pch = 18)
    legend("bottomright", c("'True' Perf - Alt TL","Fit/Forecast - Alt TL","'Noisy' Perf (Fitting Vals)"),
           fill = c("darkgreen","red","black"))
  dev.off()
  
  setEPS(height = 10, width = 10)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/alt_all_lines.eps"))
  plot(s, p_true_all, type = "n",
         main = paste0("Threshold Set | All Lines | Alt Performance"),
         xlab = "Day", ylab = "Performance (Arbitrary Unit)",
         ylim = c(min(c(p_true_all,apply(grid_a_perf_alt, 1, min)),na.rm=TRUE)-50,
                  max(c(p_true_all,apply(grid_a_perf_alt, 1, max)),na.rm=TRUE)+50)
         )
    for (i in 1:length(grid_a$k1)){
      lines(s,grid_a_perf_alt[,i], col = "red",lty=2)
    }
    lines(s,p_true_alt, col = "darkgreen")
    points(s,p_noise_all, col = "black", pch = 18)
    legend("bottomright", c("'True' Perf - Alt TL","Fit/Forecast - Alt TL","'Noisy' Perf (Fitting Vals)"),
           fill = c("darkgreen","red","black"))
  dev.off()
  
# Ribbon Plots for Each Threshold Set
  
  dailymin <- apply(grid_a_perf, 1, min)
  dailymax <- apply(grid_a_perf, 1, max)
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/primary_model.pdf"), 
      width = 10, height = 10)
  plot(s,p_true_all,type = "n",
       ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
       main = "All Parameter Sets: Fit and Forecast Range",
       ylab = "Performance (Arbitrary Unit)", xlab = "Day")
  polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
  lines(s, p_true_all, col = "blue")
  lines(s, dailymin, col = "red", lty=2)
  lines(s, dailymax, col = "red", lty=2)
  points(s,p_noise_all,pch=18,col = "black")
  legend("bottomright", c("'True' Perf - Primary TL","Fit/Forecast - Primary TL",
                          "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
  dev.off()
  
  setEPS(height = 10, width = 10)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/primary_model.eps"))
  plot(s,p_true_all,type = "n",
       ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
       main = "All Parameter Sets: Fit and Forecast Range",
       ylab = "Performance (Arbitrary Unit)", xlab = "Day")
  polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
  lines(s, p_true_all, col = "blue")
  lines(s, dailymin, col = "red", lty=2)
  lines(s, dailymax, col = "red", lty=2)
  points(s,p_noise_all,pch=18,col = "black")
  legend("bottomright", c("'True' Perf - Primary TL","Fit/Forecast - Primary TL",
                          "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
  dev.off()
  
  dailymin <- apply(grid_a_perf_alt, 1, min)
  dailymax <- apply(grid_a_perf_alt, 1, max)
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/alternative_model.pdf"), 
      width = 10, height = 10)
  plot(s,p_true_all,type = "n",ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
       main = "All Parameter Sets: Fit and Forecast Range (Alt TL)",
       ylab = "Performance (Arbitrary Unit)", xlab = "Day")
  polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
  lines(s, p_true_alt, col = "darkgreen")
  lines(s, dailymin, col = "red", lty=2)
  lines(s, dailymax, col = "red", lty=2)
  points(s,p_noise_all,pch=18,col = "black")
  legend("bottomright", c("'True' Perf - Alt TL","Fit/Forecast - Alt TL","Noisy Perf (Fitting Vals"), 
         fill = c("darkgreen","red","black"))
  dev.off()
  
  setEPS(height = 10, width = 10)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/ribbons/alternative_model.eps"))
   plot(s,p_true_all,type = "n",ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
       main = "All Parameter Sets: Fit and Forecast Range (Alt TL)",
       ylab = "Performance (Arbitrary Unit)", xlab = "Day")
  polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
  lines(s, p_true_alt, col = "darkgreen")
  lines(s, dailymin, col = "red", lty=2)
  lines(s, dailymax, col = "red", lty=2)
  points(s,p_noise_all,pch=18,col = "black")
  legend("bottomright", c("'True' Perf - Alt TL","Fit/Forecast - Alt TL","Noisy Perf (Fitting Vals"), 
         fill = c("darkgreen","red","black"))
  dev.off()

# Plot 3D Scatterplot (z = cost_fn values, x = k2/k1, y = tau1/tau2)  

setEPS(height = 10, width = 10)
postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/ratio_3d_map.eps"))
scatterplot3d(grid_a$`k2/k1`, grid_a$`T1/T2`, grid_a$fn_val, grid = TRUE,
              pch =18, color = "red", type = "h", angle = 25,
              xlab = 
            expression(k[2] * ' / ' * k[1]),
          ylab = expression(tau[1] * ' / ' * tau[2]),
          zlab = "MSE",
          main = paste0("MSE ~ k2/k1 * T1/T2"))
dev.off()

pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/ratio_3d_map.pdf"), height = 10, width = 10)
scatterplot3d(grid_a$`k2/k1`, grid_a$`T1/T2`, grid_a$fn_val, grid = TRUE,
              pch =18, color = "red", type = "h", angle = 25,
              xlab = 
            expression(k[2] * ' / ' * k[1]),
          ylab = expression(tau[1] * ' / ' * tau[2]),
          zlab = "MSE",
          main = paste0("MSE ~ k2/k1 * T1/T2"))
dev.off()

# Flatted 3D Plot

pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/pars/ratio_flat_map.pdf"),height = 10, width = 10)
levelplot(grid_a$fn_val ~ grid_a$`k2/k1`*grid_a$`T1/T2`,
          panel = panel.levelplot.points, cex = 1.2, xlab = 
            expression(k[2] * ' / ' * k[1]),
          ylab = expression(tau[1] * ' / ' * tau[2]),
          main = paste0("MSE ~ k2/k1 * T1/T2"))
dev.off()

# CLUSTERING
# ------------------------------------------------------------------------------
if (freq_index < 5){
# Scale Data
  cluster_data <- grid_a[,1:5]
  cluster_data[,6:9] <- scale(cluster_data[,1:4],  scale = TRUE)
  colnames(cluster_data) <- c("k1","T1","k2","T2","MSE","k1_scaled","T1_scaled",
                                   "k2_scaled","T2_scaled")

  if (length(grid_a[,1]) <= 1000){
# Elbow Plot (Kmeans Total Within Sum of Squares)
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_elbow_wss.pdf"), width = 10, height = 10)
  print(fviz_nbclust(cluster_data[,6:9], kmeans, method = "wss", k.max = 10))
  dev.off()
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_elbow_wss.pdf"), width = 10, height = 10)
  print(fviz_nbclust(cluster_data[,6:9], pam, method = "wss", k.max = 10))
  dev.off()
  
# Average Silhouette Plot
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_silhouette.pdf"), width = 10, height = 10)
  print(fviz_nbclust(cluster_data[,6:9], kmeans, method = "silhouette", k.max = 10))
  dev.off()
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_silhouette.pdf"), width = 10, height = 10)
  print(fviz_nbclust(cluster_data[,6:9], pam, method = "silhouette", k.max = 10))
  dev.off()

}
  
if (length(grid_a[,1]) <= 100){
# Plot the distance matrix
  setEPS(width = 12, height = 12)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/dist_matrix.eps"))
  print(fviz_dist(get_dist(cluster_data[,6:9], method = "euclidean")))
  dev.off()
}
  
# Reduce bootstrap time depending on starting values
if (length(grid_a[,1]) <= 50000){
  boostrap_n <- 250
}
if (length(grid_a[,1]) <= 10000){
  bootstrap_n <- 500
}
if (length(grid_a[,1]) <= 250){
  bootstrap_n <- 2500
}
if (length(grid_a[,1]) <= 100){
  bootstrap_n <- 5000
}

# Compute Gap Statistic for PAM and Kmeans
  gap_stat_kmeans <- clusGap(cluster_data[,6:9], 
                      FUN = kmeans, nstart = 25,
                      K.max = 6, B = bootstrap_n, d.power = 2)
  gap_stat_pam <- clusGap(cluster_data[,6:9], 
                      FUN = pam,
                      K.max = 6, B = bootstrap_n,
                      )
  saveRDS(gap_stat_kmeans, paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/gapstat_kmeans.Rda"))
  saveRDS(gap_stat_pam, paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/gapstat_pam.Rda"))

# Visualise Gap Statistic
  setEPS(width = 6, height = 6)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/gapstat_pam.eps"))
  fviz_gap_stat(gap_stat_pam, maxSE = list(method = "Tibs2001SEmax"))
  dev.off()
  
  setEPS(width = 6, height = 6)
  postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/gapstat_kmeans.eps"))
  fviz_gap_stat(gap_stat_kmeans, maxSE = list(method = "Tibs2001SEmax"))
  dev.off()

# Compute K-Means with Optimum K
  k_n_1 <- maxSE(gap_stat_kmeans$Tab[, "gap"], gap_stat_kmeans$Tab[, "SE.sim"], method="Tibs2001SEmax")
  set.seed(123)
  kmeans_fit <- kmeans(cluster_data[,6:9], k_n_1)
  cat(paste0("Number of k-means Clusters: ", k_n_1," | Method = Tibs2001SEmax (Gap Statistic)"),
      file = paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/num_kmeans_clusters.txt"))
  saveRDS(kmeans_fit, paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_fit.Rda"))  

# Visualise K-Means Cluster Plots
  if (length(grid_a[,1]) <= 1000){
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_vis.pdf"), height = 12, width = 12)
  print(fviz_cluster(kmeans_fit, data = cluster_data[,6:9]))
  dev.off()
  }
  
# K-Means Cluster Assignment to Data Object
  cluster_data[,"kmeans_cluster"] <- kmeans_fit$cluster

# Compute the Means of the K-Means Clusters
  k1_means_km <- tapply(cluster_data$k1, cluster_data$kmeans_cluster, mean)
  T1_means_km <- tapply(cluster_data$T1, cluster_data$kmeans_cluster, mean)
  k2_means_km <- tapply(cluster_data$k2, cluster_data$kmeans_cluster, mean)
  T2_means_km <- tapply(cluster_data$T2, cluster_data$kmeans_cluster, mean)
  km_cluster_means <- data.frame(cbind(k1_means_km,T1_means_km,k2_means_km,T2_means_km))
  
# Compute Average Numerical Fit and Forecast Accuracy Across each Cluster
  cbind_clusterdata <- cbind(cluster_data, grid_a[,6:18])
  km_cluster_means[,5] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_A_Fitted_True, cbind_clusterdata$kmeans_cluster, mean))
  km_cluster_means[,6] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_A_Fitted_True, cbind_clusterdata$kmeans_cluster, sd))
  km_cluster_means[,7] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_B_Forecast_True, cbind_clusterdata$kmeans_cluster, mean))
  km_cluster_means[,8] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_B_Forecast_True, cbind_clusterdata$kmeans_cluster, sd))
  km_cluster_means[,9] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_B_AltForecast_AltTrue, cbind_clusterdata$kmeans_cluster, mean))
  km_cluster_means[,10] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_B_AltForecast_AltTrue, cbind_clusterdata$kmeans_cluster, sd))

   colnames(km_cluster_means) <- c("k1","T1","k2","T2","mean_rmse_fitted_true",
                                  "sd_rmse_fitted_true",
                                  "mean_rmse_forecast_true",
                                  "sd_rmse_forecast_true",
                                  "mean_rmse_alt_forecast_true",
                                  "sd_rmse_alt_forecast_true")
  
  write.csv(km_cluster_means,
            paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/km_cluster_means.csv"))

# Compute PAM with Optimum K
  k_n_2 <- maxSE(gap_stat_pam$Tab[, "gap"], gap_stat_pam$Tab[, "SE.sim"], method="Tibs2001SEmax")
  set.seed(123)
  pam_fit <- pam(cluster_data[,6:9], k_n_2)
  cat(paste0("Number of pam Clusters: ", k_n_2," | Method = Tibs2001SEmax (Gap Statistic)"),
      file = paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/num_pam_clusters.txt"))
  saveRDS(kmeans_fit, paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_fit.Rda"))  

# Visualise PAM Cluster Plots
  if (length(grid_a[,1]) <= 1000){
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_vis.pdf"),
      width = 12, height = 12)
  print(fviz_cluster(pam_fit, data = cluster_data[,6:9]))
  dev.off()
  }
  
# PAM Cluster Assignment to Data Object
  cluster_data[,"pam_cluster"] <- pam_fit$clustering
  
# Compute the Means of the PAM Clusters
  k1_means_pam <- tapply(cluster_data$k1, cluster_data$pam_cluster, mean)
  T1_means_pam <- tapply(cluster_data$T1, cluster_data$pam_cluster, mean)
  k2_means_pam <- tapply(cluster_data$k2, cluster_data$pam_cluster, mean)
  T2_means_pam <- tapply(cluster_data$T2, cluster_data$pam_cluster, mean)
  pam_cluster_means <-data.frame(cbind(k1_means_pam,T1_means_pam,k2_means_pam,T2_means_pam))
  cbind_clusterdata <- cbind(cluster_data, grid_a[,6:18])
  
   pam_cluster_means[,5] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_A_Fitted_True, cbind_clusterdata$pam_cluster, mean))
  pam_cluster_means[,6] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_A_Fitted_True, cbind_clusterdata$pam_cluster, sd))
  pam_cluster_means[,7] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_B_Forecast_True, cbind_clusterdata$pam_cluster, mean))
  pam_cluster_means[,8] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_B_Forecast_True, cbind_clusterdata$pam_cluster, sd))
  pam_cluster_means[,9] <-as.numeric(tapply(
    cbind_clusterdata$RMSE_B_AltForecast_AltTrue, cbind_clusterdata$pam_cluster, mean))
  pam_cluster_means[,10] <- as.numeric(tapply(
    cbind_clusterdata$RMSE_B_AltForecast_AltTrue, cbind_clusterdata$pam_cluster, sd))

  colnames(pam_cluster_means) <- c("k1","T1","k2","T2","mean_rmse_fitted_true",
                                  "sd_rmse_fitted_true",
                                  "mean_rmse_forecast_true",
                                  "sd_rmse_forecast_true",
                                  "mean_rmse_alt_forecast_true",
                                  "sd_rmse_alt_forecast_true")
  
  write.csv(pam_cluster_means,
            paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_cluster_means.csv"))
  
  # Save Cluster Data
saveRDS(cluster_data, 
          paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/cluster_data.Rda"))
write.csv(cluster_data, 
            paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/cluster_data.csv"))

# Plotting for K-Means (TODO)

  # Plot the Cluster Sets and 3d scatterplot of MSE ~ k2/k1 * T1/T2
  
  for (i in 1:k_n_1){
    temp_data <- cluster_data[cluster_data$kmeans_cluster == i,1:5]
    temp_perf <- as.data.frame(apply(temp_data[,1:4], 1, ffm4_apply, p0, s, ws_all))
    temp_altperf <- as.data.frame(apply(temp_data[,1:4], 1, ffm4_apply, p0, s, ws_alt_all))
    
    # Plot Each Line
    
      # Primary Training Load (PDF and EPS) [Plot All Lines]
    
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l.pdf"), 
            width = 10, height = 10)
          plot(s, p_true_all, type = "n",
               main = paste0("K-means Cluster ",i," | All Lines | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(temp_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(temp_perf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_perf[,j], col = "red",lty=2)
          }
          lines(s,p_true_all, col = "blue")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i, " Fit/Forecast - Primary TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("blue","red","black"))
        dev.off()
        
        setEPS(height = 10, width = 10)
        postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l.eps"))
        plot(s, p_true_all, type = "n",
               main = paste0("K-Means Cluster ",i," | All Lines | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(temp_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(temp_perf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_perf[,j], col = "red",lty=2)
          }
          lines(s,p_true_all, col = "blue")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i, " Fit/Forecast - Primary TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("blue","red","black"))
        dev.off()     
        
      # Alt Training Load (PDF and EPS) [Plot All Lines]
        
        pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l_alt.pdf"), 
            width = 10, height = 10)
        plot(s, p_true_alt, type = "n",
               main = paste0("K-Means Cluster ",i," | All Lines | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_altperf[,j], col = "red",lty=2)
          }
          lines(s,p_true_alt, col = "darkgreen")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i, " Fit/Forecast - Alt TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("darkgreen","red","black"))
        dev.off()
        
        setEPS(height = 10, width = 10)
        postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l_alt.eps"))
        plot(s, p_true_alt, type = "n",
               main = paste0("K-Means Cluster ",i," | All Lines | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_altperf[,j], col = "red",lty=2)
          }
          lines(s,p_true_alt, col = "darkgreen")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i, " Fit/Forecast - Alt TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("darkgreen","red","black"))
        dev.off()        
        
    # Plot Ribbons
    
      # Primary Training Load (PDF and EPS) [Ribbon Plots]
      dailymin <- apply(temp_perf, 1, min)
      dailymax <- apply(temp_perf, 1, max)
      
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_rib.pdf"), 
          width = 10, height = 10)
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("K-Means Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_all, col = "blue")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i," Fit/Forecast - Primary TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
      dev.off()
      
      setEPS(height = 10, width = 10)
      postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_rib.eps"))
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("K-Means Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_all, col = "blue")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i," Fit/Forecast - Primary TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
      dev.off()
      
  # Alternate Training Load (PDF and EPS) [Ribbon Plots]
      
      dailymin <- apply(temp_altperf, 1, min)
      dailymax <- apply(temp_altperf, 1, max)
      
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_altrib.pdf"), 
          width = 10, height = 10)
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("K-Means Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_alt, col = "darkgreen")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i," Fit/Forecast - Alt TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("darkgreen","red","black"))
      dev.off()
      
      setEPS(height = 10, width = 10)
      postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_altrib.eps"))
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_alt, col = "darkgreen")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i," Fit/Forecast - Alt TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("darkgreen","red","black"))
      dev.off()
      
    # 3D Scatterplot (PDF)
    cluster_k2k1 <- temp_data$k2 / temp_data$k1
    cluster_T1T2 <- temp_data$T1 / temp_data$T2
    
    # Plot 3D Scatterplot (z = cost_fn values, x = k2/k1, y = tau1/tau2)  

    setEPS(height = 10, width = 10)
    postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_3dratio.eps"))
    scatterplot3d(cluster_k2k1, cluster_T1T2, temp_data$MSE, grid = TRUE,
                  pch =18, color = "red", type = "h", angle = 25,
                  xlab = 
                expression(k[2] * ' / ' * k[1]),
              ylab = expression(tau[1] * ' / ' * tau[2]),
              zlab = "MSE",
              main = paste0("MSE ~ k2/k1 * T1/T2"))
    dev.off()
    
    pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_3dratio.pdf"), height = 10, width = 10)
    scatterplot3d(cluster_k2k1, cluster_T1T2, temp_data$MSE, grid = TRUE,
                  pch =18, color = "red", type = "h", angle = 25,
                  xlab = 
                expression(k[2] * ' / ' * k[1]),
              ylab = expression(tau[1] * ' / ' * tau[2]),
              zlab = "MSE",
              main = paste0("MSE ~ k2/k1 * T1/T2"))
    dev.off()
    
  }


  # Plot Cluster Means
  cluster_mean_perf <- apply(km_cluster_means[,1:4], 1, ffm4_apply, p0=p0, s=s, ws=ws_all)
  cluster_mean_altperf <- apply(km_cluster_means[,1:4], 1, ffm4_apply, p0=p0, s=s, ws=ws_alt_all)
  cluster_mean_colors <- rainbow(length(km_cluster_means[,1]), alpha = 1)
  cluster_mean_legend <- c("'True' Perf - Primary TL","'Noisy' Perf (Fitting Vals)",
                           paste0("Cluster ",1:k_n_1," Mean"))
  cluster_mean_legend2 <- c("'True' Perf - Alt TL","'Noisy' Perf (Fitting Vals)",
                           paste0("Cluster ",1:k_n_1," Mean"))
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_primary.pdf"), 
            width = 10, height = 10)
  plot(s, p_true_all, type = "n",
               main = paste0("K-Means Cluster means | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(cluster_mean_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(cluster_mean_perf, 1, max)),na.rm=TRUE)+50)
      )
  for (j in 1:length(km_cluster_means[,1])){
            set.seed(123)
            lines(s,cluster_mean_perf[,j], 
                  col = cluster_mean_colors[j],lty=2)
  }
  lines(s,p_true_all, col = "blue")
  points(s,p_noise_all, col = "black", pch = 18)
  legend("topleft", 
         cluster_mean_legend,
         fill = c("blue","black",cluster_mean_colors))
  dev.off()
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/kmeans_alt.pdf"), 
            width = 10, height = 10)
  plot(s, p_true_all, type = "n",
               main = paste0("K-Means Cluster means | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(cluster_mean_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(cluster_mean_altperf, 1, max)),na.rm=TRUE)+50)
      )
  for (j in 1:length(km_cluster_means[,1])){
            set.seed(123)
            lines(s,cluster_mean_altperf[,j], 
                  col = cluster_mean_colors[j],lty=2)
  }
  lines(s,p_true_alt, col = "darkgreen")
  points(s,p_noise_all, col = "black", pch = 18)
  legend("topleft", 
         cluster_mean_legend2,
         fill = c("darkgreen","black",cluster_mean_colors))
  dev.off()
  
# Plotting for PAM (TODO)
  
# Plot the Cluster Sets and 3d scatterplot of MSE ~ k2/k1 * T1/T2
  
  for (i in 1:k_n_2){
    temp_data <- cluster_data[cluster_data$pam_cluster == i,1:5]
    temp_perf <- as.data.frame(apply(temp_data[,1:4], 1, ffm4_apply, p0, s, ws_all))
    temp_altperf <- as.data.frame(apply(temp_data[,1:4], 1, ffm4_apply, p0, s, ws_alt_all))
    
    # Plot Each Line
    
      # Primary Training Load (PDF and EPS) [Plot All Lines]
    
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l.pdf"), 
            width = 10, height = 10)
          plot(s, p_true_all, type = "n",
               main = paste0("Pam Cluster ",i," | All Lines | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(temp_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(temp_perf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_perf[,j], col = "red",lty=2)
          }
          lines(s,p_true_all, col = "blue")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i, " Fit/Forecast - Primary TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("blue","red","black"))
        dev.off()
        
        setEPS(height = 10, width = 10)
        postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l.eps"))
        plot(s, p_true_all, type = "n",
               main = paste0("Pam Cluster ",i," | All Lines | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(temp_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(temp_perf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_perf[,j], col = "red",lty=2)
          }
          lines(s,p_true_all, col = "blue")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i, " Fit/Forecast - Primary TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("blue","red","black"))
        dev.off()     
        
      # Alt Training Load (PDF and EPS) [Plot All Lines]
        
        pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l_alt.pdf"), 
            width = 10, height = 10)
        plot(s, p_true_alt, type = "n",
               main = paste0("Pam Cluster ",i," | All Lines | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_altperf[,j], col = "red",lty=2)
          }
          lines(s,p_true_alt, col = "darkgreen")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i, " Fit/Forecast - Alt TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("darkgreen","red","black"))
        dev.off()
        
        setEPS(height = 10, width = 10)
        postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_l_alt.eps"))
        plot(s, p_true_alt, type = "n",
               main = paste0("Pam Cluster ",i," | All Lines | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_alt,apply(temp_altperf, 1, max)),na.rm=TRUE)+50)
               )
          for (j in 1:length(temp_data$k1)){
            lines(s,temp_altperf[,j], col = "red",lty=2)
          }
          lines(s,p_true_alt, col = "darkgreen")
          points(s,p_noise_all, col = "black", pch = 18)
          legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i, " Fit/Forecast - Alt TL"),"'Noisy' Perf (Fitting Vals)"),
                 fill = c("darkgreen","red","black"))
        dev.off()        
        
    # Plot Ribbons
    
      # Primary Training Load (PDF and EPS) [Ribbon Plots]
      dailymin <- apply(temp_perf, 1, min)
      dailymax <- apply(temp_perf, 1, max)
      
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_rib.pdf"), 
          width = 10, height = 10)
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("Pam Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_all, col = "blue")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i," Fit/Forecast - Primary TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
      dev.off()
      
      setEPS(height = 10, width = 10)
      postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_rib.eps"))
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("Pam Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_all, col = "blue")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Primary TL",paste0("Cluster ",i," Fit/Forecast - Primary TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("blue","red","black"))
      dev.off()
      
  # Alternate Training Load (PDF and EPS) [Ribbon Plots]
      
      dailymin <- apply(temp_altperf, 1, min)
      dailymax <- apply(temp_altperf, 1, max)
      
      pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_altrib.pdf"), 
          width = 10, height = 10)
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("Pam Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_alt, col = "darkgreen")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i," Fit/Forecast - Alt TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("darkgreen","red","black"))
      dev.off()
      
      setEPS(height = 10, width = 10)
      postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_altrib.eps"))
      plot(s,p_true_all,type = "n",
           ylim = c(min(c(dailymin,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)-50,
                    max(c(dailymax,p_true_all,p_true_alt,p_noise_all),na.rm = TRUE)+50),
           main = paste0("Pam Cluster ",i," Parameter Sets: Fit and Forecast Range"),
           ylab = "Performance (Arbitrary Unit)", xlab = "Day")
      polygon(c(rev(s),s),c(rev(dailymax),dailymin), col = "grey85", border = NA)
      lines(s, p_true_alt, col = "darkgreen")
      lines(s, dailymin, col = "red", lty=2)
      lines(s, dailymax, col = "red", lty=2)
      points(s,p_noise_all,pch=18,col = "black")
      legend("bottomright", c("'True' Perf - Alt TL",paste0("Cluster ",i," Fit/Forecast - Alt TL"),
                              "'Noisy' Perf (Fitting Vals)"), fill = c("darkgreen","red","black"))
      dev.off()
      
    # 3D Scatterplot (PDF)
    cluster_k2k1 <- temp_data$k2 / temp_data$k1
    cluster_T1T2 <- temp_data$T1 / temp_data$T2
    
    # Plot 3D Scatterplot (z = cost_fn values, x = k2/k1, y = tau1/tau2)  

    setEPS(height = 10, width = 10)
    postscript(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_3dratio.eps"))
    scatterplot3d(cluster_k2k1, cluster_T1T2, temp_data$MSE, grid = TRUE,
                  pch =18, color = "red", type = "h", angle = 25,
                  xlab = 
                expression(k[2] * ' / ' * k[1]),
              ylab = expression(tau[1] * ' / ' * tau[2]),
              zlab = "MSE",
              main = paste0("MSE ~ k2/k1 * T1/T2"))
    dev.off()
    
    pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/clus",i,"_3dratio.pdf"), height = 10, width = 10)
    scatterplot3d(cluster_k2k1, cluster_T1T2, temp_data$MSE, grid = TRUE,
                  pch =18, color = "red", type = "h", angle = 25,
                  xlab = 
                expression(k[2] * ' / ' * k[1]),
              ylab = expression(tau[1] * ' / ' * tau[2]),
              zlab = "MSE",
              main = paste0("MSE ~ k2/k1 * T1/T2"))
    dev.off()
    
  }
  
# Plot Cluster Means
  cluster_mean_perf <- apply(pam_cluster_means[,1:4], 1, ffm4_apply, p0=p0, s=s, ws=ws_all)
  cluster_mean_altperf <- apply(pam_cluster_means[,1:4], 1, ffm4_apply, p0=p0, s=s, ws=ws_alt_all)
  cluster_mean_colors <- rainbow(length(pam_cluster_means[,1]), alpha = 1)
  cluster_mean_legend <- c("'True' Perf - Primary TL","'Noisy' Perf (Fitting Vals)",
                           paste0("Cluster ",1:k_n_2," Mean"))
  cluster_mean_legend2 <- c("'True' Perf - Alt TL","'Noisy' Perf (Fitting Vals)",
                           paste0("Cluster ",1:k_n_2," Mean"))
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_means_primary.pdf"), 
            width = 10, height = 10)
  plot(s, p_true_all, type = "n",
               main = paste0("Pam Cluster means | Primary TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(cluster_mean_perf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(cluster_mean_perf, 1, max)),na.rm=TRUE)+50)
      )
  for (j in 1:length(pam_cluster_means[,1])){
            set.seed(123)
            lines(s,cluster_mean_perf[,j], 
                  col = cluster_mean_colors[j],lty=2)
  }
  lines(s,p_true_all, col = "blue")
  points(s,p_noise_all, col = "black", pch = 18)
  legend("topleft", 
         cluster_mean_legend,
         fill = c("blue","black",cluster_mean_colors))
  dev.off()
  
  pdf(paste0("out/case_",case,"/freq_",freq_index,
             "/grid/threshold/clustering/pam_means_alt.pdf"), 
            width = 10, height = 10)
  plot(s, p_true_all, type = "n",
               main = paste0("Pam Cluster means | Alt TL Set"),
               xlab = "Day", ylab = "Performance (Arbitrary Unit)",
               ylim = c(min(c(p_noise_all,p_true_all,apply(cluster_mean_altperf, 1, min)),na.rm=TRUE)-50,
                        max(c(p_noise_all,p_true_all,apply(cluster_mean_altperf, 1, max)),na.rm=TRUE)+50)
      )
  for (j in 1:length(pam_cluster_means[,1])){
            set.seed(123)
            lines(s,cluster_mean_altperf[,j], 
                  col = cluster_mean_colors[j],lty=2)
  }
  lines(s,p_true_alt, col = "darkgreen")
  points(s,p_noise_all, col = "black", pch = 18)
  legend("topleft", 
         cluster_mean_legend2,
         fill = c("darkgreen","black",cluster_mean_colors))
  dev.off()
  
# Remove Cluster Objects
rm(gap_stat_kmeans, gap_stat_pam, cluster_data, bootstrap_n, k_n_1, kmeans_fit,
   km_cluster_means, k1_means_km, T1_means_km, k2_means_km, T2_means_km,
   cbind_clusterdata, k_n_2, pam_fit, pam_cluster_means, cluster_k2k1,
   cluster_T1T2, temp_data, temp_perf, temp_altperf,dailymin,dailymax,i,j,
   cluster_mean_perf, cluster_mean_altperf, cluster_mean_colors, 
   cluster_mean_legend, cluster_mean_legend2
   )
}
# Remove Objects and call gc()
  rm(grid_a, grid_a_perf_alt, grid_quantiles,
   grid_a_perf, grid_a_model_summary, correlation_rownames,
   correlation_colnames, grid_a_cors, k1k2_CI, k1T1_CI, k1T2_CI, k2T1_CI, 
   k2T2_CI, T1T2_CI, T1k1k2_CI, T2k1k2_CI, grid_a_parameter_summary,
   grid_a_quantiles, model_colnames, quantile_probs,
   grid_start_time, grid_end_time, cost_fn_threshold)

# Call garbage collection
  gc()
