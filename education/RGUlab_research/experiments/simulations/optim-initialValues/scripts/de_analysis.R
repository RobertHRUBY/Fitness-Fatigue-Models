# Evaluate runtime
cat(difftime(de_runtime_end,de_runtime_start), 
    file = paste0("out/case_",case,"/freq_",freq_index,
                              "/grid/grid_runtime.txt"))

# Save the raw DE object to the right output directory
saveRDS(de_optimisation, file = paste0("out/case_",case,"/freq_",
                                       freq_index,"/opt/de/de_raw.Rda"))

# Collect Parameters and Cost Fn Value and save

de_pars <- as.numeric(de_optimisation$optim$bestmem)
de_cost <- as.numeric(de_optimisation$optim$bestval)

# Calculate Model Performance Values and save
de_perf <- ffm4(p0,s,de_pars[1],de_pars[2],de_pars[3],de_pars[4],ws_all)
de_perf_alt <- ffm4(p0,s,de_pars[1],de_pars[2],de_pars[3],de_pars[4],ws_alt_all)

# Compute Model Fit Statistics (R^2/RMSE) and save
de_modelling <- c(
  # Block A
  rmse_func(de_perf[1:100],p_true_a,n=length(s1),t=s1),
  rmse_func(de_perf[1:100],p_noise_a,n=length(measure_index),t=measure_index),
  rsq_func(de_perf[1:100],p_true_a,n=length(s1),t=s1)[1],
  rsq_func(de_perf[1:100],p_true_a,n=length(s1),t=s1)[2],
  rsq_func(de_perf[1:100],p_noise_a,n=length(measure_index),t=measure_index)[1],
  rsq_func(de_perf[1:100],p_noise_a,n=length(measure_index),t=measure_index)[2],
  # Block B
  rmse_func(de_perf[101:200],p_true_b,n=length(s2),t=s2),
  rmse_func(de_perf_alt[101:200],p_true_alt_b,n=length(s2),t=s2),
  rsq_func(de_perf[101:200],p_true_b,n=length(s2),t=s2)[1],
  rsq_func(de_perf[101:200],p_true_b,n=length(s2),t=s2)[2],
  rsq_func(de_perf_alt[101:200],p_true_alt_b,n=length(s2),t=s2)[1],
  rsq_func(de_perf_alt[101:200],p_true_alt_b,n=length(s2),t=s2)[2]
)

names(de_modelling) <- c("rmse_a_true",
  "rmse_a_noisy",
  "r2_a_true",
  "r2_adj_a_true",
  "r2_a_noisy",
  "r2_adj_a_noisy",
  "rmse_b_true",
  "rmse_b_alt_true",
  "r2_b_true",
  "r2_adj_b_true",
  "r2_b_alt_true",
  "r2_adj_b_alt_true"
  )

write.csv(de_modelling, file = paste0("out/case_",case,"/freq_",
                                       freq_index,"/opt/de/de_modelling.csv"))

# Plot model fit and forecast (Primary and Alternative TL Series)

pdf(paste0("out/case_",case,"/freq_",freq_index,"/opt/de/primary_tl.pdf"),
    width = 10, height = 8)
plot(s,p_true_all,type ="l",col="blue",
     main = paste0("DE Fit and Forecast (",signif(de_pars[1],3),",",
                                           signif(de_pars[2],3),",",
                                           signif(de_pars[3],3),",",
                                           signif(de_pars[4],3),")"),
     sub = paste0("Cost fn val: ",signif(de_cost,4)," (4.s.f)"),
     ylab = "Performance (Arbitrary Unit)",
     xlab = "Day",
     ylim = c(min(c(de_perf,p_noise_all,p_true_all),na.rm=TRUE)-50,
              max(c(de_perf,p_noise_all,p_true_all),na.rm=TRUE)+50))
points(s,p_noise_all,col="black",pch=18)
lines(s,de_perf,lty=2,col="red")
legend("bottomright", legend = c("'True' Perf - Primary TL",
                                 "'Noisy' Perf (Fitting Vals)",
                                 "Fit/Forecast - Primary TL"),
       fill = c("blue","black","red"))
dev.off()

setEPS(width = 10, height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,"/opt/de/primary_tl.eps"))
plot(s,p_true_all,type ="l",col="blue",
     main = paste0("DE Fit and Forecast (",signif(de_pars[1],3),",",
                                           signif(de_pars[2],3),",",
                                           signif(de_pars[3],3),",",
                                           signif(de_pars[4],3),")"),
     sub = paste0("Cost fn val: ",signif(de_cost,4)," (4.s.f)"),
     ylab = "Performance (Arbitrary Unit)",
     xlab = "Day",
     ylim = c(min(c(de_perf,p_noise_all,p_true_all),na.rm=TRUE)-50,
              max(c(de_perf,p_noise_all,p_true_all),na.rm=TRUE)+50))
points(s,p_noise_all,col="black",pch=18)
lines(s,de_perf,lty=2,col="red")
legend("bottomright", legend = c("'True' Perf - Primary TL",
                                 "'Noisy' Perf (Fitting Vals)",
                                 "Fit/Forecast - Primary TL"),
       fill = c("blue","black","red"))
dev.off()

pdf(paste0("out/case_",case,"/freq_",freq_index,"/opt/de/alternative_tl.pdf"),
    width = 10, height = 8)
plot(s,p_true_alt,type ="l",col="darkgreen",
     main = paste0("DE Fit and Forecast (",signif(de_pars[1],3),",",
                                           signif(de_pars[2],3),",",
                                           signif(de_pars[3],3),",",
                                           signif(de_pars[4],3),")"),
     sub = paste0("Cost fn val: ",signif(de_cost,4)," (4.s.f) | ",
                  "True Parameters: (",signif(par_true[1],4),",",
                                           signif(par_true[2],4),",",
                                           signif(par_true[3],4),",",
                                           signif(par_true[4],4),")"),
     ylab = "Performance (Arbitrary Unit)",
     xlab = "Day",
     ylim = c(min(c(de_perf_alt,p_noise_all,p_true_all),na.rm=TRUE)-50,
              max(c(de_perf_alt,p_noise_all,p_true_all),na.rm=TRUE)+50))
points(s,p_noise_all,col="black",pch=18)
lines(s,de_perf_alt,lty=2,col="red")
legend("bottomright", legend = c("'True' Perf - Alt TL",
                                 "'Noisy' Perf (Fitting Vals)",
                                 "Fit/Forecast - Alt TL"),
       fill = c("darkgreen","black","red"))
dev.off()

setEPS(width = 10, height = 8)
postscript(paste0("out/case_",case,"/freq_",freq_index,"/opt/de/alternative_tl.eps"))
plot(s,p_true_alt,type ="l",col="darkgreen",
     main = paste0("DE Fit and Forecast (",signif(de_pars[1],3),",",
                                           signif(de_pars[2],3),",",
                                           signif(de_pars[3],3),",",
                                           signif(de_pars[4],3),")"),
     sub = paste0("Cost fn val: ",signif(de_cost,4)," (4.s.f) | ",
                  "True Parameters: (",signif(par_true[1],4),",",
                                           signif(par_true[2],4),",",
                                           signif(par_true[3],4),",",
                                           signif(par_true[4],4),")"),
     ylab = "Performance (Arbitrary Unit)",
     xlab = "Day",
     ylim = c(min(c(de_perf_alt,p_noise_all,p_true_all),na.rm=TRUE)-50,
              max(c(de_perf_alt,p_noise_all,p_true_all),na.rm=TRUE)+50))
points(s,p_noise_all,col="black",pch=18)
lines(s,de_perf_alt,lty=2,col="red")
legend("bottomright", legend = c("'True' Perf - Alt TL",
                                 "'Noisy' Perf (Fitting Vals)",
                                 "Fit/Forecast - Alt TL"),
       fill = c("darkgreen","black","red"))
dev.off()

rm(de_pars, de_cost, de_optimisation, de_runtime_end, de_runtime_start,
      de_modelling, de_perf, de_perf_alt)
gc()