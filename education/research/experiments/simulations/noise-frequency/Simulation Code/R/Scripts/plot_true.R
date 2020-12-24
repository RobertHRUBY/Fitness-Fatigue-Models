# ===================================(3.4.1)====================================
# Script: Plot true performance and save to output directory
# ==============================================================================

# 1. Function to take performance values and plot true performance trajectories 

PlotTruePerformance <- function(perf_trimps1, perf_trimps2, ath_name){
  tempdf <- melt(cbind(perf_trimps1, perf_trimps2))
  min_y <- round(min(perf_trimps1, perf_trimps2) - 50, -2)
  max_y <- round(max(perf_trimps1, perf_trimps2) + 50, -2)
  p <- ggplot(tempdf, aes(x=Var1, y=value, group = Var2)) + 
   geom_line(aes(linetype = Var2)) + theme_classic() + 
    scale_x_continuous(breaks = seq(1,112,5), limits = c(1,112)) + 
     geom_point(size = 1) + 
      scale_y_continuous(breaks = seq(min_y, max_y, 150), 
        limits = c(min_y,max_y)) + theme(legend.position = 'none') + 
          ggtitle(ath_name) + xlab("Day") + ylab("Power Ouput (W) - VJ")
  return(p)
}

# 2. Generate individual plots for the intermediate (p1) and advanced (p2)

temp_p1 <- PlotTruePerformance(true_int_1, true_int_2, "Intermediate Athlete")
temp_p2 <- PlotTruePerformance(true_adv_1, true_adv_2, "Advanced Athlete")

# 3. Generate stacked plot, and save to output directory

pdf("Out/True/true_performance.pdf", width = 7, height = 5)
multiplot(temp_p1, temp_p2, cols = 1)
dev.off()

# 4. Remove data from environment and the plot function

rm(temp_p1, temp_p2)
rm(PlotTruePerformance)