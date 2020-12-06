# ===================================(3.4.1)====================================
# Script: Plot TRIMP values as a grid, and save to output directory
# ==============================================================================

# 1. Function to take TRIMP values and generate geom_bar using ggplot2

TrimpsPlot <- function(trimp_vals, plot_title){
  trimp_df <- data.frame("Day" = c(1:112), "Values" = trimp_vals)
  p <- ggplot(trimp_df, aes(x = Day, y = Values)) + geom_bar(stat = "identity",
    fill = "black") + theme_classic() + 
      scale_x_continuous(breaks = seq(1,112,14), limits = c(1,112)) +
        scale_y_continuous(breaks = seq(0,400,100), limits = c(0,400)) + 
          xlab("Day (t)") + ylab("TRIMPS (a.u)") + 
            ggtitle(plot_title)
  return(p)
}

# 2. Generate individual plots

temp_p1 <- TrimpsPlot(trimps_1_int, "INT-TRIMPS-1") # Summated microcycles
temp_p2 <- TrimpsPlot(trimps_2_int, "INT-TRIMPS-2") # Wave loaded
temp_p3 <- TrimpsPlot(trimps_1_adv, "ADV-TRIMPS-1") # Summated microcycles
temp_p4 <- TrimpsPlot(trimps_2_adv, "ADV-TRIMPS-2") # Wave loaded

# 3. Generate TRIMPS-Grid, and save to output directory

pdf("Out/TRIMPS/barplot.pdf", width = 6, height = 4)
multiplot(temp_p1, temp_p2, temp_p3, temp_p4, cols = 2)
dev.off()

# 4. Remove data from environment and the plot function

rm(temp_p1, temp_p2, temp_p3, temp_p4)
rm(TrimpsPlot)