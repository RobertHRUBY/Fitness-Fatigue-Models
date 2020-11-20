# Training Load Distribution
  
  # Primary Training Load Series
  pdf(paste0("out/case_",case,"/freq_",freq_index,"/case_data/primary_load.pdf"), 
      width = 7, height = 5)
  plot(ws_all, type = "h",
       main = paste0("Case ",case," | Distribution ",cases$true_load[case]," | Primary TL Series"),
       xlab = "Day",
       ylab =  c("Training Load (Arbitrary Unit)"), col = "black")
  dev.off()
  
    # EPS Version (For Publications/Thesis)
    setEPS(width = 7, height = 5)
    postscript(paste0("out/case_",case,"/freq_",freq_index,"/case_data/primary_load.eps"))
    plot(ws_all, type = "h", 
       main = paste0("Case ",case," | Distribution ",cases$true_load[case]," | Primary TL Series"),
       xlab = "Day",
       ylab =  c("Training Load (Arbitrary Unit)"), col = "black")
    dev.off()
  
  
  # Alternative Training Load Series (Both Blocks)
  pdf(paste0("out/case_",case,"/freq_",freq_index,"/case_data/alt_load.pdf"), 
      width = 7, height = 5)
  plot(ws_alt_all, type = "h", 
       main = paste0("Case ",case," | Alternative TL Series (Block A&B)"),
       ylab = c("Training Load (Arbitrary Unit)"), xlab = "Day",
       col = "black"
       )
  dev.off()
  
    # EPS Version (For Publications/Thesis)
    setEPS(width = 7, height = 5)
    postscript(paste0("out/case_",case,"/freq_",freq_index,"/case_data/alt_load.eps"))
     plot(ws_alt_all, type = "h", 
       main = paste0("Case ",case," | Alternative TL Series (Block A&B)"),
       ylab = c("Training Load (Arbitrary Unit)"), xlab = "Day",
       col = "black"
       )
    dev.off()
  
  # Alternative Training Load Series (Second Block Only)
  pdf(paste0("out/case_",case,"/freq_",freq_index,"/case_data/alt_load_b.pdf"), 
      width = 7, height = 5)
  plot(ws_alt, type = "h", 
       main = paste0("Case ",case," | Alternative TL Series (Block B Only)"),
       ylab = c("Training Load (Arbitrary Unit)"), xlab = "Day",
       )
  dev.off()
  
    # EPS Version (For Publications/Thesis)
    setEPS(width = 7, height = 5)
    postscript(paste0("out/case_",case,"/freq_",freq_index,"/case_data/alt_load_b.eps"))
    plot(ws_alt, type = "h", 
       main = paste0("Case ",case," | Alternative TL Series (Block B Only)"),
       ylab = c("Training Load (Arbitrary Unit)"), xlab = "Day",
       )
    dev.off() 
  
# Visualise and print the 'true' and 'noisy' performance values
  
  # Plot Arguments
  perfplot_title  <- paste0("Mock Case ",case," Performance Profile")
  perfplot_ylab   <- c("Performance (Arbitrary Unit)")
  perfplot_xlab   <- c("Day")
  perfplot_legend <- c("'True' Perf - Primary TL",
                       "'True' Perf - Alt TL",
                       "'Noisy' Perf (Fitting Vals)"
                      )
  perfplot_ylim <- c(min(c(p_noise_all,p_true_all,p_true_alt),na.rm = TRUE)-50,
                     max(c(p_noise_all,p_true_all,p_true_alt),na.rm = TRUE)+50)
  
  # Generate Plot
  pdf(paste0("out/case_",case,"/freq_",freq_index,"/case_data/performance.pdf"), 
      width = 8, height = 6)
  plot(s, p_true_alt, type = "l", col = "darkgreen", main = perfplot_title,
          ylab = perfplot_ylab, xlab = perfplot_xlab,
          ylim = perfplot_ylim,
          sub = paste0("True Parameters: (",signif(par_true[1],3),",",
                                           signif(par_true[2],3),",",
                                           signif(par_true[3],3),",",
                                           signif(par_true[4],3),")"))
  points(s, p_noise_all, col = "black", pch = 18)
  lines(s, p_true_all, col = "blue")
  legend("bottomright", perfplot_legend, fill = c("blue","darkgreen","black"))
  dev.off()
  
  # EPS Version
  setEPS(width = 8, height = 6)
  postscript(paste0("out/case_",case,"/freq_",freq_index,"/case_data/performance.eps"))
  plot(s, p_true_alt, type = "l", col = "darkgreen", main = perfplot_title,
          ylab = perfplot_ylab, xlab = perfplot_xlab,
          ylim = perfplot_ylim,
          sub = paste0("True Parameters: (",signif(par_true[1],3),",",
                                           signif(par_true[2],3),",",
                                           signif(par_true[3],3),",",
                                           signif(par_true[4],3),")"))
  points(s, p_noise_all, col = "black", pch = 18)
  lines(s, p_true_all, col = "blue")
  legend("bottomright", perfplot_legend, fill = c("blue","darkgreen","black"))
  dev.off()
  
  rm(perfplot_title, perfplot_ylab, perfplot_xlab, perfplot_legend,
     perfplot_ylim)
  