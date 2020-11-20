# Primary Directory for the current measurement frequency      
dir.create(path = paste0("out/case_",case,"/freq_",freq_index))

   # Sub-Directory for the case data
   dir.create(path = paste0("out/case_",case,"/freq_",freq_index,"/case_data"))
   
   # Sub-Directory for the optimisation results
   dir.create(path = paste0("out/case_",case,"/freq_",freq_index,"/opt"))   
   
      # Sub-Directory for the DE Results
      dir.create(path = paste0("out/case_",case,"/freq_",freq_index,"/opt/de"))
   
   # Sub-Directory for the grid results
   dir.create(path = paste0("out/case_",case,"/freq_",freq_index,"/grid"))
   
      # Sub-Directory for the raw files
      dir.create(path=paste0("out/case_",case,"/freq_",freq_index,"/grid/raw"))
      
      # Sub-Directory for the grid results under a selected threshold
      dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                               "/grid/threshold"))
      
         # Sub-Directory for the parameter values
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/pars"))
         
         # Sub-Directory for summary statistics
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/stats"))
         
         # Sub-Directory for Ribbon Plots
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/ribbons"))
         
         # Sub-Directroy for Performance Data
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/perf"))
         
         # Sub-Directory for Individual Plots (Primary TL Distribution)
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/individual_plots"))
         
         # Sub-Directory for Individual Plots (Alternative TL Distribution)
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/individual_plots_alt"))
         
         # Sub-Directory for the Clustering
         dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                                  "/grid/threshold/clustering"))
      
      
      
      