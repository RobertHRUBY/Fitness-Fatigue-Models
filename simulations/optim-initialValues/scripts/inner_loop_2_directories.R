# Primary Inner Loop Directory
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets"))

# Main Sub Directories of inner Loop 2
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust"))

# Clustering sub-directories (L-BFGS-B and CMA-ES in same directory)
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/clust/bad"))

# Parameters found sub-directories (L-BFGS-B and CMA-ES in same directory)
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/pars/bad"))

# Visual sub-directories (L-BFGS-B and CMA-ES in same directory)
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad"))

dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm/quant"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge/quant"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid/quant"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good/quant"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad/quant"))

dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm/all"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge/all"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid/all"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good/all"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad/all"))

dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm/kmeans"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge/kmeans"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid/kmeans"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good/kmeans"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad/kmeans"))

dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm/pam"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge/pam"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid/pam"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good/pam"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad/pam"))

dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/norm/min"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/edge/min"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/mid/min"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/good/min"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/vis/bad/min"))

# Initial Parameters Sub-Directory
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/init/bad"))

# Performance Values Sub-Directories
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/perf/bad"))

# Raw Data Sub-Directories
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw/norm"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw/edge"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw/mid"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw/good"))
dir.create(path = paste0("out/case_",case,"/freq_",freq_index,
                         "/opt/",n_starting_sets,"_sets","/raw/bad"))