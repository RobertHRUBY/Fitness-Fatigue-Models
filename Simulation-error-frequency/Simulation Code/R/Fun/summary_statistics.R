# ===================================(3.4.1)====================================
# Functions related to summary statistic calculation and prediction errors
# ==============================================================================

# Transform Prediction Values into Prediction Errors (signed (Boolean))

TransformPred <- function(list_pred_vals, true_perf_vals, signed){
  tempList <- list()
  for (index in 1:35){
    if (signed == TRUE){
      tempList[[index]] <- list_pred_vals[[index]] - true_perf_vals
      }
    if (signed == FALSE){
      tempList[[index]] <- abs(list_pred_vals[[index]] - true_perf_vals)
      }
  }
  return(tempList)
}

# Compute DPE Values for the whole second block and isolated portions (1st/last
# week of the block - extra check for heteroscedasticity)

ComputeDPE <- function(list_of_errors){
  
  errors_first <- lapply(list_of_errors, "[", 1:7, 1:nSims, drop = FALSE)
  errors_last <- lapply(list_of_errors, "[", 50:56, 1:nSims, drop = FALSE)
  
  # Calculate Column Totals of Absolute Errors across each DF in the lists
  totals_first <- lapply(errors_first, colSums)
  totals_last <- lapply(errors_last, colSums)
  totals_whole <- lapply(list_of_errors, colSums)
  
  # Set up Objects
  DPE_s_first <- NULL; DPE_s_last <- NULL; DPE_s_whole <- NULL;
  DPE_m_first <- NULL; DPE_m_last <- NULL; DPE_m_whole <- NULL;
  # Calculate Values
  for (i in 1:35){
    DPE_s_first[i] <- diff(as.numeric(quantile(totals_first[[i]], 
      probs = c(0.16,0.84), na.rm = TRUE)))/7
    DPE_s_last[i] <- diff(as.numeric(quantile(totals_last[[i]], 
      probs = c(0.16,0.84), na.rm = TRUE)))/7
    DPE_s_whole[i] <- diff(as.numeric(quantile(totals_whole[[i]], 
      probs = c(0.16,0.84), na.rm = TRUE)))/56
    DPE_m_first[i] <- median(totals_first[[i]])/7
    DPE_m_last[i] <- median(totals_last[[i]])/7
    DPE_m_whole[i] <- median(totals_whole[[i]])/56
    } # End for-loop
  sim_freq <- c(rep('ED', 5), rep('E2D', 5), rep('E3D', 5), rep('E4D', 5),
                rep('E5D', 5), rep('E6D', 5), rep('E7D', 5))
  sim_err <- rep(seq(2,10,2), 7)
  temp_df <- data.frame(c(1:35), sim_err, sim_freq, DPE_s_first, DPE_s_last,
    DPE_s_whole, DPE_m_first, DPE_m_last, DPE_m_whole)
  colnames(temp_df) <- c("Simulation", "Error", "Frequency", "DPE_s_first",
    "DPE_s_last", "DPE_s_whole", "DPE_m_first", "DPE_m_last", "DPE_m_whole")
  return(temp_df)
}

# Time Chunk Analysis - Heteroscedasticity

TimeChunks <- function(errors_signed){
  temp_list <- list()
  for (i in 1:35){
    temp_df <- data.frame()
    for (j in 1:4){
      if (j == 1){
        temp_df[1,1] <- quantile(errors_signed[[i]][1:14,], probs = 0.16,
          na.rm = TRUE)
        temp_df[1,2] <- quantile(errors_signed[[i]][1:14,], probs = 0.84,
          na.rm = TRUE)
        temp_df[1,3] <- median(errors_signed[[i]][1:14,],  na.rm = TRUE)
        temp_df[1,4] <- mean(errors_signed[[i]][1:14,],  na.rm = TRUE)
        temp_df[1,5] <- sd(errors_signed[[i]][1:14,],  na.rm = TRUE)
        temp_df[1,6] <- IQR(errors_signed[[i]][1:14,],  na.rm = TRUE)
        }
      if (j == 2){
        temp_df[2,1] <- quantile(errors_signed[[i]][15:28,], probs = 0.16,
          na.rm = TRUE)
        temp_df[2,2] <- quantile(errors_signed[[i]][15:28,], probs = 0.84,
          na.rm = TRUE)
        temp_df[2,3] <- median(errors_signed[[i]][15:28,],  na.rm = TRUE)
        temp_df[2,4] <- mean(errors_signed[[i]][15:28,],  na.rm = TRUE)
        temp_df[2,5] <- sd(errors_signed[[i]][15:28,],  na.rm = TRUE)
        temp_df[2,6] <- IQR(errors_signed[[i]][15:28,],  na.rm = TRUE)
        }
      if (j == 3){
        temp_df[3,1] <- quantile(errors_signed[[i]][29:42,], probs = 0.16,
          na.rm = TRUE)
        temp_df[3,2] <- quantile(errors_signed[[i]][29:42,], probs = 0.84,
          na.rm = TRUE)
        temp_df[3,3] <- median(errors_signed[[i]][29:42,],  na.rm = TRUE)
        temp_df[3,4] <- mean(errors_signed[[i]][29:42,],  na.rm = TRUE)
        temp_df[3,5] <- sd(errors_signed[[i]][29:42,],  na.rm = TRUE)
        temp_df[3,6] <- IQR(errors_signed[[i]][29:42,],  na.rm = TRUE)
      }
      if (j == 4){
        temp_df[4,1] <- quantile(errors_signed[[i]][43:56,], probs = 0.16,
          na.rm = TRUE)
        temp_df[4,2] <- quantile(errors_signed[[i]][43:56,], probs = 0.84,
          na.rm = TRUE)
        temp_df[4,3] <- median(errors_signed[[i]][43:56,],  na.rm = TRUE)
        temp_df[4,4] <- mean(errors_signed[[i]][43:56,],  na.rm = TRUE)
        temp_df[4,5] <- sd(errors_signed[[i]][43:56,],  na.rm = TRUE)
        temp_df[4,6] <- IQR(errors_signed[[i]][43:56,],  na.rm = TRUE)
      }
      colnames(temp_df) <- c('0.16q', '0.84q', 'median', 'mean','SD', 'IQR')
      temp_list[[i]] <- temp_df
    }
  }
  return(temp_list)
}

# Plot the Scatterplots of Error and Frequency

PlotScatters <- function(DPE_data){
  max_val <- round(max(DPE_data$DPE_s_whole, digits = 0)) + 200
  plot <- ggplot(DPE_data, aes(Frequency, DPE_s_whole, colour = factor(Error)))+
    geom_smooth(se = FALSE, method = "lm") +
      geom_point(aes(shape= factor(Error), fill = factor(Error)), 
        colour = "black", size = 2) + 
          scale_shape_manual(values = c(21,22,23,24,25)) + theme_base() +
            scale_x_continuous(breaks = seq(1,7,1), limits = c(1,7)) +
              scale_y_continuous(breaks = seq(0, max_val, round(max_val/5)), 
                limits = c(0, max_val))
  return(plot)
}

# Compute the multiple regressions

buildregression <- function(DPE_spread){
 lm.temp <- lm(DPE_spread$DPE_s_whole ~ DPE_spread$Frequency + DPE_spread$Error)
 lm.temp.interaction <- lm(DPE_spread$DPE_s_whole ~ DPE_spread$Frequency + 
    DPE_spread$Error + DPE_spread$Error*DPE_spread$Frequency)
 regressionoutput <- list("Linear_Model" = lm.temp, 
  "lm_summary" = summary.lm(lm.temp), "With_interaction" = lm.temp.interaction, 
    "interaction_summary" = summary.lm(lm.temp.interaction), 
     "VIF_lm" = vif(lm.temp))
 return(regressionoutput)
}

# Determine correlation coefficients between the parameters

CorrelatePars <- function(parameters, pars){
  # pars = c('k1','k2') or c('T1','T2')
  correlations <- as.numeric()
  for (i in 1:35){
    temp_data <- as.data.frame(t(rbind(parameters[[i]][pars,])))
    correlations[i] <- cor(temp_data[,pars[1]], temp_data[,pars[2]], 
       use = "complete.obs")
  }
  return(correlations)
}

CorrelatePars2 <- function(parameters, parT){
  # Correlate k1/k2 ratio with T1 or T2
  correlations <- as.numeric()
  for (i in 1:35){
    temp_data <- as.data.frame(t(rbind(parameters[[i]][c('k1','k2',parT),])))
    temp_data$k1k2 <- temp_data[,'k1']/temp_data[,'k2']
    correlations[i] <- cor(temp_data[,parT], temp_data[,'k1k2'],
      use = "complete.obs")
  }
  return(correlations)
}

 # Transform Parameters for Plotting
  subsetParameterList <- function(parameters_list){
    k1 <- melt(lapply(parameters_list, "[", 1, 1:nSims, drop = FALSE))
    colnames(k1) <- c("param", "sim", "value", "scenario")
    T1 <- melt(lapply(parameters_list, "[", 2, 1:nSims, drop = FALSE))
    colnames(T1) <- c("param", "sim", "value", "scenario")
    k2 <- melt(lapply(parameters_list, "[", 3, 1:nSims, drop = FALSE))
    colnames(k2) <- c("param", "sim", "value", "scenario")
    T2 <- melt(lapply(parameters_list, "[", 4, 1:nSims, drop = FALSE))
    colnames(T2) <- c("param", "sim", "value", "scenario")
    temp_list <- list("k1" = k1, "k2" = k2, "T1" = T1, "T2" = T2)
    return(temp_list)
  }
  
 # Plot Parameter Distributions
   plotParameters <- function(plist_sub, plabel){
    ptemp <- ggplot(data = plist_sub, aes(x = value)) +geom_histogram(bins = 25, 
      fill = "#880011") + facet_wrap(~scenario, ncol = 5, scales = "free") +
        labs(x="Parameter Value", y="Count") +
          ggtitle(paste0("Histogram of Parameter values (", plabel ,
            ") by scenario"))
    return(ptemp)
  }
