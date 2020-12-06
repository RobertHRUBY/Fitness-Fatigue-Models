# Load required packages
library(Rcurl)
library(devtools)

# Load mock data and import R function
mockData <- getURL("https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/documentation/data/mockSeries1.csv")
mockData <- read.csv(textConnection(mockData))
source_url("https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/computeModels.R")

loads <- mockData$loads

# Establish some definite parameter sets (representing fitted models)
  
  # c(p*, kg, Tg, kh, Th)
  standardPars <- c(100,1.5,30,2,10)
  # c(p*, kg, Tg, kh, Th, qg, qh) - Includes initial components
  standardParsIC <- c(100,1.5,30,2,10,25,15)
  # c(p*, kg, Tg1, Tg2, kh, Th)
  calvertPars <- c(100,1.5,35,2,2,10)
  # c(p*, kg, Tg1, Tg2, kh, Th) - Includes initial components
  calvertParsIC <- c(100,1.5,35,2,2,10,25,10)
  # c(kg, kh, Tg, Th, alpha, beta, p*, g0, h0)
  turnerPars <- c(1.5,2,30,10,0.9,0.95,100,25,15)
  # c(kg, kh, Tg, Th, p*, g0, h0)
  banisterPars <- c(1.5,2,30,10,100,25,15)
  
# Compute the model values
  
  standardModel <- computeModels(model = "standard",
                                 parms = standardPars,
                                 loadSeries = loads)
  standardModelIC <- computeModels(model = "standardIC",
                                 parms = standardParsIC,
                                 loadSeries = loads)
  calvertModel <- computeModels(model = "calvert",
                                parms = calvertPars,
                                loadSeries = loads)
  calvertModelIC <- computeModels(model = "calvertIC",
                                parms = calvertParsIC,
                                loadSeries = loads)
  turnerModel <- computeModels(model = "turner",
                               parms = turnerPars,
                               loadSeries = loads)
  banisterModel <- computeModels(model = "banister",
                                 parms = banisterPars,
                                 loadSeries = loads)
  
  ymax = max(banisterModel$pHat, turnerModel$pHat,
             calvertModel, calvertModelIC, standardModel$performance, 
             standardModelIC$performance)
  
  plot(banisterModel$pHat, col = "red", type = "l", ylim = c(0,ymax))
  lines(turnerModel$pHat, col = "blue")
  lines(calvertModel, col = "green")
  lines(calvertModelIC, col = "green")
  lines(standardModel$performance, col = "orange")
  lines(standardModelIC$performance, col = "orange")
  