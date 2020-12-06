# Load required packages
library(Rcurl)
library(devtools)

# Load mock data and import R function
mockData <- getURL("https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/documentation/data/mockSeries1.csv")
mockData <- read.csv(textConnection(mockData))
source_url("https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/calvertModel.R")

# Inspect mock data structure and contents
head(mockData)
plot(mockData$load, type = "h", xlab = "day", ylab = "Training Load",
     main = "Mock training load data")
plot(mockData$performances, type = "p", xlab = "day", ylab = "Long Jump (m)", 
     pch = 20, main = "Mock performance data")

# Set up constraints and fit the model ~ NOTE c(p*, kg, Tg1, Tg2, kh, Th)
boxConstraints <- data.frame("lower" = c(5.0, 0.001, 1, 1, 0.001, 1),
                             "upper" = c(6.0, 1, 50, 25, 1, 50))
boxConstraintsInitial <- data.frame("lower" = c(5.0, 0.001, 1, 1, 0.001, 1, 0.1, 0.1),
                                    "upper" = c(6.0, 1, 50, 25, 1, 50, 100, 100))

# Fit via first-order approach with second-order approximation
startAt <- c(5.1, 0.1, 40, 5, 0.2, 6) # Starting values c(p*, kg, Tg1, Tg2, kh, Th)
startAt2 <- c(5.1, 0.1, 40, 5, 0.2, 6, 5, 10) # Starting values c(p*, kg, Tg1, Tg2, kh, Th, qg, qh)

# With no initial component
fittedModel_bfgs <- calvertModel(inputData = mockData,
                                  constraints = boxConstraints,
                                  startingValues = startAt,
                                  doTrace = TRUE)
plot(fittedModel_bfgs$main$predictions, type="l", xlab = "day", 
     ylab = "long jump (m)")
points(mockData$performances, pch = 20)

# With an initial component
fittedModel_bfgs_initial <- calvertModel(inputData = mockData,
                                          constraints = boxConstraintsInitial,
                                          startingValues = startAt2,
                                          initialComponent = TRUE,
                                          doTrace = TRUE)
points(mockData$performances, pch = 20)

# Fit via genetic algorithm and no initial component
fittedModel_genetic <- calvertModel(inputData = mockData,
                                     constraints = boxConstraints,
                                     method = "ga",
                                     doTrace = TRUE)
plot(fittedModel_genetic$main$predictions, type="l", xlab = "day", 
     ylab = "long jump (m)")
points(mockData$performances, pch = 20)

# Comparison of the three
plot(fittedModel_bfgs_initial$main$predictions, type="l", col = "green")
lines(fittedModel_bfgs_initial$main$predictions, col = "blue")
lines(fittedModel_genetic$main$predictions, col = "red")

