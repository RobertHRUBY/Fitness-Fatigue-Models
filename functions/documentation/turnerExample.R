# Source turnerModel.R function directly from github
library(devtools)
source_url(
  "https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/functions/turnerModel.R?token=AGTSF7JGOYO7CRF3GRJRCK27WRAIU"
)

# Source 'true' data built within the Turner function using a defined par set
library(RCurl)
trueData = getURL("https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/functions/data/turner_testing/trueData.csv?token=AGTSF7NPHMTS3TLOTVZUURK7WRAVO")
trueData = read.csv(textConnection(trueData))

# Note the true parameters used to construct this set
truePars = c(k1 = 1, k2 = 1.5, T1 = 19, T2 = 11, alpha = 1.3, beta = 1.5, 
             p0 = 125, g0 = 30, h0 = 10)

# Construct a 'noisy' set of data from our true set, we will use this later on
set.seed(101)

noise = 5
mockData = data.frame("days" = 1:100,
                       "performances" = trueData$performances + 
                                        rnorm(100, 0, noise),
                       "loads" = trueData$loads)

# Further subset this data set to represent a set of data without a daily value
freq = 2  # As in every 2 days
measureIndex = seq(1, 100, by = freq)
measurements = rep(NA, 100)
measurements[measureIndex] = mockData$performances[measureIndex]
mockData$performances = measurements
rm(measurements, measureIndex)

# Create some reasonable box constraints on the optimisation
# Notes:

#   kg/kh: max(mockData$loads)*3 = 270 which is higher than any of the
#          performance measures. So it is reasonable to bound this scaling
#          constant here, or even perhaps lower. We bound below just above zero.

#   Tg/Th: Effects from a single session unlikely to last beyond 50 days
#          so we bound about here, and below at 1 day.

#   a/B:   alpha and beta are trickier to bound, but we assume the exponent
#          nonlinearities of the model are unlikely to be above say 2 as this
#          would create some large values unless (speak to Ben about this)

#   p0:    mean(mockData$performances, na.rm = TRUE) = 202.4071 so we can
#          probably be safe bounding at 250 or so, but to not constrain too
#          tightly we bound at 300

#   g0/h0: Initial conditions are a bit trickier. So we leave these a bit wider
#          as below. Up to the same as p0

boxConstraints = data.frame("lower"=c(0.01, 0.01, 1, 1, 0.1, 0.1, 25, 0.1, 0.1),
                            "upper"=c(3, 3, 50, 50, 2, 2, 300, 200, 200))

# Now we see if we can recover close to the true parameters with the true data 
# as input. This forms the first basic test.

test1 = turnerModel(inputData = trueData, constraints = boxConstraints,
                    doTrace = TRUE, doParallel = TRUE)

# Second, we see what sort of results we get back for the mockData when fitting
# noisy data that should be somewhat close to the true data

test2 = turnerModel(inputData = mockData, constraints = boxConstraints,
                    doTrace = TRUE, doParallel = TRUE)