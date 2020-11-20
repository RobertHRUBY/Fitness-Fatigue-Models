# Utilities: An R toolbox

Bespoke functions, written in [R](https://www.r-project.org/), for fitting and evaluating FFMs with modern optimisers, cross-validation, and input checking. Complete [documentation]() with examples is provided for all the functions. Below are the listed functions:

<insert table>

## Getting started: An introductory example

Consider a researcher looking to fit a fitness-fatigue model to a set of experimental data (i.e. a time-series of quantified training load and measured performance values).

Note, you will need to install the following packages (`RCurl`,`devtools`) to follow along in your own R interpreter (e.g. [R-studio](https://rstudio.com/)). You can do this by running the following code:

    install.packages("RCurl")
    install.packages("devtools")

### 1. Load data

To begin with, the researcher imports or loads their data into the environment. For the purposes of the example, we import a set of mock data provided [here](documentation/data/mockData.csv) using the code below:

    library(RCurl)
    mockData = getURL()
    mockData = read.csv(textConnection(mockData))

Inspect the structure of the dataset. It should be in three column form, in order "days", "performances", "loads". NA values should be used in the performances column to indicate missing observed data, and load values of zero indicate that no training occurred on a given day.

    head(mockData)
    days   performances    loads
       1          466.2    56.35
       2             NA     0.00
       3          440.5    59.15
       4             NA   110.60
       5          402.3     0.00
       6          418.9     0.00
    
*Note: The order of the data, and appropriate use of NA and zero values as described is important, and this format is consistent across all the functions.*

### 2. Import the model function

You can read more about the available functions and associated code files in the [documentation](documentation/utilities.pdf). However, for this example we will consider the standard fitness-fatigue model, described as follows:

<img src="https://latex.codecogs.com/svg.latex?\hat{p}(t)&space;=&space;p^*&space;&plus;&space;k_g&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" title="\hat{p}(t) = p^* + k_g \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" />




## Model fitting resources

| Code file | Associated function(s) | Functionality |
|-|-|-|
| [basicModel.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/basicModel.R) | basicModel() | Function to fit the discrete one-component FFM (with optional initial component) |
| standardModel.R | standardModel() | Function to fit the discrete two-component FFM (with optional initial components on fitness and fatigue) |
| banisterModel.R | banisterModel() | Solve and fit the original first-order linear system of differential equations. Choice of a one or two component system |
| [turnerModel.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/turnerModel.R) | turnerModel() | Solve and fit the non-linear variant of the original model system proposed by Turner et al. (2017) |
| calvertModel.R | calvertModel() | Function to fit the discrete two-component FFM with exponential delay on fitness component |
| vdrModel.R | vdrModel() | Functions to fit the variable dose-response model with and without an external Hill transform (threshold saturation) fitted and applied to the training load values  |
| [kalmanModel.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/kalmanModel.R) | ... | Functions to model fitness and fatigue as latent state vector in Kalman Filter framework |

## Other resources

| File | Associated function(s) | Functionality |
|-|-|-|
| [computeModels.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/computeModels.R) | computeModels() | Function to compute model predictions for a selected FFM given a definite set of parameter values and training load series (plus initial conditions if appropriate) |