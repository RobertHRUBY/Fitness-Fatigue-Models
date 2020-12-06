# Utilities: An R toolbox for fitting and evaluating FFMs

This directory includes easy to use [R](https://www.r-project.org/) functions for fitting and evaluating FFMs with modern optimizer, cross-validation, and input checking. Complete [documentation](documentation/) is provided for each. Furthermore, code files with reproducible examples are provided [here](documentation/examples).

## Currently available functions:

| Function | Code file | Description | Usage | Optimizer | Features |
|-|-|-|-|-|-|
| `banisterModel()` | [banisterModel.R](banisterModel.R) | Numerically solve system of ODE's and fit the standard model from first principles | [docs]() [example]() | GA | Bounds, estimation of initial conditions, cross validation, genetic algorithms |
| `standardModel()` | [standardModel.R](standardModel.R) | Fitting function for the explicit solution to the standard two component model | [docs]() [example](documentation/examples/standardModelExample.R) | GA or L-BFGS-B | Bounds, include estimation of initial components, cross validation, genetic algorithms |
| `calvertModel()` | [calvertModel.R](calvertModel.R) | Fitting function for the explicit solution to the fitness-delay model | [docs]() [example]() | GA or L-BFGS-B | Bounds, include estimation of initial components, cross validation, genetic algorithms |
| `turnerModel()` | [turnerModel.R](turnerModel.R) | Numerically solve system of ODE's and fit the non-linear variant of the standard model | [docs]() [example]() | GA | Bounds, estimation of initial conditions, cross validation |
| `computeModels()` | [computeModels.R](computeModels.R) | Compute model predictions for a definite set of model parameters and load series | [docs]() [example]() | NA - Simple computation | Compute with or without initial traces for discrete models |

### Experimental features:

These are functions that are currently under experimental development. They will become full features of the project, or removed if not proven to work well. Use with caution

| Function | File | Description | 
|-|-|-|
| `basicModel()` | [basicModel.R](basicModel.R) | Fit the one-component model

### Input data format

Should be in three column form, in the order L-R of "days", "performances", "loads". NA (empty cells in excel) values should be used in the performances column to indicate missing observed data, and load values of zero indicate that no training has taken place on a given day in the series. The order of the data, and appropriate use of NA or zero values as described is important. **This format is consistent for all functions in the repository for input data**

    days   performances    loads
       1          466.2    56.35
       2             NA     0.00
       3          440.5    59.15
       4             NA   110.60
       5          402.3     0.00
       6          418.9     0.00
       ...          ...      ...


### Import a function directly into R from github

#### Step 1: Install and load the R package `devtools`

    install.packages("devtools")
    library(devtools)

#### Step 2: Import the required function

| Function | Code (copy, paste, run) |
|-|-|
| `banisterModel()` | `source_url( https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/banisterModel.R )` |
| `standardModel()` | `source_url(https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/standardModel.R)` |
| `calvertModel()` | `source_url( https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/calvertModel.R )` |
| `turnerModel()` | `source_url( https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/turnerModel.R )` |
| `computeModels()` | `source_url( https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/software/utilities/computeModels.R )` |