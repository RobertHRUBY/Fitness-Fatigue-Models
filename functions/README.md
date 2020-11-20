# Utilities: An R toolbox

Bespoke functions, written in [R](https://www.r-project.org/), for fitting and evaluating FFMs with modern optimisers, cross-validation, and input checking.

## Getting started

Consider the example that a researcher wishes to fit the standard fitness-fatigue model to a set of experimental data (i.e. quantified training load and measured performances). 

1. The first step is to import your data into the environment. 



We give an example use of these resources to fit the standard fitness-fatigue model to a set of experimental training and measured performance data. First, recall the model of interest is stated as follows:

<img src="https://latex.codecogs.com/svg.latex?\hat{p}(t)&space;=&space;p^*&space;&plus;&space;k_g&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" title="\hat{p}(t) = p^* + k_g \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" />

The first step is to 


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