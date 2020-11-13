# The R toolbox

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