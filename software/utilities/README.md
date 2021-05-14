# Utilities: An R toolbox for fitting and evaluating FFMs

This directory includes easy to use [R](https://www.r-project.org/) functions for fitting and evaluating FFMs with modern optimisation options, and cross-validation (assessment of model performance).  
  
**Under development:** The files in this directory are undergoing a major redevelopment, and as such the associated documentation is still on it's way. In the meantime, the code files themselves are mostly self-contained from an explanatory point of view.

***

**Files contained:**

| File | Contains | Model | Function name |
|-|-|-|-|
| standard_model_NLS.R | Sum-of-squares objective function | Standard FFM | `standardObjectiveSS` |
| standard_model_MLE.R | (1) Log-likelihood objective function; (2) Negative log-likelihood objective function | Standard FFM | (1) `standardObjectiveLL`, (2) `standardObjectiveNLL` |
| fitness_delay_model_NLS.R | Sum-of-squares objective function | Fitness-delay FFM | `fitnessDelayObjectiveSS` |
| fitness_delay_model_MLE.R | (1) Log-likelihood objective function (2) Negative log-likelihood objective function | Fitness-delay FFM | `fitnessDelayObjectiveLL`, `fitnessDelayObjectiveNLL` |
| vdr_model_NLS.R | Sum-of-squares objective function | VDR FFM |  |
| vdr_model_MLE.R | (1) Log-likelihood objective function; (2) Negative log-likelihood objective function | VDR FFM |  |
|  |  |  |  |

- **standard_model_NLS.R:** Least-squares objective function and simulation (prediction) function for the standard FFM (5 parameter, 2 component). Can be used in a nonlinear least-squares fitting approach
- **fitness_delay_model_NLS.R:** Least-squares objective function and simulation (prediction) function for the fitness-delay FFM (i.e., Calvert's model with exponential growth on fitness response). Can be used in a nonlinear least-squares fitting approach
- **VDR_model_NLS.R:**
- **standard_model_MLE.R:**
- **fitness_delay_model_MLE.R:**
- **VDR_model_MLE.R:**


