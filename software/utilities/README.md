# Utilities: An R toolbox for fitting and evaluating FFMs

This directory includes easy to use [R](https://www.r-project.org/) functions for fitting and evaluating FFMs with modern optimisation options, and cross-validation (assessment of model performance).  
  
**Under development:** The files in this directory are undergoing a major redevelopment, and as such the associated documentation is still on it's way. In the meantime, the code files themselves are mostly self-contained from an explanatory point of view, and the file [example](example.R) has been produced to demonstrate their use under multiple different optimization packages available in R.

***

**Files contained:**

| File | Contains | Model | Function name |
|-|-|-|-|
| standard_ffm_objective.R | Sum-of-squares function, log-likelihood function, negative log-likelihood function | Standard FFM | `standardObjectiveSS`, `standardObjectiveLL`, `standardObjectiveNLL` |
| fitness_delay_ffm_objective.R | Sum-of-squares function, log-likelihood function, negative log-likelihood function | Fitness-delay FFM | `fitnessDelayObjectiveSS`, `fitnessDelayObjectiveLL`, `fitnessDelayObjectiveNLL` |
| vdr_ffm_objective.R | Sum-of-squares function, log-likelihood function, negative log-likelihood function | VDR FFM | `vdrObjectiveSS`, `vdrObjectiveLL`, `vdrObjectiveNLL` |
| cross_validation.R | Expanding-window CV function for fitting and evaluating the FFMs | Standard, Fitness-delay, VDR FFM | `standardCrossValidate`, `fitnessDelayCrossValidate`, `vdrCrossValidate` |
| ffm_simulation.R | Functions for simulating (computing/predicting) FFMs | Standard, Fitness-delay, VDR FFM | `standardPredict`, `fitnessDelayPredict`, `vdrPredict` |
| example.R | An example file demonstrating the use of the above files and functions | All | N/A |


