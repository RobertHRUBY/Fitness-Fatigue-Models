# Utilities: An R toolbox for fitting and evaluating FFMs

This directory includes easy to use [R](https://www.r-project.org/) functions for fitting and evaluating FFMs with modern optimisation options, and cross-validation (assessment of model performance).  
  
**Under development:** The files in this directory are undergoing a major redevelopment, and as such the associated documentation is still on it's way. In the meantime, the code files themselves are mostly self-contained from an explanatory point of view

**Getting started:** To get started it is recommended the user visit the [example file](example.R), which has been produced to demonstrate the application of the files/functions contained in this repository. From there, they should then be able to get a feel for the particular bit of code they are interested in. Hopefully shortly documentation will be finished that makes this a bit easier.

***

**Files contained:**

| File | Contains | Model | Function name | Dependencies |
|-|-|-|-|-|
| [standard_ffm_objective.R](standard_ffm_objective.R) | Residual sum of squares and log-likelihood objective functions | Standard FFM | `standardObjectiveSS` `standardObjectiveLL` | - |
| [fitness_delay_ffm_objective.R](fitness_delay_ffm_objective.R) | Residual sum of squares and log-likelihood objective functions | Fitness-delay FFM | `fitnessDelayObjectiveSS` `fitnessDelayObjectiveLL` | - |
| [vdr_ffm_objective.R](vdr_ffm_objective.R) | Residual sum of squares and log-likelihood objective functions | VDR FFM | `vdrObjectiveSS` `vdrObjectiveLL` | - |
| [cross_validation.R](cross_validation.R) | Expanding-window CV function for the VDR model (fitting via parallelised L-BFGS-B multi-start) | VDR FFM | `vdrCrossValidate` | **Packages**: <br><br>`optimx` `caret`<br>`RcppAlgos` `parallel``doSNOW` `foreach`<br>**Files**:<br>[vdr_ffm_objective.R](vdr_ffm_objective.R)<br>[ffm_simulation.R](ffm_simulation.R) |
| [ffm_simulation.R](ffm_simulation.R) | Functions for simulating (computing/predicting) FFMs | Standard, Fitness-delay, VDR FFM | `standardPredict` `fitnessDelayPredict` `vdrPredict` | - |
| [example.R](example.R) | Demonstration of the use of the above files/functions under synthetic inputs and different optimisation algos | All | N/A | **Packages:** <br><br>`optimx` `GA``pso` `cmaes` <br>`DEoptim``caret``RcppAlgos` <br>`parallel``doSNOW` `foreach`<br><br>**Files:**All in folder |