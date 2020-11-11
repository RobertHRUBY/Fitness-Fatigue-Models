## Fitness-fatigue model functions and scripts

### Files

| Function            | Description                                                                                                            | Associated documentation |
|---------------------|------------------------------------------------------------------------------------------------------------------------|--------------------------|
| [standardModel.R]() | Function to train and test the standard fitness-fatigue model in R using expanding-window CV. Assumes that g(0)=h(0)=0 | TBC                      |
| [                   |                                                                                                                        |                          |

| File            | Description                                                                                       |
|-----------------|---------------------------------------------------------------------------------------------------|
| [standardModel.R](https://github.com/bsh2/Fitness-Fatigue-Model/blob/main/functions/standardModel.R) | Function to train and test the standard fitness-fatigue model in R (expanding window CV)          |
| [turnerModel.R](https://github.com/bsh2/Fitness-Fatigue-Model/blob/main/functions/turnerModel.R)   | Function to train and test the non-linear variant of  Banister's model in R (expanding window CV). See the original authors research paper [here](https://jim.turner.link/downloads/BHK-2017-0013.pdf), their C code repository [here](https://github.com/jturner314/nl_perf_model_opt) and their website [here](https://jim.turner.link/) |
| [functionTemplate.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/functionTemplate.R)    | Template-by-annotation for various functions contained in this folder                   |