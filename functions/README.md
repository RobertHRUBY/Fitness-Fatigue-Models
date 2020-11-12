## Fitness-fatigue model functions (toolbox)

### Files

| File              | Functionality                                                                                                                                    | Documentation |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| basicModel.R      | Train and test the discretised one-component model (with and without an initial component)                                                       |               |
| standardModelX1.R | Train and test the standard two-component model reduced to one scaling constant (with and without two initial components)                        |               |
| standardModel.R   | Train and test the standard two-component model (with and without two initial components)                                                        |               |
| banisterModel.R   | Solve IVP of original model system (with one or two components) and subsequently train/test to obtain initial conditions and parameter estimates |               |
| turnerModel.R     | Solve IVP of non-linear variant of original model system and subsequently train/test to obtain initial conditions and parameter estimates        |               |
| calvertModel.R    | TBC                                                                                                                                              |               |
| vdrModel.R        | Train and test the variable dose-response model (with and without two initial components)                                                        |               |
| vdrModelHill.R    | Train and test the variable dose-response model with external Hill transform on training load series (with and without two initial components)   |               |
| TBC: Kalman       | TBC: standard model estimation with Kalman filter feedback                                                                                       |               |
| parScaling.R      | Function to determine (optional) scaling values for the parameters given a set of data                                                           |               |
| computeModel.R    | Function to compute various FFMs given a definite set of parameter values (& initial conditions if appropriate) and a training load series       |               |
