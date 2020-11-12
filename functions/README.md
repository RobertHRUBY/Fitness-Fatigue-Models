## Fitness-fatigue model functions (toolbox)

### Files

| File              | Functionality                                                                                                                                                                                    | Documentation |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| basicModel.R      | Train/Test the basic one-component FFM (with and without an initial component)                                                                                                                   |               |
| standardModelX1.R | Train/Test the standard two-component model reduced to one scaling constant (with and without two initial components)                                                                            |               |
| standardModel.R   | Train/Test the standard two-component model (with and without two initial components)                                                                                                            |               |
| banisterModel.R   | Solve IVP of original model system (with either one or two components), and subsequently train/test the approximated solution to obtain suitable estimates for initial conditions and parameters |               |
| turnerModel.R     | Solve IVP of non-linear variant of original model system and subsequently train/test the approximated solution to obtain suitable estimates for initial conditions and parameters                |               |
| calvertModel.R    | TBC                                                                                                                                                                                              |               |
| vdrModel.R        | Train/Test the variable dose-response model (with and without two initial components)                                                                                                            |               |
| vdrModelHill.R    | Train/Test the variable dose-response model with external Hill transform applied to the training load series (with and without two initial components)                                           |               |
| **TBC**: Kalman   | **TBC**: standard model estimation with Kalman filter feedback                                                                                                                                   |               |
| parScaling.R      | Function to determine (optional) scaling values for the parameters given a set of data                                                                                                           |               |
| computeModel.R    | Compute various FFMs given a definite set of parameter values (& initial conditions if appropriate) and a training load series                                                                   |               |
