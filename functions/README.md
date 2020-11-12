## The toolbox

### Model calibration

| Code File              | Functionality                                                                                                                                                                                    | Documentation |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| basicModel.R      | The discrete one-component FFM (with and without an initial component)                                                                                                                   |               |
| standardModelX1.R | The standard discrete two-component model reduced to one scaling constant (with and without two initial components)                                                                            |               |
| standardModel.R   | The standard discrete two-component model (with and without two initial components)                                                                                                            |               |
| banisterModel.R   | The original first-order linear system of ODEs (with one or two components) |               |
| turnerModel.R     | The non-linear variant of original model system                |               |
| calvertModel.R    | TBC                                                                                                                                                                                              |               |
| vdrModel.R        | The variable dose-response model (with and without two initial components)                                                                                                            |               |
| **TBC**: vdrModelHill.R    | **TBC**: The variable dose-response model with external Hill transform applied to the training load series (with and without two initial components) - TBC with BenO and his work on this                                           |               |
| **TBC**: Kalman   | **TBC**: standard model estimation with Kalman filter feedback                                                                                                                                   |               |

### Other

| File        | Functionality   | Documentation   |
|-------------------|-----------------|-----------|
| parScaling.R      | Function to determine (optional) scaling values for the parameters given a set of data                                                                                                           |               |
| computeModel.R    | Compute a selected FFM given a definite set of parameter values (& initial conditions if appropriate) and a training load series                                                                   |               |
