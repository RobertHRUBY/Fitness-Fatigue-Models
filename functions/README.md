## The toolbox

### Model calibration

| Code File              | Functionality                                                                                                                                                                                    | Documentation |
|-------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------|
| basicModel.R      | The discrete one-component FFM (with and without an initial component)                                                                                                                   |               |
| standardModel.R   | The standard discrete two-component model (with and without two initial components). Included is an option to reduce the model to a single scaling constant K for both components                                                                                                           |               |
| banisterModel.R   | The original first-order linear system of ODEs (with one or two components) |               |
| [turnerModel.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/turnerModel.R)     | The non-linear variant of original model system                |               |
| calvertModel.R    | TBC                                                                                                                                                                                              |               |
| vdrModel.R        | The variable dose-response model (with and without two initial components)                                                                                                            |               |
| **TBC**: vdrModelHill.R    | **TBC**: The variable dose-response model with external Hill transform applied to the training load series (with and without two initial components) - TBC with BenO and his work on this                                           |               |
| **TBC**: Kalman   | **TBC**: standard model estimation with Kalman filter feedback                                                                                                                                   |               |

### Other

| File        | Functionality   | Documentation   |
|-------------------|-----------------|-----------|
| parScaling.R      | Function to determine (optional) scaling values for the parameters given a set of data                                                                                                           |               |
| [computeModel.R](https://github.com/bsh2/Fitness-Fatigue-Models/blob/main/functions/computeModels.R)    | Compute a selected FFM given a definite set of parameter values (& initial conditions if appropriate) and a training load series                                                                   |               |
