## The Fitness-Fatigue Model Project: A toolbox for implementing FFMs

This project offers a flexible general-purpose R toolbox for investigating impulse-response models in sport and exercise science; providing functions and information to assist with the development and implementation of models and methods. The structure of the functions contained aims to appeal to researchers seeking to investigate the models within data-collection scenarios, and those looking to extend and test models 'in vitro'.

As a brief example, fitting the standard fitness-fatigue model (below) to experimental data is as simple as:

    # Import your experimental data set
    experimentalData <- read.csv("experimentalData.csv")
    
    # Data set should be in following format
    head(experimentalData)
    days    |   performances  |   loads
    1               100             50
    2               NA              25
    3               102             75
    4               NA              0
    5               82              105
    
    # Reasonable box constraints on parameter estimates c(p*, kg, Tg, kh, Th)
    boxConstraints = data.frame("lower" = c(10, 0.01, 1, 0.01, 1),
                                "upper" = c(150, 3, 50, 3, 50))
    
    # Call the fitting function using quasi-Newton method and         
    fittedModel = standardModel(data = experimentalData,
                                constraints = boxConstraints, 
                                method = "bfgs",              # quasi-Newton
                                doTrace = TRUE                # monitor optim()
                                )

With the example above, it is also possible to supply $startingValues$
    
    # It is possible to also supply startingValues for optim(), which are
    # otherwise randomly generated with heuristics. Cross-validation is
    # performed as expanding window as standard with a 60% initial window, 
    # 
    
    

### Repository organisation

| Directory   | Description                                                                      |
|-------------|----------------------------------------------------------------------------------|
| [functions](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/functions)   | Cookbook for FFMs (bespoke R functions, scripts, and accompanying documentation) |
| [resources](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/resources)   | Notes, research papers, link directories, other docs and files                   |
| [simulations](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/simulations) | Study code and data files from experimental simulations run as part of my PhD    |

- Files in [functions](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/functions) are primarily estimation functions which include cross-validation methods such as expanding-window. 
- Functions are written to be as easy as possible to use out of the box, by researchers and practitioners. Most include input validation checks and error handling. 
- I recommend that users review the accompanying documentation for each function

### Get involved (contribute) or raise an issue with one of the files

If you would like to contribute a code file, related link (for one of the lists) or related document to this repository, have suggestions, or have found bugs or errors please raise an issue via github on this repository (leave a contact email address if you would like) and I will get back to you.


#### Current contributors:
* [Ben Ogorek](https://twitter.com/benogorek?lang=en): Estimation of FFM with Kalman filter feedback in R
* [Paul Swinton](https://www3.rgu.ac.uk/dmstaff/swinton-paul): Code review and general assistance

Note: *Code and README files in each directory reflect authorship in the event it differs from myself.*