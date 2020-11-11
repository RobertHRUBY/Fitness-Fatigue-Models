## The Fitness-Fatigue Model Project: A toolbox for implementing FFMs

This project offers a flexible general-purpose R toolbox for investigating impulse-response models in sport and exercise science; providing functions and information to assist with the development and implementation of models and methods. The structure of the functions contained aims to appeal to researchers seeking to investigate the models within data-collection scenarios, and those looking to extend and test models 'in vitro'.

### Repository organisation

| Directory   | Description                                                                      |
|-------------|----------------------------------------------------------------------------------|
| [functions](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/functions)   | R functions, scripts, and documentation |
| [resources](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/resources)   | Research papers, educational files, notes, and links to other useful resources                  |
| [simulations](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/simulations) | Code and some data files from experimental simulation research (part of my PhD thesis)    |

### Example

Fitting the standard fitness-fatigue model (below) to experimental data is as simple as:

<img src="https://latex.codecogs.com/svg.latex?\hat{p}(t)&space;=&space;p^*&space;&plus;&space;k_g&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" title="\hat{p}(t) = p^* + k_g \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" />

    # Import your experimental data set
    
    experimentalData = read.csv("experimentalData.csv")
    
    # Data set should be in following format
    
    head(experimentalData)
    
    > --------+----------------+-------
      days    |  performances  | loads
      --------+----------------+-------
      1          100             50
      2          NA              25
      3          102             75
      4          NA              0
      5          82              105
      ...        ...             ...
      --------+-----------------+------
    
    # Reasonable box constraints on parameter estimates c(p*, kg, Tg, kh, Th)
    
    boxConstraints = data.frame("lower" = c(10, 0.01, 1, 0.01, 1),
                                "upper" = c(150, 3, 50, 3, 50))
    
    # Call the fitting function using Broyden-Fletcher-Goldfarb-Shanno algo
    
    fittedModel = standardModel(data = experimentalData,
                                constraints = boxConstraints, 
                                method = "bfgs",              
                                doTrace = TRUE
                                )
Furthermore, in the basic example above it is possible to supply further arguments to tune the implementation:

A different method for fitting: the alternative is currently a genetic algorithm
    
    method = "ga"
    
A vector of starting values for optim(), which are otherwise generated randomly from a truncated Gaussian distribution with inequality constraints enforced such as Tg > Th. Starting values not required for the evolutionary strategy

    startingValues = c(75, 1, 20, 1.5, 10)               # c(p*, kg, Tg, kh, Th)
  
Parameters for the expanding window cross-validation method, which are currently as default a 60% initial window, 20% test horizon, and 5% expansion rate.

    initialWindow = 50; testHorizon = 30; expandRate = 3

Arguments for functions will vary slightly between different models. Documentation is provided for each function within the [documentation directory](https://github.com/bsh2/Fitness-Fatigue-Models/tree/main/functions/documentation).

### Installation / use

#### Option 1: Download from the github website
    
    # 1. Download from github.com/bsh2/Fitness-Fatigue-Models/functions
    # 2. Place file in working directory
    # 3. Source into your environment
    
    source("standardModel.R")
    
#### Option 2: Using the *devtools* package, obtain the 'raw' URL for the function and then source directly into R

    library(devtools)
    source_url(https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/functions/standardModel.R)



### Contribute

- Issue Tracker: https://github.com/bsh2/Fitness-Fatigue-Models/issues

#### Project authors

- Ben Stephens Hemingway | [github](github.com/bsh2)
- Ben Ogorek | [twitter](https://twitter.com/benogorek?lang=en), [github](https://github.com/baogorek), [website](https://www.ogorekdatasciences.com/), [medium](https://medium.com/@baogorek)
- Paul Swinton | [website](https://www3.rgu.ac.uk/dmstaff/swinton-paul)

Note: *Code and README files in each directory reflect authorship in the event it differs from myself.*

### Support

If you are having issues, please let us know. You can email us at

b.stephens-hemingway [at] rgu [dot] ac [dot] uk