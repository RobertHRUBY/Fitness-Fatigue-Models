# The Fitness-Fatigue Model Software Project

## Introduction

This project is developing open-source, robust, and flexible utilities for fitting and evaluating fitness-fatigue models (FFMs); with modern optimisers, out-of-sample assessment, and input checking. In addition, illustrative (i.e. didactic-style) code has been developed which guides the user through the application of several advanced methods via an interactive procedural flow. Thorough documentation, explanatory examples, and mock data are provided with each resource. The purpose of these tools is to expedite development and experimentation of fitness-fatigue models and methods in the sport and exercise sciences by reducing initial barriers to study. It is hoped the resources contained will be useful to researchers, educators, students and practitioners alike. In particular, to those investigating model validity in theoretical and experimental contexts. The repository is not an endorsement of these models as "ready to use". Rather, it should be thought of as a set of tools to aid scientific understanding, accelerate development, and enrich discussion in this area of performance modelling. Within the repository there are also rich educational materials for lecturers, students and practitioners seeking ot learn more about this area of sport science.

## Repository organisation

The repository is organized into three main folders:

| Directory | Description |
|-|-|
| [functions](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/functions) | Code resources and documentation for fitting and testing various fitness-fatigue models in R |
| [resources](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/resources) | Research papers, educational files, notes, and links to other useful resources |
| [simulations](https://github.com/bsh2/Fitness-Fatigue-Model/tree/main/simulations) | Code files and specific data related to fitness-fatigue model simulation experiments |

## An introductory example

<img src="https://latex.codecogs.com/svg.latex?\hat{p}(t)&space;=&space;p^*&space;&plus;&space;k_g&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h&space;\sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" title="\hat{p}(t) = p^* + k_g \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_g}})-k_h \sum_{i=1}^{n-1}\omega(i)(e^{\frac{-(n-i)}{\tau_h}})" />

Fitting the standard fitness-fatigue model (above) to experimental data is as simple as the following steps:

    # Import your experimental data set
    someData = read.csv("someData.csv")

As long as your data is in the following format, with NA values in the performances column indicating no measurement on a given day. Zero values in the loads column indicate no training occurred.
  
  | days | performances | loads |
  |------|--------------|-------|
  | 1    | 100          | 50    |
  | 2    | NA           | 25    |
  | 3    | 102          | 75    |
  | 4    | NA           | 0     |
  | 5    | 82           | 105   |
  | 6    | 98           | 25    |
  | ...  | ..           | ...   |

Construct some reasonable box constraints on the parameter estimates

    # order c(p*, kg, Tg, kh, Th)    
    boxConstraints = data.frame("lower" = c(10, 0.01, 1, 0.01, 1),
                                "upper" = c(150, 3, 50, 3, 50))
    
    # Call the fitting function using gradient-descent optimisation
    
    fittedModel = standardModel(data = someData,
                                constraints = boxConstraints, 
                                method = "bfgs",              
                                doTrace = TRUE,
                                initialComponent = FALSE
                                )

TODO: Provide graphic and some results output here.

The above example only scratches the surface with this particular function, and it is even possible to supply arguments to tune the implementation further (e.g. choice of optimisation approach, supply starting values for the gradient descent, and parameters for the cross-validation method), and also include the use and fitting of an initial trace on both model components as demonstrated in [Busso (1992)(eq.1)](https://link.springer.com/article/10.1007/BF00636228). All of this is described in detail within the [documentation]() where you will also find information about all the other functions provided in this repository.

## Installation / How to use the functions

### Option 1: Download source files from the github website
    
    # 1. Download from github.com/bsh2/Fitness-Fatigue-Models/functions/standardModel.R
    # 2. Place R function file in working directory
    # 3. Source into your environment as follows
    
    source("standardModel.R")
    
### Option 2: Using the *devtools* package

Obtain the 'raw' URL for the function (can also be found in the [documentation]()) and then source the function directly into R from github.

    library(devtools)
    source_url(https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/functions/standardModel.R)

## Contribute

We would love to hear from you if you have a function or code you think would fit here. Please open an issue or submit a pull request.

### Colleagues
- Ben Ogorek [twitter](https://twitter.com/benogorek?lang=en), [github](https://github.com/baogorek), [website](https://www.ogorekdatasciences.com/), [medium](https://medium.com/@baogorek)
- Ben Stephens Hemingway | [github](github.com/bsh2)
- Paul Swinton | [website](https://www3.rgu.ac.uk/dmstaff/swinton-paul)

### Support

Issue Tracker: https://github.com/bsh2/Fitness-Fatigue-Models/issues

If you are having issues, please let us know.