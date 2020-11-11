## The Fitness-Fatigue Model Project: A toolbox for implementing FFMs

This project develops a flexible general-purpose toolbox for investigating impulse-response models in sport and exercise science; providing functions and information to assist with the development and implementation of models and methods.

This is how easy it is to use:

    data <- read.csv("data.csv")
    standardModel(data, constraints, )

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