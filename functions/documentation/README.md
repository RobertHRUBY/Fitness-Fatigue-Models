# Documentation

This README introduces all of the functions and scripts included within the repository.

# Table of Contents

1. [The basic model](https://github.com/bsh2/Fitness-Fatigue-Models/tree/main/functions/documentation#the-basic-model): `basicModel()` [(source)]()

# Functions

---

## 1. The basic model

| Key information                  | Further details  |
|----------------------------------|------------------|
| Source code                      | [basicModel.R]() |
| Function call                    | `basicModel()`   |
| Source function </br> `devtools` | `source_url()`   |
| Dependencies </br> (packages)    | ``stats``, ``caret``, ``ga``                |

#### Description and functionality

*Box-constrained optimisation of the one-component impulse-response model using quasi-Newton and genetic algorithms, with walk-forward cross-validation (expanding window). Includes an option to estimate an initial trace for the model component in the case that* <img src="https://latex.codecogs.com/svg.latex?g(0)\neq&space;0" title="g(0)\neq 0" />. </br>

The basic model is a one-component impulse-response model of the effect of training on performance arising from the following linear time-invariant system: </br></br>
<img src="https://latex.codecogs.com/svg.latex?p(t)=p^*&space;&plus;&space;Kp(t)" title="p(t)=p^* + Kp(t)" /> </br></br>
<img src="https://latex.codecogs.com/svg.latex?\frac{dP(t)}{dt}=\omega(t)-\frac{1}{\tau}p(t)" title="\frac{dP(t)}{dt}=\omega(t)-\frac{1}{\tau}p(t)" />
</br></br>
The first-order linear ODE above can be solved via the method of Laplace transform applied to each term, and then rearranging the substituted transforms - assuming <img src="https://latex.codecogs.com/svg.latex?p(0)=0" title="p(0)=0" /> - in terms of <img src="https://latex.codecogs.com/svg.latex?P(s)" title="P(s)" /> to derive it's transfer function (i.e. the relationship of the system input to output): </br></br>
<img src="https://latex.codecogs.com/svg.latex?P(s)=-\frac{W(s)}{(s&plus;\frac{1}{\tau})}" title="P(s)=-\frac{W(s)}{(s+\frac{1}{\tau})}" /> </br></br>
Defining:</br></br>
<img src="https://latex.codecogs.com/svg.latex?G(s)=-\frac{1}{(s&plus;\frac{1}{\tau})}" title="G(s)=-\frac{1}{(s+\frac{1}{\tau})}" /> </br></br>
Allows us to write the product <img src="https://latex.codecogs.com/svg.latex?P(s)=G(s)W(s)" title="P(s)=G(s)W(s)" /> which can then be solved by the convolution theorem (inverse Laplace transform of the product is its convolution): </br></br>
<img src="https://latex.codecogs.com/svg.latex?\mathcal{L}^{-1}[P(s)]=\mathcal{L}^{-1}[G(s)W(s)]=(g\ast&space;w)(t)" title="\mathcal{L}^{-1}[P(s)]=\mathcal{L}^{-1}[G(s)W(s)]=(g\ast w)(t)" /> </br></br>
Which gives: </br></br>
<img src="https://latex.codecogs.com/svg.latex?p(t)=(g\ast&space;w)(t)&space;=&space;\int&space;g(t-u)w(u)du" title="p(t)=(g\ast w)(t) = \int g(t-u)w(u)du" /> </br></br>
Consulting inverse Laplace transform tables and substituting appropriately we get the solution: </br></br>
<img src="https://latex.codecogs.com/svg.latex?p(t)=\int&space;e^{-\frac{t-u}{\tau}}w(u)du" title="p(t)=\int e^{-\frac{t-u}{\tau}}w(u)du" /> </br></br>
That can then be discretised to give the one-component impulse-response model used to fit the FFM in this R function: </br></br>
<img src="https://latex.codecogs.com/svg.latex?\hat{p}(n)&space;=&space;p^*&space;&plus;&space;K&space;\sum_{i=1}^{n-1}&space;e^{-\frac{(n-i)}{\tau}}w(i)\cdot&space;\Delta_n" title="\hat{p}(n) = p^* + K \sum_{i=1}^{n-1} e^{-\frac{(n-i)}{\tau}}w(i)\cdot \Delta_n" /> </br>
<br>
We also include in the option to include an initial component denoted <img src="https://latex.codecogs.com/svg.latex?q" title="q" /> in the function to represent an estimate of the initial level of the training component at <img src="https://latex.codecogs.com/svg.latex?n=0" title="n=0" />, that subsequently decays away at the same rate as any future effects. </br></br>
<img src="https://latex.codecogs.com/svg.latex?\hat{p}(n)&space;=&space;p^*&space;&plus;&space;q\cdot&space;(e^{-\frac{n}{\tau}})&space;&plus;&space;K&space;\sum_{i=1}^{n-1}&space;e^{-\frac{(n-i)}{\tau}}w(i)\cdot&space;\Delta_n" title="\hat{p}(n) = p^* + q\cdot (e^{-\frac{n}{\tau}}) + K \sum_{i=1}^{n-1} e^{-\frac{(n-i)}{\tau}}w(i)\cdot \Delta_n" />
<br></br>

> Alternatively, you can try the function `banisterModel()` [(link to documentation)](). This function approximates the solution of the associated IVP for each new set of candidates provided by the optimisation algorithm, assessed by iterative reduction of the objective function (MSE). The ODE solver used is `lsoda()` within the `deSolve` package, and is a method that automatically switches between Adams and BDF to cope with IVPs for stiff and non-stiff first-order systems.

#### Usage

    basicModel(inputData, 
               constraints, 
               method = "bfgs",
               startingValues = NULL,
               doTrace = FALSE,
               initialComponent = FALSE,
               initialWindow = NULL, 
               testHorizon = NULL, 
               expandRate = NULL)

#### Arguments

| Argument           | Details |
|--------------------|---------|
| `inputData`        | The time series of training load values and measured performances. Data frame of three columns in order ("days","performances","loads"). NA values in performance column represent days on which performance not measured. 0 values in loads column represent days where training was not performed        |
| `constraints`      | Box constraints on the parameter space. Supplied as a data frame of two columns ("lower" = c(p1,p2,p3,p4,...), "upper" = c(p1,p2,p3,p4,...))        |
| `method`           | An optional argument to supply starting values for gradient method. If not supplied, by default the function will compute random starting values that satisfy typical relationships between the scaling factors and tau parameters, and are sampled from a Gaussian distribution with mean at the middle of the bounds.        |
| `startingValues`   |         |
| `doTrace`          |         |
| `initialComponent` | Supplied as percentage (i.e. 60 is equivalent to 60%)        |
| `initialWindow`    | Supplied as percentage (i.e. 20 is equivalent to 20%)        |
| `testHorizon`      | Supplied as percentage (i.e. 5 is equivalent to 5%)       |
| `expandRate`       |         |

#### Details

* The **loss function** used to assess model fit is mean-squared error (MSE) (i.e. mean(squared residuals)). This is preferred to provide a comparable metric of model fit between cross-validation folds (where n data points increases and thus RSS would be expected to also increase without normalisation).
* The **default optimisation algorithm** used to estimate the parameters is ``L-BFGS-B``, called by `method = 'bfgs'`. This algorithm is a box-constrained limited-memory modification of the quasi-Newton algorithm ``BFGS``, developed by Broydon, Fletcher, Goldfarb and Shanno in 1970. This algorithm is included in the ``optim`` toolbox as part of the R ``stats`` package shipped with the R kernel. The algorithm uses both the function values and gradients to iteratively search the surface of the cost function to find the minimum value.
* An **alternative optimisation algorithm** available is a genetic algorithm (GA), from the package ``GA`` available on CRAN. The ``GA`` package provides a number of genetic algorithms for stochastic optimisation of real-valued problems. The tuning parameters selected for the problem are as default tournament selection, BLX (blend) crossover, random mutation around the solution, and a population size minimum 10x the number of free parameters in the model. These options can be changed but require adjustment of the function call `ga()` within the function code file itself, and cannot be passed as arguments to the function `banisterModel()`. For simple models, it is best to first try the gradient approach implemented here, as this often faster.
* The **cross-validation approach** used as standard across all functions is a manual implementation of a walk forward approach (expanding-window) [described here]() and illustrated below. Default settings assuming that no argument is passed to the function are as follows: size of initial window ``initialWindow`` is 60% of time-series, the test window ``testHorizon`` is 20%, and the expansion rate on the initial window across slices is 5% ``expandRate``. These can be changed and users should refer to the arguments section above.

![sliding window](https://raw.githubusercontent.com/bsh2/Fitness-Fatigue-Models/main/functions/documentation/img/expanding-window.jpg?token=AGTSF7MOXP2HFIH4FK33N3C7WZOGO)

* **Appropriately bounding the parameter space** is a problem individual to the users data set, however there are a few rules you can use that might work in most cases to adequately constrain the free-parameters to reasonable values. The baseline performance value p* is unlikely to be larger than the maximum measured performance value in your data set, so you could set it just above this by a magnitude of 10%. The tau values on components are unlikely (physiologically) to be reasonable if less than 1, or more than 50 (days). The scaling values will depend on the relationship between your quantified training load values and the measured performance values. Work out the magnitude of difference and you get some insight into some reasonable bounds for these values.
* **Format of input data** should be consistent, with a three column dataframe representing the time series as follows from left to right: "days", "performances", "loads". NA values should be used to represent days on which measured performances do not exist at a specific row (day), and 0 used to indicate no training occured. Below is an example that makes this clear. The function will rename columns appropriately if exact names above are not supplied, but will break if the columns are not in the correct format or order.

    | days | performances | loads |
    |------|--------------|-------|
    | 1    | 100          | 50    |
    | 2    | NA           | 25    |
    | 3    | 102          | 75    |
    | 4    | NA           | 0     |
    | 5    | 82           | 105   |
    | 6    | 98           | 25    |
    | ...  | ..           | ...   |


#### Value (output)

For `basicModel()`, a list with components:

| List component | Details |
|----------------|---------|
| component1     |         |
| component2     |         |
| component3     |         |

#### Example implementation

| File              | Location                  |
|-------------------|---------------------------|
| example code file | [/Directory/codeFile.R]() |
| example data file | [/Directory/codeFile.R]() |

Implementation example with screenshots here (use /img folder for pics) including how to source the data file into R with RCurl.

#### Author(s)

Ben Stephens Hemingway

#### Further resources

| Resource                                          | Description        |
|---------------------------------------------------|--------------------|
| Research (author/year)                            | [link to Research title]() |
| Any other code, documentation, or online resource | [link to code resource]()  |


## 1. The Kalman Filter

| Key information                  | Further details  |
|----------------------------------|------------------|
| Source code                      | [kalmanModel.R](../kalmanModel.R) |
| Dependencies </br> (packages)    | ``stats``, ``MASS``                |

#### Description and functionality
TODO

#### Usage

The following is a demonstration of simulating from the Kalman Filter model and
then performing Maximum Likelihood-based estimation. Note that the user may run
`increase_likelihood` for as many times as desired.

```
kalman_model <- create_kalman_model(p_0 = 400, k_g = .1, k_h = .3, tau_g = 50, tau_h = 15,
                                    xi = 20,
                                    sigma_g = 8, sigma_h = 4, rho_gh = -.3,
                                    initial_g = 35, initial_h = 20,
                                    initial_sd_g = 10, initial_sd_h = 5,
                                    initial_rho_gh = 0)
print(kalman_model)

w <- rep(c(seq(10, 70), rep(0, 14)), 15)
sim_df <- simulate(kalman_model, w)
plot(sim_df$w)
plot(sim_df$y)


kf_mod <- initialize_kalman_from_data(sim_df)
print(kf_mod)

kf_orig <- kf_mod
kf_mod <- increase_likelihood(kf_mod, sim_df, reps = 10)
print(kf_mod)

filtered <- filter(kf_mod, sim_df)
sim_df$fitness_pred <- filtered$X[, 1]
sim_df$fatigue_pred <- filtered$X[, 2]

# NOTE: Better predict function
sim_df$pred <- (kf_mod$p_0 + kf_mod$C[1, 1] * sim_df$fitness_pred
                           + kf_mod$C[1, 2] * sim_df$fatigue_pred)

plot(sim_df$fitness_pred, main = "fitness")
plot(sim_df$fatigue_pred, main = "fatigue")

plot(sim_df$y)
points(sim_df$pred, col = 'blue')
```
