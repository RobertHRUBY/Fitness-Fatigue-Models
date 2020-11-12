# Documentation

# Table of Contents

TODO

# Functions

---

## The basic model

| Key information                  | Further details  |
|----------------------------------|------------------|
| Source code                      | [basicModel.R]() |
| Function call                    | `basicModel()`   |
| Source function </br> `devtools` | `source_url()`   |
| Dependencies </br> (packages)    |                  |

#### Description and functionality

*Box-constrained optimisation of the one-component impulse-response model using quasi-Newton and genetic algorithms, with walk-forward cross-validation (expanding window). Includes an option to estimate an initial trace for the model component in the case that* <img src="https://latex.codecogs.com/svg.latex?g(0)\neq&space;0" title="g(0)\neq 0" />. </br>

The basic model is a one-component impulse-response model of the effect of training on performance arising from the following linear time-invariant system: </br></br>
<img src="https://latex.codecogs.com/svg.latex?p(t)=p^*&space;&plus;&space;Kp(t)" title="p(t)=p^* + Kp(t)" /> </br></br>
<img src="https://latex.codecogs.com/svg.latex?\frac{dP(t)}{dt}=\omega(t)-\frac{1}{\tau}p(t)" title="\frac{dP(t)}{dt}=\omega(t)-\frac{1}{\tau}p(t)" />
</br></br>
The first-order linear ODE above can be solved via the method of Laplace transform applied to each term, and then rearranging the substituted transforms - assuming <img src="https://latex.codecogs.com/svg.latex?p(0)=0" title="p(0)=0" /> - in terms of <img src="https://latex.codecogs.com/svg.latex?P(s)" title="P(s)" /> to derive it's transfer function (i.e. the relationship of the system input to output): </br></br>
<img src="https://latex.codecogs.com/svg.latex?P(s)=-\frac{W(s)}{(s&plus;\frac{1}{\tau})}" title="P(s)=-\frac{W(s)}{(s+\frac{1}{\tau})}" /> </br></br>
Defining:</br></br>
<img src="https://latex.codecogs.com/svg.latex?G(s)=-\frac{1}{(s&plus;\frac{1}{\tau})}" title="G(s)=-\frac{1}{(s+\frac{1}{\tau})}" /> </br>
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

> Alternatively, you can try the function `banisterModel()` [(link to documentation)]() which repeatedly approximates the solution of the associated IVP for new candidate estimates (parameters and ICS) and which are chosen by the optimisation algorithm to iteratively reduce the objective function. The ODE solver used in this approach is `lsoda()` within the `deSolve` package: a method that automatically switches between the Adams and BDF method to cope with IVP's for stiff and non-stiff first-order systems.

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
| `inputData`        |         |
| `constraints`      |         |
| `method`           |         |
| `startingValues`   |         |
| `doTrace`          |         |
| `initialComponent` |         |
| `initialWindow`    |         |
| `testHorizon`      |         |
| `expandRate`       |         |

#### Details

* method "bfgs"
* method "ga"
* starting values developed via...
* Describe cross-validation, in particular the Expanding window parameters initialWindow, testHorizon, expandRate
* Bounding constraints
* Format of input data

#### Value (output)

For 'basicModel()', a list with components:

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

Implementation example with screenshots here (use /img folder for pics)

#### Author(s)

Ben Stephens Hemingway

#### Further resources

| Resource                                          | Description        |
|---------------------------------------------------|--------------------|
| Research (author/year)                            | [link to Research title]() |
| Any other code, documentation, or online resource | [link to code resource]()  |
