# Fitness-Fatigue model resources

The aim of this repository is to gather up and centralise my fitness fatigue model tools and resources. These are mostly R functions that have been developed as part of collaborative research projects, or by myself for my PhD thesis in *Health Science with Computing*. The plan where possible is also to include some excellent functions developed by others (with their permission). It is hoped this repository will represent a key resource for those interested in the area of fitness fatigue modelling in sport science; allowing others to develop and implement the available IR models and methods. All of the included files are under continued development and subject to improvement, so please check back from time to time to make sure you have the latest version (I may have already fixed the error you found, but still report it anyway). The repository is organised into the following folders:

 - **Papers**:  My research papers and notes
 - **Docs**: Documentation or code notebook for each function or script
 - **Functions**: FFM functions (Effectively an R cookbook for FFMs)
 - **Scripts**: Associated scripts and other helpful snippets
 - **Other**:  Files or docs that don't fit in any of the above

![StandardModel](https://i.ibb.co/NZqYgVp/Screenshot-2020-11-07-at-11-57-55.png)

The files in the **functions** folder are the backbone of this repository, offering model fitting/calibration functionality, with cross-validation integrated into the process wherever possible. I tend to favour expanding-window cross-validation just now, but this may change. These functions have been written to be as **out of the box** ready to go as is possible without having to put everything into a package (see [dorem.net](dorem.net) for this ongoing project). I decided against putting them in a package, as I think they offer more flexibility in their current form. That said, you don't need to delve into the code behind them straight away in order to use them, as I have made is as simple as possible. In fact in most cases, you the user only need to supply training and performance data in a certain format within the function call, and then decide on a few arguments (e.g. starting values (if gradient-based), optimisation method (gradient or evolution strategy), and box constraints for the parameter space). However, even then there are some defaults built in to most functions, and validation checks on the function call to stop things going too wrong.

It is a good idea to review the accompanying documentation for each function, but more specifically the description of what the function does and the example implementations. These documentation files should hopefully illuminate the structure of the function calls and provide more clarity than the abstract description here.

# Get involved
If you would like to contribute a file to this repository, have suggestions, or have found bugs or errors please raise an issue, leave a contact address if you would like, and I will get back to you.

Code files themselves reflect authorship, however briefly the following people deserve acknowledgement for either direct contribution to the code files, or through indirect efforts (research papers, insightful discussions surrounding FFMs, suggestions for methods, testing/error reports):

 - Ben Ogorek, Paul Swinton, Mladen Jovanovic, Christian Rasche, Mark Connor, Thierry Busso. 
