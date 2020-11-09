# Fitness-Fatigue model resources

The aim of this repository is to gather up and centralise my fitness fatigue model tools, resources and experiments. These are mostly code files (in R) that have been developed as part of collaborative research projects, or by myself for my PhD thesis in *Health Science with Computing*. It is hoped this repository will represent a helpful resource for those interested in the area of fitness fatigue modeling in sport science. In particular, providing tools to assist others develop and implement available impulse-response models and methods. All of the included files are under continued development and subject to improvement, so check back from time to time make sure you have the latest version of any given files. Please report any errors by raising an issue. The general organisation is as follows:

 - **Docs**: Notes and research papers
 - **Functions**: Effectively a 'cookbook' for FFMs, included here are bespoke R functions and associated scripts, alongside documentation for each of them.
 - **Simulations**: These are simulation study code files, forming the core of my PhD research
 - **Other**:  Files or docs that don't fit in any of the above

The files in the **functions** folder are the backbone of this repository, offering model estimation functionality, with cross-validation integrated into the process (most of the time). I tend to favor expanding-window cross-validation just now, but this may change. Functions are written to be ready out of the box. At some point I may put these into a package, but we already have an ongoing project in this area [dorem.net](dorem.net). I think right now the files offer flexibility in their current form for both researchers and practitioners. 

It is a good idea to review the accompanying documentation (typically a .pdf or .html document of the same name) for each function file. The documentation has been written to illuminate the structure of function calls and provide clarity in implementation.

# Get involved
If you would like to contribute a file to this repository, have suggestions, or have found bugs or errors please raise an issue via github on this repository (leave a contact address if you would like) and I will get back to you.

Code files themselves reflect authorship if it differs from myself.