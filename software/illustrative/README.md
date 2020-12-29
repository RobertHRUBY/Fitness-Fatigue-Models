Fitness Fatigue Model Notebooks
===================

These notebooks are hosted on [Kaggle](www.kaggle.com) and may be run, edited, and
forked using the (free) platform without having to install anything. They can also
be run directly from this repo using Docker.

## The Notebooks on Kaggle.com 

To work with the notebooks on Kaggle.com, visit one of the following links

- [Standard FFM, VDR, and Hill tranformation](https://www.kaggle.com/baogorek/fitness-fatigue-models-illustrative-code)
- [Kalman Filter](https://www.kaggle.com/baogorek/kalman-filter-illustrative-code)

Whether or not you are not signed in, you can click the "Copy and Edit" button on the top right
to immediately begin interacting with the notebook.

## The Notebooks using Docker

### Setup
Download [Docker Desktop](https://www.docker.com/products/docker-desktop) for your
operating system. Open a terminal (e.g., Powershell for Windows) and run:
```
docker pull jupyter/r-notebook:a0a544e6dc6e
```

If you have not already, clone this repo:
  `git clone https://github.com/bsh2/Fitness-Fatigue-Models.git`

### Starting and Stopping the Notebook Server
1. While still in the terminal, change directories to /software/illustrative/ (where this README.md is)
2. To start the notebook server, run the following command corresponding to your platform:

On Windows:
```  
docker run --rm -it -p 8888:8888 -w /home/jovyan/work -v ${PWD}:/home/jovyan/work jupyter/r-notebook:a0a544e6dc6e
```
On Mac:
```  
docker run --rm -it -p 8888:8888 -w /home/jovyan/work -v $(pwd):/home/jovyan/work jupyter/r-notebook:a0a544e6dc6e
```
On Linux:
```
docker run --rm -it -p 8888:8888 -w /home/jovyan/work -v "$PWD":/home/jovyan/work jupyter/r-notebook:a0a544e6dc6e
```

3. Follow the instructions to point your browser to the specified URL with token included,
for instance,
```
http://127.0.0.1:8888/?token=e1415230a823c4ac12b4970bc4b363c04d6bffc584eb1f6a
```

4. Press Ctrl+C in the terminal to shut down the notebook server.

## Directory contents

### Notebooks
- [./fitness-fatigue-models-illustrative-code.ipynb](./fitness-fatigue-models-illustrative-code.ipynb): a static version of the Kaggle notebook on the fitness fatigue models (plus VDR and Hill transformation)
- [./kalman-filter-illustrative-code.ipynb](./kalman-filter-illustrative-code.ipynb): a static version of the Kaggle notebook on Kalman Filter methods for the fitness fatigue model.

### Scripts
- [./ffmfunctions.R](./ffmfunctions.R): functions for fitting and using fitness-fatigue models with performance data
- [./kalmanfilterfunctions.R](./kalmanfilterfunctions.R): functions for fitting and using the Kalman Filter representation
  of the fitness-fatigue model with performance data

### Data
- [./example\_loads.csv](./example_loads.csv): a data set of example training loads created by @bsh2.
