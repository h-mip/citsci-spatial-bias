# Open Code for "Assessing and Correcting Neighborhood Socioeconomic Spatial Sampling Biases in Citizen Science Mosquito Data Collection"
This repository contains the code used for the paper: "Assessing and Correcting Neighborhood Socioeconomic Spatial Sampling Biases in Citizen Science Mosquito Data Collection".

The code is written in R in an isolated, portable, reproducible environment created with the [renv](https://rstudio.github.io/renv/) package. After cloning the repository, run [renv::restore()](https://rstudio.github.io/renv/reference/restore.html) to create an environment identical to the one in which the code was written and tested. 

We rely on the [targets](https://github.com/ropensci/targets) package to organize the code as a set of functions in the R directory, which can be run as a pipeline using tar_make(). See the [targets user manual](https://books.ropensci.org/targets/walkthrough.html) for more details. 