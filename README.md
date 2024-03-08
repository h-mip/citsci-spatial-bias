[![DOI](https://zenodo.org/badge/759879808.svg)](https://zenodo.org/doi/10.5281/zenodo.10796324)

# Open Code for "Assessing and Correcting Neighborhood Socioeconomic Spatial Sampling Biases in Citizen Science Mosquito Data Collection"
This repository contains the code used for the paper: "Assessing and Correcting Neighborhood Socioeconomic Spatial Sampling Biases in Citizen Science Mosquito Data Collection".

The code is written in R in an isolated, portable, reproducible environment created with the [renv](https://rstudio.github.io/renv/) package. After cloning the repository, run [renv::restore()](https://rstudio.github.io/renv/reference/restore.html) to create an environment identical to the one in which the code was written and tested. We rely on the [targets](https://github.com/ropensci/targets) package to organize the code as a set of functions in the R directory, which can be run as a pipeline using tar_make(). See the [targets user manual](https://books.ropensci.org/targets/walkthrough.html) for more details. 


## Directory Structure

```
│projectdir          <- Project's main directory and Git
│                       repository.
├── _targets         <- Directory managed by the targets package for R.  
|                       (Should not be modified manually.)
│
├── data             <- Directory for all data on which pipeline relies. 
|   |                   Only .gitignore and README.md files are tracked 
|   |                   by Git (in order to preserve the directory), so
|   |                   this directory must be populated manually.
|   |
│   ├── external     <- Open data from external providers. This data should 
|   |                   be downloaded before running pipeline (see Data
|   |                   section below).
|   |
│   ├── private      <- Data that cannot be made open for privacy reasons.
|                       If you do not have access to this data, you should
|                       comment out the pipeline targets that rely on it.
│
├── figures          <- Directory in which all plots and tables are written
|                       by the pipeline when it is run.
|
├── R                <- Directory containing all of the R functions used in
|                       the targets pipeline. (This is the code to inspect
|                       in order to see how the analysis is done.)
│
├── renv             <- Directory managed by the renv package.   (Should 
|                       not be modified manually.)
│
├── _targets.R       <- R script that is used to control the targets 
|                       pipeline.
│
├── .gitignore       <- File telling Git which files and directories not to
|                       track.
|
├── .Rprofile        <- Rprofile script that is run when you start R in
|                       this repository. You should modify lines 3 and 6
|                       based on your system paths.
|
├── CITATION.cff     <- Machine-readable citation file. 
|
├── LICENSE          <- GPLv3 license. 
|
├── README.md        <- This file.
|
├── renv.lock        <- File used by the renv library for keeping track of
                        package versions. Do not modify this manually.
```

## Data

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10684357.svg)](https://doi.org/10.5281/zenodo.10684357) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10674718.svg)](https://doi.org/10.5281/zenodo.10674718)

All of the [Mosquito Alert](https://www.mosquitoalert.com) data used by this code is available in open access repositories hosted on Zenodo. The Mosquito Alert reporting data (all adult mosquito and mosquito bite reports as well as all expert-validated _Ae. albopictus_ reports) is available at [doi.org/10.5281/zenodo.10684357](https://doi.org/10.5281/zenodo.10684357). The Mosquito Alert sampling effort data is available at [doi.org/10.5281/zenodo.10674718](https://doi.org/10.5281/zenodo.10674718). Both datasets are downloaded automatically from these locatations and processed by the pipeline.

The income and population data relied on here are available from the INE's [_Atlas de distribución de renta de los hogares_](https://ine.es/dyngs/INEbase/operacion.htm?c=Estadistica_C&cid=1254736177088&menu=resultados&idp=1254735976608). Census section geographic data is available from the [INE's Digital Cartography Files](https://ine.es/ss/Satellite?L=en_GB&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout&rendermode=previewnoinsite%29). The weather data relied on in the Mosquito Trap Vector Model was taken from the [Meteorological Service of Catalonia's API](https://apidocs.meteocat.gencat.cat). 

## Funding

This research received funding from the ''la Caixa'' Foundation, under Grant HR19-00336, and from the European Research Council (ERC) under the European Union's Horizon 2020 research and innovation program (Grant agreement No. 853271). 

## License

This code is licensed under the [GNU GENERAL PUBLIC LICENSE](https://github.com/h-mip/mosquito-citizen-science-neighborhood-sampling-bias-code/blob/main/LICENSE)

Copyright 2024 Álvaro Padilla-Pozo & John R.B. Palmer

Open Code for "Assessing and Correcting Neighborhood Socioeconomic Spatial Sampling Biases in Citizen Science Mosquito Data Collection" is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses.
