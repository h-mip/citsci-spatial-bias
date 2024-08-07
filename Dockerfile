# R version
FROM rocker/r-ver:4

# Install some linux libraries that R packages need
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev jags pkg-config cmake default-jre default-jdk libz-dev libglpk-dev libpng-dev gdal-bin libgdal-dev libudunits2-dev libfontconfig1-dev libgdal-dev libharfbuzz-dev libfribidi-dev

# renv version
ENV RENV_VERSION 1.0.7

# Install renv
RUN Rscript -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "remotes::install_version('renv', version='${RENV_VERSION}')"

# Create a directory named after our project directory
WORKDIR /citsci-spatial-bias

# Copy the lockfile over to the Docker image
COPY renv.lock renv.lock

# Install all R packages specified in renv.lock
RUN Rscript -e 'renv::restore()'

# install cmdstan v 2.32.2 (version is key for making sure brms models work)
RUN Rscript -e "cmdstanr::install_cmdstan(version = '2.32.2')"

COPY _targets.R _targets.R
COPY R R

RUN mkdir -p figures

# Default to bash terminal when running docker image
CMD ["bash"]