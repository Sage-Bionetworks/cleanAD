# Build an RStudio container with synapser

FROM rocker/rstudio:4.1.0

RUN apt-get update -y
RUN apt-get install -y dpkg-dev zlib1g-dev libssl-dev libffi-dev
RUN apt-get install -y curl libcurl4-openssl-dev
RUN R -e "install.packages('synapser', repos=c('http://ran.synapse.org', 'http://cran.fhcrc.org'))"

RUN install2.r --error \
    config \
    dplyr \
    glue \
    lubridate \
    purrr \
    readr \
    readxl \
    tidyr \
    log4r \
    mockery \
    optparse \
    testthat

RUN apt-get update --allow-releaseinfo-change && \
    apt-get install git-all -y
