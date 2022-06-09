# NOTE: Build without caching to ensure latest version of git repo
#       docker build --no-cache -t cleanad .
# Would be better if synapser docker images were tagged
FROM sagebionetworks/synapser:latest

RUN install2.r --error \
    config \
    dplyr \
    glue \
    lubridate \
    purrr \
    readr \
    readxl \
    rjson \
    tidyr \
    log4r \
    mockery \
    optparse \
    testthat

RUN apt-get update --allow-releaseinfo-change && \
    apt-get install git-all -y

# Clone repo and install
# Github API call will return different results if head changes, invalidating the cache for this step
WORKDIR /
ADD https://api.github.com/repos/Sage-Bionetworks/cleanAD/git/refs/heads/master version.json

RUN git clone https://github.com/Sage-Bionetworks/cleanAD.git
WORKDIR /cleanAD

RUN chmod +x update_table.sh scheduled_job_update_table.sh

RUN R CMD INSTALL .

CMD ["/bin/bash"]
