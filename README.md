# cleanAD

### Archived on 2024-12-04

The specimen table script is no longer needed because we are no longer using dccvalidator or the underlying specimen table. The issues in this repo contain some documentation on the original harmonization/clean-up of the ROSMAP, MSBB, and MayoRNASeq study metadata in 2020 but otherwise this repo is not active.

Tools for cleaning and organizing study data for the AD Knowledge Portal.

## Installation

You can install the cleanAD package from GitHub using the `remotes`  package.

```R
remotes::install_github("Sage-Bionetworks/cleanAD")
```

## Scripts

### Generate Specimen ID Table

The R script in [inst/scripts/generate-table-ad.R](https://github.com/Sage-Bionetworks/cleanAD/blob/master/inst/scripts/generate-table-ad.R) will generate and upload a specimen ID table from metadata files and annotations in the format needed by the [dccvalidator](https://github.com/Sage-Bionetworks/dccvalidator) for checking metadata files against existing specimen and individual IDs. This script requires a configuration file located at [inst/config.yml](https://github.com/Sage-Bionetworks/cleanAD/blob/master/inst/config.yml). Currently, the script does not allow for configuration files that are not installed with the package.

#### Config Options

- **directories**: List of directories (Synapse synIDs) to query for metadata files. Assumes there is a folder called Metadata at the second level of the structure or at the third level of the structure, within a folder called Data. Assumes top level folders are study names.

  ```bash
  +-- studyname1
  |   +-- Metadata
  +-- studyname2
  |   +-- Data
  |   |   +-- Metadata
  ```

- **consortia_dir**: Directory (Synapse synID) with consortia-related studies. Only used to grab cross-consortium study names to remove from the final table to reduce duplication of IDs. Assumes first level folders are study names.
- **id_table**: The table (Synapse synID) to upload the final table to.
- **file_view**: A file view (Synapse synID) scoped to the relevant data files. Used for grabbing any specimen and individual IDs not found in the metadata files.
- **task_id** (optional): Synapse synID for task entity (folder or file). The script assumes the entity has, at minimum, a `completed_successfully` boolean annotation. If the task succeeds, the annotation will be updated to `true`, else `false`. Will only update annotations on the entity if the script was able to log into Synapse and has update permissions on the entity.
- **task_view** (optional): Synapse synID for file view scoped to task entities. Will query table to force update. Will only update if the script was able to log into Synapse and has permissions to download the view.
- **log_folder** (optional): Directory (Synapse synID) to upload log files to. Will only upload a log file if the script was able to log into Synapse, has permissions to upload to the directory, and fails to complete the task. If the task succeeds, no log is uploaded.

#### Using the Script

There are 3 ways to run the script: Rscript, bash script, docker.

_Note: This is not recommended for running locally unless testing on dummy data. You should always run this script in a safe computing environment to ensure no PHI is downloaded to your local system._

##### Rscript

Ensure cleanAD is installed in your local system and you are able to run scripts via Rscript. You should be able to run the script with the following:

````bash
Rscript ./cleanAD/inst/scripts/generate-table-ad.R --config <config to use (e.g. default)> --auth_token <Synapse personal access token or have local .synapseConfig>
````

##### Bash

If you are on a Linux or Mac computer, you can use the included bash script to launch the R script, [update_table.sh](https://github.com/Sage-Bionetworks/cleanAD/blob/master/update_table.sh). Ensure cleanAD is installed in your local system and you are able to run scripts via Rscript. You should be able to run the script with the following:

```bash
./cleanAD/update_table.sh <config to use (e.g. default)> <Synapse personal access token or have local .synapseConfig>
```

##### Docker

A docker image has been created for running this script. You can use the docker by either building the image yourself with the included [Dockerfile](https://github.com/Sage-Bionetworks/cleanAD/blob/add-docker/Dockerfile) or pulling the [sagebionetworks/cleanad](https://hub.docker.com/repository/docker/sagebionetworks/cleanad/general) image from the cloud. The DockerHub image is automatically built after pushes to the main branch in this repository, although there may be a lag of up to 6 hours before the image is updated.

```bash
docker pull sagebionetworks/cleanad:latest
docker run --rm --entrypoint "./cleanAD/update_table.sh" sagebionetworks/cleanad:latest <config to use (e.g. default)> <Synapse personal access token or have local .synapseConfig>
```

To build the image locally, follow the steps below.

```bash
git clone https://github.com/Sage-Bionetworks/cleanAD.git
cd cleanAD
docker build -t cleanad .
```

##### Running as a Scheduled Job in the Sage AWS Service Catalog

Provide a Synapse PAT to the Scheduled Job secrets field named `"SYNAPSE_AUTH_TOKEN":"<your-PAT-here>"`.

Pull the public docker image and provide the following command to execute in the container (`ad` and `TRUE` are arguments to `config` and `as_scheduled_job` respectively).

```bash
./cleanAD/scheduled_job_update_table.sh ad TRUE
```



