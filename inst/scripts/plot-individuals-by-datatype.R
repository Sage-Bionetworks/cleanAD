suppressPackageStartupMessages(library("cleanAD"))
suppressPackageStartupMessages(library("log4r"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("synapser"))

option_list <- list(
  optparse::make_option(
    "--as_scheduled_job",
    type = "logical",
    action = "store",
    default = FALSE,
    help = "Logical value indicating whether the specimen table update script is
            being run as an AWS Schedule Job. If TRUE, a Synapse PAT will be read
            from the SCHEDULE_JOB_SECRETS parameter and the --auth_token argument
            should be left empty. If FALSE (default), a
            PAT must by provided to the --auth_token argument or local Synapse
            credentials must be available."
  ),
  optparse::make_option(
    "--auth_token",
    type = "character",
    action = "store",
    default = NA,
    help = "Synapse personal access token to log in with [default = %default].
            If no token given, assumes a local .synapseConfig file exists with
            credentials."
  ),
  optparse::make_option(
    "--config",
    type = "character",
    action = "store",
    help = "Synapse synIDs for top level directories to search for metadata as
            comma-separated list (e.g. --directories syn123,syn789). Folders
            within these directories should be organized as follows. The first
            level within the directories should be study folders, named with the
            study name. There are two locations allowed for metadata folders to
            exist within the study folder: either the first level of the study
            folder or the second level of the study folder, within a folder
            called Data. In either case, the folder should be called Metadata."
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opts <- optparse::parse_args(opt_parser)

# test no scheduled job and no auth token (default opts)
opts$as_scheduled_job <- FALSE
opts$auth_token <- NA
#opts$config <- "config.yml"

## Setup -----------------------------------------------------------------------

## Constants
FILE_VIEW_COLUMNS_BIND <- c("study", "individualID", "specimenID", "assay")
FILE_VIEW_COLUMNS_JOIN <- c("id", "dataType", "metadataType", "assay")
RELEVANT_METADATA_COLUMNS <- c("individualID", "specimenID", "assay")
METADATA_TYPES <- c("biospecimen", "assay", "individual")

synLogin()

## files --------------
update_task <- FALSE
upload_log <- FALSE

human_dir <- "syn5550382"

all_files <- gather_metadata_synIDs_all(human_dir)

# remove dictionaries
all_files <- all_files[grepl(".csv$", all_files$name, ignore.case = TRUE), ]
all_files <- all_files[!grepl("dictionary", all_files$name, ignore.case = TRUE), ]

# get fileview
view_query <- synapser::synTableQuery("SELECT * FROM syn11346063")$asDataFrame()

view_meta <- view_query[view_query$id %in% all_files$id, ]
all_files <- dplyr::left_join(
  all_files,
  view_meta[, FILE_VIEW_COLUMNS_JOIN]
)
## Remove any with metadataType dictionary or protocol
all_files <- all_files[!grepl("dictionary|protocol", all_files$metadataType), ]

## Remove JSON array formatting from  columns with type STRING_LIST
all_files[, "assay"] <- unlist(purrr::map(
  all_files$assay,
  ~ clean_json_string(., remove_spaces = FALSE)
))

all_meta_ids <- gather_ids_all_studies(all_files)

all_meta_ids %>%
  filter(assay == 'scrnaSeq' | assay == 'snrnaSeq' | assay == "10x Multiome") %>%
  distinct(individualID)

all_meta_ids %>%
  group_by(study, assay) %>%
  filter(!(is.na(assay) | assay == "" | assay == "NA")) %>%
  summarize(n_ind = n_distinct(individualID), n_spec = n_distinct(specimenID))

all_meta_ids %>%
  group_by(study, assay) %>%
  filter(!(is.na(assay) | assay == "" | assay == "NA")) %>%
  summarize(n_ind = n_distinct(individualID), n_spec = n_distinct(specimenID)) %>%
  distinct(study) %>%
  print(n = Inf)

# combine all ROSMAP satellites and all ADNI studies
ind_study_assay <- all_meta_ids %>%
  mutate(comb_study = case_when(str_detect(study, "ROSMAP") ~ "ROSMAP+",
                                str_detect(study, "snRNAseq") ~ "ROSMAP+",
                                str_detect(study, "ADNI") ~ "ADNI+",
                                TRUE ~ study)) %>%
  filter(!(is.na(assay) | assay == "" | assay == "NA")) %>%
  group_by(comb_study, assay) %>%
  summarize(n_ind = n_distinct(individualID), n_spec = n_distinct(specimenID)) %>%
  ungroup()

# create assay to datatype map
tmp <- tempdir()
write_csv(ind_study_assay %>%
  select(assay) %>%
  distinct(), paste0(tmp, "assay-datatype-map.csv"))

synStore(File(path = paste0(tmp, "assay-datatype-map.csv"), parent = "syn23747317"))

dtmap <- read_csv(synGet("syn52385599")$path)

# join to df
ind_study_assay %>%
  left_join(dtmap)

study_by_datatype <- all_meta_ids %>%
  left_join(dtmap) %>%
  mutate(comb_study = case_when(str_detect(study, "ROSMAP") ~ "ROSMAP+",
                                str_detect(study, "snRNAseq") ~ "ROSMAP+",
                                str_detect(study, "ADNI") ~ "ADNI+",
                                TRUE ~ study)) %>%
  filter(!(is.na(assay) | assay == "" | assay == "NA")) %>%
  group_by(comb_study, extra) %>%
  summarize(n_ind = n_distinct(individualID)) %>%
  ungroup()

write_csv(study_by_datatype, paste0(tmp, "/study_by_datatype.csv"))
synStore(File(paste0(tmp, "/study_by_datatype.csv"), "syn23747317"))

study_by_datatype %>%
  group_by(extra) %>%
  summarize(total_ind = sum(n_ind)) %>%
  filter(total_ind > 500) %>%
  ggplot(aes(x = reorder(extra, desc(total_ind)), y = total_ind)) +
  geom_col()

all_meta_ids %>%
  distinct(individualID)

