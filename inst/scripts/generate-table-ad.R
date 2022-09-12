##################################
# Generate specimen table for AD #
##################################

# testing -- load_all
devtools::load_all()

# Import package -----
suppressPackageStartupMessages(library("cleanAD"))
suppressPackageStartupMessages(library("log4r"))
suppressPackageStartupMessages(library("lubridate"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("synapser"))

## CLI Args --------------------------------------------------------------------

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

# test scheduled job
# opts$as_scheduled_job <- TRUE

# test no schedule, but auth token
opts$as_scheduled_job <- FALSE
opts$auth_token <- Sys.getenv("SYNAPSE_PAT")
opts$config <- "test-ad"

# test no scheduled job and no auth token (default opts)
# opts$as_scheduled_job <- FALSE
# opts$auth_token <- NA

## Setup -----------------------------------------------------------------------

## Constants
FILE_VIEW_COLUMNS_BIND <- c("study", "individualID", "specimenID", "assay")
FILE_VIEW_COLUMNS_JOIN <- c("id", "dataType", "metadataType", "assay")
RELEVANT_METADATA_COLUMNS <- c("individualID", "specimenID", "assay")
METADATA_TYPES <- c("biospecimen", "assay", "individual")

## Create logger if want to generate logs
log_path <- NA
logger <- NA
upload_log <- FALSE
if (!is.na(get_config("log_folder", opts$config))) {
  ## Create temp directory to store log in
  if(!dir.exists("LOGS")) {
    dir.create("LOGS")
  }
  logfile_name <- glue::glue("{year(today())}-{month(today())}")
  log_path <- glue::glue("LOGS/{logfile_name}.log")
  logger <- create.logger(logfile = log_path, level = "INFO")
  upload_log <- TRUE
}

## If running as scheduled job, extract auth token secret

if(isTRUE(opts$as_scheduled_job)) {
  authToken <-
    rjson::fromJSON(Sys.getenv("SCHEDULED_JOB_SECRETS"))$SYNAPSE_AUTH_TOKEN
  } else if (!is.na(opts$auth_token)) {
    authToken <- opts$auth_token
  } else {
    authToken <- NA
  }

## Login with Synapser
# First look for authToken; if not provided, use local synapseCreds

tryCatch(
  {
    if(is.na(authToken)) {
      synLogin()
    } else {
      synLogin(authToken = opts$auth_token)
    }
  },
  error = function(e) {
    if (upload_log) {
      failure_message <- glue::glue(
        "Log in error:\n  {e$message}"
      )
      error(logger, failure_message)
      upload_log_file(
        folder = get_config("log_folder", opts$config),
        path = log_path
      )
    }
    quit(status = 1)
  }
)

## Grab annotations on task, if provided with task_id
## If not provided with task_id, don't update
update_task <- FALSE
annots <- NA
if (!is.na(get_config("task_id", opts$config))) {
  tryCatch(
    {
      annots <<- synapser::synGetAnnotations(get_config("task_id", opts$config))
      update_task <<- TRUE
    },
    error = function(e) {
      failure_message <- glue::glue(
        "Could not gather task annotations:\n  {e$message}"
      )
      error(logger, failure_message)
      upload_log_file(
        folder = get_config("log_folder", opts$config),
        path = log_path
      )
      quit(status = 1)
    }
  )
}

## Setup done ------------------------------------------------------------------

## Get study metadata file IDs -----
all_files <- tryCatch(
  {
    purrr::map_dfr(get_config("directories", opts$config), ~ gather_metadata_synIDs_all(.))
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        task_id = get_config("task_id", opts$config),
        annots = annots,
        success = "false",
        task_view = get_config("task_view", opts$config)
      )
      if (upload_log) {
        failure_message <- glue::glue(
          "There was a problem getting synIDs for metadata files:\n  {e$message}"
        )
        error(logger, failure_message)
        upload_log_file(
          folder = get_config("log_folder", opts$config),
          path = log_path
        )
      }
    }
    quit(status = 1)
  }
)

## Remove files that are probably not metadata -----

## Only keep csv files that don't have 'dictionary' in name
all_files <- all_files[grepl(".csv$", all_files$name, ignore.case = TRUE), ]
all_files <- all_files[!grepl("dictionary", all_files$name, ignore.case = TRUE), ] # nolint
## Fix id column to be character
all_files$id <- as.character(all_files$id)

## Grab file view for annotations -----

view_query <- tryCatch(
  {
    synapser::synTableQuery(
      glue::glue("SELECT * FROM {get_config('file_view', opts$config)}")
    )$asDataFrame()
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        task_id = get_config("task_id", opts$config),
        annots = annots,
        success = "false",
        task_view = get_config("task_view", opts$config)
      )
      if (upload_log) {
        failure_message <- glue::glue(
          "There was a problem getting the file view:\n  {e$message}"
        )
        error(logger, failure_message)
        upload_log_file(
          folder = get_config("log_folder", opts$config),
          path = log_path
        )
      }
    }
    quit(status = 1)
  }
)

## Check that view has needed columns

missing_cols <- setdiff(
  unique(c(FILE_VIEW_COLUMNS_BIND, FILE_VIEW_COLUMNS_JOIN)),
  colnames(view_query)
)
if (length(missing_cols) > 0) {
  if (update_task) {
    update_task_annotation(
      task_id = get_config("task_id", opts$config),
      annots = annots,
      success = "false",
      task_view = get_config("task_view", opts$config)
    )
    if (upload_log) {
      missing <- glue::glue_collapse(missing_cols, sep = ", ")
      failure_message <- glue::glue(
        "The file view is missing these columns:\n  {missing}"
      )
      error(logger, failure_message)
      upload_log_file(
        folder = get_config("log_folder", opts$config),
        path = log_path
      )
    }
  }
  quit(status = 1)
}

## Grab metadata-related annotations from file view and join -----

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

## Join metadata -----

## Open files and gather IDs
all_meta_ids <- tryCatch({
    gather_ids_all_studies(all_files)
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        task_id = get_config("task_id", opts$config),
        annots = annots,
        success = "false",
        task_view = get_config("task_view", opts$config)
      )
      if (upload_log) {
        failure_message <- glue::glue(
          "There was a problem gathering metadata from the files:\n  {e$message}"
        )
        error(logger, failure_message)
        upload_log_file(
          folder = get_config("log_folder", opts$config),
          path = log_path
        )
      }
    }
    quit(status = 1)
  }
)

## Grab file annotations to add missing individuals/specimens -----
all_annots <- view_query[, FILE_VIEW_COLUMNS_BIND]

# Remove json from annotated study names and assays
all_annots[, "study"] <- unlist(purrr::map(
  all_annots$study,
  ~ clean_json_string(., remove_spaces = TRUE)
))

all_annots[, "assay"] <- unlist(purrr::map(
  all_annots$assay,
  ~ clean_json_string(., remove_spaces = FALSE)
))

# Separate into multiple rows for IDs that have multiple study annotations
all_annots <- tidyr::separate_rows(all_annots, study, sep = ",")
# separate into multiple rows for IDS that have multiple assay annotations
all_annots <- tidyr::separate_rows(all_annots, assay, sep = ",")
# Remove any with consortia study name
if (!is.na(get_config("consortia_dir", opts$config))) {
  all_annots <- tryCatch(
    {
      consortia_studies <- child_names(get_config("consortia_dir", opts$config))
      all_annots[!all_annots$study %in% consortia_studies, ]
    },
    error = function(e) {
      if (update_task) {
        update_task_annotation(
          task_id = get_config("task_id", opts$config),
          annots = annots,
          success = "false",
          task_view = get_config("task_view", opts$config)
        )
        if (upload_log) {
          failure_message <- glue::glue(
            "There was a problem gathering consortia study names:\n  {e$message}"
          )
          error(logger, failure_message)
          upload_log_file(
            folder = get_config("log_folder", opts$config),
            path = log_path
          )
        }
      }
      quit(status = 1)
    }
  )
}

## Remove rows where both specimenID and individualID are NA
all_annots <- all_annots[
  !(is.na(all_annots$specimenID) & is.na(all_annots$individualID)),
]

## Bind together with metadata to get full set -----
all_ids <- add_missing_specimens(meta_df = all_meta_ids, annot_df = all_annots)

## Grab samples table, delete old data, add new data -----
#! COMMENTED OUT BECAUSE THIS IS DESTRUCTIVE -- MAKE SURE THE SCRIPT
#! IS CORRECT BEFORE DEPLOYING
tryCatch(
  {
    update_samples_table(
      table_id = get_config("id_table", opts$config),
      new_data = all_ids
    )
  },
  error = function(e) {
    if (update_task) {
      update_task_annotation(
        task_id = get_config("task_id", opts$config),
        annots = annots,
        success = "false",
        task_view = get_config("task_view", opts$config)
      )
      if (upload_log) {
        failure_message <- glue::glue(
          "There was a problem updating the specimen table:\n  {e$message}"
        )
        error(logger, failure_message)
        upload_log_file(
          folder = get_config("log_folder", opts$config),
          path = log_path
        )
      }
    }
    quit(status = 1)
  }
)

## Should have finished successfully -----

if (update_task) {
  update_task_annotation(
    task_id = get_config("task_id", opts$config),
    annots = annots,
    success = "true",
    task_view = get_config("task_view", opts$config)
  )
}

quit(status = 0)
