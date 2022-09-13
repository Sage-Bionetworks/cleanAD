#' @title Gather IDs from metadata files
#'
#' @description Assumes all metadata files are csv with some combination of
#' columns: `individualID`, `specimenID`, `assay`. Gathers by study grouping.
#'
#' @param all_files dataframe with columns `id` (synID of file), `study` (name
#' of related study), `metadataType` (`assay`, `individual`, or `biospecimen`),
#' `assay` (type of assay), `dataType` (type of data).
#' @return Dataframe with columns `individualID`, `specimenID`, `assay`,
#' `study` containing all relevant metadata from `all_files`.
#' @export
gather_ids_all_studies <- function(all_files) {
  all_studies <- unique(all_files$study)
  all_metadata <- purrr::map(all_studies, function(study) {
    study_metadata <- all_files[all_files$study %in% study, ]
    study_metadata <- get_all_study_metadata(study_metadata)
    # Remove rows where data is missing
    study_metadata <- study_metadata[!is.na(study_metadata$data), ]
    study_metadata <- join_full_study_metadata(study_metadata)
    if (!inherits(study_metadata, "data.frame")) {
      return(as.character(NA))
    }
    study_metadata[, "study"] <- unique(study)
    return(study_metadata)
  })
  Reduce(rbind, all_metadata[!is.na(all_metadata)])
}

#' @title Get Metadata from Files
#'
#' @description Add a nested dataframe column that contains only the desired
#' data from each metadata file.
#'
#' @noRd
#' @param meta_files dataframe with columns `id` (synID of file), `study` (name
#' of related study), `metadataType` (`assay`, `individual`, or `biospecimen`),
#' `assay` (type of assay), `dataType` (type of data).
#' @param columns Vector of column names to keep in the data. Discards all other
#' metadata columns. Default is `c("individualID", "specimenID", "assay")`.
#' @return `meta_files` with additional list column called `data` with the data
#' from each file.
get_all_study_metadata <- function(meta_files,
                                   columns = c("individualID", "specimenID", "assay")) { # nolint
  study_data <- purrr::map2(
    meta_files$id,
    meta_files$assay,
    function(id, assay) {
      if (is.na(id)) {
        return(as.character(NA))
      }
      metadata <- get_file_data(id)
      if (!inherits(metadata, "data.frame")) {
        return(as.character(NA))
      }
      metadata <- metadata[, colnames(metadata) %in% columns]
      if (length(metadata) == 0) {
        return(as.character(NA))
      }
      return(metadata)
    }
  )
  meta_files[, "data"] <- list(study_data)
  return(meta_files)
}

#' @title Add Missing Metadata Rows
#'
#' @description Add a row to the dataframe for each missing metadata type. This
#' is just a helper for joining the data later on.
#'
#' @param meta_files dataframe with columns `id` (synID of file), `study` (name
#' of related study), `metadataType` (`assay`, `individual`, or `biospecimen`),
#' `assay` (type of assay), `dataType` (type of data).
#' @return `meta_files` with an extra row for each missing metadata type.
#' @noRd
add_missing_meta_rows <- function(meta_files) {
  file_indices <- get_file_indices(meta_files)
  # If any type is `integer(0)`, then add NA row to meta_files
  # Makes it easier to join in correct order later
  if (length(file_indices$individual) == 0) {
    meta_files <- add_empty_row(meta_files, type = "individual")
  }
  if (length(file_indices$biospecimen) == 0) {
    meta_files <- add_empty_row(meta_files, type = "biospecimen")
  }
  if (length(file_indices$assay) == 0) {
    meta_files <- add_empty_row(meta_files, type = "assay")
  }
  return(meta_files)
}

#' @title Add Empty Row
#'
#' @description Adds an empty row to dataframe with columns `id` (set to `NA`),
#' `metadataType` (set to `type`), and `study` (set to the unique value of
#' `meta_files$study`). This is just a helper for joining the data later.
#'
#' @param meta_files Dataframe with study metadata file information in columns
#' `id` (synID of file), `metadataType` (type of metadata file), `study` (study
#' name).
#' @param type The type of metadata to add an `NA` row to the dataframe for.
#' @return `meta_files` with extra row for the metadataType indicated by `type`.
#' @noRd
add_empty_row <- function(meta_files, type) {
  df_row <- data.frame(
    id = NA,
    metadataType = type,
    study = unique(meta_files$study)
  )
  # Add any extra columns that meta_files has
  for (name in setdiff(colnames(meta_files), colnames(df_row))) {
    df_row[, name] <- as.character(NA)
  }
  # Reorder columns
  df_row <- df_row[, colnames(meta_files)]
  rbind(meta_files, df_row)
}

#' @title Add Missing Metadata Columns
#'
#' @description Adds in an `NA` column for each missing metadata column that is
#' expected. This is just a helper for joining the data later on.
#'
#' @param metadata Dataframe with study metadata.
#' @param expected_columns Vector of expected metadata columns. Default is
#' `c("individualID", "specimenID", "assay")`.
#' @return `metadata` with extra `NA` column for each of the missing
#' `expected_columns`.
#' @noRd
add_missing_meta_columns <- function(metadata,
                                     expected_columns = c(
                                       "individualID",
                                       "specimenID",
                                       "assay"
                                     )) {
  if (!inherits(metadata, "data.frame")) {
    return(NA)
  }
  if (all(expected_columns %in% colnames(metadata))) {
    return(metadata)
  }
  missing_colnames <- expected_columns[
    !(expected_columns %in% colnames(metadata))
  ]
  for (col_name in missing_colnames) {
    metadata[, col_name] <- as.character(NA)
  }
  return(metadata)
}

#' @title Get Indices for All Metadata File Types
#'
#' @description Get a list of file indices for each metadata file type. There
#' should be only one individual file and one biospecimen file per study.
#'
#' @param meta_files Dataframe with column `metadataType` (type of metadata).
#' @param meta_types Vector of metadata types to get indices for. Default is
#' `c("biospecimen", "assay", "individual")`.
#' @export
get_file_indices <- function(meta_files,
                             meta_types = c(
                               "biospecimen",
                               "assay",
                               "individual"
                             )) { # nolint
  file_indices <- purrr::map(
    meta_types,
    function(meta_type) {
      inds <- which(meta_files$metadataType %in% meta_type)
      if (length(inds) < 1) {
        return(NA)
      }
      return(inds)
    }
  )
  names(file_indices) <- meta_types
  return(file_indices)
}

#' @title Get File Data
#'
#' @description Download file from Synapse and get data from it.
#'
#' @param syn_id Synapse synID for a file.
#' @return Dataframe with the file data or `NA` if the file could not be
#' opened.
#' @export
get_file_data <- function(syn_id) {
  # Be explicit in col_types to stop the messages about parsing
  # Also stop it from making NA into strings
  tryCatch(
    {
      readr::read_csv(
        synapser::synGet(syn_id)$path,
        na = character(),
        col_types = readr::cols(.default = "c")
      )
    },
    error = function(e) {
      return(as.character(NA))
    }
  )
}
