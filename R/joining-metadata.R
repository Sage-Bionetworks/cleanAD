#' @title Join study metadata set
#'
#' @description Join full study metadata set by specimenID and individualID.
#' Note: this is currently limited in scope to just individualID, specimenID,
#' and assay. Additionally, will give back error if only given assay(s) or a
#' biospecimen file with no other metadata; OR if there is more than one
#' individual or biospecimen metadata file associated with a study.
#'
#' TODO: in case of above errors, should find a way to indicate which study
#' cause the problem.
#'
#' @param meta_files Dataframe with columns `metadataType` (`assay`,
#' `individual`, or `biospecimen`), `assay` (type of assay), and `data` (nested
#' dataframe with the metadata from the file).
#' @return Dataframe with columns `individualID`, `specimenID`, `assay`.
join_full_study_metadata <- function(meta_files) {
  # Missing files should be indicated as NA
  file_indices <- get_file_indices(
    meta_files,
    meta_types = c("individual", "biospecimen", "assay")
  )
  # Order should be as given in above function
  # individual, biospecimen, assay
  not_na <- purrr::map(file_indices, ~ !is.na(.))

  # No metadata files at all; shouldn't run into this, but return for safety
  if (all(!unlist(not_na)) | all(is.na(meta_files$data))) {
    return(NA)
  }

  # Can't have more than one individual metadata file per study
  if (length(file_indices$individual) > 1) {
    stop("A study cannot have more than one individual metadata file")
    }

  # Can't have more than one biospecimen metadata file per study
  if (length(file_indices$biospecimen) > 1) {
    stop("A study cannot have more than one biospecimen metadata file")
  }

  # All metadata types present
  if (all(unlist(not_na))) {
    metadata <- join_ids_assay_all(meta_files)
    # Would be odd, but need to add any missing info from biospecimen
    bio_df <- meta_files$data[[file_indices$biospecimen]]
    metadata <- add_biospecimen_metadata(
      bio_df = bio_df,
      meta_subset = metadata
    )
    # Add missing individuals
    ind_df <- meta_files$data[[file_indices$individual]]
    metadata <- add_individual_metadata(
      ind_df = ind_df,
      meta_subset = metadata
    )
    # Reorder
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Only assay + individual -- likely
  if (all(not_na$individual, !not_na$biospecimen, unlist(not_na$assay))) {
    metadata <- join_ids_assay_all(meta_files)
    # Might be missing specimenID or individualID
    add_missing_meta_columns(metadata)
    # Add missing individuals
    ind_df <- meta_files$data[[file_indices$individual]]
    metadata <- add_individual_metadata(
      ind_df = ind_df,
      meta_subset = metadata
    )
    # Reorder
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Assay + biospecimen, but no individual -- unlikely
  if (all(!not_na$individual, not_na$biospecimen, unlist(not_na$assay))) {
    metadata <- join_ids_assay_all(meta_files)
    # Might be missing specimenID or individualID
    add_missing_meta_columns(metadata)
    # Would be odd, but need to add any missing info from biospecimen
    bio_df <- meta_files$data[[file_indices$biospecimen]]
    metadata <- add_biospecimen_metadata(
      bio_df = bio_df,
      meta_subset = metadata
    )
    # Reorder
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Only individual + biospecimen -- possible, if less likely
  if (all(not_na$individual, not_na$biospecimen, !all(not_na$assay))) {
    metadata <- dplyr::left_join(
      meta_files$data[[file_indices$individual]],
      meta_files$data[[file_indices$biospecimen]]
    )
    # Might be missing assay col; add as NA
    metadata <- add_missing_meta_columns(metadata)
    # Reorder
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Individual only -- possible
  if (all(not_na$individual, !not_na$biospecimen, !all(not_na$assay))) {
    # Add specimenID and assay cols as NA
    metadata <- add_missing_meta_columns(
      meta_files$data[[file_indices$individual]]
    )
    # Reorder
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Assay(s) only -- very unlikely; would only happen in cases of individualID
  # assays (e.g. MRI, PET) with individuals from a different study
  if (all(!not_na$individual, !not_na$biospecimen, all(not_na$assay))) {
    # Join up the assays
    metadata <- join_ids_assay_all(meta_files)
    metadata <- metadata[, c("individualID", "specimenID", "assay")]
    return(metadata)
  }

  # Should have returned by now. Error if get to this.
  # This means should have come across an oddity, like a single biospecimen file
  stop(
    "The metadata set does not have any expected combinations of filetypes"
  )
}

#' @title Join all assay metadata
#'
#' @description Join all assay metadata files. Note: this is currently limited
#' in scope to `individualID`, `specimenID`, and `assay` columns. Requires all
#' metadata types to be present in `meta_files`.
#'
#' @noRd
#' @inheritParams join_full_study_metadata
join_ids_assay_all <- function(meta_files) {
  # Find biospecimen metadata or send as NA
  indices <- get_file_indices(
    meta_files = meta_files,
    meta_types = "biospecimen"
  )
  bio_df <- NA
  if (!is.na(indices$biospecimen) &
    !all(is.na(meta_files$data[indices$biospecimen]))) {
    bio_df <- meta_files$data[[indices$biospecimen]]
  }
  assay_indices <- meta_files$metadataType %in% "assay"
  joined_dfs <- purrr::map2(
    meta_files$data[assay_indices],
    meta_files$assay[assay_indices], function(df, assay) {
      join_ids_assay(
        assay_df = df,
        bio_df = bio_df,
        assay = assay
      )
    }
  )
  # Assays with only individualID will be missing specimenID col; add as NA
  joined_dfs <- purrr::map(joined_dfs, ~ add_missing_meta_columns(.))
  # Combine into one df
  df_joined <- Reduce(rbind, joined_dfs)
  return(df_joined)
}

#' @title Add missing biospecimens
#'
#' @description Add missing biospecimens to metadata set. Note: this is
#' currently limited in scope to `individualID`, `specimenID`, `assay`.
#'
#' @noRd
#' @param bio_df Dataframe with biospecimen metadata.
#' @param meta_subset Dataframe of metadata to add missing biospecimens to.
add_biospecimen_metadata <- function(bio_df, meta_subset) {
  # Check if there are any missing specimens
  missing <- setdiff(bio_df$specimenID, meta_subset$specimenID)
  if (length(missing) < 1) {
    return(meta_subset)
  } else {
    # Check if biospecimen has assay col
    has_assay <- !is.na(has_col(bio_df, "assay"))
    if (!has_assay) {
      # No assay col, just add as NA
      bio_df <- add_missing_meta_columns(bio_df)
    }
    # Bind rows with missing specimens
    bio_missing <- bio_df[bio_df$specimenID %in% missing, ]
    metadata <- rbind(meta_subset, bio_missing)
    return(metadata)
  }
}

#' @title Add missing individuals
#'
#' @description Add missing individuals to metadata set. Note: this is
#' currently limited in scope to `individualID`, `specimenID`, `assay`.
#'
#' @noRd
#' @inheritParams add_biospecimen_metadata
#' @param ind_df Dataframe with individual metadata.
add_individual_metadata <- function(ind_df, meta_subset) {
  # Check all individuals in dataset
  missing_ids <- setdiff(ind_df$individualID, meta_subset$individualID)
  # Remove NA
  missing_ids <- missing_ids[!is.na(missing_ids)]
  if (length(missing_ids) > 0) {
    # Filter to missing ids
    missing_df <- ind_df[ind_df$individualID %in% missing_ids, ]
    # Add assay, specimenID as NA
    missing_df <- add_missing_meta_columns(missing_df)
    # Add ids to end of joined assay metadata
    meta_subset <- rbind(meta_subset, missing_df)
  }
  return(meta_subset)
}

#' @title Has column
#'
#' @description Check if column(s) of interest exist in dataframe.
#'
#' @noRd
#' @param df Data frame
#' @param col_name Vector of column names to check for.
#' @return col_name that exist in df. If no col_name in df, return `NA`.
has_col <- function(df, col_name) {
  names <- col_name[col_name %in% colnames(df)]
  if (length(names) < 1) {
    return(NA)
  }
  return(names)
}

#' @title Get ID & assay
#'
#' @description Returns specimenID or individualID with assay information.
#'
#' @noRd
#' @param assay_df Dataframe of assay metadata.
#' @param bio_df Dataframe of biospecimen metadata.
#' @param assay Expected assay type.
#' @return Data frame with individualID or specimenID and assay. For assays with
#' specimenIDs, if the biospecimen metadata does not contain an assay column,
#' or the specimens in the assay metadata do not exist in the biospecimen
#' metadata, then the assay column will be the expected assay type. For assays
#' with individualID, the assay column will be the expected assay type.
join_ids_assay <- function(assay_df, assay, bio_df = NA) {
  # Make sure assay has an assay column and fill in any missing with
  # expected assay
  assay_df <- add_update_assay(assay_df, assay)
  # If assay has individualID, should be done
  # If assay has specimenID, send off to get processed
  if ("specimenID" %in% names(assay_df) & inherits(bio_df, "data.frame")) {
    assay_df <- join_ids_assay_specimen(assay_df, bio_df, assay)
  }
  # If no bio_df, then there's something odd going on and send back as is
  return(assay_df)
}

#' @title Add or update assay
#'
#' @description Add or update assay column.
#'
#' If assay column exists, adds `assay` to any missing values in column. If
#' assay column does not exist, adds column with value `assay`.
#'
#' @noRd
#' @inheritParams join_ids_assay
add_update_assay <- function(assay_df, assay) {
  has_assay <- !is.na(has_col(assay_df, "assay"))
  if (has_assay) {
    # Replace any NA with given assay
    assay_df$assay[is.na(assay_df$assay)] <- assay
  } else {
    # Use given assay
    assay_df[, "assay"] <- assay
  }
  return(assay_df)
}

#' @title Join assay and biospecimen metadata
#'
#' @description Join assay and biospecimen metadata. Note: this is
#' currently limited in scope to `individualID`, `specimenID`, `assay`.
#'
#' @noRd
#' @inheritParams join_ids_assay
join_ids_assay_specimen <- function(assay_df, bio_df, assay) {
  bio_has_assay <- !is.na(has_col(bio_df, "assay"))
  if (bio_has_assay) {
    # Are the assays in assay_df in bio_df?
    assays_exist <- unique(assay_df$assay) %in% unique(na.omit(bio_df$assay))
    if (all(assays_exist)) {
      # Looks good, join will join on specimenID and assay
      assay_df <- dplyr::left_join(assay_df, bio_df)
    } else if (any(assays_exist)) {
      # Uh oh...not all assays in assay metadata are in the biospecimen metadata
      same_assays <- unique(assay_df$assay)[assays_exist]
      # Join the rows of each where assays exist in both
      temp_df_same <- dplyr::left_join(
        assay_df[assay_df$assay %in% same_assays, ],
        bio_df[bio_df$assay %in% same_assays, ]
      )
      # then assume any bio_df with the same specimenID and has NA for assay is
      # for this assay
      temp_df_missing <- dplyr::left_join(
        assay_df[!(assay_df$assay %in% same_assays), ],
        bio_df[is.na(bio_df$assay), c("individualID", "specimenID")]
      )
      # Bind the two sets together, making sure column order is same
      assay_df <- rbind(temp_df_same, temp_df_missing[colnames(temp_df_same)])
    } else {
      # ruh-roh, Raggy! No assays in the assay metadata are in biospecimen
      # Assume any bio_df with the same specimenID and has NA for assay is
      # for this assay
      assay_df <- dplyr::left_join(
        assay_df,
        bio_df[is.na(bio_df$assay), c("individualID", "specimenID")]
      )
    }
  } else {
    # Biospecimen doesn't have assay column
    # Assume any bio_df with the same ID(s) is for this assay metadata
    assay_df <- dplyr::left_join(assay_df, bio_df)
  }
  return(assay_df)
}

#' @title Add missing specimenID rows
#'
#' @description Add missing `specimenID`s from `annot_df` to `meta_df`. Groups
#' the check for missing `specimenIDs` by `assay` and `study`.
#'
#' @export
#' @param meta_df Dataframe with columns `specimenID`, `individualID`, `assay`,
#' `study`.
#' @param annot_df Dataframe with columns `specimenID`, `individualID`, `assay`,
#' `study`.
add_missing_specimens <- function(meta_df, annot_df) {
  ## Make a new unique ID based on assay, study, and specimenID
  meta_df[, "new_id"] <- glue::glue(
    "{meta_df$study}_{meta_df$assay}_{meta_df$specimenID}"
  )
  annot_df[, "new_id"] <- glue::glue(
    "{annot_df$study}_{annot_df$assay}_{annot_df$specimenID}"
  )
  ## Grab the rows with newids that are not in the meta_df
  missing <- annot_df[!(annot_df$new_id %in% meta_df$new_id), ]
  ## Bind and pass back with only the rows we originally expected
  all_ids <- rbind(meta_df, missing)
  return(all_ids[, c("individualID", "specimenID", "assay", "study")])
}
