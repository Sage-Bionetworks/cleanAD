% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joining-metadata.R
\name{join_full_study_metadata}
\alias{join_full_study_metadata}
\title{Join study metadata set}
\usage{
join_full_study_metadata(meta_files)
}
\arguments{
\item{meta_files}{Dataframe with columns \code{metadataType} (\code{assay},
\code{individual}, or \code{biospecimen}), \code{assay} (type of assay), and \code{data} (nested
dataframe with the metadata from the file).}
}
\value{
Dataframe with columns \code{individualID}, \code{specimenID}, \code{assay}.
}
\description{
Join full study metadata set by specimenID and individualID.
Note: this is currently limited in scope to just individualID, specimenID,
and assay. Additionally, will give back error if only given assay(s) or a
biospecimen file with no other metadata; OR if there is more than one
individual or biospecimen metadata file associated with a study.

TODO: in case of above errors, should find a way to indicate which study
cause the problem.
}
