% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locating-metadata-files.R
\name{gather_study_metadata_synIDs}
\alias{gather_study_metadata_synIDs}
\title{Gather Metadata synIDs By Study}
\usage{
gather_study_metadata_synIDs(dir_id, study)
}
\arguments{
\item{dir_id}{Synapse synID for study directory.}

\item{study}{Name of study.}
}
\description{
Gather synIDs for metadata files. Assumes metadata files will
be in a folder called \code{Metadata} or \code{metadata}. This folder will be either a
child of \code{dir_id} or in a folder called \code{Data} or \code{data} that is within
\code{dir_id}.
}
