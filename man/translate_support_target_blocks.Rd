% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MTP_load.R
\name{translate_support_target_blocks}
\alias{translate_support_target_blocks}
\title{Translate support engines tps in blocks}
\usage{
translate_support_target_blocks(
  mtp_directory,
  project_name,
  reftime,
  refsleep,
  timeout_translations,
  remDr
)
}
\arguments{
\item{mtp_directory}{Working directory (string)}

\item{project_name}{Translation project name (string)}

\item{reftime}{Refreshing time, numeric. Default 60 (seconds)}

\item{refsleep}{Sleeping time, numeric. Default 60 (seconds)}

\item{timeout_translations}{Default 20 seconds}

\item{remDr}{The active remote driver}
}
\value{
support_translations_file tcs A data frame with the TCs and the related score

A data frame with appending support translations
}
\description{
Translate support engines tps in blocks
}
