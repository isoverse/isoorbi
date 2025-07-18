% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blocks_functions.R
\name{orbi_adjust_block}
\alias{orbi_adjust_block}
\title{Manually adjust block delimiters}
\usage{
orbi_adjust_block(
  dataset,
  block,
  filename = NULL,
  shift_start_time.min = NULL,
  shift_end_time.min = NULL,
  shift_start_scan.no = NULL,
  shift_end_scan.no = NULL,
  set_start_time.min = NULL,
  set_end_time.min = NULL,
  set_start_scan.no = NULL,
  set_end_scan.no = NULL
)
}
\arguments{
\item{dataset}{tibble produced by \code{\link[=orbi_define_blocks_for_dual_inlet]{orbi_define_blocks_for_dual_inlet()}}}

\item{block}{the block for which to adjust the start and/or end}

\item{filename}{needs to be specified only if the \code{dataset} has more than one \code{filename}}

\item{shift_start_time.min}{if provided, the start time of the block will be shifted by this many minutes (use negative numbers to shift back)}

\item{shift_end_time.min}{if provided, the end time of the block will be shifted by this many minutes (use negative numbers to shift back)}

\item{shift_start_scan.no}{if provided, the start of the block will be shifted by this many scans (use negative numbers to shift back)}

\item{shift_end_scan.no}{if provided, the end of the block will be shifted by this many scans (use negative numbers to shift back)}

\item{set_start_time.min}{if provided, sets the start time of the block as close as possible to this time}

\item{set_end_time.min}{if provided, sets the end time of the block as close as possible to this time}

\item{set_start_scan.no}{if provided, sets the start of the block to this scan number (scan must exist in the \code{dataset})}

\item{set_end_scan.no}{if provided, sets the end of the block to this scan number (scan must exist in the \code{dataset})}
}
\value{
A data frame (tibble) with block limits altered according to the provided start/end change parameters. Any data that is no longer part of the original block will be marked with the value of \code{orbi_get_option("data_type_unused")}. Any previously applied segmentation will be discarded (\code{segment} column set to \code{NA}) to avoid unintended side effects.
}
\description{
This function can be used to manually adjust where certain \code{block} starts or ends using either time or scan number.
Note that adjusting blocks removes all block segmentation. Make sure to call \code{\link[=orbi_segment_blocks]{orbi_segment_blocks()}} \strong{after} adjusting block delimiters.
}
