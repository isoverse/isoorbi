% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_isox.R
\name{orbi_simplify_isox}
\alias{orbi_simplify_isox}
\title{Simplify IsoX data}
\usage{
orbi_simplify_isox(dataset, add = c())
}
\arguments{
\item{dataset}{IsoX data that is to be simplified}

\item{add}{additional columns to keep}
}
\value{
A tibble containing only the 9 columns: \code{filepath}, \code{filename}, \code{scan.no}, \code{time.min}, \code{compound}, \code{isotopocule}, \code{ions.incremental}, \code{tic}, \code{it.ms}, plus any additional columns defined in the \code{add} argument
}
\description{
Keep only columns that are directly relevant for isotopocule ratio analysis. This function is optional and does not affect any downstream function calls.
}
\examples{
fpath <- system.file("extdata", "testfile_flow.isox", package="isoorbi")
df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()

}
