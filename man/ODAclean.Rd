% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ODAclean.R
\name{ODAclean}
\alias{ODAclean}
\title{Read and clean a data.csv input file and transform variables for
  ODArun()}
\usage{
ODAclean(
  data = "",
  output = "",
  miss = "",
  ipsative = "",
  normative = "",
  id = "",
  overwrite = FALSE
)
}
\arguments{
\item{data}{The character name of the .csv file to be loaded and cleaned. The
current working directory must be set to the Runs folder.}

\item{output}{An integer that specifies of the Runs subdirectory folder in
which to export the cleaned data. If the subdirectory does not exist, it
will be created. If it does exist, the user will be asked whether the files
should be overwritten.}

\item{miss}{A numeric value e.g., -9 that is substituted for all \code{NA}
values in the imported dataframe where missing or \code{NA} values exist.}

\item{ipsative}{A character vector of variable names \code{x} in the data
which will be ipsatively standardized within \code{id} groups i.e., \eqn{x
- mean(x) / sd(x)}. If ipsative standardization is desired, an \code{id}
variable must be supplied.}

\item{normative}{A character vector of variable names \code{x} in the cleaned
data which will be normatively standardized \eqn{x - mean(x) / sd(x)}.}

\item{id}{A character vector that represents a block of \code{id}
variables within which ipsative standardization can be completed. More than
1 observation per subject is needed for ipsative standardization.}

\item{overwrite}{Logical value specifying whether files in output directory
should be overwritten. Can be overridden by specifying \code{TRUE} or
\code{FALSE}.}
}
\value{
Cleaned data is moved to the Runs folder as both .txt and .csv files.
  \item{data.txt}{A cleaned data file is moved to the output directory. Row
  and column names are removed from the .txt file. If specified, missing
  value replacements and standarized variables are also passed to this file.}
  \item{data.csv}{The data.csv file is moved to the output dir as a
  reference. The column names are maintained for use with
  \code{\link{ODAload}} and \code{\link{ODAparse}}}
}
\description{
A valid .csv file is imported, cleaned, and moved to output
  folder. Data frame objects called \code{key} and \code{data} are loaded in
  the environment.
}
\examples{
# Not Run
# ODAclean(data="data.csv",output=1, miss=-9)
}
\author{
Nathaniel J. Rhodes
}
