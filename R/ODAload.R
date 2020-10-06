#' @title Load data files and variable key for ODA
#'
#' @description Loads the primary data file from the specified run folder and
#'   generates an ODA-friendly key for the user.
#'
#' @param run The numerical value of the folder number containing the data
#'   specified by the run number. This number will also be used to name objects
#'   uniquely by appending \code{run} to the object e.g. data.1 for Run 1. At
#'   least one value for this parameter must be supplied by the user as no
#'   defaults are supplied.
#' @param path The working directory of the project stored. the working
#'   directory should be set above the level of the Runs folder.
#' @param ... Additional data files to load and review, if desired.
#'
#' @return The following objects are loaded into R \item{data}{Data frame of
#'   data.csv from specified \code{run} folder.} \item{key}{Data frame
#'   containing 2 columns: the variable names from the \code{data} and an
#'   ODA-friendly alias e.g., v1 v2}
#' @export
#'
#' @details The current working directory is stored as the \code{path} and the
#'   files to be loaded must be located in the \code{Run} folder.
#'
#' @importFrom utils read.csv
#'
#' @author Nathaniel J. Rhodes
#'
#' @examples
#' # Not run:
#' # ODAload(1)
ODAload <- function(run="",path = getwd(),...){
  if(run==""){
    stop("Error: User must specify which ~/Runs folder from which to load data file(s).\n")
  }
  otherruns <- list(...)
  if(length(otherruns) > 0) {
    allruns <- c(run,unlist(otherruns))
  }
  else {
    allruns <- run
  }
  for(thisrun in allruns){
    input_dir <- paste(path,"Runs",thisrun,sep="/")
    filename <- "data.csv"
    outfile <- paste(input_dir, filename, sep="/")
    if (file.exists(outfile)){
      filename <- outfile
      if (file.exists(filename)) {
        data <- read.csv(file=filename)
        header <- names(data)
        c.header <- seq_len(ncol(data))
        key <- data.frame(header, paste("v",c.header,sep=""))
        colnames(key) <- c("label","variable")
        assign(paste0("key.",thisrun), key, pos = parent.frame())
        assign(paste0("data.",thisrun), data, pos = parent.frame())
      }
    }
  }
}
