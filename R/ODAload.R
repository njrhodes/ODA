#' @title Load data files and variable key for ODA or CTA
#'
#' @description Loads the data file from the specified model folder and
#'   generates a variable key for use with \code{ODArun} and \code{CTArun}.
#'
#' @param run The numerical value of the folder number containing the data
#'   specified by the run number. This number will also be used to name objects
#'   uniquely by appending \code{run} to the object e.g. data.1 for Run 1. At
#'   least one value for this parameter must be supplied by the user as no
#'   defaults are supplied.
#' @param type A character string which should contain either "ODA" or "CTA".
#'   Default value is "ODA".
#' @param ... Additional data files to load and review, if desired.
#'
#' @return The following objects are loaded into R \item{data}{Data frame of
#'   data.csv from specified \code{run} folder.} \item{key}{Data frame
#'   containing 2 columns: the variable names from the \code{data} and the coded
#'   variable names for use in with \code{ODArun} and \code{CTArun}.}
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
#' # ODAload(1,type="CTA")
ODAload <- function(run="",type="",...){
  if(run==""){
    stop("Error: User must specify which ~/Runs folder from which to load data file(s).\n")
  }
  if("" %in% type){
    type <- "ODA"
    cat("Message: The default model type of 'ODA' was applied.\n")
  }
  else {
    if(!type %in% list("CTA","cta","ODA","oda")){
      stop(cat("Error: Model type must be either 'ODA' or 'CTA'.\n"))
    }
    else(type <- type)
  }
  otherruns <- list(...)
  if(length(otherruns) > 0) {
    allruns <- c(run,unlist(otherruns))
  }
  else {
    allruns <- run
  }
  for(thisrun in allruns){
    input_dir <- paste(getwd(),type,thisrun,sep="/")
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
        assign(paste0(type,".key.",thisrun), key, pos = parent.frame())
        assign(paste0(cta,".data.",thisrun), data, pos = parent.frame())
      }
    }
  }
}
