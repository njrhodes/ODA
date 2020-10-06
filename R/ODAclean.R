#' @title Read and clean a data.csv input file and transform variables for
#'   ODArun()
#'
#' @description A valid .csv file is imported, cleaned, and moved to output
#'   folder. Data frame objects called \code{key} and \code{data} are loaded in
#'   the environment.
#'
#' @param data The character name of the .csv file to be loaded and cleaned. The
#'   current working directory must be set to the Runs folder.
#' @param output An integer that specifies of the Runs subdirectory folder in
#'   which to export the cleaned data. If the subdirectory does not exist, it
#'   will be created. If it does exist, the user will be asked whether the files
#'   should be overwritten.
#' @param miss A numeric value e.g., -9 that is substituted for all \code{NA}
#'   values in the imported dataframe where missing or \code{NA} values exist.
#' @param ipsative A character vector of variable names \code{x} in the data
#'   which will be ipsatively standardized within \code{id} groups i.e., \eqn{x
#'   - mean(x) / sd(x)}. If ipsative standardization is desired, an \code{id}
#'   variable must be supplied.
#' @param normative A character vector of variable names \code{x} in the cleaned
#'   data which will be normatively standardized \eqn{x - mean(x) / sd(x)}.
#' @param id A character vector that represents a block of \code{id}
#'   variables within which ipsative standardization can be completed. More than
#'   1 observation per subject is needed for ipsative standardization.
#' @param overwrite Logical value specifying whether files in output directory
#'   should be overwritten. Can be overridden by specifying \code{TRUE} or
#'   \code{FALSE}.
#'
#' @return Cleaned data is moved to the Runs folder as both .txt and .csv files.
#'   \item{data.txt}{A cleaned data file is moved to the output directory. Row
#'   and column names are removed from the .txt file. If specified, missing
#'   value replacements and standarized variables are also passed to this file.}
#'   \item{data.csv}{The data.csv file is moved to the output dir as a
#'   reference. The column names are maintained for use with
#'   \code{\link{ODAload}} and \code{\link{ODAparse}}}
#' @export
#'
#' @importFrom utils read.csv write.csv write.table
#' @importFrom stats ave sd
#'
#' @author Nathaniel J. Rhodes
#'
#' @examples
#' # Not Run
#' # ODAclean(data="data.csv",output=1, miss=-9)
ODAclean <- function(data="", output="", miss="", ipsative = "", normative = "", id = "", overwrite=FALSE){
  `%notin%` <- Negate(`%in%`)
  input_dir <- paste(getwd(),"Runs",sep="/")
  if(data==""){
    data <- "data.csv"
    cat("Warning: The default file name of data.csv was applied.\n")
  }
  if("" %in% output){
    output <- 0
    cat("Message: The default output folder name of 0 was applied.\n")
  }
  else {
    output <- output
  }
  infile <- paste(input_dir,data,sep="/")
  if(!file.exists(infile)){
    stop(cat("Error: No valid data file found in the Runs folder.\n"))
  }
  data <- utils::read.csv(infile)
  if ("" %notin% normative) {
    zform <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm =T)}
    data[normative] <- apply(data[normative],2, zform)
  }
  if ("" %notin% ipsative & "" %in% id){
    stop("Error: Ipsative standardization requires an id variable.\n")
  }
  if ("" %notin% ipsative & "" %notin% id){
    index <- as.numeric(which(names(data) %in% "id"))
    var <- which(names(data) %in% ipsative)
    for (i in var){
      data.temp <- data[c(index,i)]
      data[i] <- ave(data.temp[[2]], data$id , FUN=function(x) (x-mean(x))/sd(x))
    }
    data <- data
  }
  if (!dir.exists(paste(input_dir,output,sep="/"))) {
    dir.create(paste(input_dir,output,sep="/"))
  }
  else {
    cat("warning: The output directory already exists. Do you wish to overwrite it?\n")
    overwrite <- readline(prompt="Overwrite all files in output directory (Y/N): ")
    if (overwrite == "Y" | overwrite == "y"){
      overwrite <- TRUE
    }
    else{
      stop("Warning: ODAclean() stopped at the user's request. The specified directory already exists.")
    }
  }
  utils::write.csv(data,file="data.csv", row.names=F)
  if (miss == ""){
    data[is.na(data)] <- -9
    cat("Warning: A default value of -9 was imputed for all missing values.\n")
  }
  else{
    data[is.na(data)] <- miss
  }
  utils::write.table(data, file="data.txt", col.names=F, quote = FALSE, row.names = FALSE)
  output_dir <- paste(input_dir,output,sep="/")
  file.copy(c("data.csv","data.txt"),output_dir,overwrite=overwrite)
  file.remove(c("data.csv","data.txt"))
  files <- list.files(output_dir)
  nfiles <- length(files)
  if (nfiles < 2) {
    stop(cat("Error: Cleaned data files are missing. A possible solution is make sure the primary .csv file is located in the ~/Runs folder.\n"))
  }
  else{
    cat("Message: Cleaned data files copied to output directory. Files can be loaded using ODAload().\n")
  }
}
