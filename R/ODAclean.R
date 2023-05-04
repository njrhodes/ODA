#' @title Read and clean a data.csv input file and transform variables for use
#'   in \code{ODArun} or \code{CTArun}
#'
#' @description A valid .csv file is imported, cleaned, and moved to a specified
#'   output folder. Data frame objects called \code{key} and \code{data} are
#'   loaded in the environment. The .csv file should be one level above the
#'   output folder.
#'
#' @param data The character name of the .csv file to be loaded and cleaned. The
#'   current working directory must be set to the Project folder.
#' @param output An integer that specifies of the folder into which the cleaned
#'   data files will be written. If the directory does not exist, it will be
#'   created. If it does exist, the user will be asked whether the file contents
#'   should be overwritten.
#' @param type A character string which should contain either "ODA" or "CTA".
#'   Default value is "ODA".
#' @param miss A numeric value e.g., -9 that is imputed for all \code{NA} values
#'   in the imported data frame where missing or \code{NA} values exist.
#' @param ipsative A character vector of variable names \code{x} in the data
#'   which will be standardized within an \code{id} i.e., \eqn{x - mean(x) /
#'   sd(x)}. An \code{id} variable must be supplied.
#' @param normative A character vector of variable names \code{x} in the cleaned
#'   data which will be standardized across columns \eqn{x - mean(x) / sd(x)}.
#' @param id A character vector that represents repeated measures. Observations
#'   within an \code{id} are standardized to the mean of the \code{id} number.
#'   More than 1 observation per subject is required.
#' @param overwrite Logical value specifying whether files in output directory
#'   should be overwritten. Can be overridden by specifying \code{TRUE} or
#'   \code{FALSE}.
#'
#' @return Cleaned data is moved to the model type folder as both .txt and .csv
#'   files. \item{data.txt}{A cleaned data file is moved to the output
#'   directory. Row and column names are removed from the .txt file. If
#'   specified, missing value replacements and standardized variables are also
#'   written to this file.} \item{data.csv}{The .csv file is also moved to the
#'   output directory.}
#' @export
#'
#' @importFrom utils read.csv write.csv write.table
#' @importFrom stats ave sd
#'
#' @author Nathaniel J. Rhodes
#'
#' @examples
#' # Not Run
#' # ODAclean(data="data.csv",type='ODA',output=1, miss=-9)
ODAclean <- function(data="", output="", type= "", miss="", ipsative = "", normative = "", id = "", overwrite=FALSE){
  `%notin%` <- Negate(`%in%`)
  if(data==""){
    data <- "data.csv"
    cat("Warning: The default file name of data.csv was applied.\n")
  }
  if("" %in% output){
    output <- 1
    cat("Message: The default Run number of '1' was applied.\n")
  }
  else {
    if(is.numeric(output) && output > 0 && output %% 1 == 0){
      output <- output
    }
    else {stop(cat("Error: Output file name must be an integer value'.\n"))
    }
  }
  if("" %in% type){
    type <- "ODA"
    cat("Message: The default model type of 'ODA' was applied.\n")
  }
  else {
    if(type %notin% list("CTA","cta","ODA","oda")){
      stop(cat("Error: Model type must be either 'ODA' or 'CTA'.\n"))
    }
    else(type <- type)
  }
  input_dir <- paste(getwd(),type,sep="/")
  infile <- paste(input_dir,data,sep="/")
  if(!file.exists(infile)){
    stop(cat("Error: No valid data file found in the specified folder.\n"))
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
    index <- as.numeric(which(names(data) %in% id))
    var <- as.numeric(which(names(data) %in% ipsative))
    for (i in var){
      data.temp <- data[c(index,i)]
      data[i] <- ave(data.temp[[2]], data[index] , FUN=function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
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
    stop(cat("Error: Data files are missing. Please review contents of model folders.\n"))
  }
  else{
    cat("Message: Cleaned data files copied to output directory. Use ODAload() to view data.\n")
  }
}
