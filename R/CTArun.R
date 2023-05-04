#' @title Execute an CTA model run.
#'
#' @description Creates and processes files needed for CTA.exe program
#'
#' @param run A numerical value specifying the run folder containing the data
#' @param path The working directory of the project stored as \code{path}
#' @param data A character name specifying the data.txt file in the model folder
#' @param out A character name specifying the output file with "model.out" as
#'   the default
#' @param vstart A character name specifying the start variable, see \code{key}
#'   object from \code{\link{ODAload}}
#' @param vend A character name specifying the end variable, see \code{key}
#'   object from \code{\link{ODAload}}
#' @param class A character name specifying the class variable, see \code{key}
#'   object from \code{\link{ODAload}}
#' @param attribute A character name specifying the attributes, see \code{key}
#'   object from \code{\link{ODAload}}
#' @param categorical An optional character name specifying categorical
#'   attributes, see \code{key} object from \code{\link{ODAload}}
#' @param include An optional character name specifying the variable and a value
#'   of observations that are included e.g., \code{"v2=2"} or \code{"v3>=50"}
#' @param exclude An optional character name specifying the variable and a value
#'   that are excluded
#' @param direction An optional character name and direction e.g., \code{v2 < 1
#'   or v2 LT 1} specifying a directional hypothesis for the class variable
#' @param force A logical specifying whether CTA should insert \code{nodevar}
#'   attribute at \code{forcenode} node in the solution tree
#' @param forcenode A numeric specifying the node to insert \code{nodevar} if
#'   \code{force} = T
#' @param nodevar A character string specifiying the variable to be inserted at
#'   node \code{forcenode} if \code{force} = T
#' @param miss A numeric value specifying a missing or \code{NA} value in the
#'   data with a default value set at \code{-9}, see \code{\link{ODAclean}}
#' @param weight An optional character name of a variable containing a positive,
#'   non zero, weight value; cannot be the same as variables declared as
#'   \code{class} \code{attribute} or \code{categorical} attributes
#' @param usefisher A logical specifying that all p values for categorical
#'   attributes will be determined using Fisher's Exact test rather than Monte
#'   Carlo
#' @param mciter A numeric value specifying the number of Monte Carlo iterations
#'   used to estimate the type I error rate for a given attribute within the
#'   solution tree
#' @param cutoff A cutoff value for the Type I error rate as an additional
#'   argument to \code{mciter} and \code{stop}
#' @param stop A confidence level in percent which will stop the processing of a
#'   given attribute after the number of Monte Carlo iterations specified in
#'   \code{mciter} are completed and the Type I error \code{cutoff} is exceeded
#' @param nopriors A logical specifying whether the ODA criterion will be
#'   weighted by the reciprocal of sample class membership. The default of
#'   leaving 'PRIORS' undeclared is equivalent to \code{nopriors} = F
#' @param mindenom A numeric value that specifies that only those attributes
#'   which yield a solution tree node denominator of size 'MINDENOM' or larger
#'   will be allowed in the solution tree
#' @param maxlevel A numeric value that specifies the deepest level of the
#'   solution tree
#' @param prune A numeric value specifying the upper bound Type I error rate
#'   with which to prune the solution tree or. When \code{nopriors} = T, then
#'   \code{prune} should be specified as 'NOPRIORS'. The default value is 0.05
#' @param skipnode A numeric value specifying the node number to skip.
#' @param enumerate A logical specifying whether the top three attribute or a
#'   limited character string. 'ROOT' specifies that only the top node will have
#'   all attributes evaluated. 'MINOBS' followed by a numeric value e.g.,
#'   'MINOBS 1' which will only allow solution trees with at least MINOBS.
#' @param loo A character string or numeric value specifying the upper bound
#'   Type 1 error rate required in jackknife i.e., leave-one-out or LOO cross
#'   generalizability analysis for every attribute in the tree. A specification
#'   of 'STABLE', the default value, requires that attributes have the same ESS
#'   in training and in LOO in order to be included in the solution tree
#' @param overwrite An optional logical value specifying whether previous files
#'   in the model folder should be overwritten with \code{overwrite = FALSE} as
#'   the default
#'
#' @details The working directory of the project is stored as \code{path}. All
#'   files needed for the model run must be located in the appropriate model run
#'   folder. See \code{\link{ODAclean}}.
#'
#'   \code{CTArun} will produce a command file with a .pgm extension, a model
#'   file with a .out extension, and a batch file with .bat extension. The .bat
#'   file must be executed within the model directory. Model output files can be
#'   parsed using \code{\link{CTAparse}}.
#'
#' @return Nothing is returned. Three files are created in the model folder:
#'   \item{MODEL.out}{The model file that contains the commands from CTA syntax
#'   and analysis results, see \code{\link{ODAmanual}}. This file is required
#'   for \code{\link{CTAparse}}.}\item{cta.pgm}{The command file that contains
#'   all of the commands for CTA passed from R based on user input to
#'   \code{CTArun}.}\item{ctarun.bat}{The batch file that must be executed to
#'   begin the model run.}
#' @export
#'
#' @importFrom utils read.table
#'
#' @seealso \code{\link{ODAmanual}} \code{\link{ODAclean}} \code{\link{ODAload}}
#'
#' @author Nathaniel J. Rhodes
#'
#' @examples
#' # Not run:
#' # CTArun(run=1,vstart="v1", vend="v201", class="v46 to v201", exclude ="v3=1", attr="v2 v4 to v44", mindenom = 1, enumerate = T, loo="STABLE")
#'
#' @references Yarnold P.R. and Soltysik R.C. (2005). \emph{Optimal data
#'   analysis: Guidebook with software for Windows}. APA Books.
#'
#'   Yarnold, P.R. and Soltysik, R.C. (2016). \emph{Maximizing Predictive
#'   Accuracy}. ODA Books. DOI: 10.13140/RG.2.1.1368.3286
#'
#'
CTArun <-function(run="", path=getwd(), data="data.txt", out="model1.txt", vstart="", vend="", class="", attribute="", categorical="", include="", exclude="", direction="", force=F, forcenode="", nodevar="", miss="", weight="", usefisher=F, mciter ="", cutoff="", stop = "", nopriors=F, mindenom="",maxlevel="", prune="", skipnode="",enumerate="",loo="",overwrite = FALSE) {
  `%notin%` <- Negate(`%in%`)
  if(run == ""){
    cat("Error: User must specify the folder in which to execute CTArun().")
  }
  else{
    rundir <- paste(path,"CTA",run,sep="/")
    ctadir <- paste(path,"Program",sep="/")
    prevrun <- paste(rundir,out,sep="/")
    if (file.exists(prevrun)) {
      cat("Warning: The specified run directory already contains at least one CTA model. Do you wish to overwrite the files in this folder?\n")
      overwrite <- readline(prompt="Message: Overwrite all model files in this folder (Y/N): ")
      if (overwrite == "Y" | overwrite == "y"){
        overwrite <- TRUE
      }
      else{
        stop(cat("Warning: CTArun() stopped at user request. File directory already exists.\n"))
      }
    }
    file.copy(paste(ctadir,"CTA.EXE",sep="/"), rundir)
    current <- getwd()
    setwd(rundir)
  }
  ## Pass arguments to temporary command objects and check for errors
  writefile <- function(n) {
    if (data == "" & file.exists("data.txt")) {
      datafile  <- paste0("OPEN ","data.txt",";\n")
    }
    else(datafile <- paste0("OPEN ",data,";\n")
    )
    if (out == "") {
      outfile <- paste0("OUTPUT ","model1.out",";\n")
    }
    else(outfile <- paste0("OUTPUT ",out,";\n")
    )
    if (vstart=="" | vend =="") {
      stop(cat("Error: The starting and ending variables must be supplied as quoted strings.\n"))
    }
    else(rangevars <- paste0("VARS ",vstart," to ",vend,";\n")
    )
    if (class=="") {
      stop(cat("Error: The class variable is not declared.\n"))
    }
    else(classvar <- paste0("CLASS ",class,";\n")
    )
    if (attribute == "") {
      stop(cat("Error: At least one of the following are required: a single attribute, a space-separated list of attributes, or a range of attributes separated by the keyword TO (e.g., v1 to v6).\n"))
    }
    else(attvar <- paste0("ATTRIBUTE ",attribute,";\n")
    )
    if (categorical != "") {
      catvars <- paste0("CAT ",categorical,";\n")
    }
    else(catvars <- ""
    )
    if (include != "") {
      invars <- paste0("IN ",include,";\n")
    }
    else(invars <- ""
    )
    if (exclude != "") {
      exvars <- paste0("EX ",exclude,";\n")
    }
    else(exvars <- ""
    )
    if (direction != "") {
      dirvars <- paste0("DIR ",direction,";\n")
    }
    else(dirvars <- ""
    )
    if (force %in% list("F","FALSE") & (forcenode != "" | nodevar != "")) {
      stop(cat("Error: forcenode and nodevar are declared when force = F.\n"))
    }
    if (force %in% list("T","TRUE") & (forcenode == "" | nodevar == "")) {
      stop(cat("Error: forcenode and nodevar not declared when force = T.\n"))
    }
    if (force %in% list("T","TRUE") & (forcenode != "" & nodevar != "")) {
      forcevars <- paste0("FORCENODE ",forcenode," ",nodevar,";\n")
    }
    else(forcevars <- ""
    )
    if (miss != "") {
      miss_val <- paste0("MISSING ALL ","(",miss,")",";\n")
    }
    else(miss_val <- ""
    )
    if (weight != "") {
      wt <- paste0("WEIGHT ",weight,";\n")
    }
    else(wt <- ""
    )
    if (usefisher %in% list("T","TRUE")) {
      fisher <- paste0("USEFISHER",";\n")
    }
    else(fisher <- ""
    )
    if (mciter != "" ){
      mcit <- paste0("MC ITER ",mciter," ")
    }
    else(mcit <- paste0("MC ITER ",5000," ")
    )
    if (cutoff != "" ){
      coff <-  paste0("CUTOFF ",cutoff," ")
    }
    else(coff <- paste0("CUTOFF ",0.05," ")
    )
    if(stop != "" ){
      stp <-  paste0("STOP ",stop," ")
    }
    else(stp <- paste0("STOP ",99.9,"")
    )
    if (nopriors %in% list("T","TRUE")) {
      priors <- paste0("PRIORS OFF",";\n")
    }
    else(priors <- ""
    )
    if (mindenom== "") {
      mind <- ""
    }
    if (mindenom!= "" & is.numeric(mindenom)==T) {
      mind <- paste0("MINDENOM ",mindenom,";\n")
    }
    else(
      stop(cat("Error: mindenom must be a numeric value.\n"))
    )
    if (maxlevel== "") {
      maxlvl <- ""
    }
    else{if (maxlevel!= "" & is.numeric(maxlevel)==T) {
      maxlvl <- paste0("MAXLEVEL ",maxlevel,";\n")
    }
      else(
        stop(cat("Error: maxlevel must be a numeric value.\n"))
      )
    }
    if (prune==""){
      prun <- paste0("PRUNE 0.05;\n")
    }
    else{
      if (prune!= "" & (prune %in% list("nopriors","NOPRIORS") | (prune >0 && prune < 1))) {
        prun <- paste0("PRUNE ",prune,";\n")
      }
      else(stop(cat("Error: prune must be a numeric p-value or 'nopriors'.\n"))
      )
    }
    if (skipnode == ""){
      skipn <- ""
    }
    else {
      if (skipnode != "" & is.numeric(skipnode) == T) {
        skipn <- paste0("SKIPNODE ",skipnode,";\n")
      }
      else(
        stop(cat("Error: skipnode must be a numeric value.\n"))
      )
    }
    if (enumerate != "" & enumerate %in% list("T","TRUE")) {
      enum <- paste0("ENUMERATE;\n")
    }
    else{
      if(enumerate !="" & enumerate %in% list("root","ROOT","minobs","MINOBS")){
        enum <- paste0("ENUMERATE ",enumerate,";\n")
      }
      else(enum <- ""
      )
    }
    if (loo=="") {
      looval <- paste0("LOO STABLE;\n")
    }
    else {if(loo!="" & (loo %in% list("stable","STABLE") | (is.numeric(loo)==T && loo >0 && loo<1))){
      looval <- paste0("LOO ", loo,";\n")
    }
      else(stop(cat("Error: loo must be: a numeric p-value, 'off' or 'stable'.\n")))
    }
    mc <- paste0(mcit, coff, stp,";\n")
    go <-paste0("GO",";\n")

    ## Error checking for potentially problematic variable specifications
    w <- list(class,attribute,categorical)
    wgt <- lapply(w, function(ch) grep(weight, ch))
    if (weight != "" & sum(unlist(wgt)) != 0) {
      stop(cat("Error: \"weight\" variable cannot be the same variable specified as class, attribute, categorical variables.\n"))
    }
    ## Paste all commands into .pgm file for command line execution.
    ctarun <- c(datafile,
                outfile,
                rangevars,
                classvar,
                attvar,
                catvars,
                invars,
                exvars,
                dirvars,
                forcevars,
                priors,
                miss_val,
                wt,
                fisher,
                mc,
                mind,
                maxlvl,
                prun,
                skipn,
                enum,
                looval,
                go)
    ctanames <- names(ctarun)
    file.create("cta.pgm")
    out<-sink("cta.pgm",type="output")
    writeLines(as.character(cat(rbind(ctanames,ctarun),sep="")))
    sink()
  }
  ## Check to see if all needed files are present for run
  files <- ifelse(file.exists("cta.exe"),
                  ifelse(file.exists("data.txt"),
                         ifelse(file.exists("cta.pgm"),
                                ifelse(file.exists("data.csv"),1,0),0),0),0)
  nfiles <- length(files)
  if (nfiles == 0) {
    stop(cat("Error: Missing files for CTArun() in CTA folder.\n"))
  }
  ctalist <- list()
  for (n in 1:nfiles) {
    ctalist <- writefile(n)
  }
  ## Create batch file for execution and run in cmd window externally
  file.create("ctarun.bat")
  out<-sink("ctarun.bat",type="output")
  writeLines(
    as.character(
      cat(
        "@echo off
echo. & echo Beginning CTA run.... & echo.
start /wait cmd /k \"cta.exe cta.pgm & pause >nul & exit\"
echo. & echo Run complete. Cleaning up.... & echo.
echo off
del cta.exe
mkdir etc
move cta.pgm etc
mkdir inputs
move data.csv inputs
move data.txt inputs
mkdir outputs
move model* outputs
copy ctarun.bat etc
echo. & echo Press any key to complete run and close this window... & echo.
pause > nul
erase ctarun.bat
")
    )
  )
  sink()
  shell("ctarun.bat",wait=F)
  setwd(current)
  return(invisible(T))
}
