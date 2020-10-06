#' @title Execute an ODA model run.
#'
#' @description Creates an command file using the parameters below and calls
#'   MegaODA
#'
#' @param run A numerical value specifying the run folder containing the data
#' @param path The working directory of the project stored as \code{path}
#' @param data A character name specifying the data.txt file in the runs folder
#' @param out A character name specifying the output file with "model.out" as
#'   the default
#' @param hold A character name specifying the holdout data file, omitted by
#'   default
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
#'   or v2 LT 1} specifiying a directional hypothesis
#' @param degen An optional character name specifying attributes for which
#'   degenerate solutions are allowed, off by default
#' @param gen An optional character name specifying the variable whose values
#'   denote groups for a multisample generalizablity analysis, off by default
#' @param primary An optional character vector specifying the primary criterion
#'   for choosing among optimal solutions, see \code{Details}
#' @param secondary An optional character vector specifying the secondary
#'   criterion for choosing among optimal solutions, see \code{Details}
#' @param nopriors An optional logical value specifying whether the ODA
#'   criterion is weighted by the reciprocal of class membership, with
#'   \code{nopriors = FALSE} as the default
#' @param miss A numeric value specifying a missing or \code{NA} value in the
#'   data with a default value set at \code{-9}, see \code{\link{ODAclean}}
#' @param weight An optional character name of a variable containing a positive,
#'   non zero, weight value; cannot be the same as variables declared as
#'   \code{class} \code{attribute} \code{categorical} or \code{gen}
#' @param mcarlo A logical value specifying whether Monte Carlo analysis should
#'   be used to estimate Type I error with \code{mcarlo = TRUE} as the default
#' @param iter An integer value specifying the maximum number of Monte Carlo
#'   iteratations to be reached before halting and must be specified if
#'   \code{mcarlo = TRUE}
#' @param target An optional numerical value specifying the target level of
#'   \code{alpha < target} to be reached before halting and must be specified if
#'   \code{sidak} or \code{stop} are utilized
#' @param sidak An optional integer value specifying an adjustment to
#'   \code{target} based on the number of experiment wise comparisons and must
#'   be combined with \code{target}
#' @param stop An optional numerical value specifying the confidence level that
#'   Type I error is less than \code{target} to be reached before halting and
#'   must be combined with \code{target}
#' @param adjust An optional logical value specifying whether tied Monte Carlo
#'   iterations are to be split in half with \code{adjust = FALSE} as the
#'   default
#' @param setseed An optional integer value specifying a seed number for Monte
#'   Carlo analysis with the current system time as the default
#' @param loooff An optional logical value specifying whether leave one out or
#'   LOO analysis should be turned off with \code{loooff = FALSE} as the default
#' @param overwrite An optional logical value specifying whether previous files
#'   in the \code{Run} folder should be overwritten with \code{overwrite =
#'   FALSE} as the default
#'
#' @details The working directory of the project is stored as \code{path}. All
#'   files needed for the run must be located in the appropriate \code{run}
#'   folder beneath this level. See \code{\link{ODAclean}}.
#'
#'   \code{ODArun} will produce a command file with an extension of .pgm and two
#'   files with .out extensions. Resulting MODEL.OUT files can be parsed using
#'   \code{\link{ODAparse}}.
#'
#'   The ODA algorithm explicitly maximizes the classification accuracy which is
#'   achieved for the training sample see Yarnold, 2005.
#'
#'   The use of Optimal in Optimal Data Analysis means that an ODA model
#'   achieves the theoretically maximum possible level of accuracy in any given
#'   application. For more information see \code{\link{ODAmanual}}.
#'
#'   ODA utilizes primary and secondary criteria for selecting among multiple
#'   optimal solutions. By default, when not specified and when \code{priorsoff
#'   = FALSE} \code{primary} is set to \code{maxsens}. By default, when not
#'   specified \code{secondary} is set to \code{samplerep}.
#'
#'   When \code{gen} is not active, other options include: \code{maxsens}
#'   \code{meansens} \code{samplerep} \code{balanced} \code{distance}
#'   \code{random sens(attribute)}
#'
#'   When \code{gen} is active, other options include: \code{genmean} and
#'   \code{gensens(attribute)}
#'
#'   There are several disallowed specifications. Error checking is
#'   automatically performed on the user inputs. However, if the \code{to}
#'   keyword is used with a range of variables, it is suggested that the user
#'   confirm that the desired analysis was performed as some error checking is
#'   not available. The following cannot be combined in \code{ODArun}:
#'   \code{weight} cannot be both declared and listed as any of the following
#'   \code{class attribute gen}. Likewise, \code{gen} cannot be both declared
#'   and listed as any of the following \code{attribute} \code{categorical}
#'   \code{class} \code{weight}.
#'
#' @return Nothing is returned. Three files are created in the \code{Run}
#'   folder: \item{MODEL.out}{The model file that contains the commands from
#'   MEGAODA syntax and analysis results, see \code{\link{ODAmanual}}. This file
#'   is required for \code{\link{ODAparse}}} \item{OUT.out}{The echo file that
#'   contains the inital commands for OPEN and OUTPUT from MEGAODA syntax, see
#'   \code{\link{ODAmanual}}.} \item{OUT.pgm}{The command file that contains all
#'   of the commands for MEGAODA passed from R based on user input to
#'   \code{ODArun}.}
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
#' # ODArun(run=1, vstart="v1",vend="v45",class="v45",attribute="v1 to v44")
#'
#' @references Yarnold P.R. and Soltysik R.C. (2005). \emph{Optimal data
#'   analysis: Guidebook with software for Windows}. APA Books.
#'
#'   Yarnold, P.R. and Soltysik, R.C. (2016). \emph{Maximizing Predictive
#'   Accuracy}. ODA Books. DOI: 10.13140/RG.2.1.1368.3286
#'
ODArun <-function(run="", path=getwd(), data="data.txt", out="model.out", hold="", vstart="", vend="", class="", attribute="", categorical="", include="", exclude="", direction="", degen="", gen="", primary="", secondary="", nopriors=F, miss="", weight="", mcarlo=T, iter = "1000", target = "", sidak = "", stop = "", adjust=F, setseed = "", loooff=F, overwrite = FALSE) {
  `%notin%` <- Negate(`%in%`)
  if(run == ""){
    cat("Error: User must specify the run folder in which to execute ODArun().")
  }
  else{
    rundir <- paste(path,"Runs",run,sep="/")
    odadir <- paste(path,"Program",sep="/")
    prevrun <- paste(rundir,out,sep="/")
    if (file.exists(prevrun)) {
      cat("Warning: The specified run directory already contains at least one ODA model. Do you wish to overwrite the files in this run?\n")
      overwrite <- readline(prompt="Message: Overwrite all files in this run (Y/N): ")
      if (overwrite == "Y" | overwrite == "y"){
        overwrite <- TRUE
      }
      else{
        stop(cat("Warning: ODArun() stopped at user request. Run directory already exists.\n"))
      }
    }
    file.copy(paste(odadir,"MEGAODA.EXE",sep="/"), rundir)
    current <- getwd()
    setwd(rundir)
  }
  writefile <- function(n) {
    ## Pass arguments to temporary command objects and check for errors
    if (data == "" & file.exists("data.txt")) {
      datafile  <- paste0("OPEN ","data.txt",";\n")
    }
    else(datafile <- paste0("OPEN ",data,";\n")
    )
    if (out == "") {
      outfile <- paste0("OUTPUT ","model.out",";\n")
    }
    else(outfile <- paste0("OUTPUT ",out,";\n")
    )
    if (hold != "") {
      holdout <- paste0("HOLD ",hold,";\n")
    }
    else(holdout <- ""
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
      stop(cat("Error: At least one of the following are required: a single attribute, a space-separated list of attributes, or a range of attributed separated by the TO keyword (e.g., v1 to v6).\n"))
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
    if (degen != "") {
      degenvars <- paste0("DEGEN ",degen,";\n")
    }
    else(degenvars <- ""
    )
    if (gen != "") {
      genvar <- paste0("GEN ",gen,";\n")
    }
    else(genvar <- ""
    )
    if (primary != "" & (primary %in% list("maxsens","meansens","samplerep","balanced","distance","random", "genmean") |  grepl("sens ",primary,fixed=T) | grepl("gensens ",primary,fixed=T))) {
      pri <- paste0("PRI ",primary,";\n")
    }
    else(pri <- ""
    )
    if (secondary != "" & (secondary %in% list("maxsens","meansens","samplerep","balanced","distance","random", "genmean") | grepl("sens ",secondary,fixed=T) | grepl("gensens ",secondary,fixed=T))) {
      sec <- paste0("SEC ",secondary,";\n")
    }
    else(sec <- ""
    )
    if (nopriors %in% list("T","TRUE")) {
      priors <- paste0("PRIORS OFF",";\n")
    }
    else(priors <- ""
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
    if (mcarlo %in% list("T","TRUE")){
      if (iter != "" ){
        it <- paste0("ITER ",iter," ")
      }
      else(it <- ""
      )
      if(target != "" ){
        tar <-  paste0("TARGET ",target," ")
      }
      else(tar <- ""
      )
      if(sidak != "" ){
        sid <-  paste0("SIDAK ",sidak," ")
      }
      else(sid <- ""
      )
      if(stop != "" ){
        stp <-  paste0("STOP ",stop," ")
      }
      else(stp <- ""
      )
      if(adjust %in% list("T","TRUE")){
        adj <- paste0("ADJUST ")
      }
      else(adj <- ""
      )
      mc <- paste0("MCARLO ", it, tar, sid, stp, adj,";\n")
    }
    if (mcarlo %in% list("F","FALSE")){
      mc <- "MCARLO OFF;\n"
    }
    if (setseed == "") {
      seed <- ""
    }
    else(seed <- paste0("SEED ",setseed,";\n")
    )
    if (loooff %in% list("T","TRUE")) {
      loo <- paste0("LOO OFF",";\n")
    }
    else(loo <- paste0("LOO",";\n")
    )
    go <-paste0("GO",";\n")
    ## Error checking for potentially problematic variable specifications
    w <- list(class,attribute,gen,categorical)
    x <- list(attribute,class,categorical,weight)
    y <- lapply(x, function(ch) grep("to", ch))
    z <- list("maxsens","meansens","samplerep","balanced","distance","random","genmean")
    ca <- lapply(attribute, function(ch) grep(categorical, ch))
    ge <- lapply(x, function(ch) grep(gen, ch))
    wgt <- lapply(w, function(ch) grep(weight, ch))
    if (length(y) > 0) {
      cat("Warning: The \"to\" operator was specified within the ODArun command. Review the MODEL.OUT file to confirm the proper analysis was conducted.\n")
    }
    if (categorical != "" & sum(unlist(ca))==0) {
      stop(cat("Error: \"categotical\" variable must be listed in the attribute block to initiate a categorical model.\n"))
    }
    if (gen != "" & sum(unlist(ge)) != 0)  {
      stop(cat("Error: \"gen\" variable cannot be the same variable specified as class, attribute, categorical, or weight variables.\n"))
    }
    if (gen == "" & (primary  %in% list("genmean") | secondary  %in% list("genmean") | grepl("gensens ",primary ,fixed=T) |  grepl("gensens ",secondary,fixed=T))) {
      stop(cat("Error: \"genmean\" and \"gensens\" can only be used as primary or secondary criteria for choosing an optimal solution when GEN is active.\n"))
    }
    if (primary != "" & (primary %in% z | grepl("sens ",primary,fixed=T) | grepl("gensens ",primary,fixed=T)) | primary == "") {
    }
    else(
      stop(cat(paste0("Error: ", primary," is not a valid primary critierion.\n")))
    )
    if (secondary != "" & (secondary %in% z | grepl("sens ",secondary,fixed=T) | grepl("gensens ",secondary,fixed=T)) | secondary == "") {
    }
    else(
      stop(cat(paste0("Error: ", secondary," is not a valid secondary critierion.\n")))
    )
    if (sidak != "" & target ==""){
      stop(cat("Error: Target significance must be declared for Sidak adjustment.\n"))
    }
    if (weight != "" & sum(unlist(wgt)) != 0) {
      stop(cat("Error: \"weight\" variable cannot be the same variable specified as class, attribute, categorical, or gen variables.\n"))
    }
    #Paste all commands into .pgm file for command line execution.
    odarun <- c(datafile,
                outfile,
                holdout,
                rangevars,
                classvar,
                attvar,
                catvars,
                invars,
                exvars,
                dirvars,
                degenvars,
                genvar,
                pri,
                sec,
                priors,
                miss_val,
                wt,
                mc,
                seed,
                loo,
                go)
    odanames <- names(odarun)
    file.create("oda.pgm")
    out<-sink("oda.pgm",type="output")
    writeLines(as.character(cat(rbind(odanames,odarun),sep="")))
    sink()
  }
  files <- ifelse(file.exists("megaoda.exe"),
                  ifelse(file.exists("data.txt"),
                         ifelse(file.exists("oda.pgm"),
                                ifelse(file.exists("data.csv"),1,0),0),0),0)
  nfiles <- length(files)
  if (nfiles == 0) {
    stop(cat("Error: Missing files for ODArun() in Run folder.\n"))
  }
  odalist <- list()
  for (n in 1:nfiles) {
    odalist <- writefile(n)
  }
  shell("megaoda.exe oda.pgm", intern=F)
  #Clean up files and reset wd to current
  if(file.exists("MEGAODA.EXE")){
    file.remove("MEGAODA.EXE")
  }
  setwd(current)
  return(invisible(T))
}
