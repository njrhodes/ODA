#' Generate a folder tree for an ODA project
#'
#' Establishes a subdirectory for an ODA project within a given working
#' directory.
#'
#' @param project A character string of a new project, e.g. "New Project"
#' @param folder The full path to the root folder for the new project. Default
#'   is the current working directory.
#'
#' @return A new folder named as \code{project} containing the following
#'   subfolders:\item{CTA}{The folder to contain the \code{\link{CTArun}}
#'   outputs and data files for the project.} \item{ODA}{The folder to contain
#'   the \code{\link{ODArun}} outputs and data files for the project.}
#'   \item{Program}{The folder containing the executable programs used for
#'   \code{\link{ODArun}} and \code{\link{CTArun}} analyses.} \item{Rscript}{The
#'   folder for the Rscript file with a skeleton Rscript containing command
#'   syntax for the project.}
#'
#' @export
#'
#' @examples
#' ##Not run
#' ##ODAtree("NewProject")
ODAtree <- function (project = "NewProject", folder = getwd()){
  newFolder <- paste(folder, project, sep = "/")
  current <- getwd()
  dir.create(newFolder)
  setwd(newFolder)
  dir.create("CTA")
  dir.create("ODA")
  dir.create("Program")
  dir.create("Rscript")
  writeLines(c("#Visit https://odajournal.com/ for helpful articles and citations",
               "library(ODA)",
               "",
               "ODAmanual() # for help with using ODA() for R",
               "",
               "#Navigate to the project and copy your data.csv file to the ODA folder.",
               "",
               paste("setwd(\"", newFolder, "/ODA\")", sep = ""),
               "",
               "#### Return to the main project directory to execute subsequent commands ####",
               "",
               paste("setwd(\"", newFolder, "\")", sep = ""),
               "",
               "#### Clean data files for ODArun() ####",
               "",
               "ODAclean(data=\"data.csv\",output=1)",
               "",
               "#### Load data file, view data summary and variable labels, and prepare ODArun() syntax ####",
               "",
               "ODAload(1)",
               "",
               "key.1 # obtain vstart and vend from this list",
               "ls(data.1)",
               "summary(data.1)",
               "",
               "#### ODA Run 1 - [add analysis description here] ####",
               "",
               "ODArun(run=1,\n
               vstart=\"[replace with starting variable]\",\n
               vend=\"[replace with ending variable]\",\n
               class=\"[replace with class variable]\",\n
               attribute=\"[replace with attribute(s) including categorical attributes in same order as categorical block]\",\n
               categorical=\"[replace with categorial attribute(s), if any, in same order as attribute block otherwise leave blank]\",\n
               miss=\"[Default value -9, change if needed]\")",
               "",
               "# note that the first column of data is vstart and the last column is vend",
               "",
               "#### Parse Run 1. ODAparse() will load objects including data and key, predictions, model summaries, model performance metrics, a list of confusion matricies, and a list of model stats before and after LOO (default) ####",
               "",
               "ODAparse(1)",
               "",
               "print(oda.model.1)",
               "",
               "#### Merge ODAparse() reports for model and performance ####",
               "",
               "ODAsummary(1)",
               "",
               "print(oda.summary.1)",
               "",
               "write.csv(oda.summary.1,\"ODAsummary1.csv\") #for viewing and/or filtering report by user",
               ""
               ),
             con = paste(newFolder, "/Rscript/", project, ".R", sep = ""))
  file.copy(paste(system.file("win32", "bin", "MegaODA.EXE", package = "ODA")),
            paste(newFolder,"/Program",sep=""))
  file.copy(paste(system.file("win32", "bin", "CTA.EXE", package = "ODA")),
            paste(newFolder,"/Program",sep=""))
  setwd(current)
}
