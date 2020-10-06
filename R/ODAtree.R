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
#'   subfolders: \item{Rscript}{The folder for the Rscript file with templated
#'   syntax as a skeleton Rscript for the project.} \item{Runs}{The folder for
#'   the \code{\link{ODArun}} outputs and data files for cleaning and analysis.}
#'   \item{Program}{The folder containing the executable program used for all
#'   \code{\link{ODArun}} analyses.}
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
  dir.create("Runs")
  dir.create("Rscript")
  dir.create("Program")
  writeLines(c("#Visit https://odajournal.com/ for helpful articles and citations",
               "library(ODA)",
               "",
               "ODAmanual() # for help with using ODA() for R",
               "",
               "#Navigate to the project and copy your data.csv file to the Runs folder.",
               "",
               paste("setwd(\"", newFolder, "/Runs\")", sep = ""),
               "",
               "#### Return to the main project directory to execute subsequent commands ####",
               "",
               paste("setwd(\"", newFolder, "\")", sep = ""),
               "",
               "#### Clean data files for ODArun() ####",
               "",
               "ODAclean(data=\"data.csv\",output=1)",
               "",
               "#### Load data file, view data summary and variable lables, and prepare ODArun() syntax ####",
               "",
               "ODAload(1)",
               "",
               "key.1",
               "ls(data.1)",
               "summary(data.1)",
               "",
               "#### ODA Run 1 - [add analysis description here] ####",
               "",
               paste("setwd(\"", newFolder, "/Runs/1\")", sep = ""),
               "",
               "ODArun(run=1, vstart=\"[replace with starting variable]\",vend=\"[replace with ending variable]\",
               class=\"[replace with class variable]\",attribute=\"[replace with attribute(s)]\",
               categorical=\"[replace with categorial attribute(s), if any]\",miss=\"[Default value -9, change if needed]\",
               mcarlo=T,iter=\"[replace with number of desired iterations]\")",
               "",
               "#### Parse Run 1. ODAparse() will load objects including data and key, predictions, model summaries, model performance metrics, a list of confusion matricies, and a list of model stats before and after LOO (default) ####",
               "",
               "ODAparse(1)"
               ),
             con = paste(newFolder, "/Rscript/", project, ".R", sep = ""))
  file.copy(paste(system.file("win32", "bin", "MegaODA.EXE", package = "ODA")),
            paste(newFolder,"/Program",sep=""))
  setwd(current)
}
