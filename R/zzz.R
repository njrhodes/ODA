## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  if(.Platform$OS.type != "windows"){win.warn <- "Warning: ODA is not currently supported on platforms other than Windows. \n\n"
  } else(win.warn <- "Package loaded on a Windows Platform.\n\n")
  if(!file.exists(paste(system.file("win32", "bin", "MegaODA.EXE", package = "ODA")))){oda.warn <- "Warning: the MegaODA.exe file is missing in the package directory ~/ODA/win32/bin location.\n\n Full use of this package requires a valid licensed copy of MegaODA.exe.\n\n"} else(oda.warn <-"")
  start_message <- c("Welcome to the ODA package for R, version 1.1.0. \n\n",
                     "For updates to this package or to contribute visit our GitHub. \n\n",
                     "Use ODAmanual() to get help with the package. \n\n",
                     "For information about ODA and ODA applications, please visit https://odajournal.com/. \n\n",
                     "For news and updates to this package, see NEWS.md file. \n",
                     win.warn,
                     oda.warn
  )
  packageStartupMessage(start_message)
  invisible()
}

#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
#'
.onLoad <- function(libname, pkgname) {
  op <- options(encoding="UTF-8")
  op.ODA <- list(
    ODA.path = "~/R-dev",
    ODA.install.args = "",
    ODA.name = "Nathaniel J. Rhodes",
    ODA.desc.author = "Nathaniel Rhodes <nrhode@midwestern.edu> [aut, cre]",
    ODA.desc.license = "GPL-3",
    ODA.desc.suggests = NULL,
    ODA.desc = list()
  )
  toset <- !(names(op.ODA) %in% names(op))
  if (any(toset)) options(op.ODA[toset])
  invisible()
}
