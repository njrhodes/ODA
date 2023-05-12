#' @title Open user and function manuals.
#'
#' @description Opens the ODA User Manual and function libraries
#'
#' @details Help for using ODA package
#'
#' @export
#'
ODAmanual <- function() {
  pdf1_path <- normalizePath(system.file("doc", "pdf", "ODA_2.0.0.pdf", package = "ODA"), winslash = "\\")
  pdf2_path <- normalizePath(system.file("doc", "pdf", "MPE.pdf", package = "ODA"), winslash = "\\")
  if (.Platform$OS.type == "windows") {
    shell.exec(pdf1_path)
    shell.exec(pdf2_path)
  } else {
    shell(paste("open", shQuote(pdf1_path)))
    shell(paste("open", shQuote(pdf2_path)))
  }
}


