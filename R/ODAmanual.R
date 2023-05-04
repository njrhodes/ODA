#' @title Open user and function manuals.
#'
#' @description Opens the ODA User Manual and function libraries
#'
#' @details Help for using ODA package
#'
#' @export
#'
ODAmanual <- function () {
  shell(paste(shQuote(normalizePath(
    system.file("doc","pdf", "ODA_2.0.0.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
  shell(paste(shQuote(normalizePath(
    system.file("doc","pdf", "MPE.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
}
