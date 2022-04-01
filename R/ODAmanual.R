#' @title Open user and function manuals.
#'
#' @description Opens the ODA User Manual and function libraries
#'
#' @details Help for using ODA package
#'
#' @export
#'
ODAmanual <- function () {
  shell(paste("start acrord32.exe ",shQuote(normalizePath(
    system.file("doc","pdf", "ODA_1.2.0.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
  shell(paste("start acrord32.exe ",shQuote(normalizePath(
    system.file("doc","pdf", "ODA-User-Guide.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
  shell(paste("start acrord32.exe ",shQuote(normalizePath(
    system.file("doc","pdf", "Command_List.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
  shell(paste("start acrord32.exe ",shQuote(normalizePath(
    system.file("doc","pdf", "MPE.pdf", package = "ODA"),  winslash = "\\")), sep = ""))
}
