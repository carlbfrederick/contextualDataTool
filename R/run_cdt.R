#' Run the current version of the contextual data tool
#'
#' Include a legend
#' Consider how breaks are made-
#'   consistency across measures?
#'   natural breaks?
#'
#' @return null
#' @export
#'
#' @importFrom shiny runApp
runCDT <- function() {
  shiny::runApp(system.file("cdt_app", package = "contextualDataTool"), launch.browser = TRUE)
  invisible(NULL)
}
