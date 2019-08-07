#' Run the current version of the contextual data tool
#'
#'
#' @return null
#' @export
#'
#' @importFrom shiny runApp
runCDT <- function() {
  shiny::runApp(system.file("cdt_app", package = "contextualDataTool"), launch.browser = TRUE)
  invisible(NULL)
}
