#' Launch FSF Shiny Application
#'
#' @return launches the shiny app
#' @export
#'
#' @examples
launchApp <- function() {
  appDir <- system.file("shiny-app", "FlowSoFineApp", package = "FlowSoFineApp")
  shiny::runApp(appDir, display.mode = "normal")
}
