#' Launch Shiny App for diagMutAnalysis
#'
#' A function that launches the Shiny app for diagMutAnalysis
#' The purpose of this app is only to illustrate how a Shiny
#' app works. The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' diagMutAnalysis::rundiagMutAnalysis()
#' }
#'
#' @references
#' Jung, J. (2022) diagMutAnalysis: R package that analyzes somatic
#' mutation data for BCB410H: Applied Bioinformatics. Unpublished.
#' URL https://github.com/wjdwogud24/diagMutAnalysis.
#'
#' @export
#' @importFrom shiny runApp

rundiagMutAnalysis <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "diagMutAnalysis")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}
# [END]
