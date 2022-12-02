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
#' Silva, A. (2022) TestingPackage: An Example R Package For
#' BCB410H. Unpublished. https://github.com/anjalisilva/TestingPackage
#'
#' @export
#' @importFrom shiny runApp

rundiaMutAnalysis <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "diagMutAnalysis")
  actionShiny <- shiny::runApp(appDir, display.mode = "normal")
  return(actionShiny)
}
# [END]
