#' Runs the shiny app.
#'
#' I mean, thats about as much explanation as it needs
#'
#' @export
#'

run_cbb_shiny <- function() {
  library(tidyverse)
  library(lubridate)
  library(DT)
  library(shiny)
  library(shinythemes)
  appDir <- system.file("cbb_shiny.R", package = "devinwcbb")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `devinwcbb`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}