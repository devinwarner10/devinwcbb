#' Run Basketball Shiny App
#'
#' This is the main method to be called in the package. Runs the shiny file in
#' the /inst directory of the package. 
#'
#' @export
#' 
#' @seealso 
#' "/inst/cbb_shiny"
#' 
#' @example 
#' 
#' ## Run the Shiny App
#' run_cbb_shiny()
#'

run_cbb_shiny <- function() {
  library(tidyverse)
  library(lubridate)
  library(DT)
  library(shiny)
  library(shinythemes)
  library(glmnet)
  library(umap)
  appDir <- system.file("cbb_shiny.R", package = "devinwcbb")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `devinwcbb`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}