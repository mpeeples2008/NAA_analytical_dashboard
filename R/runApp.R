#' Run Application
#'
#' @return
#' @export
#'
#' @examples
runArchaeoDash = function(){
  appDir <- system.file("app", package = "ArchaeoDash")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ArchaeoDash`.", call. = FALSE)
  }
  warning(paste("app directory is ",appDir))
  shiny::runApp(appDir, display.mode = "normal")
}

