#' server.R
library(ArchaeoDash)
library(shiny)

shinyServer(function(input, output, session) {

  ####  create reactive values  ####
  rvals = reactiveValues()

  ## for testing
  # rvals <<- reactiveValues(); showNotification("warning: global variable is only for testing")
  # input <<- input

  #### Import data ####
  dataInputServer(input,output,session,rvals)

  ####  Impute & Transform ####
  imputeTransformServer(input,output,session,rvals)

  ####   Ordination   ####
  ordinationServer(input,output,session,rvals)

  ####   Cluster  ####
  clusterServer(input,output,session,rvals)

  ####   Visualize & Assign  ####
  visualizeAssignServer(input,output,session,rvals)

  ####   Save & Export  ####
  saveExportServer(input,output,session,rvals)

}) # end server
