# ui for Compositional Analysis Dashboard - Compositions

# <<Input details on public use license here -- MIT or GNU or??>>

# Need to think of a better name "TACA" is a placeholder

# Potential options:
  # TACA (Tools for Archaeological Compositional Analysis)
  # ACAD (Archaeological Compositional Analysis Dashboard)
  # ArchaeoDash
  # ArchCompAnalysis
  # GeocompAnalysis
  # AECA (Archaeological elemental compositonal analysis)
  # Comp_Dash

library(tidyverse)
library(randomForest)
library(ggplot2)
library(mice)
library(factoextra)
library(DataExplorer)
library(DT)
library(plotly)
library(dendextend)
library(shinythemes)
  
shinyUI(  
  
  navbarPage(theme = shinytheme("sandstone"),
             title = strong("ArchaeoDash"), 
             windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis", 
             fluid = TRUE, id = "nav",
             
             source("tabs/homeTab.R", local = TRUE)$value,
             source("tabs/datainputTab.R", local = TRUE)$value,
             source("tabs/imputetransformTab.R", local = TRUE)$value,
             source("tabs/ordinationTab.R", local = TRUE)$value,
             source("tabs/clusterTab.R", local = TRUE)$value,
             source("tabs/visualizeassignTab.R", local = TRUE)$value,
             source("tabs/saveexportTab.R", local = TRUE)$value
             
  )
)