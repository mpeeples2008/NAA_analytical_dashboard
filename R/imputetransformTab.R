
#' UI elements for Impute - Transform Tab
#'
#' @return
#' @export
#'
#' @examples
imputetransformTab = function(){tabPanel(title = "Impute & Transform", icon = icon("transfer", lib= "glyphicon"), id = "imputetransform",
         
         sidebarLayout(
           sidebarPanel(
             uiOutput("impute.options"),
             br(),
             uiOutput("ui.impute"),
             tags$hr(),
             uiOutput("transform.options"),
             br(),
             uiOutput("ui.transform"),
             tags$hr(),
             "Numbers of samples with missing data by element (pre-imputation)",
             plotOutput("miss.plot", width = "250px")
           ), # end sidebarPanel
           
           mainPanel(
             tabsetPanel(
               id = "dataset.impute",
               tabPanel("Elements Imputed", DT::dataTableOutput("impute.contents")),
               tabPanel("Elements Transformed", DT::dataTableOutput("transform.contents")),
               tabPanel("Univariate Plots", 
                        uiOutput("ui.univariate"),
                        uiOutput("ui.hist.bin"),
                        plotOutput("element.hist")),
               tabPanel("Compositional Profile Plot", br(), uiOutput("ui.comp"),
                        plotOutput("comp.profile")))
           ) # end mainPanel Impute
         ) # end sidebarLayout Impute
) # end tabPanel "Impute"
}