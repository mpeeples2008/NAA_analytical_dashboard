# UI elements for ordination


tabPanel(title = "Ordination", icon = icon("equalizer", lib = "glyphicon"), 
                  sidebarLayout(
                    sidebarPanel(
                      uiOutput("chem.pca"),
                      uiOutput("pca.button")
                    ), # end sidebarPanel
                    
                    mainPanel(
                      fluidRow(
                        column(6,plotOutput("pca.plot")),
                        column(6,plotOutput("pca.el.plot"))),
                      fluidRow(
                        column(6,plotOutput("eigen.plot")))
                    ) # end mainPanel Ordination
                  ) # end sidebarLayout Ordination
)
         
         
       
         

