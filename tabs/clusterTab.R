# UI elements for cluster tab

# Future home for cluster analysis, hierarchical and divisive/kmeans/kmedoids etc. 



tabPanel(title = "Cluster", icon = icon("adjust", lib = "glyphicon"),
         
                  sidebarLayout(
                    sidebarPanel(
                      #uiOutput("chem.pca"),
                      #uiOutput("pca.button"),
                      #br(),
                      #column(12,plotOutput("eigen.plot", width = 300, height = 300))
                    ), # end sidebarPanel
                    
                    mainPanel(
                      
                    ) # end mainPanel PCA
                  ) # end sidebarLayout PCA
         ) # end tabPanel "Cluster Analysis"
         
         
         

