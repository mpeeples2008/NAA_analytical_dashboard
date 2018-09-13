# UI elements for cluster tab

# Future home for cluster analysis, hierarchical and divisive/kmeans/kmedoids etc. 



tabPanel(title = "Cluster", icon = icon("adjust", lib = "glyphicon"),
         
         sidebarLayout(
           sidebarPanel(
             uiOutput("cluster.options"),
             br(),
             uiOutput("ui.cluster"), 
             br(), 
             uiOutput("hclust.options"),
             uiOutput("dist.options")
           ), # end sidebarPanel
                    
          mainPanel(
            tabsetPanel(
              id = "dataset.cluster",
              tabPanel("Hierarchical Clustering", 
                       plotOutput("element.dend"))
                       
            )

                      
                    ) # end mainPanel PCA
                  ) # end sidebarLayout PCA
         ) # end tabPanel "Cluster"
         
         
         

