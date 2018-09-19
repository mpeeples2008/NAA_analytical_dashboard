# UI elements for cluster tab

# Future home for cluster analysis, hierarchical and divisive/kmeans/kmedoids etc. 



tabPanel(title = "Cluster", icon = icon("adjust", lib = "glyphicon"),
         
         sidebarLayout(
           sidebarPanel(
             radioButtons("cluster.parent", "Select Clustering Method", 
                          choices = c("None", 
                                      "Hierarchical Agglomerative Clustering" = "hca", 
                                      "Hierarchical Divisive Clustering" = "hdca", 
                                      "k-means" = "kmeans",
                                      "k-mediods" = "kmediods"), 
                          selected = "None"),
             uiOutput("cluster.options"), 
             uiOutput("cluster.button")
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
         
         
         

