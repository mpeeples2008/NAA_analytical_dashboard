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
             uiOutput("cluster.button"), 
             br(),
             uiOutput("cluster.assign.button")
           ), # end sidebarPanel
                    
          mainPanel(
             plotOutput("element.dend", width = "100%", height = "auto")
                       
            

                      
                    ) # end mainPanel PCA
                  ) # end sidebarLayout PCA
         ) # end tabPanel "Cluster"
         
         
         

