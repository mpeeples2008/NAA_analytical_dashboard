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
                                      "k-mediods" = "kmedoids"), 
                          selected = "None"),
             uiOutput("cluster.options"), 
             uiOutput("cluster.button"), 
             br(),
             uiOutput("cluster.column.text"),
             uiOutput("cluster.assign.button")
           ), # end sidebarPanel
                    
          mainPanel(
            tabsetPanel(
              tabPanel("HCA", 
                  plotOutput("element.dend.hca", width = "100%", height = "auto")),
              tabPanel("HDCA",
                  plotOutput("element.dend.hdca", width = "100%", height = "auto")),
              tabPanel("K-means"),
              tabPanel("K-medoids")
            ) # end tabset panel  
                    ) # end mainPanel PCA
                  ) # end sidebarLayout PCA
         ) # end tabPanel "Cluster"
         
         
         

