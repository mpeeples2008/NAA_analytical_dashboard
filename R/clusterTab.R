
#' UI elements for cluster tab
#'
#'Future home for cluster analysis, hierarchical and divisive/kmeans/kmedoids etc. 
#'
#' @return
#' @export
#'
#' @examples
clusterTab = function(){
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
             uiOutput("cluster.column.text"),
             uiOutput("cluster.button"), 
             br(),
             uiOutput("cluster.assign.button")
           ), # end sidebarPanel
                    
          mainPanel(
            tabsetPanel(
              tabPanel("Optimal Clusters",
                  plotOutput("optim.clusters")),
              tabPanel("HCA", 
                  plotOutput("element.dend.hca", width = "100%", height = "auto"), 
                  DT::dataTableOutput("hca.clusters")),
              tabPanel("HDCA",
                  plotOutput("element.dend.hdca", width = "100%", height = "auto"), 
                  DT::dataTableOutput("hcda.clusters")),
              tabPanel("K-means", 
                       plotOutput("element.kmeans", width = "100%", height = "auto"), 
                       DT::dataTableOutput("kmeans.clusters")),
              tabPanel("K-medoids", 
                       plotOutput("element.kmedoids", width = "100%", height = "auto"), 
                       DT::dataTableOutput("kmedoids.clusters"))
            ) # end tabset panel  
                    ) # end mainPanel PCA
                  ) # end sidebarLayout PCA
         ) # end tabPanel "Cluster"
}