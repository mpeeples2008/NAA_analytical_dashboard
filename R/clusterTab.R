
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
                            choices = c("View optimal number of clusters" = "nClust",
                                        "Hierarchical Agglomerative Clustering" = "hca",
                                        "Hierarchical Divisive Clustering" = "hdca",
                                        "k-means" = "kmeans",
                                        "k-medoids" = "kmedoids"),
                            selected = "nClust"),
               uiOutput("cluster.options"),
               uiOutput("cluster.column.text"),
               uiOutput("cluster.button"),
               br(),
               uiOutput("cluster.assign.button")
             ), # end sidebarPanel

             mainPanel(
               plotOutput("clusterPlot"),
               DT::dataTableOutput("clusterDT")
  ) # end mainPanel PCA
  ) # end sidebarLayout PCA
) # end tabPanel "Cluster"
}
