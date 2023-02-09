
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

#' Cluster Server
#'
#' @param input
#' @param output
#' @param session
#' @param rvals
#'
#' @return
#' @export
#'
#' @examples
clusterServer = function(input,output,session,rvals){
  # Render button to run clustering algorithm
  output$cluster.button <- renderUI({
    req(input$file1)
    actionButton("cluster.button", "Run")
  })

  # Render button to run clustering algorithm
  output$cluster.assign.button <- renderUI({
    req(input$file1)
    actionButton("cluster.assign.button", "Record cluster assignments")
  })

  # Text input for name of cluster solution assignment column name
  output$cluster.column.text <- renderUI({
    req(input$file1)
    textInput("cluster.column.text",
              "Input column name for cluster solution")
  })

  # Render WSS and Silhouette graphs for optimal number of clusters for each method

  observeEvent(input$cluster.button, {
    req(rvals$chemicalData)
    try({
      if(input$cluster.column.text == "") clusterName = "cluster" else clusterName = input$cluster.column.text
      if (input$cluster.parent == "nClust") {
        kmeans_wss <-
          factoextra::fviz_nbclust(rvals$chemicalData, kmeans, method = "wss") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmeans Elbow Method")
        kmeans_sil <-
          factoextra::fviz_nbclust(rvals$chemicalData, kmeans, method = "silhouette") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmeans Silhouette Method")
        kmedoids_wss <-
          factoextra::fviz_nbclust(rvals$chemicalData, cluster::pam, method = "wss") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmedoids Elbow Method")
        kmedoids_sil <-
          factoextra::fviz_nbclust(rvals$chemicalData, cluster::pam, method = "silhouette") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmedoids ")

        rvals$clusterPlot = function(){cowplot::plot_grid(kmeans_wss, kmeans_sil, kmedoids_wss, kmedoids_sil)}
      } else if (input$cluster.parent == "hca") {
        hc = as.dendrogram(
          hclust(
            dist(rvals$chemicalData, method = input$clust.dist.method),
            method = input$hclust.method
          )
        )
        rvals$clusterPlot = function(){plot(
          dendextend::color_branches(hc,
                                     k = input$hca.cutree.k),
          cex.axis = 0.75,
          cex.lab = 0.75,
          horiz = TRUE,
          nodePar = list(
            lab.cex = input$hca.leaf.text.size,
            pch = NA
          ),
          xlab = paste0(
            input$clust.dist.method,
            " distance;",
            input$hclust.method,
            " linkage"
          )
        )}
        rvals$clusterDT <-
          tibble::as_tibble(dendextend::cutree(hc,
                                   k = input$hca.cutree.k))
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "hdca") {
        hc = as.dendrogram(
          cluster::diana(rvals$chemicalData, metric = input$hdca.dist.method)
        )
        rvals$clusterPlot = function(){plot(
          dendextend::color_branches(hc,
                                     k = input$hdca.cutree.k),
          cex.axis = 0.75,
          cex.lab = 0.75,
          horiz = TRUE,
          nodePar = list(
            lab.cex = input$hdca.leaf.text.size,
            pch = NA
          ),
          xlab = paste0(input$clust.dist.method, " distance")
        )}
        rvals$clusterDT <-
          tibble::as_tibble(dendextend::cutree(hc,
                                   k = input$hdca.cutree.k))
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmeans") {
        kmeans_solution = kmeans(
          rvals$chemicalData,
          centers = input$kmeans.centers,
          iter.max = input$kmeans.iter.max,
          nstart = input$kmeans.nstart
        )
        rvals$clusterPlot = function(){factoextra::fviz_cluster(
          kmeans_solution, data = rvals$chemicalData
        ) +
            ggplot2::theme_bw()}
        rvals$clusterDT = kmeans_solution$cluster
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmedoids") {
        pam_solution =
          cluster::pam(
            rvals$chemicalData,
            k = input$kmedoids.k,
            metric = input$kmedoids.dist.method
          )
        rvals$clusterPlot = function(){factoextra::fviz_cluster(pam_solution, data = rvals$chemicalData) + ggplot2::theme_bw()}
        rvals$clusterDT <- pam_solution$cluster
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      }
    })
  })

  output$clusterPlot <- renderPlot({
    req(rvals$clusterPlot)
    rvals$clusterPlot()
  })

  output$clusterDT<- DT::renderDataTable({
    req(rvals$clusterDT)
    DT::datatable(rvals$clusterDT, rownames = F)
  })


  # Render UI options for cluster analysis
  output$cluster.options <- renderUI({
    req(rvals$chemicalData)
    # Output of options if HCA chosen
    if (input$cluster.parent == "hca") {
      cluster_input_selections <- list(
        # HCA distance method
        selectInput(
          "clust.dist.method",
          label = "Select HCA Distance Method",
          choices = list(
            "Euclidean" = "euclidean",
            "Manhattan" = "manhattan",
            "Minkowski" = "minkowski",
            "Maximum" = "maximum"
          ),
          selected = "euclidean"
        ),
        # HCA linkage criterion choices
        selectInput(
          "hclust.method",
          label = ("Select HCA Linkage Criterion"),
          choices = list(
            "Average Linkage" = "average",
            "Complete Linkage" = "complete",
            "Ward's" = "ward.D",
            "Ward's squared" = "ward.D2"
          ),
          selected = "average"
        ),
        # HCA dendrogram leaf text size
        numericInput(
          "hca.leaf.text.size",
          label = "Leaf Text Size",
          value = 1,
          min = 0.05,
          max = 10,
          step = 0.05
        ),
        # HCA dendrogram cutree clusters
        numericInput(
          "hca.cutree.k",
          label = "Choose Numer of Clusters",
          value = 1,
          min = 1,
          max = 500,
          step = 1
        )
      )

    } else if (input$cluster.parent == "hdca") {
      # Output of options if HDCA is chosen
      cluster_input_selections <- list(
        # HDCA distance method
        selectInput(
          "hdca.dist.method",
          label = "Select HDCA Distance Method",
          choices = list("Euclidean" = "euclidean",
                         "Manhattan" = "manhattan"),
          selected = "euclidean"
        ),
        # HCDA dendrogram leaf text size
        numericInput(
          "hdca.leaf.text.size",
          label = "Leaf Text Size",
          value = 1,
          min = 0.05,
          max = 10,
          step = 0.05
        ),
        # HCDA dendrogram cutree clusters
        numericInput(
          "hdca.cutree.k",
          label = "Choose Numer of Clusters",
          value = 1,
          min = 1,
          max = 500,
          step = 1
        )
      )

    } else if (input$cluster.parent == "kmeans") {
      # Output of options if k-means is chosen
      cluster_input_selections <- list(
        # k-means number of centers
        numericInput(
          "kmeans.centers",
          label = "Choose Number of Clusters",
          value = 2,
          min = 1,
          max = 20,
          step = 1
        ),
        # k-means number of random initial configurations
        # best one is chosen and used
        numericInput(
          "kmeans.nstart",
          label = "Choose Number of Initial Configurations",
          value = 5,
          min = 1,
          max = 100,
          step = 1
        ),
        # k-means number of maximum iterations to converge and
        # reach stopping criterion
        numericInput(
          "kmeans.iter.max",
          label = "Maximum Number of Iterations",
          value = 10,
          min = 1,
          max = 200,
          step = 1
        )
      )
    } else if (input$cluster.parent == "kmedoids") {
      # Output of options if k-medoids is chosen
      cluster_input_selections <- list(
        # k-medoids Distance Method choices
        selectInput(
          "kmedoids.dist.method",
          label = "Select HDCA Distance Method",
          choices = list("Euclidean" = "euclidean",
                         "Manhattan" = "manhattan"),
          selected = "euclidean"
        ),
        # k-medoids number of clusters
        numericInput(
          "kmedoids.k",
          label = "Choose Number of Clusters",
          value = 2,
          min = 1,
          max = 20,
          step = 1
        )
      )
    } else {
      cluster_input_selections = NULL
    }

    # Initialize selections based on clustering method chosen
    cluster_input_selections
  })

  # Assign cluster assignments based on cluster solution

  observeEvent(input$cluster.assign.button,{
    req(rvals$chemicalData)
    req(rvals$clusterDT)
    rvals$attrData =
      rvals$attrData %>%
      dplyr::bind_cols(rvals$clusterDT %>%
                         dplyr::select(-Sample) %>%
                         dplyr::mutate_all(factor))
    showNotification("assigned cluster")
  })
}
