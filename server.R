#' server.R  

library(tidyverse)
library(randomForest)
library(ggplot2)
library(mice)
library(factoextra)
library(DataExplorer)
library(DT)
library(plotly)
library(dendextend)
library(shinythemes)
library(cluster)
library(stats)
library(shiny)
library(shinydashboard)
library(cowplot)


shinyServer(
  function(input, output, session) {
      
#### Data Input Chunk ####

    
    # Reactive input for file data
    filedata <<- reactive({
      infile <- input$file1
      if (is.null(infile)) return(NULL)
      read.csv(infile$datapath, row.names=1, header=TRUE) # read in datafile with default options
    })
      
    # Render multi-select lookup for choosing attribute columns
    output$attr <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      # Remove numeric columns from default selection
      nums1 <- unlist(lapply(df, is.numeric)) 
      items = names(df[,!nums1])
      # Set names as all columns in datatable
      items.all <- names(df)
      names(items.all) = items.all
      names(items) = items
      selectInput("attr","Select all of the attribute variables you want to display:",
                  items.all, multiple = TRUE, selected = items)
    })
    
    # Render multi-select lookup for choosing chemical concentration columns
    output$chem <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      # Onclude only numeric columns in default selection
      nums <- unlist(lapply(df, is.numeric)) 
      items=names(df[,nums])
      # Set names as all columns in datatable
      items.all <- names(df)
      names(items) = items
      names(items.all) = items.all
      selectInput("chem", "Select all of the element concentrations:",
                  items.all, multiple = TRUE, selected = items)
    })
    
    # Render datatable of chemical data based on selected variables, save to global environment
    output$chem.contents <- DT::renderDataTable({
      input$action
      isolate({   
        df <- filedata()
        chem1 <- df[input$chem]
        chem1[chem1 <= 0] <- NA # set 0 values to NA
        chem1 <<- chem1 # save to global environment
        return(chem1) # return output for datatable render
      })   
    })
    
    # Render datatable of attribute data based on selected variables, save to global environment
    output$attr.contents <- DT::renderDataTable({
      input$action
      isolate({   
        df <- filedata()
        attr1 <<- df[input$attr] # save to global environment
        return(attr1) # return output for datatable render
      })   
    })
    
    # Render button to update datatable based on variable selections
    output$ui.action <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("action", "Press to confirm selections")
    })
      
    
####  Impute & Transform  ####
  
      
    # Render button and controls to Impute data
    output$ui.impute <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("impute", "Impute missing data")
    })
    
    # Render datatable of imputed chemical data 
    output$impute.contents <- DT::renderDataTable({
      input$impute
      isolate({   
        if (is.null(filedata())){ 
          return()
          } else if (input$impute.method == "none") {
          chem.imp <<- chem1
          return(chem.imp)
          } else {
          chem.imp <<- mice::complete(mice(chem1,method=input$impute.method)) # save to global environment
          rownames(chem.imp) <<- rownames(chem1) # retain rownames after imputation
          return(chem.imp)} # return output for datatable render
      })   
    })
    
    # Render options for data imputation
    output$impute.options <- renderUI({
      df <- filedata()
      radioButtons("impute.method", label = ("Select Imputation Method"), 
                   choices=list("None" = "none", 
                                "Random Forest" = "rf", 
                                "Predictive Mean Matching" = "pmm", 
                                "Weighted Predictive Mean Matching" = "midastouch"),
                   selected = "none")
    })
    
    # Render button and controls to Impute data
    output$ui.transform <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("transform", "Transform data")
    })
    
    # Render options for data imputation
    output$transform.options <- renderUI({
      df <- filedata()
      if (is.null(df)) return()
      radioButtons("transform.method", label = ("Select Transformation"), 
                   choices=list("None" = "none", 
                                "Log-10" = "log10", 
                                "Natural Log" = "log", 
                                "Percent/Z-score" = "z.score"),
                   selected = "none")
    })
    
    # Render datatable of transformed chemical data 
    output$transform.contents <- DT::renderDataTable({
      input$transform
      isolate({
        if (is.null(filedata())){
          return() 
        } else if (input$transform.method == 'none') {
          chem.t <<- chem.imp
          return(round(chem.t, 3))}
        else {if (input$transform.method == 'log10') {
          chem.t <<- log10(chem.imp)
          return(round(chem.t, 3))}
          else {if (input$transform.method == 'log') {
            chem.t <<- log(chem.imp)
            return(round(chem.t, 3))}
            else if (input$transform.method == 'z.score') {
              chem.t <<- as.data.frame(scale(prop.table(as.matrix(chem.imp), 1) * 100))
              return(round(chem.t, 3))}}}
      })   
    })
    
    # Render missing data plot
    output$miss.plot <- renderPlot({
      input$action
      if (length(input$action) == 0) return(NULL)
      isolate({
        plot_missing(chem1, ggtheme = theme_bw())
      })
    })
    
    # Render UI for univariate displays
    output$ui.univariate <- renderUI({
      if (is.null(input$file1)) return()
      isolate({
        selectInput("hist.el","Element",choices=names(chem.t))
      })
    })
    
    # Render UI for univariate displays
    output$ui.hist.bin <- renderUI({
      if (is.null(input$file1)) return()
      isolate({
        sliderInput("hist.bin","Number of Bins",min=2, max=100, value=30, step=1)
      })
    })
    
    # Render reset button for compositional profile plot
    output$ui.comp <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("comp.reset", "Reset Plot")
    })
    
    # Render compositional profile plot 
    output$comp.profile <- renderPlot({
      input$comp.reset
      if (is.null(input$file1)) return()
      comp.profile(chem.t)
    })
    
    # Render Element Histogram plot UI
    output$element.hist <- renderPlot({
      if (length(chem.t[input$hist.el]) == 0) return(NULL)
      ggplot(data = chem.t, aes_string(x = input$hist.el)) + 
        geom_histogram(fill = "blue", alpha = 0.5, bins = input$hist.bin) + 
        labs(x = input$hist.el, y = " ")  
    })

      
####   Ordination   ####
      
      
    # Render multi-select lookup for choosing chemical concentration columns to include in 
    # Principal Components Analysis
    output$chem.pca <- renderUI({
      items.all <- names(chem.t)
      names(items.all)=items.all
      selectInput("chem.pca.sel","Select transformed elements to include in PCA:", items.all,
                  multiple=TRUE, selected=items.all)
    })
    
    output$pca.button <- renderUI({
      actionButton("runPCA","Run PCA and Save Results")
    })
    
    # Render PCA plot 
    output$pca.plot <- renderPlot({
      input$runPCA
      if (is.null(input$runPCA)) return(NULL)
      isolate({
        pca1 <<- prcomp(chem.t[input$chem.pca.sel])
        fviz_pca_ind(pca1,
                     col.ind = "cos2", # Color by the quality of representation
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     label='none',
                     repel = TRUE)     # Avoid text overlapping
      })
    })
    
    # Render PCA Eigenvalue plot 
    output$eigen.plot <- renderPlot({
      input$runPCA
      if (is.null(input$runPCA)) return(NULL)
      isolate({
        fviz_eig(pca1)
      })
    })  
    
    # Render PCA Eigenvalue plot 
    output$pca.el.plot <- renderPlot({
      input$runPCA
      if (is.null(input$runPCA)) return(NULL)
      isolate({
        fviz_pca_var(pca1,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE)     # Avoid text overlapping
      })
    })  
    
      
      
####   Cluster  ####
  
      
    # Render button to run clustering algorithm 
    output$cluster.button <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("cluster.button", "Run clustering algorithm")
    })
    
    # Render button to run clustering algorithm 
    output$cluster.assign.button <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("cluster.assign.button", "Record cluster assignments")
    })
    
    # Text input for name of cluster solution assignment column name
    output$cluster.column.text <- renderUI({
      if (is.null(input$file1)) return()
      textInput("cluster.column.text", "Input column name for cluster solution")
    })
    
    # Render WSS and Silhouette graphs for optimal number of clusters for each method
    output$optim.clusters <- renderPlot({
      isolate({
         kmeans_wss <-  fviz_nbclust(chem.t, kmeans, method = "wss") + 
                            labs(title = "Optimal # of Cluster, Kmeans Elbow Method")
         kmeans_sil <-  fviz_nbclust(chem.t, kmeans, method = "silhouette") + 
                            labs(title = "Optimal # of Cluster, Kmeans Silhouette Method")
         kmedoids_wss <- fviz_nbclust(chem.t, pam, method = "wss") + 
                            labs(title = "Optimal # of Cluster, Kmedoids Elbow Method")
         kmedoids_sil <-  fviz_nbclust(chem.t, pam, method = "silhouette") + 
                            labs(title = "Optimal # of Cluster, Kmedoids ")
         
         plot_grid(kmeans_wss, kmeans_sil, kmedoids_wss, kmedoids_sil)
      })
    })
    
    # Render UI options for cluster analysis
    output$cluster.options <- renderUI({
      df <- filedata()
      
      # Output if no cluster option selected 
      if (input$cluster.parent == "None"){
        return()
      }
      
      # Output of options if HCA chosen
      if (input$cluster.parent == "hca"){
        
        cluster_input_selections <- list(# HCA distance method 
                                         selectInput("clust.dist.method", 
                                                label = "Select HCA Distance Method", 
                                                choices=list("Euclidean" = "euclidean", 
                                                             "Manhattan" = "manhattan", 
                                                             "Minkowski" = "minkowski",
                                                             "Maximum" = "maximum"), 
                                                   selected = "euclidean"), 
                                         # HCA linkage criterion choices
                                         selectInput("hclust.method", 
                                                     label = ("Select HCA Linkage Criterion"), 
                                                     choices=list("Average Linkage" = "average", 
                                                                  "Complete Linkage" = "complete", 
                                                                  "Ward's" = "ward.D", 
                                                                  "Ward's squared" = "ward.D2"),
                                                     selected = "average"),
                                         # HCA dendrogram leaf text size
                                         numericInput("hca.leaf.text.size",
                                                      label = "Leaf Text Size",
                                                      value = 1, min = 0.05, max = 10, step = 0.05),
                                         # HCA dendrogram cutree clusters
                                         numericInput("hca.cutree.k",
                                                      label = "Choose Numer of Clusters",
                                                      value = 1, min = 1, max = 500, step = 1)
                                          )
          
      }
      
      # Output of options if HDCA is chosen
      if (input$cluster.parent == "hdca"){
        cluster_input_selections <- list(# HDCA distance method 
                                          selectInput("hdca.dist.method", 
                                                      label = "Select HDCA Distance Method", 
                                                      choices=list("Euclidean" = "euclidean", 
                                                                   "Manhattan" = "manhattan"),
                                                      selected = "euclidean"), 
                                          # HCDA dendrogram leaf text size
                                          numericInput("hdca.leaf.text.size",
                                                       label = "Leaf Text Size",
                                                       value = 1, min = 0.05, max = 10, step = 0.05),
                                          # HCDA dendrogram cutree clusters
                                          numericInput("hdca.cutree.k",
                                                       label = "Choose Numer of Clusters",
                                                       value = 1, min = 1, max = 500, step = 1)
                                        )
        
      }
      
      # Output of options if k-means is chosen
      if (input$cluster.parent == "kmeans"){
        cluster_input_selections <- list(# k-means number of centers
                                          numericInput("kmeans.centers", 
                                                       label = "Choose Number of Clusters",
                                                       value = 2, min = 1, max = 20, step = 1),
                                          # k-means number of random initial configurations
                                          # best one is chosen and used
                                          numericInput("kmeans.nstart",
                                                       label = "Choose Number of Initial Configurations",
                                                       value = 5, min = 1, max = 100, step = 1),
                                          # k-means number of maximum iterations to converge and 
                                          # reach stopping criterion
                                          numericInput("kmeans.iter.max",
                                                       label = "Maximum Number of Iterations",
                                                       value = 10, min = 1, max = 200, step = 1)
                                        )
      }
      
      # Output of options if k-medoids is chosen
      if (input$cluster.parent == "kmedoids"){
        cluster_input_selections <- list(# k-medoids Distance Method choices
                                         selectInput("kmedoids.dist.method", 
                                                    label = "Select HDCA Distance Method", 
                                                    choices=list("Euclidean" = "euclidean", 
                                                                 "Manhattan" = "manhattan"),
                                                    selected = "euclidean"),
                                         # k-medoids number of clusters 
                                          numericInput("kmedoids.k", 
                                                       label = "Choose Number of Clusters",
                                                       value = 2, min = 1, max = 20, step = 1)
                                         )
        
      
      }
      
      # Initialize selections based on clustering method chosen
      cluster_input_selections
    })
    
    
    # Render HCA dendrogram
    output$element.dend.hca <- renderPlot({
        if (is.null(input$cluster.button)) return(NULL)
          isolate({
              plot(color_branches(as.dendrogram(hclust(dist(chem.t, method = input$clust.dist.method), 
                                                method = input$hclust.method)), 
                                  k = input$hca.cutree.k),
                   cex.axis = 0.75, cex.lab = 0.75, horiz = TRUE,
                   nodePar = list(lab.cex = input$hca.leaf.text.size, pch = NA),
                   xlab = paste0(input$clust.dist.method, " distance;", input$hclust.method, " linkage")
                 ) 
                })
      }, height = 900, width = 700
      )
     
    # Render datatable of HCA dendrgram cluster solutions
    output$hca.clusters <- DT::renderDataTable({
      input$cluster.button
      isolate({   
        hca.clusterDT <- tbl_df(cutree(as.dendrogram(hclust(dist(chem.t, 
                                                     method = input$clust.dist.method), 
                                                     method = input$hclust.method)), 
                                                     k = input$hca.cutree.k))
        hca.clusterDT <- rownames_to_column(as.data.frame(hca.clusterDT), var = "Sample")
        colnames(hca.clusterDT) <- c("Sample", input$cluster.column.text)
        return(hca.clusterDT)
      })   
    })
      
    # Render HDCA dendrogram
      output$element.dend.hdca <- renderPlot({
        if (is.null(input$cluster.button)) return(NULL)
        isolate({
          plot(color_branches(as.dendrogram(diana(chem.t, metric = input$hdca.dist.method)), 
                              k = input$hdca.cutree.k),
               cex.axis = 0.75, cex.lab = 0.75, horiz = TRUE,
               nodePar = list(lab.cex = input$hdca.leaf.text.size, pch = NA),
               xlab = paste0(input$clust.dist.method, " distance")
          ) 
        })
      }, height = 900, width = 700
      )
      
    # Render datatable of HDCA dendrgram cluster solutions
    output$hcda.clusters <- DT::renderDataTable({
      input$cluster.button
      isolate({   
        hdca.clusterDT <- tbl_df(cutree(as.dendrogram(diana(chem.t, 
                                                           metric = input$hdca.dist.method)), 
                                                           k = input$hdca.cutree.k))
        hdca.clusterDT <- rownames_to_column(as.data.frame(hdca.clusterDT), var = "Sample")
        colnames(hdca.clusterDT) <- c("Sample", input$cluster.column.text)
        return(hdca.clusterDT)
      })   
    }) 
      
    # Render K-means 
      output$element.kmeans<- renderPlot({
        if (is.null(input$cluster.button)) return(NULL)
        isolate({
          kmeans_solution <<- kmeans(chem.t, centers = input$kmeans.centers, 
                                     iter.max = input$kmeans.iter.max, 
                                     nstart = input$kmeans.nstart)
          fviz_cluster(kmeans_solution, data = chem.t) + theme_bw()
        })
      }, height = 900, width = 700
      )
      
    # Render datatable of K-means cluster solutions
    output$kmeans.clusters <- DT::renderDataTable({
      input$cluster.button
      isolate({   
        kmeans.clusterDT <- kmeans_solution$cluster
        kmeans.clusterDT <- rownames_to_column(as.data.frame(kmeans.clusterDT), var = "Sample")
        colnames(kmeans.clusterDT) <- c("Sample", input$cluster.column.text)
        return(kmeans.clusterDT)
      })   
    })
      
    # Render K-medoids   
      output$element.kmedoids <- renderPlot({
        if (is.null(input$cluster.button)) return(NULL)
        isolate({
          pam_solution <<- pam(chem.t, k = input$kmedoids.k, metric = input$kmedoids.dist.method)
          fviz_cluster(pam_solution, data = chem.t) + theme_bw()
        })
      }, height = 900, width = 700
      )
    
    # Render datatable of K-medoids cluster solutions
    output$kmedoids.clusters <- DT::renderDataTable({
      input$cluster.button
      isolate({   
        kmedoids.clusterDT <- pam_solution$cluster
        kmedoids.clusterDT <- rownames_to_column(as.data.frame(kmedoids.clusterDT), var = "Sample")
        colnames(kmedoids.clusterDT) <- c("Sample", input$cluster.column.text)
        return(kmedoids.clusterDT)
      })   
    })
      
    # Assign cluster assignments based on cluster solution
    
      
####   Visualize & Assign  ####
  
      
      
      
      
      
####   Save & Export  ####
  
      
      
      
      
    }) # end server