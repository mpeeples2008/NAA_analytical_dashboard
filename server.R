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
        chem1[chem1<=0] <- NA # set 0 values to NA
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
                                                             "Maximum" = "maximum", 
                                                             "Mahalanobis" = "mahalanobis_clust"),
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
                                          selectInput("clust.dist.method", 
                                                      label = "Select HDCA Distance Method", 
                                                      choices=list("Euclidean" = "euclidean", 
                                                                   "Manhattan" = "manhattan"),
                                                      selected = "euclidean")
          
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
      
      # Output of options if k-mediods is chosen
      if (input$cluster.parent == "kmediods"){
        cluster_input_selections <- list(# k-mediods Distance Method choices
                                         selectInput("kmediods.dist.method", 
                                                    label = "Select HDCA Distance Method", 
                                                    choices=list("Euclidean" = "euclidean", 
                                                                 "Manhattan" = "manhattan"),
                                                    selected = "euclidean"),
                                         # k-mediods number of clusters 
                                          numericInput("kmediods.k", 
                                                       label = "Choose Number of Clusters",
                                                       value = 2, min = 1, max = 20, step = 1)
                                         )
        
      
      }
      
      # Initialize selections based on clustering method chosen
      cluster_input_selections
    })
    
    
    # Render HCA dendrogram
      output$element.dend <- renderPlot({
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
      
    # Assign cluster assignments based on HCA dendrogram selections
    
      
####   Visualize & Assign  ####
  
      
      
      
      
      
####   Save & Export  ####
  
      
      
      
      
    }) # end server