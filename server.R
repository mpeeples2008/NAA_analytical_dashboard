#' server.R
library(ArchaeoDash)
library(shiny)

shinyServer(function(input, output, session) {
  # create reactive values
  # rvals = reactiveValues()
  rvals <<- reactiveValues(); showNotification("warning: global variable is only for testing")
  input <<- input


  #### Import data ####
  observeEvent(input$file1, {
    print("importing file")
    if (!is.null(input$file1)) {
      rvals$importedData = rvals$selectedData = rio::import(input$file1$datapath,setclass = 'tibble')
    }
  })

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(rvals$importedData)
    df <- rvals$importedData
    # Remove numeric columns from default selection
    nums1 <- unlist(lapply(df, is.numeric))
    items = names(df[, !nums1])
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items.all) = items.all
    names(items) = items
    tagList(
    selectInput(
      "attr",
      "Select all of the attribute variables you want to display:",
      items.all,
      multiple = TRUE,
      selected = items
    ),
    selectInput(
      "attrGroups",
      "Select the attributes that represent groups/categories/clusters:",
      items.all,
      multiple = TRUE,
      selected = items
    )
    )
  })

  # Render multi-select lookup for choosing chemical concentration columns
  output$chem <- renderUI({
    df <- rvals$importedData
    if (is.null(df))
      return(NULL)
    # Only include numeric columns in default selection
    nums <- unlist(lapply(df, is.numeric))
    items = names(df[, nums])
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items) = items
    names(items.all) = items.all
    selectInput(
      "chem",
      "Select all of the element concentrations:",
      items.all,
      multiple = TRUE,
      selected = items
    )
  })

  # create subset data frames
  observeEvent(input$action, {
    rvals$attrData = rvals$importedData %>%
      dplyr::select(tidyselect::any_of(input$attr)) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::any_of(input$attrGroups)),factor)
    rvals$chemicalData = rvals$importedData %>%
      dplyr::select(tidyselect::any_of(input$chem)) %>%
      # set below zero to
      mutate_all(list(function(x)
        case_when(x < 0 ~ 0, TRUE ~ x))) %>%
      mutate_all(list(function(x)
        dplyr::na_if(x, 0)))
  })

  # Render datatable of chemical data based on selected variables
  output$chem.contents <- DT::renderDataTable({
    req(rvals$chemicalData)
    DT::datatable(rvals$chemicalData, rownames = F)
  })

  # Render datatable of attribute data based on selected variables
  output$attr.contents <- DT::renderDataTable({
    req(rvals$attrData)
    DT::datatable(rvals$attrData, rownames = F)
  })

  # Render button to update datatable based on variable selections
  output$ui.action <- renderUI({
    req(input$file1)
    actionButton("action", "Press to confirm selections")
  })


  ####  Impute & Transform ####

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(rvals$chemicalData)
    radioButtons(
      "impute.method",
      label = ("Select Imputation Method"),
      choices = list(
        "None" = "none",
        "Random Forest" = "rf",
        "Predictive Mean Matching" = "pmm",
        "Weighted Predictive Mean Matching" = "midastouch"
      ),
      selected = "none"
    )
  })

  # Render button and controls to Impute data
  output$ui.impute <- renderUI({
    req(input$file1)
    actionButton("impute", "Impute missing data")
  })

  observeEvent(input$impute, {
    req(input$impute.method)
    if (input$impute.method != "none") {
      showNotification("imputing data")
      rvals$chemicalData = mice::complete(mice::mice(rvals$chemicalData, method = input$impute.method))
      showNotification("completed imputing data")
    }
  })

  # Render datatable of imputed chemical data
  output$elementsDT <- DT::renderDataTable({
    req(rvals$chemicalData)
    DT::datatable(rvals$chemicalData, rownames = F)
  })


  # Render button and controls to transform data
  output$ui.transform <- renderUI({
    req(input$file1)
    actionButton("transform", "Transform data")
  })

  # Render options for data transformation
  output$transform.options <- renderUI({
    req(rvals$chemicalData)
    radioButtons(
      "transform.method",
      label = ("Select Transformation"),
      choices = list(
        "None" = "none",
        "Log-10" = "log10",
        "Natural Log" = "log",
        "Percent/Z-score" = "zScore"
      ),
      selected = "none"
    )
  })

  observeEvent(input$transform, {
    req(rvals$chemicalData)
    if (input$transform.method == 'zscale') {
      rvals$chemicalData = zScale(rvals$chemicalData)
    } else if (input$transform.method %in% c("log10", "log")) {
      rvals$chemicalData = rvals$chemicalData %>%
        dplyr::mutate_all(input$transform.method) %>%
        dplyr::mutate_all(round, digits = 3)
    } else if (input$transform.method == "none") {
      rvals$chemicalData = rvals$chemicalData %>%
        dplyr::mutate_all(round, digits = 3)
    }
    # get rid of infinite values
    rvals$chemicalData = rvals$chemicalData %>% dplyr::mutate_all(list(function(c)
      case_when(!is.finite(c) ~ 0, TRUE ~ c)))
  })

  # Render datatable of transformed chemical data
  output$transform.contents <- DT::renderDataTable({
    req(rvals$chemicalData)
    DT::datatable(rvals$chemicalData, rownames = F)
  })

  # Render missing data plot
  output$miss.plot <- renderPlot({
    plot_missing(rvals$chemicalData, ggtheme = theme_bw())
  })

  # Render UI for univariate displays
  output$ui.univariate <- renderUI({
    selectInput("hist.el", "Element", choices = names(rvals$chemicalData))
  })

  # Render UI for univariate displays
  output$ui.hist.bin <- renderUI({
    sliderInput(
      "hist.bin",
      "Number of Bins",
      min = 2,
      max = 100,
      value = 30,
      step = 1
    )
  })

  # # Render reset button for compositional profile plot
  # output$ui.comp <- renderUI({
  #   req(input$file1)
  #   actionButton("comp.reset", "Reset Plot")
  # })

  # Render compositional profile plot
  output$comp.profile <- renderPlot({
    req(rvals$chemicalData)
    comp.profile(rvals$chemicalData)
  })

  # Render Element Histogram plot UI
  output$element.hist <- renderPlot({
    if (length(rvals$chemicalData[input$hist.el]) == 0)
      return(NULL)
    ggplot(data = rvals$chemicalData, aes_string(x = input$hist.el)) +
      geom_histogram(fill = "blue",
                     alpha = 0.5,
                     bins = input$hist.bin) +
      labs(x = input$hist.el, y = " ")
  })


  ####   Ordination   ####


  # Render multi-select lookup for choosing chemical concentration columns to include in
  # Principal Components Analysis
  output$chem.pca <- renderUI({
    items.all <- names(rvals$chemicalData)
    names(items.all) = items.all
    selectInput(
      "chem.pca.sel",
      "Select transformed elements to include in PCA:",
      items.all,
      multiple = TRUE,
      selected = items.all
    )
  })

  output$pca.button <- renderUI({
    actionButton("runPCA", "Run PCA and Save Results")
  })

  observeEvent(input$runPCA, {
    req(rvals$chemicalData)
    req(input$chem.pca.sel)
    rvals$pca1 = prcomp(rvals$chemicalData[input$chem.pca.sel])
  })

  # Render PCA plot
  output$pca.plot <- renderPlot({
    req(rvals$pca1)
    fviz_pca_ind(
      rvals$pca1,
      col.ind = "cos2",
      # Color by the quality of representation
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      label = 'none',
      repel = TRUE
    )     # Avoid text overlapping
  })

  # Render PCA Eigenvalue plot
  output$eigen.plot <- renderPlot({
    req(rvals$pca1)
    fviz_eig(rvals$pca1)
  })

  # Render PCA Eigenvalue plot
  output$pca.el.plot <- renderPlot({
    req(rvals$pca1)
    fviz_pca_var(
      rvals$pca1,
      col.var = "contrib",
      # Color by contributions to the PC
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      repel = TRUE
    )     # Avoid text overlapping
  })



  ####   Cluster  ####


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
          fviz_nbclust(rvals$chemicalData, kmeans, method = "wss") +
          labs(title = "Optimal # of Cluster, Kmeans Elbow Method")
        kmeans_sil <-
          fviz_nbclust(rvals$chemicalData, kmeans, method = "silhouette") +
          labs(title = "Optimal # of Cluster, Kmeans Silhouette Method")
        kmedoids_wss <-
          fviz_nbclust(rvals$chemicalData, pam, method = "wss") +
          labs(title = "Optimal # of Cluster, Kmedoids Elbow Method")
        kmedoids_sil <-
          fviz_nbclust(rvals$chemicalData, pam, method = "silhouette") +
          labs(title = "Optimal # of Cluster, Kmedoids ")

        rvals$clusterPlot = function(){plot_grid(kmeans_wss, kmeans_sil, kmedoids_wss, kmedoids_sil)}
      } else if (input$cluster.parent == "hca") {
        hc = as.dendrogram(
          hclust(
            dist(rvals$chemicalData, method = input$clust.dist.method),
            method = input$hclust.method
          )
        )
        rvals$clusterPlot = function(){plot(
          color_branches(hc,
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
          tbl_df(cutree(hc,
          k = input$hca.cutree.k))
        rvals$clusterDT <-
          rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "hdca") {
        hc = as.dendrogram(
          diana(rvals$chemicalData, metric = input$hdca.dist.method)
        )
        rvals$clusterPlot = function(){plot(
          color_branches(hc,
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
        rvals$clusterDT<-
          tbl_df(cutree(hc,
          k = input$hdca.cutree.k))
        rvals$clusterDT <-
          rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmeans") {
        kmeans_solution = kmeans(
          rvals$chemicalData,
          centers = input$kmeans.centers,
          iter.max = input$kmeans.iter.max,
          nstart = input$kmeans.nstart
        )
        rvals$clusterPlot = function(){fviz_cluster(
          kmeans_solution, data = rvals$chemicalData
          ) +
          theme_bw()}
        rvals$clusterDT = kmeans_solution$cluster
        rvals$clusterDT <-
          rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmedoids") {
        pam_solution =
          pam(
            rvals$chemicalData,
            k = input$kmedoids.k,
            metric = input$kmedoids.dist.method
          )
        rvals$clusterPlot = function(){fviz_cluster(pam_solution, data = rvals$chemicalData) + theme_bw()}
        rvals$clusterDT <- pam_solution$cluster
        rvals$clusterDT <-
          rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
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
    })

  ####   Visualize & Assign  ####

  output$sel <- renderUI({
    vals = rvals$attrData[[input$Code]] %>% unique %>% sort
    checkboxGroupInput("groups",
                       "Groups to show:",
                       choices = vals,
                       selected = vals)
  })

  output$xvarUI = renderUI({
    if (input$data.src == 'principal components') {
      df = try(rvals$pca1$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$chemicalData, silent = T)
    }
    selectInput('xvar', 'X', names(df), selected = names(df)[1])
  })

  output$yvarUI = renderUI({
    if (input$data.src == 'principal components') {
      df = try(rvals$pca1$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$chemicalData, silent = T)
    }
    selectInput('yvar', 'y', names(df), selected = names(df)[2])
  })

  output$CodeUI = renderUI({
    selectInput('Code', 'GROUP', choices = input$attrGroups)
  })

  observeEvent({
    input$Code
    input$xvar
    input$yvar
    input$groups
    input$data.src
  }, {
    req(input$Code)
    req(input$xvar)
    req(input$yvar)
    req(input$groups)
    if (input$data.src == 'principal components') {
      df = try(rvals$pca1$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$chemicalData, silent = T)
    }
    rvals$plotlydf = tryCatch({
      df %>%
        dplyr::mutate(rowid = 1:dplyr::n()) %>%
        dplyr::select(rowid,
                      x = tidyselect::all_of(input$xvar),
                      y = tidyselect::all_of(input$yvar)) %>%
        dplyr::bind_cols(rvals$attrData %>% dplyr::select(group = tidyselect::all_of(input$Code))) %>%
        dplyr::filter(group %in% input$groups)
    },
    error = function(e) {
      showNotification("Error returning plot dataset")
      return(tibble::tibble)
    })
  })

  observeEvent(input$`plotly_selected-A`, {
    plotlySelect <<- plotly::event_data("plotly_selected")
    if (length(plotlySelect) > 0) {
      rvals$brushSelected = rvals$plotlydf %>%
        dplyr::filter(rowid %in% plotlySelect$key)
      rvals$attrBrush = rvals$attrData %>%
        tibble::rowid_to_column() %>%
        dplyr::filter(rowid %in% plotlySelect$key)
    }
  })

  observeEvent(input$Change, {
    req(rvals$brushSelected)
    new = rvals$attrData %>%
      dplyr::mutate(rowid = 1:n()) %>%
      dplyr::filter(rowid %in% rvals$brushSelected$rowid) %>%
      dplyr::mutate(!!as.name(input$Code) := input$NewGroup)
    old = rvals$attrData %>%
      dplyr::mutate(rowid = 1:n()) %>%
      dplyr::filter(!rowid %in% rvals$brushSelected$rowid)
    rvals$attrData = dplyr::bind_rows(new, old) %>%
      dplyr::arrange(rowid) %>%
      dplyr::select(-rowid)
  })

  # plot
  output$plot <- renderPlotly({
    req(rvals$plotlydf)
    p1 <-
      ggplot(rvals$plotlydf,
             aes(
               x = x,
               y = y,
               color = group,
               shape = group,
               key = rowid
             )) +
      geom_point() +
      labs(
        x = input$xvar,
        y = input$yvar,
        color = input$Code,
        shape = input$Code
      )
    if (input$Conf) {
      n = rvals$plotlydf$group %>% unique %>% length()
      if (n > 10) {
        showNotification("too many group members to plot confidence ellipses")
      } else {
        p1 <- p1 + stat_ellipse(level = input$int.set)
      }
    }
    plotly::ggplotly(p1) %>% plotly::layout(dragmode = 'select')
  })

  output$brush <- renderUI({
    if (is.null(rvals$attrBrush)) {
      p("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
    } else {
      renderTable(rvals$attrBrush)
    }
  })

  #### multiplots ####

  output$multiplot = renderPlot({
    req(rvals$chemicalData)
    p = rvals$chemicalData %>%
      dplyr::bind_cols(rvals$attrData %>% dplyr::select(tidyselect::any_of(group))) %>%
      dplyr::select(tidyselect::any_of(c(xvar,yvar,group))) %>%
      tidyr::pivot_longer(-tidyselect::all_of(c(xvar,group))) %>%
      ggplot2::ggplot(ggplot2::aes(y = !!as.name(xvar), x = value, color = !!as.name(group))) +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::theme_bw()
    if(length(yvar) > 1){
      p = p +
        ggplot2::facet_wrap(~name, scales = "free_x", strip.position = "bottom") +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = '#404040'),
              strip.text = ggplot2::element_text(color = "white"))
    } else {
      p = p + ggplot2::xlab(yvar)
    }
    p
  })

  ####   Save & Export  ####

  output$Save <- downloadHandler(
    filename = function() {
      input$ExportName
    },
    content = function(file) {
      rio::export(dplyr::bind_cols(rvals$attrData, rvals$chemicalData),
                  file)
    }
  )



}) # end server
