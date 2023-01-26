# ui.R
library(shiny)
library(randomForest)
library(ggplot2)
library(mice)
library(factoextra)
library(DataExplorer)
library(DT)
library(plotly)
library(dendextend)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Compositional Data Analysis"),
    
    tabsetPanel(
        
      tabPanel("Data input", 
        sidebarLayout(
          sidebarPanel(
          fileInput("file1", "Choose CSV File", accept=c("text/csv", 
                                                         "text/comma-separated-values,text/plain", 
                                                         ".csv")),
          uiOutput("attr"),
          uiOutput("chem"),
          uiOutput("ui.action") # button that defines data columns and plots to mainPanel window as datatable
          ), # end sidebarPanel
          
        mainPanel(
          tabsetPanel(
          id = "dataset",
          tabPanel("Attributes", DT::dataTableOutput("attr.contents")),
          tabPanel("Element Concentrations (ppm)", DT::dataTableOutput("chem.contents")))
        ) # end mainPanel
        ) # end sidebarLayout
      ), # end tabPanel "Data input"
    
      tabPanel("Impute and Transform",
        sidebarLayout(
          sidebarPanel(
          uiOutput("impute.options"),
          br(),
          uiOutput("ui.impute"),
          tags$hr(),
          uiOutput("transform.options"),
          br(),
          uiOutput("ui.transform"),
          tags$hr(),
          "Numbers of samples with missing data by element (pre-imputation)",
          plotOutput("miss.plot", width = 400, height = 400)
          ), # end sidebarPanel
               
        mainPanel(
          tabsetPanel(
          id = "dataset.impute",
          tabPanel("Elements Imputed", DT::dataTableOutput("impute.contents")),
          tabPanel("Elements Transformed", DT::dataTableOutput("transform.contents")),
          tabPanel("Univariate Plots", uiOutput("ui.univariate"),
                   uiOutput("ui.hist.bin"),
                   plotOutput("element.hist")),
          tabPanel("Compositional Profile Plot", br(), uiOutput("ui.comp"),
                   plotOutput("comp.profile")))
        ) # end mainPanel Impute
        ) # end sidebarLayout Impute
    ), # end tabPanel "Impute"

    tabPanel("PCA",
             sidebarLayout(
               sidebarPanel(
               uiOutput("chem.pca"),
               uiOutput("pca.button")
             ), # end sidebarPanel
               
               mainPanel(
                 fluidRow(
                   column(6,plotOutput("pca.plot")),
                   column(6,plotOutput("pca.el.plot"))),
                 fluidRow(
                   column(6,plotOutput("eigen.plot")))
               ) # end mainPanel PCA
             ) # end sidebarLayout PCA
    ), # end tabPanel "PCA"
    
    tabPanel("Cluster Analysis",
             sidebarLayout(
               sidebarPanel(
                 #uiOutput("chem.pca"),
                 #uiOutput("pca.button"),
                 #br(),
                 #column(12,plotOutput("eigen.plot", width = 300, height = 300))
               ), # end sidebarPanel
               
               mainPanel(
                 
               ) # end mainPanel PCA
             ) # end sidebarLayout PCA
    ), # end tabPanel "Cluster Analysis"
    
    tabPanel("Save and Export",
             sidebarLayout(
                sidebarPanel(
                "File will be exported to the current R working directory. Do not include file extension in name.",
                textInput('ExportName', label = 'Type name for export'),
                br(),
                actionButton("Save","Click here to save file")
               ), # end sidebarPanel
               
               mainPanel(
                
               ) # end mainPanel Save and Export
             ) # end sidebarLayout Save and Export
    ) # end tabPanel "Save and Export"
    
     
  ) # end tabsetPanel
) #end fluidpage ui




# server.R===================================
server <- (function(input, output) {
  
#### Data Input Chunk
  # reactive input for file data
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) return(NULL)
    read.csv(infile$datapath,row.names=1,header=TRUE) # read in datafile with default options
  })
  
  # render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    # remove numeric columns from default selection
    nums1 <- unlist(lapply(df, is.numeric)) 
    items=names(df[,!nums1])
    # set names as all columns in datatable
    items.all <- names(df)
    names(items.all)=items.all
    names(items)=items
    selectInput("attr","Select all of the attribute variables you want to display:",items.all,multiple=TRUE,selected=items)
  })
  
  # render multi-select lookup for choosing chemical concentration columns
    output$chem <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    # include only numeric columns in default selection
    nums <- unlist(lapply(df, is.numeric)) 
    items=names(df[,nums])
    # set names as all columns in datatable
    items.all <- names(df)
    names(items)=items
    names(items.all)=items.all
    selectInput("chem","Select all of the element concentrations:",items.all,multiple=TRUE,selected=items)
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
    actionButton("action", "Press after reading file and selecting variables")
  })
  
  # Render button and controls to Impute data
  output$ui.impute <- renderUI({
    if (is.null(input$file1)) return()
    actionButton("impute", "Impute missing data")
  })

  # Render datatable of imputed chemical data 
  output$impute.contents <- DT::renderDataTable({
    input$impute
    isolate({   
      if (input$impute.method == "none") {
        chem.imp <<- chem1
        return(chem.imp)}
      else {
      chem.imp <<- complete(mice(chem1,method=input$impute.method)) # save to global environment
      return(chem.imp)} # return output for datatable render
    })   
  })
  
  # render options for data imputation
  output$impute.options <- renderUI({
    df <- filedata()
    radioButtons("impute.method", label=("Select Imputation Method"), 
                       choices=list("None" = "none", "Random Forest" = "rf", "Predictive Mean Matching" = "pmm", "Weighted Predictive Mean Matching" = "midastouch"),
                       selected = "none")
  })
  
  # Render button and controls to Impute data
  output$ui.transform <- renderUI({
    if (is.null(input$file1)) return()
    actionButton("transform", "Transform data")
  })
  
  # render options for data imputation
  output$transform.options <- renderUI({
    df <- filedata()
    if (is.null(df)) return()
    radioButtons("transform.method", label=("Select Transformation"), 
                 choices=list("None" = "none", "Log-10" = "log10", "Natural Log" = "log", "Percent/Z-score" = "z.score"),
                 selected = "none")
  })
  
  # Render datatable of transformed chemical data 
  output$transform.contents <- DT::renderDataTable({
    input$transform
    isolate({
        if (input$transform.method=='none') {
          chem.t <<- chem.imp
          return(round(chem.t,3))}
        else {if (input$transform.method=='log10') {
          chem.t <<- log10(chem.imp)
          return(round(chem.t,3))}
        else {if (input$transform.method=='log') {
          chem.t <<- log(chem.imp)
          return(round(chem.t,3))}
        else if (input$transform.method=='z.score') {
          chem.t <<- as.data.frame(scale(prop.table(as.matrix(chem.imp),1)*100))
          return(round(chem.t,3))}}}
    })   
  })
  
  # Render missing data plot
  output$miss.plot <- renderPlot({
    input$action
    if (length(input$action)==0) return(NULL)
    isolate({
    plot_missing(chem1)
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
      sliderInput("hist.bin","Number of Bins",min=2,max=100,value=30,step=1)
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
    if (length(chem.t[input$hist.el])==0) return(NULL)
    ggplot(data=chem.t, aes(x=chem.t[input$hist.el])) + 
    geom_histogram(fill="blue", alpha = 0.5, bins=input$hist.bin) + 
        labs(x=input$hist.el, y=" ")  
  })
  
  # render multi-select lookup for choosing chemical concentration columns to include in Principal Components Analysis
  output$chem.pca <- renderUI({
    items.all <- names(chem.t)
    names(items.all)=items.all
    selectInput("chem.pca.sel","Select transformed elements to include in PCA:",items.all,multiple=TRUE,selected=items.all)
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
  
      
}) # end server


shinyApp(ui,server)

