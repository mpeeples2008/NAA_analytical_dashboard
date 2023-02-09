#' UI elements for Data Input tab
#'
#' @return
#' @export
#'
#' @examples
datainputTab = function(){tabPanel(title = "Data Input", icon = icon("upload"), id = "datainput",

         sidebarLayout(
           sidebarPanel(
             fileInput("file1", "Choose File (csv, xlsx or other supported format)"),
             uiOutput("attr"),
             uiOutput("chem"),
             uiOutput("ui.action") # button that defines data columns & plots to mainPanel window as datatable
           ), # end sidebarPanel

           mainPanel(
             tabsetPanel(
               id = "dataset",
               tabPanel("Attributes", DT::dataTableOutput("attr.contents")),
               tabPanel("Element Concentrations (ppm)", DT::dataTableOutput("chem.contents")))
           ) # end mainPanel
         ) # end sidebarLayout
) # end tabPanel "Data input"
}

#' Data Input
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
dataInputServer = function(input,output,session,rvals){
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
      dplyr::mutate_all(list(function(x)
        dplyr::case_when(x < 0 ~ 0, TRUE ~ x))) %>%
      dplyr::mutate_all(list(function(x)
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
}
