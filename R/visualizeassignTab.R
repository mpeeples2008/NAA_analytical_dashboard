

#' UI elements for visualization and group reassignment
#'
#' @return
#' @export
#'
#' @examples
visualizeassignTab = function() {
  tabPanel(
    title = "Visualize & Assign",
    icon = icon("signal", lib = "glyphicon"),
    tabsetPanel(id = "visualize",
                tabPanel(title = "visualize and select",
                         fluidRow(column(9,
                                         plotly::plotlyOutput('plot', width = '100%', height = '600px')
                         ),
                         column(3,
                                div(
                                  uiOutput('sel')),
                                style = "max-height: 600px !important; overflow: auto;"
                         )
                         ),
                         fluidRow(
                           column(
                             2,
                             selectInput(
                               'data.src',
                               'Choose Dataset',
                               choices = c('elements', 'principal components'),
                               selected = 'elements'
                             ),
                             uiOutput('xvarUI'),
                             uiOutput('yvarUI'),
                           ),
                           column(
                             3,
                             offset = 0.5,
                             uiOutput('CodeUI'),

                             checkboxInput('Conf', 'Confidence Elipse', value =
                                             TRUE),
                             sliderInput(
                               'int.set',
                               'Set Confidence Interval',
                               min = 0.80,
                               max = 0.99,
                               step = 0.01,
                               value = 0.90
                             )
                           ),
                           column(
                             3,
                             offset = 0.5,
                             br(),
                             actionButton('addGroup',"Add New Group"),
                             br(),
                             actionButton('Change', 'Change Group Assignment'),
                             textInput('NewGroup', label = 'Enter new group designation')
                           ),
                           column(
                             3,
                             offset = 0.5,
                             br()
                           )
                         ),
                         uiOutput('brush')
                ),
                tabPanel(title = "multiplots",
                         fluidRow(column(3,uiOutput('xvar2UI')),
                                  column(1),
                                  column(3,
                                         uiOutput('yvar2UI')
                                  ),
                                  column(1),
                                  column(3,
                                         uiOutput('Code2UI')
                                  )
                         ),
                         fluidRow(column(3,actionButton("updateMultiplot","update")),column(1),column(3,numericInput("plotHeight",label = "plot height in pixels",min = 500,max = 2000, value = 900, step = 50)),column(1),column(4,actionButton('savePlot',"Save Plot"))),
                         fluidRow(
                           uiOutput('multiplotUI')
                         )
                )
    )
  )
}

#' Visualize Assign Server
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
visualizeAssignServer = function(input,output,session,rvals){

  output$sel <- renderUI({
    req(input$Code)
    req(rvals$attrData)
    vals = rvals$attrData[[input$Code]] %>% unique %>% sort
    checkboxGroupInput("groups",
                       "Groups to show:",
                       choices = vals,
                       selected = vals)
  })

  output$xvarUI = renderUI({
    req(rvals$chemicalData)
    if (input$data.src == 'principal components') {
      df = try(rvals$pca1$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$chemicalData, silent = T)
    }
    selectInput('xvar', 'X', names(df), selected = names(df)[1])
  })

  output$yvarUI = renderUI({
    req(rvals$chemicalData)
    if (input$data.src == 'principal components') {
      df = try(rvals$pca1$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$chemicalData, silent = T)
    }
    selectInput('yvar', 'y', names(df), selected = names(df)[2])
  })

  output$CodeUI = renderUI({
    selectInput('Code', 'GROUP', choices = colnames(rvals$attrData)[sapply(rvals$attrData, is.factor)])
  })

  output$xvar2UI = renderUI({
    req(rvals$chemicalData)
    selectInput('xvar2', 'X', names(rvals$chemicalData))
  })

  output$yvar2UI = renderUI({
    req(rvals$chemicalData)
    req(input$xvar2)
    choices = names(rvals$chemicalData)[which(names(rvals$chemicalData) != input$xvar2)]
    selectInput('yvar2', 'Y', choices = choices, multiple = T, selected = choices)
  })

  output$Code2UI = renderUI({
    selectInput('multigroup', 'GROUP', choices = colnames(rvals$attrData)[sapply(rvals$attrData, is.factor)])
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

  observeEvent(input$addGroup,{
    showModal(modalDialog(
      textInput('createGroup',"New Group Name",value = "cluster"),
      textInput('createGroupVal',"New Group Default Value",value = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createSubmit", "Submit")
      )
    ))
  })

  observeEvent(input$createSubmit,{
    removeModal()
    rvals$attrData = rvals$attrData %>%
      dplyr::mutate(!!as.name(input$createGroup) := factor(input$createGroupVal))
  })

  observeEvent(input$Change, {
    req(rvals$brushSelected)
    new = rvals$attrData %>%
      dplyr::mutate(rowid = 1:dplyr::n()) %>%
      dplyr::filter(rowid %in% rvals$brushSelected$rowid) %>%
      dplyr::mutate(!!as.name(input$Code) := input$NewGroup)
    old = rvals$attrData %>%
      dplyr::mutate(rowid = 1:dplyr::n()) %>%
      dplyr::filter(!rowid %in% rvals$brushSelected$rowid)
    rvals$attrData = dplyr::bind_rows(new, old) %>%
      dplyr::arrange(rowid) %>%
      dplyr::select(-rowid) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::all_of(input$Code)),factor)
  })

  # plot
  output$plot <- plotly::renderPlotly({
    req(rvals$plotlydf)
    p1 <-
      ggplot2::ggplot(rvals$plotlydf,
                      ggplot2::aes(
                        x = x,
                        y = y,
                        color = group,
                        shape = group,
                        key = rowid
                      )) +
      ggplot2::geom_point() +
      ggplot2::labs(
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
        p1 <- p1 + ggplot2::stat_ellipse(level = input$int.set)
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

  output$multiplotUI = renderUI({
    plotOutput("multiplot",width = "auto",height = input$plotHeight)
  })

  observeEvent(input$updateMultiplot,{
    req(rvals$chemicalData)
    req(input$xvar2)
    p = rvals$chemicalData %>%
      dplyr::bind_cols(rvals$attrData %>% dplyr::select(tidyselect::any_of(input$multigroup))) %>%
      dplyr::select(tidyselect::any_of(c(input$xvar2,input$yvar2,input$multigroup))) %>%
      tidyr::pivot_longer(-tidyselect::all_of(c(input$xvar2,input$multigroup))) %>%
      ggplot2::ggplot(ggplot2::aes(y = !!as.name(input$xvar2), x = value, color = !!as.name(input$multigroup))) +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::theme_bw()
    if(length(input$yvar2) > 1){
      p = p +
        ggplot2::facet_wrap(~name, scales = "free_x", strip.position = "bottom") +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = '#404040'),
                       strip.text = ggplot2::element_text(color = "white"))
    } else {
      p = p + ggplot2::xlab(input$yvar2)
    }
    rvals$multiplot = p
  })

  output$multiplot = renderPlot({
    req(rvals$multiplot)
    rvals$multiplot
  })

  observeEvent(input$savePlot,{
    showModal(modalDialog(
      title = "Save Plot",
      textInput("plotfilename", "File name:", value = "ggplot.png"),
      numericInput("width", "Width (inches):", value = 7),
      numericInput("height", "Height (inches):", value = 5),
      numericInput("res", "Resolution (dpi):", value = 300),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("saveMultiPlot", "Save")
      )
    ))
  })

  output$saveMultiPlot <- downloadHandler(
    filename = function() {
      input$plotfilename
    },
    content = function(file) {
      ggplot2::ggsave(filename = file, plot = rvals$multiplot,
                      width = input$width, height = input$height, dpi = input$res)
    }
  )

}
