

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
                         fluidRow(column(3,actionButton("updateMultiplot","update")),column(1),column(3,numericInput("plotHeight",label = "plot height in pixels",min = 500,max = 2000, value = 900, step = 50))),
                         fluidRow(
                           uiOutput('multiplotUI')
                         )
                )
    )
  )
}
