

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
    fluidPage(sidebarLayout(
      position = 'right',
      sidebarPanel(uiOutput('sel')),
      mainPanel(
        plotlyOutput('plot', width = '1000px', height = '600px'),
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
            br()#,
            # actionButton("exit", label = "Return to R and write data")
          )
        ),
        uiOutput('brush'),
        verbatimTextOutput('checkGrps') # what does this do?
      )
    ))
  )
}
