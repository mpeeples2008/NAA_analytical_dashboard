

#' plot lasso updated3
#'
#' @param attributes
#' @param dat1
#' @param grps
#'
#' @return
#' @export
#'
#' @examples
plotApp <- function(attributes,dat1,grps) {
  library(plotly)
  library(shiny)
  library(knitr)
  library(kableExtra)

  theme_set(theme_light())
dataset <- cbind(attributes,dat1)
pc <- as.data.frame(princomp(dat1)$scores)
vv <- NULL

ui <- fluidPage(
  sidebarLayout(position='right',
    sidebarPanel(uiOutput('sel')),
    mainPanel(
  plotlyOutput('plot', width='1000px', height='600px'),
  fluidRow(
      column(2,
          selectInput('data.src','Choose Dataset',choices=c('elements','principal components'),selected='elements'),
          selectInput('xvar','X',names(dat1),selected='cs'),
          selectInput('yvar','Y',names(dat1),selected='ta')),
      column(3,offset=0.5,
          selectInput('Code','GROUP',names(attributes),selected='CORE'),
          checkboxInput('Conf','Confidence Elipse',value=TRUE),
          sliderInput('int.set','Set Confidence Interval',min=0.80,max=0.99,step=0.01,value=0.90)),
      column(3,offset=0.5,
          br(),
          actionButton('Change','Change Group Assignment'),
          textInput('NewGroup', label = 'Enter new group designation')),
      column(3,offset=0.5,
          br(),
          actionButton("exit", label = "Return to R and write data"))),
  verbatimTextOutput('brush'),
  verbatimTextOutput('checkGrps')
)))



server <- function(input, output) {

  data.sel <- reactive({
    dataset[,c(input$xvar,input$yvar,input$Code)]
  })

  output$sel <- renderUI({
    checkboxGroupInput("groups", "Groups to show:",
                       unique(data.sel()[,3]),selected=unique(data.sel()[which(unique(data.sel())[,3] != ''),3]))
  })

  output$plot <- renderPlotly({
    g1 <- data.sel()
    g1 <- g1[which(g1[,3] %in% input$groups),]
    p1 <- ggplot(g1, aes(x=g1[,1], y=g1[,2], color=g1[,3], shape=g1[,3])) +
      geom_point() +
      labs(x=input$xvar,y=input$yvar,color=input$Code,shape=input$Code)
      if(input$Conf) {p1 <- p1 + stat_ellipse(level=input$int.set)}
    ggplotly(p1) %>% layout(dragmode = 'select')
  })




  output$brush<- renderPrint({
    g1 <- data.sel()
    d <- event_data('plotly_selected')
    dd <- round(cbind(d[[3]],d[[4]]),3)
    vv <- attributes[which(round(g1[,1],3) %in% dd[,1] & round(g1[,2],3) %in% dd[,2]),]
    vv <<- vv
    if (is.null(vv)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else kable(vv)
  })

  observeEvent(input$Change > 0, {
  if (!is.null(vv)) {
    dataset[which(row.names(dataset) %in% row.names(vv)),]$CORE <<- input$NewGroup
      }})

  observe({
  if(input$exit > 0)
  stopApp()})

  }

runApp(shinyApp(ui, server))
return(dataset)
}

# read in sample data INAA_test, create attribute and element data.frames, impute missing data and transform
mydat <- read.csv('INAA_test.csv',header=T,row.names=1)
attr1 <- mydat[,c(1,3,5,7)] # pull out attributes for plotting
chem1 <- mydat[,c(8:21,23:40)] # pull out element data (excluing Ni)
chem1[chem1==0] <- NA # set 0 values to NA
# chem.imp <- complete(mice(chem1,method='rf')) # impute missing data using the random forest approach
chem.t <- log10(chem1) # log-base-10 transform raw element data

# Run app
chem.t = chem.t %>% mutate_all(list(function(c)case_when(is.infinite(c)~0,is.na(c)~0,TRUE~c)))
g1 <- plotApp(attr1,chem.t,attr1$CORE)

