## Forked from https://gist.github.com/dgrapov/128e3be71965bf00495768e47f0428b9
# plotly box or lasso select linked to rhandsontable


library(ggplot2)
library(plotly)
library(dplyr) 
library(ISLR) 
library(shiny)
library(DT)
library(autoplotly)
library(rhandsontable)

#reactive app
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  fluidRow(
    column(12, plotlyOutput("plot", height = "600px")),
    column(12,rHandsontableOutput("hot"))
    # column(12, verbatimTextOutput("text"))
  )
)


server <- function(input, output){
  
  output$plot <- renderPlotly({
    req(data())
    p < -ggplot(data = data()$data, mapping = aes(x = age, y = wage)) + 
         geom_point() + theme_bw() 
    
    obj <- data()$sel
    if(nrow(obj)!=0) {
      p < -p + geom_point(data=obj,color="red",size=4)
    }
    
    ggplotly(p,source="master")
  })
  
  #selected
  selected<-reactive({
    # event_data("plotly_click", source = "master")
    event_data("plotly_selected", source = "master")
  })
  
  output$text <- renderPrint({ 
    list(selection=selected(),
         dims=data()$sel)
  })
  
  output$hot <- renderRHandsontable({
    rhandsontable(data()$sel
   )
})
  
  #reactive data
  data <- reactive({
    tmp <- Wage 
    
    sel <- tryCatch(Wage[(selected()$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    
    list(data=tmp,sel=sel)
    
  })
  
  
}  

shinyApp(ui,server)

