## Forked in part from https://gist.github.com/dgrapov/128e3be71965bf00495768e47f0428b9
## plotly box or lasso select linked to rhandsontable

library(ggplot2)
library(plotly)
library(dplyr) 
library(ISLR) 
library(shiny)
library(DT)
library(autoplotly)
library(rhandsontable)
library(shinythemes)

# read in sample data INAA_test, create attribute and element data.frames, impute missing data and transform
mydat <- read.csv('INAA_test.csv',header=T,row.names=1)
attr1 <- mydat[,c(1,3,5,7)] # pull out attributes for plotting
chem1 <- mydat[,c(8:21,23:40)] # pull out element data (excluing Ni)
chem1[chem1==0] <- NA # set 0 values to NA
chem.imp <- complete(mice(chem1,method='rf')) # impute missing data using the random forest approach
chem.t <- log10(chem.imp) # log-base-10 transform raw element data
pca1 <- prcomp(chem.t)

attr1$CORE <- as.character(attr1$CORE)
key <- row.names(attr1)
attr1 <- cbind(key,attr1)
dataset <<- cbind(attr1,chem.t,pca1$x[1:307,])

#reactive app
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  # Set theme
  mainPanel(
    column(12, plotlyOutput("plot", height = "600px", width="1000px")),
    fluidRow(
      column(2,offset=0.5,
             selectInput("data.src","Choose Dataset",choices=c("elements","PCA"),selected="elements"),
             selectInput("xvar","X",choices=names(dataset)[(ncol(attr1)+1):ncol(dataset)],selected="cs"),
             selectInput("yvar","Y",choices=names(dataset)[(ncol(attr1)+1):ncol(dataset)],selected="ta")),
      column(3,offset=0.5,
             selectInput("Code","GROUP",choices=names(attr1[2:ncol(attr1)]),selected="CORE"),
             checkboxInput("Conf","Confidence Elipse",value=TRUE),
             sliderInput("int.set","Set Confidence Interval",min=0.80,max=0.99,step=0.01,value=0.90)),
      h5(strong("Update plots")),
             actionButton("Change","Commit changes"),
     column(12, rHandsontableOutput("hot")),
     br(),
     column(12, verbatimTextOutput("text"))
      
    )
  )
)

server <- function(input, output){
  
  # ggplotly code
  output$plot <- renderPlotly({
    
    # basic ggplot setup
    p <- ggplot(data = data()$data, mapping = aes(x=get(input$xvar), y=get(input$yvar), key=key, color=get(input$Code), shape=get(input$Code))) + 
         geom_point() +
         theme_bw() +
         labs(x=input$xvar,y=input$yvar,color=input$Code,shape=input$Code) 
            # add ellipses at specified level if box checked
            if(input$Conf) {p <- p + stat_ellipse(data=data()$data, aes(x=get(input$xvar),y=get(input$yvar),
                                 color=get(input$Code),group=get(input$Code)),level=input$int.set)}
    
    # draw circles around selected points
    obj <- data()$sel
    if(nrow(obj)!=0) {
      p <- p + geom_point(data=obj, aes(x=get(input$xvar),y=get(input$yvar)), color="red",size=4,shape=1)}
    
    # ggplotly render command with lasso as default tool
    ggplotly(p,source="master") %>% layout(dragmode = "lasso") 
  })
  
  # reactive function to determine points selected in ggplotly
  selected<-reactive({
    event_data("plotly_selected", source = "master")
  })
  
  # text for debugging
  #output$text <- renderPrint({ 
  #   hot_to_r(input$hot)
  #})
  
  # output rhandsontable for viewing and editing
  output$hot <- renderRHandsontable({
    rhandsontable(data()$sel[,1:ncol(attr1)],rowHeaders=F) %>% hot_cols(columnSorting = TRUE)
  })
  
  # reactive data call with error handling
  data <- reactive({
    tmp <- dataset
    sel <- tryCatch(dataset[which(dataset$key %in% selected()$key),,drop=FALSE] , error=function(e){NULL})
    list(data=tmp,sel=sel)
  })
  
  # update dataset with changes from rhandsontable on button press, update figure on deselect
  observeEvent(input$Change, {
    input$Change
    isolate({   # isolate function and associated with button
      if (input$Change == 0) {
        dataset <<- data()$data
        return(dataset)}
      else {
        dataset[which(dataset$key %in% selected()$key),1:ncol(attr1)] <<- hot_to_r(input$hot) # update dataset in the global environment
        return(dataset)} # return dataset
    })
  })
  
}# close server.R  
  

# run Shiny app
shinyApp(ui,server)

