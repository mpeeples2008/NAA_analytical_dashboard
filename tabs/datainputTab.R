# UI elements for Data Input tab



tabPanel(title = "Data Input", icon = icon("upload"), id = "datainput",
         
         sidebarLayout(
           sidebarPanel(
             fileInput("file1", "Choose CSV File", accept = c("text/csv", 
                                                            "text/comma-separated-values,text/plain", 
                                                            ".csv")),
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
         
