
#' UI Elements for Saving and Exporting 
#'
#' @return
#' @export
#'
#' @examples
saveexportTab = function(){tabPanel(title = "Save & Export", icon = icon("download"),
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
}