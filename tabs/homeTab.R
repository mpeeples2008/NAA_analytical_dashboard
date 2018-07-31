# UI elements for the Home tab
# The home tab provides an introduction and basic overview of functionality
# In lieu of directing the user to a self-contained Help section, the 
# Home tab will provide links to resources for assistance. These will 
# include a blog with posts regarding functionality and updates, contact 
# info for developers, and a link to a Google group or similar forum 
# for Q+A's as well as FAQ's

tabPanel(title = "Home", icon = icon("home"),
  
         
         div(id = "home", 
             p(class = "lead", "Welcome to", strong("TACA"),": ", strong("T"), "ools for", strong("A"),
               "rchaeological ", strong("C"), "ompositional", strong("A"), "nalysis, a user friendly web application for the statistical analysis and visualization of geochemical compositional data of archaeological materials.")
             
# Once a name and overall functionality is decided upon, this area can be populated with a basic overview
# of the shiny app and resources to learn more
             
             )
         )
         


