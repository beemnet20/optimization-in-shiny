source("global.R")

library(shiny)
library(bslib)


ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  tags$head(
    tags$link(rel ="stylesheet", type="text/css", href = "css/styles.css"),
    #--------------------------
    #load project title from global.R
    tags$title({project_title}),
    #--------------------------
  ),
  #Home and About page UI components are 
  #imported in headerUI component
  headerUI("header"),
  homeUI("home"),
  

)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  headerServer("header")
  homeServer("home")

}

shinyApp(ui = ui, server = server)