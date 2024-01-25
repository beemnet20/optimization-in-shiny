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
  # initialize shinyStore
  shinyStore::initStore("store", "shiny-demo-store")
  

)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  #---------------------------------------------------
  # We need to pass store to modules that require local storage
  store <- reactive({
    input$store
  })
  #---------------------------------------------------
  headerServer("header")
  homeServer("home", store=store)

}

shinyApp(ui = ui, server = server)