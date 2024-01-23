library(shiny)

aboutUI <- function(id) {
  
  # wrap input and output ids with ns
  # namespace ns
  ns <- NS(id)
  
  fluidPage(
    tags$p("About page")
  )
}

aboutServer <- function(id, store=NULL) {
  moduleServer(id, function(input, output, session) {
  })
}
