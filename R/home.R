library(shiny)

homeUI <- function(id){
  
  fluidPage(
    mainPageUI("main"),
  )
}

homeServer <- function(id){
  mainPageServer("main")
  moduleServer(id, function(input, output, session){
    #module code
  })
  
}