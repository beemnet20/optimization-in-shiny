library(shiny)

footerUI <- function(id){
  #namespace ns 
  #wrap input and output ids with ns 
  ns <-NS(id)
  tags$footer(
    div( class = "sticky-footer row",
      p("Built by Bee Workeneh")     
    )
  )
  
}

footerServer <- function(id){
  moduleServer(id, function(input, output,session){

  })
}