library(shiny)



mainPageUI <- function(id) {
  ns <- NS(id)
  fluidRow(tags$h1("Optimization Visualised"),
           p(
             paste(
               "Optimization is choosing the best option from a set of possible options.",
               "Algorithms can be used in different kinds of optimization problems.",
               "Lets consider this scenario: You are considering where to construct hospitals",
               "You have to take into account the density of residentials to make sure you have hospitals at convenient locations.",
               "Let's see if you can find the optimal location to build two hospitals.",
               "The optimal locations will have the minimum cost -- in this case the minimum distance",
               sep = " "
             ),
             tags$a(href = "https://simple.wikipedia.org/wiki/Manhattan_distance",
                    target = "_blank" , '(Manhatten distance) '),
             paste(
               "from each house to a hospital. Drag and drop the hospital icons and drop them into the optimal locations."
             ),
             tags$em("The location of the houses is randomly generated. If they appear too close together you can refresh 
                     the page and new locations will be set."),
             gridUI("demo-grid-1", manual=TRUE),
             
           )

           )

}


mainPageServer <- function(id, store = NULL) {
  gridServer("demo-grid-1", manual=TRUE)
  moduleServer(id, function(input, output, session) {
    
  })
}