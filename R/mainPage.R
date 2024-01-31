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
             p(
               "So how did we calculate the optimal cost?  We use an optimization technique known as the", tags$a(target="_blank",
               href="https://www.geeksforgeeks.org/introduction-hill-climbing-artificial-intelligence/#", 'hill climbing'), "algorithm. 
               The purpose of this algorithm is to find the most efficient arrangement of hospitals on a grid that contains houses. 
               The goal is to minimize the total distance from each house to its nearest hospital, which we refer to as minimizing the cost."
             )
            
           ),
           tags$ol(
             tags$li("We start with hospitals placed at random positions on our grid."),
             tags$li("The algorithm examines the grid to see if moving a hospital to a different position reduces the total distance to the houses."),
             tags$li("If a better position is found, the algorithm moves a hospital to this new position."),
             tags$li("This process is repeated for each hospital, iteratively exploring different positions."),
             tags$li("The process continues until no further reduction in total distance is possible."),
             tags$li("The final arrangement represents the optimized solution with minimized total distance from all houses to their nearest hospital.")
           )

           )

}


mainPageServer <- function(id, store = NULL) {
  gridServer("demo-grid-1", manual=TRUE)
  moduleServer(id, function(input, output, session) {
    
  })
}