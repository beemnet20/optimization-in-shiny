library(shiny)
library(shinyDND)
library(bsicons)


num_houses <- 4
num_hospitals <- 2
grid_columns <- 10
grid_rows <- 5

house_coordinates <- list(list(i = 2, j = 3),
                          list(i = 4, j = 2),
                          list(i = 1, j = 9),
                          list(i = 5, j = 7))

house_coordinates_str <-
  lapply(house_coordinates, function(coord)
    paste0(coord$i, ',', coord$j))

mainPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$h1("Optimization Visualised"),
    p(
      "Optimization is choosing the best option from a set of possible options. Algorithms can be used in different kinds of optimization problems."
    ),
    div(class = "grid-container",
        fluidRow(style = "margin: 0 auto;", lapply(1:5, function(i) {
          fluidRow(style = "margin: 0 auto;", lapply(1:10, function(j) {
            current_coordinate <- paste0(i, ',', j)
            if (current_coordinate %in% house_coordinates_str) {
              div(
                class = "grid-box",
                span("🏠", style = "font-size: xxx-large; background: none; width: auto; padding: 0")
              )
            } else {
              dropUI(
                ns(paste0("box", i, "_", j)),
                row_n = 1,
                col_n = 1,
                class = "grid-box dropelement"
              )
            }
            
          }))
        })), ),
    fluidRow(style = "background: black; width: 820px; margin-left: 10px; height: 80px;",
             column(
               1,
               dragUI(ns("hospital1"), div("🏥", tags$sup("1")), style = "font-size: xxx-large; background: none; width: auto; padding: 0;"),
             ),
             column(
               1,
               dragUI(ns("hospital2"), tags$span("🏥", tags$sup("2")), style = "font-size: xxx-large; background: none; width: auto; padding: 0;"),
             )),
    
    
    
  )
  
}

mainPageServer <- function(id, store = NULL) {
  moduleServer(id, function(input, output, session) {
    #create the initial state 
    initial_state <- matrix("", 5, 10)
    
    for(index in seq_along(house_coordinates)) {
      i <- house_coordinates[[index]]$i
      j <- house_coordinates[[index]]$j
      item_id <- paste0("house-", index)
      initial_state[i, j] <- item_id
    }
    # Create a reactive value to store the state of the board
    grid_state <- reactiveVal(initial_state)
    
    # Function to update the board state
    update_grid_state <- function(box_id, item_id) {
      state <- grid_state()
      
      # Extract row and column index from the box_id
      indices <-
        as.numeric(unlist(strsplit(sub(
          "box", "", box_id
        ), "_")))
      if (state[indices[1], indices[2]] != item_id) {
        # Clear the previous position of the item
        state[state == item_id] <- ""
        state[indices[1], indices[2]] <- item_id
        grid_state(state)
      }
    }
    

    
    # Observe events for all 16 boxes
    observe({
      lapply(1:5, function(i) {
        lapply(1:10, function(j) {
          current_coordinate <- paste0(i, ',', j)
          if (!current_coordinate %in% house_coordinates_str) {
            box_id <- paste0("box", i, "_", j)
            observeEvent(input[[box_id]], {
              hospital_id <- ifelse(grepl("1",input[[box_id]]), "hospital-1", "hospital-2")
              update_grid_state(box_id, hospital_id)
            }, ignoreNULL = TRUE)
          }
        })
      })
    })
    
    
    # Print the state of the board whenever it changes
    observe({
      print(grid_state())
    })
  })
  
}