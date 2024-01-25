library(shiny)
library(shinyDND)
library(bsicons)


num_houses <- 4
num_hospitals <- 4
grid_columns <- 15
grid_rows <- 5

window_width <- 800
grid_box_size <- window_width / grid_columns
window_height <- grid_box_size * grid_rows

elements_font_size <- (grid_box_size / 16) - 1.5

house_coordinates <- list()

for (k in 1:num_houses) {
  i <- sample(grid_rows, 1)    # Randomly select a row index
  j <- sample(grid_columns, 1)  # Randomly select a column index
  
  # Append the coordinate to the list
  house_coordinates[[k]] <- list(i = i, j = j)
}


house_coordinates_str <-
  lapply(house_coordinates, function(coord)
    paste0(coord$i, ',', coord$j))


create_hospital_elements <- function(ns) {
  lapply(1:num_hospitals, function(num) {
    column(1,
           dragUI(
             ns(paste0("hospital-",num)),
             div("🏥", tags$sup(num)),
             style = paste0(
               "font-size:",
               elements_font_size,
               "em;",
               "background: none; width: auto; padding: 0"
             )
           ),)
  })
}

mainPageUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$h1("Optimization Visualised"),
    p(
      "Optimization is choosing the best option from a set of possible options. Algorithms can be used in different kinds of optimization problems."
    ),
    div(
      class = "grid-container",
      style = paste0(
        "width:",
        window_width + 20,
        "px;",
        "height:",
        window_height + 20,
        "px;"
      ),
      fluidRow(style = "margin: 0 auto;", lapply(1:grid_rows, function(i) {
        fluidRow(style = "margin: 0 auto;", lapply(1:grid_columns, function(j) {
          current_coordinate <- paste0(i, ',', j)
          if (current_coordinate %in% house_coordinates_str) {
            div(
              class = "grid-box",
              style = paste0(
                "width:",
                grid_box_size,
                "px !important; height:",
                grid_box_size,
                "px !important;"
              ),
              span(
                "🏠",
                style = paste0(
                  "font-size:",
                  elements_font_size,
                  "em;",
                  "background: none; width: auto; padding: 0"
                )
              )
            )
          } else {
            dropUI(
              ns(paste0("box", i, "_", j)),
              row_n = 1,
              col_n = 1,
              class = "grid-box dropelement",
              style = paste0(
                "width:",
                grid_box_size,
                "px !important; height:",
                grid_box_size,
                "px !important;"
              )
            )
          }
          
        }))
      })),
      
    ),
    fluidRow(
      style = paste0(
        "background: black; width:",
        window_width + 20 ,
        "px;", "margin-left: 10px; height:",
        grid_box_size, "px;"
      ),
    create_hospital_elements(ns),
    ),
  )
  
}

mainPageServer <- function(id, store = NULL) {
  moduleServer(id, function(input, output, session) {
    #create the initial state
    initial_state <- matrix("", grid_rows, grid_columns)
    
    for (index in seq_along(house_coordinates)) {
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
      lapply(1:grid_rows, function(i) {
        lapply(1:grid_columns, function(j) {
          current_coordinate <- paste0(i, ',', j)
          if (!current_coordinate %in% house_coordinates_str) {
            box_id <- paste0("box", i, "_", j)
            observeEvent(input[[box_id]], {
              hospital_id <- paste0("hospital-",as.numeric(gsub("\\D", "",input[[box_id]])))
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