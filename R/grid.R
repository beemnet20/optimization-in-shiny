library(shiny)
library(shinyDND)
library(bsicons)

gridUI <- function(id, manual = TRUE) {
  ns <- NS(id)
  fluidPage(uiOutput(ns("dynamic_grid")),
            if (manual == TRUE) {
              uiOutput(ns("dynamic_hospitals"))
            })
}

calculate_cost <- function(houses, hospitals) {
  cost <- 0
  
  for (house in houses) {
    house_i <- house$i
    house_j <- house$j
    
    distances <- sapply(hospitals, function(hospital) {
      hospital_i <- hospital[['i']]
      hospital_j <- hospital[['j']]
      abs(house_i - hospital_i) + abs(house_j - hospital_j)
    })
    
    cost <- cost + min(distances)
  }
  
  return(cost)
}

perform_hill_climbing <- function(initial_state, house_coordinates, num_hospitals, num_rows, num_columns) {
  best_state <- initial_state
  best_cost <- num_rows*num_columns
  
  for (iter in 1:100) {  # Adjust the number of iterations as needed
    current_state <- best_state
    improved <- FALSE

    for (hospital_num in 1:num_hospitals) {
      for (i in 1:num_rows) {
        for (j in 1:num_columns) {
          # Check if current position is a house
          if (!is_house_position(i, j, house_coordinates)) {
            # Try moving a hospital to this position
            new_state <- move_hospital(current_state, hospital_num, i, j, num_hospitals, house_coordinates)
            new_cost <- calculate_cost(house_coordinates, extract_hospitals(new_state))
            
            if (new_cost < best_cost) {
              best_cost <- new_cost
              best_state <- new_state
              improved <- TRUE
            }
          }
        }
      }
    }
    
    if (!improved) {
      break  # No better state found
    }
  }
  
  return(best_state)
}

is_house_position <- function(i, j, house_coordinates) {
  any(sapply(house_coordinates, function(coord) coord$i == i && coord$j == j))
}

move_hospital <- function(state, hospital_num, new_i, new_j, num_hospitals, house_coordinates) {
  # Find the current position of the hospital to move
  current_hospital_label <- paste0("hospital-", hospital_num)
  current_hospital_coords <- which(state == current_hospital_label, arr.ind = TRUE)
  
  # If the hospital is already placed in the grid, clear its current position
  if (length(current_hospital_coords) > 0) {
    state[current_hospital_coords] <- ""
  }
  
  # Place the hospital at the new location if it's not a house position
  if (!is_house_position(new_i, new_j, house_coordinates)) {
    state[new_i, new_j] <- current_hospital_label
  }
  return(state)
}




extract_hospitals <- function(state) {
  occupied_coordinates <- which(state != "", arr.ind = TRUE)
  hospital_coordinates <- list()
  
  for (idx in 1:nrow(occupied_coordinates)) {
    i <- occupied_coordinates[idx, 1]
    j <- occupied_coordinates[idx, 2]
    
    element <- state[i, j]
    if (grepl("hospital", element)){
      hospital_coordinates[[length(hospital_coordinates)+1]] = list(i=i, j=j)
    }
  }
  return(hospital_coordinates)
}


gridServer <-
  function(id,
           num_houses = 4,
           num_hospitals = 2,
           grid_columns = 10,
           grid_rows = 5,
           manual = TRUE) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      window_width <- 800
      grid_box_size <- window_width / grid_columns
      window_height <- grid_box_size * grid_rows
      elements_font_size <- (grid_box_size / 16) - 1.8
      
      house_coordinates <- list()
      for (k in 1:num_houses) {
        i <- sample(grid_rows, 1)
        j <- sample(grid_columns, 1)
        house_coordinates[[k]] <- list(i = i, j = j)
      }
      house_coordinates_str <-
        lapply(house_coordinates, function(coord)
          paste0(coord$i, ',', coord$j))
      
      createHospitalElements <- function(ns) {
        lapply(1:num_hospitals, function(num) {
          column(1, dragUI(
            ns(paste0("hospital-", num)),
            div("🏥", tags$sup(num)),
            style = paste0(
              "font-size:",
              elements_font_size,
              "em; background: none; width: auto; padding: 0; margin: -10px;"
            )
          ))
        })
      }
      
      initial_state <- matrix("", grid_rows, grid_columns)
      for (index in seq_along(house_coordinates)) {
        i <- house_coordinates[[index]]$i
        j <- house_coordinates[[index]]$j
        item_id <- paste0("house-", index)
        initial_state[i, j] <- item_id
      }
      grid_state <- reactiveVal(initial_state)
      optimal_state <- perform_hill_climbing(initial_state, house_coordinates,num_hospitals, grid_rows, grid_columns )
      optimal_cost <- calculate_cost(house_coordinates, extract_hospitals(optimal_state))
      output$optimal_cost <- renderText(paste0("Optimal cost: ", optimal_cost))
      current_cost <- reactiveVal(NULL)
      
      update_current_cost <- function(){
        state <- grid_state()
        hospital_coordinates <- extract_hospitals(state)
        
        cost <- calculate_cost(house_coordinates, hospital_coordinates)
        current_cost(cost)
        output$current_cost <- renderText(paste0("Current cost: ", cost))
      }
      
      update_grid_state <- function(box_id, item_id) {
        state <- grid_state()
        indices <-
          as.numeric(unlist(strsplit(sub(
            "box", "", box_id
          ), "_")))
        if (state[indices[1], indices[2]] != item_id) {
          state[state == item_id] <- ""
          state[indices[1], indices[2]] <- item_id
          grid_state(state)
        }
      }
      
      output$dynamic_grid <- renderUI({
        div(
          div(
            style = paste0(
              "background: black; width:",
              window_width + 20,
              "px; margin: 0 auto; height:",
              grid_box_size/2,
              "px;"
            ), 
            fluidRow(
              column(6,
                     tags$h3(style="color: white; margin: auto; width: 80%;", textOutput(ns("optimal_cost")))
                     ),
              column(6,
                     tags$h3(style="color: white; margin: auto; width: 80%;", textOutput(ns("current_cost")))
                     )
            )
          ),
          div(
          class = "grid-container",
          style = paste0(
            "width:",
            window_width + 20,
            "px; height:",
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
                      "em; background: none; width: auto; padding: 0"
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
          }))
        ))
      })
      
      if (manual == TRUE) {
        output$dynamic_hospitals <- renderUI({
          fluidRow(
            style = paste0(
              "background: black; width:",
              window_width + 20,
              "px; margin: 10px auto; height:",
              grid_box_size,
              "px;"
            ),
            createHospitalElements(ns)
          )
        })
      }
      
      
      observe({
        lapply(1:grid_rows, function(i) {
          lapply(1:grid_columns, function(j) {
            current_coordinate <- paste0(i, ',', j)
            if (!current_coordinate %in% house_coordinates_str) {
              box_id <- paste0("box", i, "_", j)
              observeEvent(input[[box_id]], {
                hospital_id <-
                  paste0("hospital-", as.numeric(gsub("\\D", "", input[[box_id]])))
                update_grid_state(box_id, hospital_id)
                update_current_cost()
              }, ignoreNULL = TRUE)
            }
          })
        })
      })
      
      observe({
        print(grid_state())
      })
    })
  }
