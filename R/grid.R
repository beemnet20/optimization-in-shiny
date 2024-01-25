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

gridServer <-
  function(id,
           num_houses = 4,
           num_hospitals = 2,
           grid_columns = 10,
           grid_rows = 5,
           manual = TRUE,
           store = NULL) {
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
                print(hospital_id)
                update_grid_state(box_id, hospital_id)
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