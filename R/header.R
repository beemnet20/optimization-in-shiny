library(shiny)
library(bsicons)

# load global variables if they don't already exist
if (!exists("desc_content")) {
  source("global.R")
}

headerUI <- function(id) {
  # wrap input and output ids with ns
  # namespace ns
  ns <- NS(id)
  
  fluidRow(
    class = "header",
    column(
      2,
      tags$img(
        class = "avatar",
        src = "assets/avatar1.jpg",
        width = 120,
        alt = "avatar"
      )
    ),
    column(
      8,
      div(class = "chip text",
          "Beemnet Workeneh"),
      tags$br(),
      div(class = "chip text",
          "Software Engineer"),
      tags$br(),
      div(class = "chip",
          a(
            href = "tel:206-355-0608",
            bs_icon("telephone-fill", class = "text-dark", style = "font-size: 2rem;")
          ),),
      div(class = "chip",
          a(
            href = "mailto:beemnet17@gmail.com",
            bs_icon("envelope-fill", class = "text-dark", style = "font-size: 2rem;")
          ),),
      div(
        class = "chip",
        a(
          href = "https://www.linkedin.com/in/beemnet-workeneh-25b579b3/",
          target = "_blank",
          bs_icon("linkedin", class = "text-dark", style = "font-size: 2rem;")
        ),
      ),
      div(
        class = "chip",
        a(
          href = "https://github.com/beemnet20",
          target = "_blank",
          bs_icon("github", class = "text-dark", style = "font-size: 2rem;")
        ),
      ),
      div(
        class = "chip",
        a(class="text-dark", href = "https://beemnet17.wixsite.com/beemnetworkeneh", target = "_blank", "Portfolio")
      )
      
    )
  )
}

headerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # navbarPage functions
  })
}
