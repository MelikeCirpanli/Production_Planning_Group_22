library(shiny)

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Best N (MA)", ma_n_ui("ma_n"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  ma_n_server("ma_n")
}

shinyApp(ui, server)