library(shiny)

ui <- navbarPage("Production Planning",
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 tabPanel("Moving Average", moving_average_ui("moving_average")),
                 tabPanel("Yap / Al", yap_al_ui("yap_al"))
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  yap_al_server("yap_al")
}

shinyApp(ui, server)