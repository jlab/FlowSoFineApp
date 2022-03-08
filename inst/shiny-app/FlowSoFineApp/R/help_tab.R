# Display help video and link to ressources

helpTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Help"),

    h3("Brief explanatory video:"),

    tags$video(id="walkthrough", type = "video/mp4",src = "FlowSoFine_demo.mp4", controls = "controls"),

    h3("Further reading:"),

    tags$a(href = "https://github.com/jlab/FlowSoFine#readme", "https://github.com/jlab/FlowSoFine#readme", target="_blank")

  )
}

helpTabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


    }
  )
}
