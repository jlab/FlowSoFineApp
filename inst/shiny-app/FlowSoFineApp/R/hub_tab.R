hubTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Hub"),

    fluidRow(
      div(style = "cursor: pointer", id = ns("configure"),
          infoBox("configure", "Configure", icon =  icon("cogs"))
      ),

      div(style = "cursor: pointer", id = ns("export"),
          infoBox("export", "Details & Export", icon =  icon("search"))
      )
    ),

    h3("Grid based"),

    fluidRow(


      div(style = "cursor: pointer", id = ns("visualize"),
          infoBox("inspect and visualize samples", "Sample Inspector", icon =  icon("paint-brush"), color = "black")
      ),
      div(style = "cursor: pointer", id = ns("tscores"),
          infoBox("Visualize population shifts", "t-Scores", icon =  icon("braille"), color = "black")
      )
    ),

    h3("Distance matrix based"),

    fluidRow(

      div(style = "cursor: pointer", id = ns("nmds"),
          infoBox("Ordination", "NMDS", icon =  icon("project-diagram"))
      ),
      div(style = "cursor: pointer", id = ns("betadisper"),
          infoBox("Test for beta dispersion", "Beta dispersion", icon =  icon("spinner"))
      ),
      div(style = "cursor: pointer", id = ns("permanova"),
          infoBox("Perform pairwise adonis2", "PERMANOVA", icon =  icon("asterisk"))
      )
    ),

    h3("Authors"),
    fluidRow(
      div(style = "cursor: pointer", id = ns("authors"),
          infoBox("authors", "Authors", icon =  icon("at"), color = "black")
      )
    ),
  )
}

hubTabServer <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      onclick("visualize", updateTabItems(parent_session, "tabs",
                                                selected = "6_visualize"))

      onclick("tscores", updateTabItems(parent_session, "tabs",
                                             selected = "7_tscores"))

      onclick("configure", updateTabItems(parent_session, "tabs",
                                             selected = "4_configureProject"))

      onclick("nmds", updateTabItems(parent_session, "tabs",
                                           selected = "8_nmds"))

      onclick("permanova", updateTabItems(parent_session, "tabs",
                                        selected = "9_permanova"))

      onclick("export", updateTabItems(parent_session, "tabs",
                                             selected = "12_detail"))

      onclick("authors", updateTabItems(parent_session, "tabs",
                                        selected = "11_impressum"))

      onclick("betadisper", updateTabItems(parent_session, "tabs",
                                           selected = "13_betadisper"))

    }
  )
}
