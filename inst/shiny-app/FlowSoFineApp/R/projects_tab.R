# Initial tab. Decide to load or create a new project


projectsTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("FlowSoFine"),
    fluidRow(align = "center",
             div(id=ns('clickNewProj'), style = "cursor: pointer",
                 infoBox("Create",
                         subtitle = tags$p("New Project", style = "font-size: 200%;"),
                         icon = icon("microscope"),
                         width = 6,
                         color = "aqua")
             )),

    fluidRow(align = "center",
             div(id=ns('clickLoadProj'), style = "cursor: pointer",
                 infoBox("Load",
                         subtitle = tags$p("Existing Project", style = "font-size: 200%;"),
                         icon = icon("file-upload"),
                         width = 6,
                         color = "aqua")
             ),
             shinyjs::hidden(
               fileInput(ns("dirBut"), "Choose a Project File",
                         multiple = FALSE,
                         accept = c(".RData"))
             )

    )
  )
}

projectsTabServer <- function(id, global, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      onclick("clickNewProj", updateTabItems(parent_session, "tabs",
                                                selected = "2_newProject"))

      onclick("clickLoadProj", click("dirBut"))


      observeEvent(input$dirBut, {

        req(input$dirBut)

        load(input$dirBut$datapath, loadingEnv <- new.env())
        isolate(global$ND <- loadingEnv$ND)
        isolate(global$template <- global$ND)
        global$distM <- loadingEnv$distM
        global$distanceString <- loadingEnv$distanceString
        global$metadata <- loadingEnv$metadata
        global$title <- loadingEnv$title

        global$justLoaded <- TRUE


        runjs("$('header').css('display', '');")

        updateTabItems(session = parent_session, "tabs",
                       selected = "4_configureProject")

      })


    }
  )
}
