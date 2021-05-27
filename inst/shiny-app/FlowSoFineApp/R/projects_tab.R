
projectsTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("FlowSoFine - A cool slogan here!"),
    fluidRow(align = "center",
             div(id=ns('clickNewProj'), style = "cursor: pointer",
                 infoBox("Create",
                         subtitle = tags$p("New Project", style = "font-size: 200%;"),
                         icon = icon("star"),
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
             shinyDirButton(ns("dirBut"),"select directory", "select directory", style = "visibility: hidden")

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

      shinyDirChoose(input, "dirBut", roots = getVolumes()())

      observeEvent(input$dirBut, {

        dir <- parseDirPath(roots = getVolumes(), input$dirBut)
        global$projectPath <- dir
        if (length(dir)) {

          hexF <- paste0(dir,"/","template.rds")
          infoF <- paste0(dir,"/","info.rds")

          if(file.exists(hexF)) {
            global$ND <- readRDS(hexF)
          }

          if(file.exists(infoF)) {
            info <- readRDS(infoF)
            global$title <- info[[1]]
            global$metadata <- info[[2]]
            #global$projectPath <- info[[2]]
          }

          runjs("$('header').css('display', '');")

          updateTabItems(session = parent_session, "tabs",
                         selected = "4_configureProject")

        }
      })


    }
  )
}
