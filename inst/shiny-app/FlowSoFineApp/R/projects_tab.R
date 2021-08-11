
projectsTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("FlowSoFine"),
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
             shinyjs::hidden(
               fileInput(ns("dirBut"), "Choose a Project File",
                         multiple = FALSE,
                         accept = c(".RData"))
             )
             #shinyDirButton(ns("dirBut"),"select directory", "select directory", style = "visibility: hidden")

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

      #shinyDirChoose(input, "dirBut", roots = getVolumes()())


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

        # dir <- parseDirPath(roots = getVolumes(), input$dirBut)
        # global$projectPath <- dir
        # if (length(dir)) {
        #
        #   hexF <- paste0(dir,"/","template.rds")
        #   infoF <- paste0(dir,"/","info.rds")
        #
        #   if(file.exists(hexF)) {
        #     global$ND <- readRDS(hexF)
        #   }
        #
        #   if(file.exists(infoF)) {
        #     info <- readRDS(infoF)
        #     global$title <- info[[1]]
        #     global$metadata <- info[[2]]
        #     #global$projectPath <- info[[2]]
        #   }

        runjs("$('header').css('display', '');")

        updateTabItems(session = parent_session, "tabs",
                       selected = "4_configureProject")

        #}
      })


    }
  )
}
