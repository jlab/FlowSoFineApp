newProjectTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("New Project"),
    fluidRow(
      box(title = "Directory",
        textInput(ns("projName"), "Name of the project:", "untitled"),
        tags$b("Project directory:"),
        verbatimTextOutput(ns("dirText"), placeholder = T),
        shinyDirButton(ns("changeDir"),"Select Directory", "Select Directory")
      )
    ),
    fluidRow(
      box(title = "Load .fcs files",
        shinyFilesButton(ns('loadFCS'),
                         label='File select',
                         title='Select fcs files',
                         multiple=T),
        div(id = ns("check1"), icon("check"), style = "color: green;")#,
      )
    ),
    fluidRow(
      box(title = "Load metadata .csv file",
          shinyFilesButton(ns('loadCSV'),
                           label='File select',
                           title='Please select a file',
                           multiple=F),
          div(id = ns("check2"), icon("check"), style = "color: green;"),
      )
    ),

    div(actionButton(ns("checkLinking"), "Inspect Linking")),
    actionBttn(ns("createButton"), "Create Project", style = "simple", color = "primary"),

  )
}

newProjectTabServer <- function(id, global, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      output$dirText <- renderText({
        if(is.null(global$projectPath)) {
          global$projectPath <- getwd()
        }
        global$projectPath
      })

      shinyFileChoose(input, "loadFCS",
                      roots = getVolumes(), filetype = "fcs")

      shinyFileChoose(input, "loadCSV",
                      roots = getVolumes(), filetype = "csv")

      shinyDirChoose(input, "changeDir",
                     roots = getVolumes()())

      observeEvent(input$loadFCS, {
        files <- parseFilePaths(roots = getVolumes(), input$loadFCS)
          if(nrow(files)) {
            global$fcsPath <- files$datapath
            #global$fcs <- read.flowSet(files$datapath)
          }
        })

      observeEvent(input$loadCSV, {

        files <- parseFilePaths(roots = getVolumes(), input$loadCSV)
        if(nrow(files)) {
          #global$metadataPath <- files$datapath
          global$metadata <- read.table(files$datapath, sep = ";", header = T)
        }

      })

      observeEvent(input$changeDir, {

        dir <- parseDirPath(roots = getVolumes(), input$changeDir)

        if (length(dir)) {
          global$projectPath <- dir
        }

      })

      observe({

        if(!is.null(global$fcsPath)) {
          shinyjs::show("check1")
        } else {
          shinyjs::hide("check1")
        }

        if(!is.null(global$metadata)) {
          shinyjs::show("check2")
        } else {
          shinyjs::hide("check2")
        }

        if (!is.null(global$metadata) & !is.null(global$fcsPath)) {
          shinyjs::show("createButton")
          shinyjs::show("checkLinking")
        } else {
          shinyjs::hide("createButton")
          shinyjs::hide("checkLinking")
        }
      })

      observeEvent(input$createButton, {
        if(!is.null(global$metadata) & !is.null(global$fcsPath)) {
          #global$metadata <- read.table(global$metadataPath, sep = ";", header = T)
          global$fcs <- read.flowSet(global$fcsPath)
          global$title <- input$projName
          b <- list()
          b[[1]] <- global$title
          b[[2]] <- global$metadata
          saveRDS(b, file = paste0(global$projectPath,"/","info.rds"))

          runjs("$('header').css('display', '');")

          updateTabItems(session = parent_session, "tabs",
                         selected = "4_configureProject")
        }
      })

      observeEvent(input$checkLinking, {
        showModal({

          ns <- session$ns

          tags$div(id = "inspectModal",
          modalDialog(title = "Metadata", easyClose = T,
                              #"Check if FACS files and metadata link up correctly",
                              tableOutput(ns("table"))
                      )

        )}
        )
      })

      output$table <- renderTable(

        if(length(global$fcsPath)) {
          #fl <- as.list(global$fcs@frames)
          #fl <- names(fl)[order(names(fl))]
          fl <- global$fcsPath
          if(length(fl) == nrow(global$metadata)) {
            cbind(fl, global$metadata)
          } else {
            fl
          }

        })

    }
  )
}
