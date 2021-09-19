newProjectTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("New Project"),
    fluidRow(
      box(title = "Title",
        textInput(ns("projName"), "Name of the project:", "untitled"),

      )
    ),
    fluidRow(
      box(title = "Load .fcs files",
        # shinyFilesButton(ns('loadFCS'),
        #                  label='File select',
        #                  title='Select fcs files',
        #                  multiple=T),
        fileInput(ns("fcsFiles"), "Choose FCS Files",
                  multiple = TRUE,
                  accept = c(".fcs")),
        #div(id = ns("check1"), icon("check"), style = "color: green;")#,
      )
    ),
    fluidRow(
      box(title = "Load metadata .csv file",

          checkboxInput(ns("csvOptions"), label = "Edit CSV options"),

          conditionalPanel("input.csvOptions == true", ns = ns,
            textInput(ns("sepText"), label = "separator:", value = ";"),
            textInput(ns("decText"), label = "decimal point:", value = ",")
          ),

          fileInput(ns("csvFile"), "Choose CSV File",
                    multiple = FALSE,
                    accept = c(".csv")),
      )
    ),

    div(actionButton(ns("checkLinking"), "Inspect Linking")),
    actionBttn(ns("createButton"), "Load FCS files", style = "simple", color = "primary"),

  )
}

newProjectTabServer <- function(id, global, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$csvFile, {

        global$metadata <- read.csv(input$csvFile$datapath,
                                      sep = input$sepText,
                                      dec = input$decText,
                                      header = T)

      })

      observeEvent(input$changeDir, {

        dir <- parseDirPath(roots = getVolumes(), input$changeDir)

        if (length(dir)) {
          global$projectPath <- dir
        }

      })

      observe({

        # if(!is.null(global$fcsPath)) {
        #   shinyjs::show("check1")
        # } else {
        #   shinyjs::hide("check1")
        # }
        #
        # if(!is.null(global$metadata)) {
        #   shinyjs::show("check2")
        # } else {
        #   shinyjs::hide("check2")
        # }

        if (!is.null(input$csvFile) & !is.null(input$fcsFiles)) {
          shinyjs::show("createButton")
          shinyjs::show("checkLinking")
        } else {
          shinyjs::hide("createButton")
          shinyjs::hide("checkLinking")
        }
      })

      observeEvent(input$createButton, {
        req(input$fcsFiles)
        req(input$csvFile)

        global$fcs <- read.flowSet(input$fcsFiles$datapath)
        sampleNames(global$fcs) <- input$fcsFiles$name

        global$title <- input$projName

        runjs("$('header').css('display', '');")

        updateTabItems(session = parent_session, "tabs",
                       selected = "4_configureProject")

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

      output$table <- renderTable({

          #fl <- as.list(global$fcs@frames)
          #fl <- names(fl)[order(names(fl))]
          fl <- input$fcsFiles$name
          if(length(fl) == nrow(global$metadata)) {
            cbind(fl, global$metadata)
          } else {
            fl
          }

        })

    }
  )
}
