# create a new project - upload required files

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

        fileInput(ns("fcsFiles"), "Choose FCS Files",
                  multiple = TRUE,
                  accept = c(".fcs")),
      )
    ),
    fluidRow(
      box(title = "Load metadata .csv or .tsv file",

          checkboxInput(ns("csvOptions"), label = "File settings"),

          conditionalPanel("input.csvOptions == true", ns = ns,
            textInput(ns("sepText"), label = "seperator:", value = ";"),
            textInput(ns("decText"), label = "decimal point:", value = ",")
          ),

          fileInput(ns("csvFile"), "Choose metadata file (csv, tsv, txt)",
                    multiple = FALSE,
                    accept = c(".csv", ".tsv", ".txt")),
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

        sepText <- input$sepText
        decText <- input$decText

        if (sepText == "\\t") sepText <- "\t"

        if (any(endsWith(input$csvFile$datapath, c(".csv", ".tsv", ".txt")))) {


          global$metadata <- read.table(input$csvFile$datapath,
                                      sep = sepText,
                                      dec = decText,
                                      header = T)

          global$metadata[global$metadata == ""] <- NA #replace empty strings with NA for nmds plot

        } else {

          shinyjs::reset("csvFile")

          sendSweetAlert(
            session = session,
            title = "!",
            text = "Uploaded file is not a .csv, .tsv or .txt file",
            type = "error"
          )

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

      observeEvent(input$fcsFiles, {
        req(input$fcsFiles)
        if (!all(endsWith(input$fcsFiles$datapath, ".fcs"))) {
          shinyjs::reset("fcsFiles")

          sendSweetAlert(
            session = session,
            title = "!",
            text = "Uploaded files are not .fcs files",
            type = "error"
          )

        }

      })

      observe({

        if (!is.null(global$metadata) & !is.null(input$fcsFiles)) {

          if(nrow(global$metadata) == nrow(input$fcsFiles)) {

            shinyjs::show("createButton")
            shinyjs::show("checkLinking")

          } else {

            sendSweetAlert(
              session = session,
              title = "!",
              text = "Number of rows in the metadata table and number of .fcs files do not match up",
              type = "error"
            )

          }

        } else {
          shinyjs::hide("createButton")
          shinyjs::hide("checkLinking")
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

      output$table <- renderTable({

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
