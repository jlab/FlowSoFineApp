# Details about the template and distance matrix. Also for importing/exporting


detailTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(title = "Metadata:",
        dataTableOutput(ns("metadataTable")),
        downloadButton(ns("metadataDownload"), "Download .csv")
        ),
        box(title = "Template:",
        textOutput(ns("hexNum")),
        textOutput(ns("nSamples")),
        textOutput(ns("dimen")),

        tabsetPanel(
          tabPanel("Counts",
                   DT::dataTableOutput(ns("countsTable")),
                   downloadButton(ns("countsDownload"), "Download .csv")),
          tabPanel("Coordinates",
                   DT::dataTableOutput(ns("coordsTable")),
                   downloadButton(ns("coordsDownload"), "Download .csv")),
          tabPanel("Distance Matrix",
                   DT::dataTableOutput(ns("distTable")),
                   downloadButton(ns("distDownload"), "Download .csv"))
        )
        #actionButton(ns("checkMetadata"), "Metadata")
        ),
    fileInput(ns("fcsFiles"), "Reload FCS Files",
              multiple = TRUE,
              accept = c(".fcs")),
    fileInput(ns("csvFile"), "Reload metadata",
              multiple = FALSE,
              accept = c(".csv")),
    fileInput(ns("distFile"), "Overwrite distance matrix",
              multiple = FALSE,
              accept = c(".csv"))

  )
}

detailTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      output$metadataTable <- renderDataTable({
        global$metadata
      },
      rownames = T,
      options = list(scrollX = T)
      )

      output$countsTable <- DT::renderDataTable({
        global$template@counts
      },
      rownames = T,
      options = list(scrollX = T)
      )

      output$coordsTable <- DT::renderDataTable({
        global$template@coords
      },
      rownames = T,
      options = list(scrollX = T)
      )

      output$distTable <- DT::renderDataTable({
        if(!is.null(global$distM)) {
          as.matrix(global$distM)
        }
      },
      rownames = T,
      options = list(scrollX = T)
      )

      output$countsDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "COUNTS-", toString(colnames(global$template@coords)), "-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv(global$template@counts, con)
        }
      )

      output$coordsDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "COORDINATES-", toString(colnames(global$template@coords)), "-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv(global$template@coords, con)
        }
      )

      output$distDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "DISTANCE_MATRIX-", toString(colnames(global$template@coords)), "-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv2(as.matrix(vegdist(frequencies(global$template))), con)
        }
      )

      output$metadataDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "METADATA-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv2(global$metadata, con)
        }
      )

      observeEvent(input$fcsFiles, {
        req(input$fcsFiles)

        global$fcs <- read.flowSet(input$fcsFiles$datapath)
        sampleNames(global$fcs) <- input$fcsFiles$name

      })

      observeEvent(input$csvFile, {

        global$metadata <- read.table(input$csvFile$datapath, sep = ";", header = T)

      })

      observeEvent(input$distFile, {

        global$distM <- as.dist(read.table(input$distFile$datapath, sep = ";", header = T, row.names = 1, dec = "."))
        global$distanceString <- "OVERWRITTEN"

      })

    }
  )
}
