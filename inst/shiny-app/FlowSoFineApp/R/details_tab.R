
detailTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    box(title = "Metadata:",
        # shinyFilesButton(ns('reloadFCS'),
        #                  label='Reload fcs files',
        #                  title='Select fcs files',
        #                  multiple=T),

        # shinyFilesButton(ns('reloadCSV'),
        #                  label='Reload metadata',
        #                  title='Please select a csv file',
        #                  multiple=F),
        # shinyFilesButton(ns('reloadDist'),
        #                  label='Overwrite distance matrix',
        #                  title='Please select a csv file',
        #                  multiple=F),
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

    ##add export buttons
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
          write.csv2(global$template@counts, con)
        }
      )

      output$coordsDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "COORDINATES-", toString(colnames(global$template@coords)), "-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv2(global$template@coords, con)
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

      # shinyFileChoose(input, "reloadFCS",
      #                 roots = getVolumes(), filetype = "fcs")

      # shinyFileChoose(input, "reloadCSV",
      #                 roots = getVolumes(), filetype = "csv")

      # shinyFileChoose(input, "reloadDist",
      #                 roots = getVolumes(), filetype = "csv")

      # observeEvent(input$reloadFCS, {
      #   files <- parseFilePaths(roots = getVolumes(), input$reloadFCS)
      #   if(nrow(files)) {
      #     global$fcs <- read.flowSet(files$datapath)
      #
      #     sendSweetAlert(
      #       session = session,
      #       title = "Warning",
      #       text = "You need to manually recreate your template for this to take effect",
      #       type = "warning"
      #     )
      #
      #   }
      # })

      observeEvent(input$fcsFiles, {
        req(input$fcsFiles)

        global$fcs <- read.flowSet(input$fcsFiles$datapath)
        sampleNames(global$fcs) <- input$fcsFiles$name

      })

      observeEvent(input$csvFile, {

        global$metadata <- read.table(input$csvFile$datapath, sep = ";", header = T)

      })

      observeEvent(input$distFile, {

        global$distM <- as.dist(read.table(input$distFile$datapath, sep = ";", header = T, row.names = 1, dec = ","))
        global$distanceString <- "OVERWRITTEN"

      })

      # observeEvent(input$reloadDist, {
      #
      #   files <- parseFilePaths(roots = getVolumes(), input$reloadDist)
      #   if(nrow(files)) {
      #     global$distM <- as.dist(read.table(files$datapath, sep = ";", header = T, row.names = 1, dec = ","))
      #     print(typeof(global$distM))
      #     #global$distM <- as.dist(global$distM)
      #     global$distanceString <- "OVERWRITTEN"
      #   }
      #
      # })

      # observeEvent(input$reloadCSV, {
      #
      #   files <- parseFilePaths(roots = getVolumes(), input$reloadCSV)
      #   if(nrow(files)) {
      #     #global$metadataPath <- files$datapath
      #     global$metadata <- read.table(files$datapath, sep = ";", header = T)
      #   }
      #
      # })



    }
  )
}
