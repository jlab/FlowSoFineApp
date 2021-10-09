
tscoresTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("t-Scores"),
    box(title = "Calculate t-Scores:",
        dropdownButton(
        selectInput(ns("metadataGroup"),
                    label = "Metadata:",
                    choices = character(0)
                    ),
        actionBttn(ns("calculate"), "Calculate", style = "simple", color = "primary"),
        br(),
        selectInput(ns("indexG"),
                    label = "Select grouping:",
                    choices = character(0)
                    ),


        checkboxInput(ns("textCheck"), "Add info to the plot"),
        conditionalPanel("input.textCheck == true", ns = ns,
                      numericInput(ns("fontsize"), "Text size:", min = .5, max = 10, value = 3, step = .5)
        ),

        circle = TRUE, status = "success",
        icon = icon("cogs"), width = "300px",

        tooltip = tooltipOptions(title = "Configure Plot")
        ),
        plotOutput(ns("tscoresPlot"), height = "600px")
        ),
    box(title = "t-scores:",
        DT::dataTableOutput(ns("tscoresTable")),
        downloadButton(ns("tscoreDownload"), "Download .csv")
        )
  )
}

tscoresTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$ts <- NULL
      loc$tsPlot <- NULL
      loc$gatesPlot <- NULL
      loc$seed <- NULL


      observeEvent(global$ND, {
        if(!is.null(global$ND)) {
          #only groups with > 1 unique level AND no level with only one sample
          s <- apply(global$metadata,2, function(x) length(unique(x)) > 1 & !any(table(x) == 1))
          updateSelectInput(session, "metadataGroup",
                            choices = colnames(global$metadata[,s]))
          #cat(input$indexG)
        }
      })

      observeEvent({input$indexG
        loc$ts
        input$fontsize}, {
        if(!is.null(loc$ts) & input$indexG %in% colnames(loc$ts)) {

                    loc$tsPlot <- plotTscores(global$template,
                                    loc$ts[,input$indexG, drop = F],
                                    limits = c(min(loc$ts, na.rm = T),max(loc$ts, na.rm = T))
                                    )# +

          #   labs(x = global$hexT@xChannel, y = global$hexT@yChannel)
        }
      })

      observeEvent(input$calculate, {
        loc$ts <- tscores(global$template, global$metadata[,input$metadataGroup])
        updateSelectInput(session, "indexG",
                          choices = colnames(loc$ts)
                          )


      })

      output$tscoresPlot <- renderPlot({

          if(input$textCheck) {

            lab = round(loc$ts[,input$indexG], 1)
            #size = input$fontsize

            isolate(
              loc$tsPlot <- loc$tsPlot + geom_text(aes(label = lab),
                                                    size = input$fontsize)
            )

          }

          loc$tsPlot
      })

      output$tscoresTable <- DT::renderDataTable({
        as.data.frame(loc$ts)
        },
      rownames = T,
      options = list(scrollX = T)
      )

      output$tscoreDownload <- downloadHandler(
        filename = function() {
          paste0(global$title, "-", "TSCORES-", input$metadataGroup , "-", toString(colnames(global$template@coords)), "-", Sys.Date(), ".csv")
        },
        content = function(con) {
          write.csv2(as.data.frame(loc$ts), con)
        }
      )


    }
  )
}
