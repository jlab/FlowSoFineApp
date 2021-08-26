
betadisperTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Beta dispersion"),
    box(title = "Configure:",

        selectInput(ns("pChoose"), label = "Term:", choices = c(1,2)),
        actionBttn(ns("calculate"), "(Re)calculate", style = "simple", color = "primary")
    ),

    box(title = "Output",
        plotOutput(ns("boxplot")),
        verbatimTextOutput(ns("permutest"), placeholder = T))
  )
}

betadisperTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$bd <- NULL

      observeEvent(global$metadata, {
        s <- apply(global$metadata, 2, function(x) length(unique(x)) > 1)
        updateSelectInput(session, "pChoose", choices = colnames(global$metadata[,s]))
      })


      observeEvent(input$calculate, {

        if(!is.null(global$distM)) {

          loc$bd <- betadisper(global$distM, group = global$metadata[, input$pChoose])

        } else {

          sendSweetAlert(
            session = session,
            title = "Warning",
            text = "Calculate distance matrix first",
            type = "warning"
          )

        }


      })

      output$permutest <- renderPrint({

        req(loc$bd)

        permutest(loc$bd, pairwise = T)

      })

      output$boxplot <- renderPlot({

        req(loc$bd)

        boxplot(loc$bd)

      })

    }
  )
}
