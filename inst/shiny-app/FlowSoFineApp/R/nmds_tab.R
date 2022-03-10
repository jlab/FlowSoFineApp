# Compute nmds based on the distance matrix

nmdsTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("NMDS"),
    box(title = "Configure",

        actionBttn(ns("calculate"), "(Re)calculate", style = "simple", color = "primary"),

        selectInput(ns("metadataGroup"), label = "Metadata:", choices = NULL)

        ),

    box(title = "NMDS Plot",
        plotOutput(ns("nmdsPlot"), height = "600px"),
        verbatimTextOutput(ns("nmdsOut"), placeholder = T)
        )


  )
}

nmdsTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$nmd <- NULL
      loc$out <- NULL

      observeEvent(global$template, {
          req(global$template)
          s <- apply(global$metadata,2, function(x) length(unique(x)) > 1)
          updateSelectInput(session, "metadataGroup",
                            choices = colnames(global$metadata[,s]))
      })

      observeEvent(input$calculate, {

        if(!is.null(global$distM)) {

          loc$out <- capture.output(loc$nmd <- metaMDS(global$distM))

        } else {

          sendSweetAlert(
            session = session,
            title = "Warning",
            text = "Calculate distance matrix first",
            type = "warning"
          )

        }



      })

      output$nmdsPlot <- renderPlot({
        req(loc$nmd)

        grp <- global$metadata[,input$metadataGroup]
        col <- hcl.colors(length(unique(grp)), "Fall")
        col_p <- col[as.factor(grp)]

        fig <- ordiplot(loc$nmd, display = "sites")
        points(fig, "sites",bg = col_p, pch = 21)
        ordispider(loc$nmd, groups = grp)
        ordiellipse(loc$nmd, groups = grp, label = T, draw = "polygon", col = col)
      })

      output$nmdsOut <- renderPrint({
        req(loc$out)

        loc$out

      })

    }
  )
}
