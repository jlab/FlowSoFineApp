nmdsTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("NMDS"),
    box(title = "Configure",

        actionBttn(ns("calculate"), "(Re)calculate", style = "simple", color = "primary"),

        selectInput(ns("metadataGroup"), label = "Metadata:", choices = NULL)

        ),

    box(title = "NMDS Plot",
        plotOutput(ns("nmdsPlot"), height = "600px")
        )


  )
}

nmdsTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      plots <- reactiveValues()
      plots$nmd <- NULL

      observeEvent(global$template, {
        if(!is.null(global$template)) {
          s <- apply(global$metadata,2, function(x) length(unique(x)) > 1)
          updateSelectInput(session, "metadataGroup",
                            choices = colnames(global$metadata[,s]))
        }
      })

      observeEvent(input$calculate, {

        if(!is.null(global$distM)) {
          plots$nmd <- metaMDS(global$distM)
        } else {
          sendSweetAlert(
            session = session,
            title = "Warning",
            text = "Need to calculate distance matrix first",
            type = "warning"
          )
        }



      })

      output$nmdsPlot <- renderPlot({
        if (!is.null(plots$nmd)) {

          grp <- global$metadata[,input$metadataGroup]
          col <- hcl.colors(length(unique(grp)), "Fall")
          col_p <- col[as.factor(grp)]

          fig <- ordiplot(plots$nmd, display = "sites")
          points(fig, "sites",bg = col_p, pch = 21)
          ordispider(plots$nmd, groups = grp)
          ordiellipse(plots$nmd, groups = grp, label = T, draw = "polygon", col = col)
        }
      })

    }
  )
}
