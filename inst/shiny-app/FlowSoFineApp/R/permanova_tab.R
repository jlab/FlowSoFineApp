permanovaTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("PERMANOVA"),
    box(title = "Configure:",

        h3("Adonis2 by margins"),

        checkboxGroupButtons(ns("formulaChoose"), label = "Metadata:", choices = c(1,2)),

        actionBttn(ns("calculateAD"), "Calculate", style = "simple", color = "primary"),


        h3("Pairwise Adonis2"),

        selectInput(ns("pwChoose"), label = "Term:", choices = c(1,2)),

        selectInput(ns("adjustChoose"), label = "Adjustment method",
                    choices =  c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),
                    selected = "bonferroni"),

        actionBttn(ns("calculatePW"), "Calculate pairwise", style = "simple", color = "primary"),

        ),

    box(title = "Output",
        verbatimTextOutput(ns("adonis"), placeholder = T),
        verbatimTextOutput(ns("adonisPW"), placeholder = T)
        )
  )
}

permanovaTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$pwAD <- NULL
      loc$AD <- NULL

      observeEvent(global$metadata, {

        s <- apply(global$metadata, 2, \(x) {
          length(unique(x)) > 1 & !anyNA(x)
        }) #only groups with more than one unique entry AND no missing values (no reasonable way to use complete.cases with dynamic formula)

        updateSelectInput(session, "pwChoose", choices = colnames(global$metadata[,s]))
        updateCheckboxGroupButtons(session, "formulaChoose", choices = colnames(global$metadata[,s]))
      })

      observeEvent(input$calculatePW, {
        req(global$distM)

        loc$pwAD <- capture.output(pw.adonis2(global$distM,
                                              term = input$pwChoose,
                                              data = global$metadata,
                                              adjust = input$adjustChoose))

      })

      observeEvent(input$calculateAD, {
        req(global$distM)

        distM <- global$distM
        response <- "distM ~"
        terms <- paste(input$formulaChoose, collapse = "+")
        form <- formula(paste(response, terms, collapse = " "))
        loc$AD <- adonis2(form, data = global$metadata, by = "margin")


      })


      output$adonisPW <- renderPrint({

        req(loc$pwAD)

        cat(loc$pwAD, sep = "\n")

      })

      output$adonis <- renderPrint({

        req(loc$AD)

        loc$AD

      })



    }
  )
}
