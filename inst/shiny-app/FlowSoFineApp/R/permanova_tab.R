
permanovaTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("PERMANOVA"),
    box(title = "Configure:",
        # selectInput(ns("distanceMetric"), label = "Distance Metric:",
        #             choices = list("weightedBray" = 1, "vegdist" = 2)),
        #
        # conditionalPanel("input.distanceMetric == 2", ns = ns,
        #                  selectInput(ns("vegMethod"), "Method:",
        #                              choices = c("manhattan",
        #                                          "euclidean",
        #                                          "canberra",
        #                                          "clark",
        #                                          "bray",
        #                                          "kulczynski",
        #                                          "jaccard",
        #                                          "gower",
        #                                          "altGower",
        #                                          "morisita",
        #                                          "horn",
        #                                          "mountford",
        #                                          "raup",
        #                                          "binomial",
        #                                          "chao",
        #                                          "cao",
        #                                          "mahalanobis"), selected = "bray"
        #                  )
        # ),
        selectInput(ns("pairwiseChoose"), label = "Select:", choices = c("standard", "pairwise"), selected = "standard"),
        conditionalPanel("input.pairwiseChoose == 'standard'",ns = ns,
          selectInput(ns("byChoose"), label = "Test by:", choices = c("terms", "margin")),
          uiOutput(ns("metaUI"))

        ),
        conditionalPanel("input.pairwiseChoose == 'pairwise'", ns = ns,
                         selectInput(ns("pwChoose"), label = "Term:", choices = c(1,2))
                         ),
        actionBttn(ns("calculate"), "(Re)calculate", style = "simple", color = "primary")
        ),

    box(title = "Output",
        verbatimTextOutput(ns("adonis"), placeholder = T))
  )
}

permanovaTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$adonis <- NULL
      loc$pressed <- F
      #loc$form <- NULL


      observeEvent(global$metadata, {
        s <- apply(global$metadata, 2, function(x) length(unique(x)) > 1)
        updateSelectInput(session, "pwChoose", choices = colnames(global$metadata[,s]))
      })

      observeEvent(input$pwChoose, loc$pressed <- F)

      observeEvent(input$calculate, {
        # if(input$distanceMetric == 1) {
        #   distM <- weightedBray(global$template)#, w = global$wM)
        # } else {
        #   distM <- vegan::vegdist(frequencies(global$template), method = input$vegMethod)
        # }
        if(!is.null(global$distM)) {
          if(input$pairwiseChoose == "standard") {
            distM <- global$distM
            response <- "distM ~"
            terms <- paste(input$dest_order$text, collapse = "+")
            form <- formula(paste(response, terms, collapse = " "))
            #loc$form <- input$dest_order
            loc$adonis <- adonis2(form, data = global$metadata, by = input$byChoose)
          } else {
            loc$pressed <- T
            #distM <- global$distM
            #loc$adonis <- capture.output(pw.adonis2(distM, term = input$pwChoose, data = global$metadata))
          }
        } else {
          sendSweetAlert(
            session = session,
            title = "Warning",
            text = "Need to calculate distance matrix first",
            type = "warning"
          )
        }


      })

      output$adonis <- renderPrint({
        if(input$pairwiseChoose == "standard") {
          loc$adonis
        } else {
          if(loc$pressed) {pw.adonis2(global$distM, term = input$pwChoose, data = global$metadata)}
        }
        #loc$form
      })

      observe(
        if(!is.null(global$template)) {
          s <- apply(global$metadata, 2, function(x) length(unique(x)) > 1)

          # m <- sapply(1:ncol(global$metadata), function(x) {
          #   if(length(unique(global$metadata [,x])) > 1) {
          #     return(x)
          #   }
          #   return(0)
          #   }) #only metadata groups with more than one unique level

          names <- colnames(global$metadata[,s])

          output$metaUI <- renderUI({
            ns = session$ns
            div(
              orderInput(ns("source"), "Metadata groups:", items = names,
                         as_source = F, connect = "permanovaTab-dest"),
              orderInput(ns("dest"), "To be analyzed:", items = NULL, placeholder = "Drag metadata groups here...",
                         connect = "permanovaTab-source")
            )
          })
        }
      )

    }
  )
}
