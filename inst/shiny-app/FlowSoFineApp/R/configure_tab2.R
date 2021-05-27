configureTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Configure"),
    fluidRow(
      div(class = "col-sm-12 col-md-12 col-lg-6",
          box(title = "Configure the Template", width = "100%",
            multiInput(ns("channels"), "Select Channels:", choices = c("1","2"), options = list(enable_search = T)),
            sliderInput(ns("resolution"), label = "Resolution (number of bins on each axis):", min = 5,
                        max = 500, value = 50),
            selectInput(ns("transFun"), label = "Transformation:",
                        choices = list("log10", "asinh", "no transformation"),
                        selected = 1),
            actionBttn(ns('applyButton'),
                       label = "(Step 1) Build Template",
                       style = "simple",
                       color = "primary"),
            # actionBttn(ns('checkButton'),
            #            label = "Check for optimal resolution",
            #            style = "simple",
            #            color = "primary"),
            #selectInput(ns("ct"), label = "Template to analyze:", choices = NULL),
            #plotOutput(ns("checkPlot"), height = "600px")
          )
      ),
      div(class = "col-sm-12 col-md-12 col-lg-6",
          box(title = "Configure distance Matrix", width = "100%",
              selectInput(ns("distanceMetric"), label = "Distance Matrix:",
                          choices = list("weightedBray" = 1, "vegdist" = 2)),

              conditionalPanel("input.distanceMetric == 2", ns = ns,
                               selectInput(ns("vegMethod"), "Metric:",
                                           choices = c("manhattan",
                                                       "euclidean",
                                                       "canberra",
                                                       "clark",
                                                       "bray",
                                                       "kulczynski",
                                                       "jaccard",
                                                       "gower",
                                                       "altGower",
                                                       "morisita",
                                                       "horn",
                                                       "mountford",
                                                       "raup",
                                                       "binomial",
                                                       "chao",
                                                       "cao",
                                                       "mahalanobis"), selected = "bray"
                               )
              ),
              conditionalPanel("input.distanceMetric == 1", ns = ns,
                checkboxInput(ns("advanced"), "Advanced Settings"),
                conditionalPanel("input.advanced == true", ns = ns,
                                 selectInput(ns("weightMethod"), label = "Method:",
                                             choices = list(exponential = "exp", discrete = "disc"),
                                             selected = 1)
                ),
                conditionalPanel(condition = "input.weightMethod == 'exp'", ns = ns,
                                 sliderInput(ns("gamma"), label = "gamma:", min = 1, max = 30, value = 8)
                                 ),
                conditionalPanel(condition = "input.weightMethod == 'disc'", ns = ns,
                                 textInput(ns("discInput"), label = "Values/Expression:", value = "c(1, 0.5, 0.5, 0.25)")
                                 ),

                plotOutput(ns("weightPlot"))

              ),

              actionBttn(ns("distanceButton"), "(Step 2) Calculate distance matrix", color = "primary", style = "simple"),



          )
      )
    )

  )
}

configureTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      #loc$dF <- NULL
      #loc$checkPlot <- NULL
      #loc$weightPlot <- NULL
      loc$sdDist <- NULL
      loc$weightMatrix <- NULL


      observeEvent(global$fcs, {
        updateMultiInput(session, "channels", choices = colnames(global$fcs))
      })

      observeEvent(input$applyButton, {
        if(!is.null(global$fcs) & (length(input$channels) > 0)) {

          #get transformation function based on selectinput
          if(input$transFun == "no transformation") {
            trFun = NULL
          } else {
            trFun = get(input$transFun)
          }

          #create max dimensional template
          global$ND <- FSFTemplate(global$fcs,
                                    channels = input$channels,
                                    resolution = input$resolution,
                                    transformation = trFun)

          #save template in project path
          b <- global$ND#reactiveValuesToList(global)
          saveRDS(b, file = paste0(global$projectPath,"/","template.rds"))

        } else {

          sendSweetAlert(
            session = session,
            title = "!",
            text = "No fcs files loaded or no channels selected",
            type = "error"
          )

        }

      })

      # observeEvent(input$checkButton, {
      #
      #   if(!is.null(global$fcs)) {
      #
      #     withProgress(message = 'Calculating...', value = 0, {
      #       loc$sdDist <- sapply(seq(20,200,20), function(x) {
      #         ct <- CoreTemplate(global$fcs,
      #                            channels = input$channels,
      #                            resolution = x)
      #
      #         incProgress(1/10, detail = paste0(x/20,"/10"))
      #
      #         sd(vegdist(frequencies(ct), na.rm = T))
      #       })
      #
      #     })
      #
      #   } else {
      #     sendSweetAlert(
      #       session = session,
      #       title = "!",
      #       text = "No fcs files loaded",
      #       type = "error"
      #     )
      #   }
      #
      # })

      # output$checkPlot <- renderPlot({
      #   if(!is.null(loc$sdDist)) {
      #     ggplot(mapping = aes(x = seq(20,200,20), y = loc$sdDist)) +
      #       geom_line()+
      #       geom_point() +
      #       theme_minimal() +
      #       labs(x = "Resolution", y = "sd of the inter-sample distance")
      #   }
      # })

      observeEvent(input$distanceButton, {
        if(!is.null(global$template)) {
          if(input$distanceMetric == 1) {
            if(nrow(global$template@counts) <= 1000) {
              loc$weightMatrix <- FlowSoFine::weightMatrix(global$template,
                                                           gamma = input$gamma,
                                                           method = input$weightMethod,
                                                           val = eval(parse(text = input$discInput)
                                                                      )
                                                           )
              global$distM <- FlowSoFine::weightedBray(global$template, w = loc$weightMatrix)
              global$distanceString <- paste0("WeightedBray, gamma = ", input$gamma)
            } else {
              sendSweetAlert(
                session = session,
                title = "!",
                text = "WeightedBray with templates containing > 1000 bins is not possible. Decrease the resolution,
                or use vegdist.",
                type = "error"
              )
            }
          } else {
            global$distM <- vegan::vegdist(frequencies(global$template), method = input$vegMethod)
            global$distanceString <- paste0("vegdist, method = ", input$vegMethod)
          }
        }
      })

      output$weightPlot <- renderPlot({

        if((!is.null(loc$weightMatrix)) & (!is.null(global$distM)) & (ncol(global$template@coords) == 2)) {
          middle <- nrow(global$template@counts)/2
          plot(global$template, mapping = aes(fill = loc$weightMatrix[,middle]))
        }

      })




      # observeEvent(global$template, {
      #   loc$templatePlot <- plot(global$template, 1)# +
      #     #geom_sf_text(aes(label = 1:global$hexT@nHex), size = 70/global$hexT@dimen[[2]]) +
      #     #labs(x = global$hexT@xChannel, y = global$hexT@yChannel)
      #   #updateSelectInput(session, "ct", choices = combn(global$ND))
      # })

      # observeEvent(global$ND, {
      #   channels <- colnames(global$ND@coords)
      #   comb <- lapply(1:(length(channels)-1), function(x) combn(channels, x, simplify = F))
      #   comb <- c(comb, list(list(channels)))
      #   comb <- unlist(comb, recursive = F)
      #
      #   names(comb) <- lapply(comb, toString)
      #
      #   updateSelectInput(session, "ct", choices = names(comb))
      # })
      #
      # observeEvent(input$ct, {
      #   if(!is.null(global$ND)) {
      #
      #     channels <- colnames(global$ND@coords)
      #     comb <- lapply(1:(length(channels)-1), function(x) combn(channels, x, simplify = F))
      #     comb <- c(comb, list(list(channels)))
      #     comb <- unlist(comb, recursive = F)
      #
      #     names(comb) <- lapply(comb, toString)
      #
      #     global$template <- shrink(global$ND, comb[[input$ct]])
      #     }
      # })

      # observeEvent({
      #   input$applyButton2
      #   global$template
      # }, {
      #   if(!is.null(global$hexT)) {
      #     global$wM <- weightMatrix(global$hexT, method = input$weightMethod, gamma = input$gamma, val = eval(parse(text = input$discInput)))
      #     loc$weightPlot <- plot(global$hexT, mapping = aes(fill = global$wM[,input$hexVis])) +
      #       geom_sf_text(aes(label = 1:global$hexT@nHex), size = 1326.5/global$hexT@nHex, color = "white") + labs(fill = "Weight", x = global$hexT@xChannel,
      #                                                                                                             y = global$hexT@yChannel)
      #   }
      #
      # })
      #
      # observeEvent(input$checkButton, {
      #
      #   loc$dF <- determineNHex(global$fcs,
      #                             xChannel = input$xChannel2,
      #                             yChannel = input$yChannel2, seqq = seq(5,50,5))
      # })

      # output$templatePlot <- renderPlot({
      #   if(!is.null(global$template)) plot(global$template, 1)
      #   #loc$templatePlot
      # })

      # output$weightPlot <- renderPlot({
      #   loc$weightPlot
      # }, execOnResize = T)
      #
      # output$detPlot <- renderPlot({
      #   if(!is.null(loc$dF)) {
      #     plot(loc$dF$resolution, loc$dF$meanDist, type = "b")
      #   }
      # })

    }
  )
}
