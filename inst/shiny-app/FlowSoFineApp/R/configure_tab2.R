# For creating the FSFTemlate and the distance matrix

configureTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Configure"),
    fluidRow(
      div(class = "col-sm-12 col-md-12 col-lg-6",
          box(title = "Configure the Template", width = "100%",
            multiInput(ns("channels"), "Select Channels:", choices = c("1","2"), options = list(enable_search = T)),
            sliderInput(ns("resolution"), label = "Resolution (number of bins on each axis):", min = 5,
                        max = 200, value = 20),
            selectInput(ns("transFun"), label = "Transformation:",
                        choices = list("log10", "asinh", "no transformation"),
                        selected = 1),
            actionBttn(ns('applyButton'),
                       label = "(Step 1) Build Template",
                       style = "simple",
                       color = "primary"),

          )
      ),
      div(class = "col-sm-12 col-md-12 col-lg-6",
          box(title = "Configure distance Matrix", width = "100%",
                               selectInput(ns("vegMethod"), "Metric:",
                                           choices = c("weighted bray",
                                                      "manhattan",
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
                                                       "mahalanobis"), selected = "weighted bray"
                               ),
              conditionalPanel("input.vegMethod == 'weighted bray'", ns = ns,
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
                                 )#,


              ),

              actionBttn(ns("distanceButton"), "(Step 2) Calculate distance matrix", color = "primary", style = "simple")
          ),
          actionBttn(ns("continueButton"), "Continue to Hub ->", color = "primary", style = "simple")
      )
    )

  )
}

configureTabServer <- function(id, global, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
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



        } else {

          sendSweetAlert(
            session = session,
            title = "!",
            text = "No fcs files loaded or no channels selected",
            type = "error"
          )

        }

      })

      observeEvent(input$distanceButton, {
        req(global$template)
        if(input$vegMethod == "weighted bray") {
          if(nrow(global$template@counts) <= 1000) {
            loc$weightMatrix <- FlowSoFine::weightMatrix(global$template,
                                                         gamma = input$gamma,
                                                         method = input$weightMethod,
                                                         val = eval(parse(text = input$discInput)
                                                                    )
                                                         )
            global$distM <- FlowSoFine::weightedBray(global$template, w = loc$weightMatrix)
            global$distanceString <- paste0("Weighted bray, gamma = ", input$gamma)
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
          global$distanceString <- paste0(input$vegMethod)
        }
      })

      observe({

        shinyjs::hide("continueButton")

        if(!is.null(global$template) & !is.null(global$distM)) {

          shinyjs::show("continueButton")

        }

      })

      observeEvent(input$continueButton, {
        updateTabItems(parent_session, "tabs",
                       selected = "5_hub")
      })

    }
  )
}
