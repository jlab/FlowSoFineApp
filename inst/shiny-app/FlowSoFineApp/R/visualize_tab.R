# several ways to visualize FSFTemplates or raw FCS files

visualizeTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Sample Viewer"),

    box(
      title = tags$b("Plot"),

      dropdownButton(

        tags$h3("Configure plots"),

        selectInput(inputId = ns("plotType"),
                    label = "Type of Plot",
                    choices = c("Template (1-2 Channels)",
                                "Colour mapping (3 Channels)",
                                "Multiplot",
                                "FlowFrame Plot")),


        conditionalPanel("input.plotType == 'Template (1-2 Channels)'", ns = ns,
                         multiInput(ns("channelInput"), "Select Channels to plot", choices = c(1,2)),
                         checkboxInput(ns("textCheck"), "Add info to the plot"),
                         conditionalPanel("input.textCheck == true", ns = ns,
                                          selectInput(ns("textSelect"),
                                                      "Info to add:",
                                                      choices = c("Events/Bin",
                                                                  "Percentage/Bin",
                                                                  "Bin ID")),
                                          numericInput(ns("fontsize"), "Text size:", min = .5, max = 10, value = 3, step = .5)
                                          )
        ),
        conditionalPanel("input.plotType == 'Multiplot'", ns = ns,
                         selectInput(ns("xChannel"), "X Channel", choices = c(1,2)),
                         selectInput(ns("yChannel"), "Y Channel", choices = c(1,2))
        ),

        conditionalPanel("input.plotType == 'Colour mapping (3 Channels)'", ns = ns,
                         selectInput(ns("channel1CM"), "Channel 1", choices = c(1,2)),
                         selectInput(ns("channel2CM"), "Channel 2", choices = c(1,2)),
                         selectInput(ns("channelZ"), "z-Channel", choices = c(1,2))
        ),

        conditionalPanel("input.plotType == 'FlowFrame Plot'", ns = ns,
                         multiInput(ns("channelInputF"), "Select Channels to plot", choices = c(1,2)),
                         selectInput(ns("transFun"), label = "Transformation:",
                                     choices = list("log10", "asinh", "no transformation"),
                                     selected = 1),
                         sliderInput(ns("resolution"), label = "Resolution (number of bins on each axis):", min = 5,
                                     max = 300, value = 50, step = 5)
        ),

        checkboxInput(ns("coordFlipCheck"), "Flip coordinates"),
        checkboxInput(ns("limitCheck"), "Define color scale limits"),
        conditionalPanel("input.limitCheck == true", ns = ns,
                         sliderInput(ns("limitSlider"), label = "Limits", min = 0,
                                     max = 10, value = c(0, 5), step = .25)
        ),


        circle = TRUE, status = "danger",
        icon = icon("cogs"), width = "300px",

        tooltip = tooltipOptions(title = "Configure Plot")
      ),
      plotOutput(ns("mainPlot"), height = "600px")


    ),
    box(
      conditionalPanel("input.plotType != 'Multiplot'",ns = ns, DTOutput(ns("metaTable"))

      ),
      conditionalPanel("input.plotType == 'Multiplot'",ns = ns, DTOutput(ns("metaTableMulti"))

      )

    )

  )
}

visualizeTabServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {

      loc <- reactiveValues()
      loc$sampleOptions <- NULL
      loc$limits <- NULL

      observeEvent({input$limitCheck
        input$limitSlider}, {
        if(input$limitCheck) {
          loc$limits <- input$limitSlider
        } else {
          loc$limits <- NULL
        }
      })

      observeEvent(global$ND, {
        updateMultiInput(session, "channelInput", choices = colnames(global$ND@coords), selected = colnames(global$ND@coords)[1:2])
        updateSelectInput(session, "xChannel", choices = colnames(global$ND@coords))
        updateSelectInput(session, "yChannel", choices = colnames(global$ND@coords))
        updateSelectInput(session, "channel1CM", choices = colnames(global$ND@coords))
        updateSelectInput(session, "channel2CM", choices = colnames(global$ND@coords))
        updateSelectInput(session, "channelZ", choices = colnames(global$ND@coords))
        loc$sampleOptions <- colnames(global$ND@counts)

      })

      observeEvent(global$fcs, {
        updateSelectInput(session, "channelInputF", choices = colnames(global$fcs))
      })

      output$mainPlot <- renderPlot({
        if(!is.null(global$ND)) {

          if(input$plotType == "Template (1-2 Channels)") {

              if(!is.null(input$metaTable_row_last_clicked) & !is.null(input$channelInput)) {

                temp <- shrink(global$ND, c(input$channelInput))

                p <- plot(temp,
                         input$metaTable_row_last_clicked,
                         limits = loc$limits)#input$sampleSelect)


                if(input$textCheck) {

                  if(input$textSelect == "Bin ID") {
                    lab <- 1:nrow(temp@counts)
                  } else if(input$textSelect == "Percentage/Bin"){
                    lab <- frequencies(temp)[input$metaTable_row_last_clicked,]
                    lab <- round(lab, 3)
                  } else if(input$textSelect == "Events/Bin") {
                    lab <- temp@counts[,input$metaTable_row_last_clicked]
                  }

                  p <- p + geom_text(aes(label = lab), size = input$fontsize)
                }

                if(input$coordFlipCheck) {
                  p <- p + coord_flip()
                }

                p
              }

          } else if(input$plotType == "FlowFrame Plot") {

            if(!is.null(global$fcs)) {

                if(input$transFun == "no transformation") {
                  trFun = NULL
                } else {
                  trFun = get(input$transFun)
                }

                p <- plotFF(global$fcs[[input$metaTable_row_last_clicked]],
                            channels = input$channelInputF,
                            transformation = trFun,
                            resolution = input$resolution,
                            limits = loc$limits)

                if(input$coordFlipCheck) {
                  p <- p + coord_flip()
                }

                p


            }

          } else if(input$plotType == "Multiplot") {

            if(input$xChannel != input$yChannel) {
              if(length(input$metaTableMulti_rows_selected) > 0) {
                channels <- c(input$xChannel, input$yChannel)
                t2 <- shrink(global$ND, channels)
                glist <- lapply(input$metaTableMulti_rows_selected, function(x) {
                  plot(t2, x, limits = loc$limits) +
                    theme(legend.position = "none", text = element_text(size = 8))
                })

                grid.arrange(grobs = glist)
              }

            }

          } else if(input$plotType == "Colour mapping (3 Channels)") {

            if(!is.null(input$metaTable_row_last_clicked)) {
              if(length(unique(c(input$channel1CM, input$channel2CM, input$channelZ))) == 3) {
                temp <- shrink(global$ND, c(input$channel1CM, input$channel2CM, input$channelZ))

                colormap(temp,
                         input$metaTable_row_last_clicked,
                         input$channelZ,
                         limits = loc$limits)
              }
            }

          }
        }
      })


      output$metaTable <- renderDT({
        if(!is.null(global$metadata)) global$metadata
      }, rownames = T, options = list(scrollX = T, scrollY = T), selection = "single")

      output$metaTableMulti <- renderDT({
        if(!is.null(global$metadata)) global$metadata
      }, rownames = T, options = list(scrollX = T, scrollY = T), selection = "multiple")



    }
  )
}
