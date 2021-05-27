plotUI <- function(id, title) {
  ns <- NS(id)

  div(id = id,

      #jqui_draggable(
      #jqui_resizable(
        box(
          title = tags$b(title),
          #style = "display: flex;",
          #height = "100%",
          dropdownButton(

            tags$h3("Configure plots"),

            selectInput(inputId = ns("plotType"),
                        label = "Type of Plot",
                        choices = c("Template (1-3 Channels)",
                                    "Multiplot",
                                    "Colour mapping (3 Channels) (WIP)",
                                    "FlowFrame Plot")),

            selectInput(inputId = ns("sampleSelect"),
                        label = "Select Sample",
                        choices = c(1,2)),


            conditionalPanel("input.plotType == 'Template (1-3 Channels)'", ns = ns,
                              multiInput(ns("channelInput"), "Select Channels to plot", choices = c(1,2))
                             ),
            conditionalPanel("input.plotType == 'Multiplot'", ns = ns,
                             selectInput(ns("xChannel"), "X Channel", choices = c(1,2)),
                             selectInput(ns("yChannel"), "Y Channel", choices = c(1,2))
                             ),

            conditionalPanel("input.plotType == 'FlowFrame Plot'", ns = ns,
                             selectInput(ns("xChannelF"), "X Channel", choices = c(1,2)),
                             selectInput(ns("yChannelF"), "Y Channel", choices = c(1,2)),
                             sliderInput(ns("resolution"), label = "Resolution (number of bins on each axis):", min = 5,
                                         max = 500, value = 50)
            ),



            # selectInput(inputId = ns("channel1"),
            #             label = "Channel 1",
            #             choices = c(1,2)),
            #
            # selectInput(inputId = ns("channel2"),
            #             label = "Channel 2",
            #             choices = c(1,2)),



            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",

            tooltip = tooltipOptions(title = "Configure Plot")
          ),
          conditionalPanel("(input.channelInput.length <= 2) || !(input.plotType == 'Template (1-3 Channels)')", ns = ns,
            plotOutput(ns("mainPlot"), height = "600px")

          ),
          conditionalPanel("(input.channelInput.length == 3) && (input.plotType == 'Template (1-3 Channels)')", ns = ns,
                           plotlyOutput(ns("mainPlot2"), height = "600px")

          ),

      )
      #)
      #)
  )

}

updatePlotUI <- function(id, global, visInput) {
  moduleServer(
    id,
    function(input, output, session) {




    }
  )
}
