options(shiny.maxRequestSize = 100*1024^2)

ui <- dashboardPage(
    dashboardHeader(disable = T, title = "FlowSoFine",
                    tags$li(class = "dropdown",
                            actionBttn("homeButton",
                                       icon = icon("home"),
                                       style = "simple",
                                       color = "primary"),
                            ),
                    tags$li(class = "dropdown",
                            # actionBttn("helpButton",
                            #            icon = icon("save"),
                            #            style = "simple",
                            #            color = "primary"
                            #            )
                            downloadBttn("saveDownload",
                                         label = "",
                                         style = "simple",
                                         color = "primary")

                            ),
                    tags$li(class = "dropdown",
                            div(selectInput("ct", label = NULL, choices = NULL, width = "400px"), style = "margin-bottom:-15px;margin-right: 10px")
                    ),
                    tags$li(class = "dropdown",
                                tags$div(HTML(paste0(textOutput("title"))), style = "padding-top: 8px; color: white")

                            )
                    ),

    dashboardSidebar(collapsed = T,
        sidebarMenu(id = "tabs",
            div(menuItem("Projects", tabName = "1_projects"), style = "display: none"),
            div(menuItem("New Project", tabName = "2_newProject"), style = "display: none"),
            menuItem("Hub", tabName = "5_hub", icon = icon("th")),
            menuItem("Configure", tabName = "4_configureProject", icon = icon("cogs")),
            menuItem("Visualize", tabName = "6_visualize", icon = icon("paint-brush")),
            menuItem("t-Scores", tabName = "7_tscores", icon = icon("braille")),
            menuItem("NMDS", tabName = "8_nmds", icon = icon("project-diagram")),
            menuItem("Beta dispersion", tabName = "13_betadisper", icon = icon("spinner")),
            menuItem("PERMANOVA", tabName = "9_permanova", icon = icon("asterisk")),
            menuItem("Details/Export", tabName = "12_detail", icon = icon("search")),
            menuItem("Authors", tabName = "11_impressum", icon = icon("align-left"))


    )),

    dashboardBody(
        tags$head( tags$meta(name = "viewport", content = "width=600")),
        tags$style(
            '.info-box-icon.bg-aqua {background-color: #53a84a !important; }
            .bttn-simple.bttn-primary {background: #53a84a !important; }
            .skin-blue .main-header .navbar {background-color: #53a84a !important; }
            .skin-blue .main-header .logo {background-color: #41853a !important; }
            .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #41853a !important; }
            .skin-blue .sidebar-menu a:hover, .skin-blue .sidebar-menu li.active{
                border-left-color: #41853a !important;
            }
            .irs-bar {
                border-top: 1px solid #41853a !important;
                border-bottom: 1px solid #41853a !important;
                background: #41853a !important;
                }
            .irs-bar-edge {
            	border: 1px solid #41853a !important;
            	background: #41853a !important;
                }
            .irs-single {
                background: #41853a !important;
            }

            .main-header .navbar-custom-menu {
                float: left !important;
                padding: 5px !important;
            }


            #inspectModal .modal-dialog {
                width: auto !important;
            }'),
        useShinyjs(),

        add_busy_spinner(spin = "breeding-rhombus" ,color = "#323232", timeout = 600, position = "top-right",
                         margins = c(70,30)),

        tabItems(
            tabItem("1_projects", projectsTabUI("projTab")),
            tabItem("2_newProject", newProjectTabUI("newProjTab")),
            tabItem("4_configureProject", configureTabUI("configureTab")),
            tabItem("5_hub", hubTabUI("hubTab")),
            tabItem("6_visualize", visualizeTabUI("visualizeTab")),
            tabItem("7_tscores", tscoresTabUI("tscoresTab")),
            tabItem("8_nmds", nmdsTabUI("nmdsTab")),
            tabItem("9_permanova", permanovaTabUI("permanovaTab")),
            tabItem("11_impressum", impressumTabUI("impressumTab")),
            tabItem("12_detail", detailTabUI("detailTab")),
            tabItem("13_betadisper", betadisperTabUI("betadisperTab"))
        ),

    )
)

server <- function(input, output, session) {

    global <- reactiveValues()
    global$fcs <- NULL
    global$metadata <- NULL
    global$template <- NULL
    global$ND <- NULL
    global$distM <- NULL
    global$wM <- list()

    global$title <- "NONE"
    global$distanceString <- "NONE"
    global$templateString <- "NONE"

    global$justLoaded <- FALSE


    observeEvent(input$homeButton, updateTabItems(session, "tabs",
                                                  selected = "5_hub"))

    output$saveDownload <- downloadHandler(
        filename = function() {
            paste0(global$title, "-FlowSoFine-", Sys.Date(), ".RData")
        },
        content = function(con) {

            ND <- global$ND
            title <- global$title
            metadata <- global$metadata
            if(identical(global$ND, global$template)) {
                distM <- global$distM
                distanceString <- global$distanceString
            } else {
                distM <- NULL
                distanceString <- "NONE"

                sendSweetAlert(
                    session = session,
                    title = "Warning",
                    text = "Subset of the max dimensional template is currently active. Distance matrix will not be saved.",
                    type = "warning")

            }

            save(ND,
                 title,
                 metadata,
                 distM,
                 distanceString,
                 file = con)
        }
    )

    output$title <- renderText({
            paste0( "- Title: ",global$title, " - Template: ", global$templateString, " - Distance Matrix: ", global$distanceString)
        })


    observeEvent(global$ND, {

        channels <- colnames(global$ND@coords)
        comb <- lapply(1:(length(channels)-1), function(x) combn(channels, x, simplify = F))
        comb <- c(comb, list(list(channels)))
        comb <- unlist(comb, recursive = F)

        names(comb) <- lapply(comb, toString)

        updateSelectInput(session, "ct", choices = names(comb), selected = tail(names(comb), 1))

    })

    observeEvent({

        global$ND
        global$template

    }, {
        if(global$justLoaded == FALSE) {
            global$distM <- NULL
            global$distanceString <- "NONE"

            sendSweetAlert(
                session = session,
                title = "Warning",
                text = "Remember to (re)calculate the distance Matrix",
                type = "warning"
            )
        } else {
            global$justLoaded <- FALSE
        }

    })

    observeEvent(global$template, {
        global$templateString <- paste0(global$template@resolution, " resolution, ", ncol(global$template@counts), " samples")
    })

    observeEvent({
        input$ct
        global$ND
        }, {
        if(!is.null(global$ND) & !(input$ct == "")) {

            channels <- colnames(global$ND@coords)
            comb <- lapply(1:(length(channels)-1), function(x) combn(channels, x, simplify = F))
            comb <- c(comb, list(list(channels)))
            comb <- unlist(comb, recursive = F)

            names(comb) <- lapply(comb, toString)

            global$template <- shrink(global$ND, comb[[input$ct]])
        }
    })


    projectsTabServer("projTab", global, parent_session = session)
    newProjectTabServer("newProjTab", global, parent_session = session)
    configureTabServer("configureTab", global)
    hubTabServer("hubTab", parent_session = session)
    visualizeTabServer("visualizeTab", global)
    tscoresTabServer("tscoresTab", global)
    nmdsTabServer("nmdsTab", global)
    permanovaTabServer("permanovaTab", global)
    impressumTabServer("impressumTab")
    detailTabServer("detailTab", global)
    betadisperTabServer("betadisperTab", global)
}

shinyApp(ui = ui, server = server)
