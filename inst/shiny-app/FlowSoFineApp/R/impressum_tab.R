# Details about the authors

impressumTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h1("Authors"),
    fluidRow(
      column(4,
        tags$a(
          tags$img(src = "iuf_logo.jpg", style = "box-shadow: 0 .5px 1px 0 rgba(0, 0, 0, 0.2), 0 .75px 2.5px 0 rgba(0, 0, 0, 0.19);",
                   width = "150px", height = "160px"),
          href = "https://www.iuf-duesseldorf.de/en/",
          target = "_blank",
          style="display:block;padding:10px; width: 150px;",
        )
      ),
      column(10,
        tags$p(tags$b("Prof. Dr. Charlotte Esser"), style = "margin-left: 10px; margin-top: 15px"),
        tags$p(tags$b("Jonas Kupschus"), style = "margin-left: 10px"),
        tags$p(tags$b("Dr. Katrin Hochrath"), style = "margin-left: 10px"),
      )
    ),
    fluidRow(
      column(4,
        tags$a(
          tags$img(src = "tud_logo.jpg", style = "box-shadow: 0 .5px 1px 0 rgba(0, 0, 0, 0.2), 0 .75px 2.5px 0 rgba(0, 0, 0, 0.19);",
                   width = "581px", height = "160px"),
          href = "https://www.tu-dortmund.de/",
          target = "_blank",
          style="display:block;padding:10px; width: 581px;"
        )
      ),
      column(10,
        tags$p(tags$b("Prof. Dr. Katja Ickstadt"), style = "margin-left: 10px; margin-top: 15px"),
        tags$p(tags$b("Jonathan Rathjens"), style = "margin-left: 10px; margin-top: 15px"),
        tags$p(tags$b("Carsten Sonntag"), style = "margin-left: 10px; margin-top: 15px")
      )
    ),
    fluidRow(
      column(4,
        tags$a(
          tags$img(src = "logo_jlab.png", style = "box-shadow: 0 .5px 1px 0 rgba(0, 0, 0, 0.2), 0 .75px 2.5px 0 rgba(0, 0, 0, 0.19);",
                   width = "565px", height = "177px"),
          href =  "https://www.uni-giessen.de/fbz/fb08/Inst/algorithm-bioinformatik",
          target = "_blank",
          style="display:block;padding:10px; width: 565px;"
        )
      ),
      column(10,
        tags$p(tags$b("Prof. Dr. Stefan Janssen"), style = "margin-left: 10px; margin-top: 15px"),
        tags$p(tags$b("Andreas Hoek"), style = "margin-left: 10px; margin-top: 15px")
      )
    )
  )
}

impressumTabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


    }
  )
}
