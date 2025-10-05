#' About Page
#'
#' This function creates the about page for the investR application.
#' Each Brochure page has its own independent Shiny session.
#'
#' @return A brochure page object
#' @import shiny
#' @importFrom brochure page
#' @noRd
page_about <- function() {
  brochure::page(
    href = "/about",
    ui = function(request) {
      tagList(
        fluidPage(
          # Navigation
          div(
            style = "margin-bottom: 20px;",
            tags$a(href = "/", class = "btn btn-default", "< Back to Home")
          ),

          # Page content
          titlePanel("About investR"),

          fluidRow(
            column(12,
              div(class = "well",
                h2("About This Application"),
                p("investR is a production-grade Shiny application built using modern R development practices."),

                h3("Technologies"),
                tags$ul(
                  tags$li(strong("golem"), " - Framework for building robust Shiny applications as R packages"),
                  tags$li(strong("Brochure"), " - Multi-page Shiny application support with independent sessions"),
                  tags$li(strong("Shiny"), " - Web application framework for R")
                ),

                h3("Architecture"),
                p("This application follows a modular architecture where:"),
                tags$ul(
                  tags$li("Each page runs in its own Shiny session"),
                  tags$li("Code is organized as an R package for better maintainability"),
                  tags$li("Configuration is managed through golem-config.yml"),
                  tags$li("External resources (CSS, JS) are properly bundled")
                )
              )
            )
          ),

          fluidRow(
            column(6,
              div(class = "well",
                h3("Version Information"),
                verbatimTextOutput("version_info")
              )
            ),
            column(6,
              div(class = "well",
                h3("System Information"),
                verbatimTextOutput("system_info")
              )
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Server logic for about page

      output$version_info <- renderPrint({
        cat("Package: investR\n")
        cat("Version: 0.0.0.9000\n")
        cat("R Version:", R.version.string, "\n")
      })

      output$system_info <- renderPrint({
        cat("Platform:", R.version$platform, "\n")
        cat("OS:", Sys.info()["sysname"], "\n")
        cat("User:", Sys.info()["user"], "\n")
      })
    }
  )
}