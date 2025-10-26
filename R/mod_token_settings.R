#' Token Settings Module
#'
#' Simple UI for updating Questrade refresh token weekly
#'
#' @name mod_token_settings
#' @import shiny
#' @importFrom logger log_info log_error
NULL

#' Token Settings UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
#' @export
mod_token_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Questrade Token Management"),

    # Status display
    uiOutput(ns("status_display")),

    br(),

    # Token input
    div(
      class = "well",
      h4("Update Refresh Token"),
      p("Get a new token from:",
        tags$a(
          href = "https://login.questrade.com/",
          target = "_blank",
          "Questrade Account Settings → API Access"
        )
      ),
      textInput(
        ns("new_token"),
        "New Refresh Token:",
        value = "",
        width = "100%",
        placeholder = "Paste your new token here..."
      ),
      actionButton(
        ns("save_btn"),
        "Save Token",
        class = "btn-primary btn-lg",
        icon = icon("save")
      )
    ),

    br(),

    # Result message
    uiOutput(ns("result_message"))
  )
}

#' Token Settings Server
#'
#' @param id Module namespace ID
#' @export
mod_token_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Display current token status
    output$status_display <- renderUI({
      token_file <- get_token_file_path()
      cached <- read_token_file()

      if (!is.null(cached)) {
        time_left <- difftime(cached$expires_at, Sys.time(), units = "mins")

        if (time_left > 0) {
          status_msg <- sprintf(
            "Access token valid (expires in %d minutes)",
            round(time_left)
          )
          div(class = "alert alert-success", icon("check-circle"), " ", status_msg)
        } else {
          div(class = "alert alert-warning", icon("exclamation-triangle"),
              " Access token expired (will refresh on next API call)")
        }
      } else {
        div(class = "alert alert-info", icon("info-circle"),
            " No cached token found. Enter a fresh token from Questrade.")
      }
    })

    # Handle save button
    observeEvent(input$save_btn, {
      new_token <- trimws(input$new_token)

      # Validate token is not empty
      if (new_token == "") {
        output$result_message <- renderUI({
          div(class = "alert alert-danger",
              icon("times-circle"),
              " Error: Token cannot be empty")
        })
        return()
      }

      # Update .Renviron file
      tryCatch({
        renviron_path <- file.path(getwd(), ".Renviron")

        # Read existing .Renviron
        if (file.exists(renviron_path)) {
          lines <- readLines(renviron_path, warn = FALSE)
        } else {
          lines <- character(0)
        }

        # Remove old token line
        lines <- lines[!grepl("^QUESTRADE_REFRESH_TOKEN=", lines)]

        # Add new token
        lines <- c(lines, paste0("QUESTRADE_REFRESH_TOKEN=", new_token))

        # Write back
        writeLines(lines, renviron_path)

        # Delete cached token file to force refresh
        token_file <- get_token_file_path()
        if (file.exists(token_file)) {
          file.remove(token_file)
          log_info("Token Settings: Deleted cached token file")
        }

        log_info("Token Settings: Updated refresh token in .Renviron")

        # Clear input
        updateTextInput(session, "new_token", value = "")

        # Show success message
        output$result_message <- renderUI({
          div(
            class = "alert alert-success",
            icon("check-circle"),
            " Success! Token updated. The app will use the new token on the next API call."
          )
        })

      }, error = function(e) {
        log_error("Token Settings: Failed to update token - {e$message}")

        output$result_message <- renderUI({
          div(
            class = "alert alert-danger",
            icon("times-circle"),
            sprintf(" Error: Failed to save token - %s", e$message)
          )
        })
      })
    })
  })
}
