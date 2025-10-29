#' extrinsic_value_scanner_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_extrinsic_value_scanner_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      # Slider for expiry
      sliderInput(
        inputId = ns("max_expiry_days"),
        label = "Maximum Days to Expiry",
        min = 15,
        max = 90,
        value = 45,
        step = 1
      ),
      actionButton(
        inputId = ns("scan_button"),
        label = "Run Scan",
        icon = icon("search")
      )
    )
  )
}

#' extrinsic_value_scanner_controls Server Functions
#'
#' @noRd
mod_extrinsic_value_scanner_controls_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Return reactive inputs from this module
    return(list(
      max_expiry_days = reactive(input$max_expiry_days),
      scan_button = reactive(input$scan_button)
    ))
  })
}