#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Your application server logic
  r <- reactiveValues()
  mod_instruments_server("instruments_ui_1", r = r)
  mod_parallel_server("parallel_ui_1",r = r)
  mod_nonparallel_server("nonparallel_ui_1", r = r)
}
