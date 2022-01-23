#' nonparallel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_nonparallel_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' nonparallel Server Functions
#'
#' @noRd 
mod_nonparallel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_nonparallel_ui("nonparallel_ui_1")
    
## To be copied in the server
# mod_nonparallel_server("nonparallel_ui_1")
