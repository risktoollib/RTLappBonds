#' parallel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parallel_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::tableOutput(ns("mtm"))
  )
}
    
#' parallel Server Functions
#'
#' @noRd 
mod_parallel_server <- function(id, r = port){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$mtm <- shiny::renderTable({
      #shinipsum::random_DT(10, 10, "numeric")
      #iris
      #browser()
      port
    })
 
  })
}
    
## To be copied in the UI
# mod_parallel_ui("parallel_ui_1")
    
## To be copied in the server
# mod_parallel_server("parallel_ui_1")
