#' instruments UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r
#' @noRd 
#' @importFrom shiny NS tagList 
mod_instruments_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("portfolio")),
  )
}
    
#' instruments Server Functions
#'
#' @noRd 
mod_instruments_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    Notional <- Maturity <- Coupon <- YTM <- NULL 

    #initialize a blank dataframe
    r <- shiny::reactiveValues(data = {
      dplyr::tibble(Notional = c(-1e6,2e6,-1e6),
                    Maturity = c(2,10,30),
                    Coupon = c(.05,.03,0.01),
                    YTM = Coupon
      )
    })
    
    #when there is any edit to a cell, write that edit to the initial dataframe
    shiny::observeEvent(input$portfolio_cell_edit, {
      #get values
      info = input$portfolio_cell_edit
      i = as.numeric(info$row)
      j = as.numeric(info$col)
      k = as.numeric(info$value)
    })
    
    shiny::observeEvent(input$portfolio, {
      r$data <- r$data
    })

    output$portfolio <- DT::renderDT({
      DT::datatable(r$data, editable = TRUE, options = list(dom = 't'))
    })
    
  })
}
    
## To be copied in the UI
# mod_instruments_ui("instruments_ui_1")
    
## To be copied in the server
# mod_instruments_server("instruments_ui_1")
