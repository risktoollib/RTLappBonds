#' instruments UI Function
#'
#' @description A shiny Module.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a list returning reactiveValues given the choice of port.
#' @noRd 
#' @importFrom shiny NS tagList 
#' @import RTL
#' @import tidyr
#' @import dplyr
#' @importFrom scales dollar

mod_instruments_ui <- function(id){
  ns <- NS(id)

    tagList(
    tags$br(),
    shiny::textOutput(ns("portMTM")),
    tags$br(),
    DT::dataTableOutput(ns("port")),
    tags$br()
  )
    
}
    
#' instruments Server Functions
#'
#' @noRd 
mod_instruments_server <- function(id, r) {
  moduleServer(id,
               function(input, output, session) {
                 
                 Notional <- Maturity <- Coupon <- YTM <- portMTM <- x <- cmt <- NULL
                 cmt <- tidyquant::tq_get(c("DGS2","DGS10","DGS30"), get = "economic.data") %>% 
                   dplyr::filter(date == dplyr::last(date))
                 coupons <- round(cmt$price / 0.25) * .25 / 100
                 
                 r$port <-  dplyr::tibble(
                   Notional = c(500000, 500000, -1e6),
                   Maturity = c(2, 10, 30),
                   Coupon = c(coupons[1],coupons[2],coupons[3]),
                   YTM = c(cmt$price[1]/100, cmt$price[2]/100, cmt$price[3]/100),
                   Shock = c(50,20,20))
                 
                 # shiny::observeEvent(input$stepSize, {
                 #   r$stepsize <- as.numeric(input$stepSize) / 10000
                 # })
                 
                 proxy <- DT::dataTableProxy('port')
                 
                 shiny::observeEvent(input$port_cell_edit, {
                   r$port <- DT::editData(r$port, input$port_cell_edit)
                   
                   r$portMTM <- r$port %>% 
                     dplyr::mutate(MTM = round(mapply(RTL::bond,YTM,Coupon,Maturity)/100*Notional,0)) 
                   })
                 
                 output$port <- DT::renderDataTable({
                   r$port %>% 
                     dplyr::mutate(MTM = round(mapply(RTL::bond,YTM,Coupon,Maturity)/100*Notional,0)) 
                   
                 }, 
                 selection = 'none', editable = TRUE, extensions = 'Responsive', options = list(dom = 't')
                 #selection = 'none', editable = list(target = "column", disable = list(columns = c(5))), extensions = 'Responsive', options = list(dom = 't')
                 )
                 output$portMTM <- shiny::renderText({
                   x <- r$port %>% 
                     dplyr::mutate(MTM = round(mapply(RTL::bond,YTM,Coupon,Maturity)/100*Notional,0))
                   x <- round(sum(x$MTM))
                   paste("The initial portfolio MTM value is:",scales::dollar(x),".")
                   }
                 )
               })
  
}
    
## To be copied in the UI
# mod_instruments_ui("instruments_ui_1")
    
## To be copied in the server
# mod_instruments_server("instruments_ui_1")
