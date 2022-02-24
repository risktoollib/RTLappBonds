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
  
  # cc <- sort(unique(gsub(pattern = "[0-9]+",replacement = "",x = RTL::dflong$series))[1:6])
  # cc <- cc[!cc %in% c("BRN","WCW")]
  tagList(
    #shiny::radioButtons(ns("port"),"Select port",choices = cc, selected = "CL", inline = TRUE)#,
    DT::dataTableOutput(ns("port")),
    tags$br(),
    shiny::textOutput(ns("portMTM")),
    tags$br(),
  )
}
    
#' instruments Server Functions
#'
#' @noRd 
mod_instruments_server <- function(id, r) {
  moduleServer(id,
               function(input, output, session) {
                 
                 Notional <- Maturity <- Coupon <- YTM <- portMTM <- x <- NULL
                 
                 r$port <-  dplyr::tibble(
                   Notional = c(-1e6, 2e6, -1e6),
                   Maturity = c(2, 10, 30),
                   Coupon = c(.05, .03, 0.01),
                   YTM = Coupon)
                 
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
                   paste("The portfolio MTM value is:",scales::dollar(x),".")
                   }
                 )
               })
  
}
    
## To be copied in the UI
# mod_instruments_ui("instruments_ui_1")
    
## To be copied in the server
# mod_instruments_server("instruments_ui_1")
