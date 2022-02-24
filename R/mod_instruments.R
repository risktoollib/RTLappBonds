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

mod_instruments_ui <- function(id){
  ns <- NS(id)
  
  cc <- sort(unique(gsub(pattern = "[0-9]+",replacement = "",x = RTL::dflong$series))[1:6])
  cc <- cc[!cc %in% c("BRN","WCW")]
  tagList(
    shiny::radioButtons(ns("port"),"Select port",choices = cc, selected = "CL", inline = TRUE)#,
    #DT::DTOutput(ns("portfolio")),
  )
}
    
#' instruments Server Functions
#'
#' @noRd 
mod_instruments_server <- function(id, r){
  moduleServer( id, 
                function(input, output, session){
    shiny::observeEvent(input$port, {
      
      # assigns selected port code
      r$port <- input$port
      
      # avoid no visible bindings for local vars
      dflong <- series <- value <- c1c2 <- fp <- . <- x <- tmp <- NULL
      
      # assigns computed datLong and datWide
      x <- RTL::dfwide %>%
        dplyr::select(date, contains(input$port)) %>%
        tidyr::drop_na()
      #dplyr::filter(grepl(input$port, series)) %>%
      #tidyr::pivot_wider(names_from = series, values_from = value) %>%
      #dplyr::select_if(~ !any(is.na(.)))
      x <- x %>%
        tidyr::pivot_longer(-date, "series", "value") %>%
        dplyr::mutate(value = case_when(
          grepl("HO", series) ~ value * 42,
          grepl("RB", series) ~ value * 42,
          TRUE ~ value
        ))
      if (input$port == "CL") {
        x <- x %>% dplyr::filter(date != "2020-04-20")
      }
      
      r$datLong <- x
        
    })
  })

    # Notional <- Maturity <- Coupon <- YTM <- NULL 
    # 
    # #initialize a blank dataframe
    # r <- shiny::reactiveValues(data = {
    #   dplyr::tibble(Notional = c(-1e6,2e6,-1e6),
    #                 Maturity = c(2,10,30),
    #                 Coupon = c(.05,.03,0.01),
    #                 YTM = Coupon
    #   )
    # })
    # 
    # #when there is any edit to a cell, write that edit to the initial dataframe
    # shiny::observeEvent(input$portfolio_cell_edit, {
    #   #get values
    #   info = input$portfolio_cell_edit
    #   i = as.numeric(info$row)
    #   j = as.numeric(info$col)
    #   k = as.numeric(info$value)
    # })
    # 
    # shiny::observeEvent(input$portfolio, {
    #   r$data <- r$data
    # })
    # 
    # output$portfolio <- DT::renderDT({
    #   DT::datatable(r$data, editable = TRUE, options = list(dom = 't'))
    # })
    
}
    
## To be copied in the UI
# mod_instruments_ui("instruments_ui_1")
    
## To be copied in the server
# mod_instruments_server("instruments_ui_1")
