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
    tags$br(),
    shiny::radioButtons(ns("stepSize"), "Step size in basis points:", choices = c("1", "5", "10", "25", "50"), selected = "1", inline = TRUE)
  )
    
}
    
#' instruments Server Functions
#'
#' @noRd 
mod_instruments_server <- function(id, r) {
  moduleServer(id,
               function(input, output, session) {
                 
                 Notional <- Maturity <- Coupon <- YTM <- portMTM <- x <- cmt <- NULL
                 port <- Yield <- PricePlus <- PriceMinus <- Price <- DeltaPL <- GammaPL <- x <- PriceLocal <- DeltaLocal <- GammaLocal <- ActualPL <- Notional <- UnexplainedPL <- NULL
                 
                 cmt <- tidyquant::tq_get(c("DGS2","DGS10","DGS30"), get = "economic.data") %>% 
                   dplyr::filter(date == dplyr::last(date))
                 coupons <- round(cmt$price / 0.05) * .05 / 100
                 
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

                # sensitivities cpp -------------------------------------------------------
                 shiny::observeEvent(c(input$stepSize, input$port), {
                   r$stepSize <- as.numeric(input$stepSize) / 10000
                   for (i in 1:3) {
                     x <-
                       dplyr::tibble(
                         Notional = r$port$Notional[i],
                         Maturity = r$port$Maturity[i],
                         Coupon = r$port$Coupon[i],
                         YTM = r$port$YTM[i],
                         Shock = r$port$Shock[i],
                         Yield = round(seq(-0.03, 0.03, r$stepSize), 4), # ytm shock from current level
                         Duration = 0,
                         Price = 0,
                         PriceMinus = 0,
                         PricePlus = 0,
                         Delta = 0,
                         Gamma = 0,
                         DeltaApprox = 0,
                         ActualPL = 0,
                         DeltaPL = 0,
                         GammaPL = 0,
                         DeltaGammaPL = 0,
                         UnexplainedPL = 0
                       )
                     if (i == 1) {sens <- x} else {
                       sens <- rbind(sens,x)
                     }
                   }
                   
                   #Rcpp::sourceCpp("./src/rcppPortParallel.cpp")
                   r$sens <- rcppPortParallel(x = base::as.matrix(sens), stepSize = r$stepSize) %>% 
                     dplyr::as_tibble() 
                   
                   local <- dplyr::filter(r$sens,Yield == 0)
                   
                   r$sens <- r$sens %>%
                     dplyr::mutate(
                       PriceLocal = dplyr::case_when(
                         (Notional == r$port$Notional[1] & Maturity == r$port$Maturity[1]) ~ local$Price[1],
                         (Notional == r$port$Notional[2] & Maturity == r$port$Maturity[2]) ~ local$Price[2],
                         (Notional == r$port$Notional[3] & Maturity == r$port$Maturity[3]) ~ local$Price[3],
                         TRUE ~ 0
                       ),
                       DeltaLocal = dplyr::case_when(
                         (Notional == r$port$Notional[1] & Maturity == r$port$Maturity[1]) ~ local$Delta[1],
                         (Notional == r$port$Notional[2] & Maturity == r$port$Maturity[2]) ~ local$Delta[2],
                         (Notional == r$port$Notional[3] & Maturity == r$port$Maturity[3]) ~ local$Delta[3],
                         TRUE ~ 0
                       ),
                       GammaLocal = dplyr::case_when(
                         (Notional == r$port$Notional[1] & Maturity == r$port$Maturity[1]) ~ local$Gamma[1],
                         (Notional == r$port$Notional[2] & Maturity == r$port$Maturity[2]) ~ local$Gamma[2],
                         (Notional == r$port$Notional[3] & Maturity == r$port$Maturity[3]) ~ local$Gamma[3],
                         TRUE ~ 0
                       ),
                       DeltaApprox = PriceLocal + DeltaLocal * (Yield / r$stepSize),
                       ActualPL = Price - PriceLocal,
                       DeltaPL = DeltaLocal * (Yield / r$stepSize),
                       GammaPL = GammaLocal * (Yield)^2,
                       DeltaGammaPL = DeltaPL + GammaPL,
                       UnexplainedPL = Price - (PriceLocal + DeltaPL + GammaPL)
                     )
                  }
                 )
               })
  
}
    
## To be copied in the UI
# mod_instruments_ui("instruments_ui_1")
    
## To be copied in the server
# mod_instruments_server("instruments_ui_1")
