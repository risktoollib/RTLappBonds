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
    tags$h5(tags$span(style = "color:lime;font-style: italic;font-size:1.0em", "Amend the Shock column for non-parallel changes in YTM (bps)")),
    shiny::dataTableOutput(ns("nonParallel"))
  )
}
    
#' nonparallel Server Functions
#'
#' @noRd 
mod_nonparallel_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    Bond <- Delat <- Gamma <- Shock <- ActualPL <- DeltaGammaPL <- x <- NULL
    Yield <- Price <- PriceMinus <- PricePlus <- Delta <- DeltaPL <- GammaPL <- NULL
    
    output$nonParallel <- shiny::renderDataTable({
      s = 0 # initial ytm
      
      plAttrib <- function(x = r$port, pos = 1) {
        s = 0 # initial ytm
        StepSize = 0.0001
        b <- x$Shock[pos]
        tmp <- dplyr::tibble(
          Bond = pos,
          Yield = round(seq(s - StepSize, s + StepSize, StepSize), 4),
          Price = 0,
          x = tidyr::nest(.data = r$port, data = dplyr::everything())
        ) %>%
          dplyr::mutate(Shock = b,
                        Price = purrr::pmap(.l = list(x = x$data, shockpar = Yield, shockInd = FALSE, output = paste0("b",pos)), .f = portfolioMTM),
                        PriceMinus = purrr::pmap(.l = list(x = x$data, shockpar = Yield - StepSize, shockInd = FALSE, output = paste0("b",pos)), .f = portfolioMTM),
                        PricePlus = purrr::pmap(.l = list(x = x$data, shockpar = Yield + StepSize, shockInd = FALSE, output = paste0("b",pos)), .f = portfolioMTM)
          ) %>%
          dplyr::mutate(Price = purrr::map_dbl(Price, .f = as.numeric),
                       PriceMinus = purrr::map_dbl(PriceMinus, .f = as.numeric),
                       PricePlus = purrr::map_dbl(PricePlus, .f = as.numeric),
                       Delta = (PricePlus - PriceMinus) / (2 * StepSize) / 10000,
                       Gamma = 0.5 * ((PricePlus - 2 * Price + PriceMinus) /  StepSize ^ 2) / 10000 ^ 2) %>% 
          dplyr::filter(Yield == 0) %>% 
          dplyr::select(Bond, Price, Delta, Gamma, Shock) %>% 
          dplyr::mutate(ActualPL = portfolioMTM(x, shockInd = T, output = paste0("b",pos)) - Price,
                        DeltaPL = Delta * Shock,
                        GammaPL = Gamma * (Shock)^2,
                        DeltaGammaPL = DeltaPL + GammaPL,
                        UnexplainedPL = ActualPL - DeltaGammaPL)
        
        return(tmp)
      }
      #browser()
      round(rbind(plAttrib(pos = 1),plAttrib(pos = 2),plAttrib(pos = 3)),4)
      
    }, options = list(dom = 't')
    )
 
  })
}
    
## To be copied in the UI
# mod_nonparallel_ui("nonparallel_ui_1")
    
## To be copied in the server
# mod_nonparallel_server("nonparallel_ui_1")
