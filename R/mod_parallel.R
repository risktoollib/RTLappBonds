#' parallel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session,  Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plot_ly add_trace layout
#' @import dplyr
mod_parallel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #tags$br(),
    tags$h5(tags$span(style = "color:lime;font-style: italic;font-size:1.0em", "Observe the change in Delta (Gamma) move away from current YTM.")),
    plotly::plotlyOutput(ns("chartParallel"), height = "400px"),
    tags$h5(tags$span(style = "color:lime;font-style: italic;font-size:1.0em", "Observe the changes in estimates by changing the step size.")),
    shiny::radioButtons(ns("stepSize"), "Step size in basis points:", choices = c("1", "5", "10", "25", "50"), selected = "5", inline = TRUE),
  )
}
    
#' parallel Server Functions
#'
#' @noRd 
mod_parallel_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    
    ns <- NS(id)
    
    port <- Yield <- PricePlus <- PriceMinus <- Price <- DeltaPL <- GammaPL <- x <- NULL
    
    output$chartParallel <- plotly::renderPlotly({
      pp <- r$port
      s = 0 # initial ytm

      StepSize <- shiny::reactive(as.numeric(input$stepSize) / 10000)
      #browser()

      junk <-
        dplyr::tibble(
          Yield = round(seq(s - 0.03, s + 0.03, StepSize()), 4),
          Price = 0,
          x = tidyr::nest(.data = r$port, data = dplyr::everything())
        ) %>%
        dplyr::mutate(Price = purrr::pmap(.l = list(x = x$data, shockpar = Yield), .f = portfolioMTM),
                      PriceMinus = purrr::pmap(.l = list(x = x$data, shockpar = Yield - StepSize()), .f = portfolioMTM),
                      PricePlus = purrr::pmap(.l = list(x = x$data, shockpar = Yield + StepSize()), .f = portfolioMTM)
                      ) %>%
        plyr::mutate(Price = purrr::map_dbl(Price, .f = as.numeric),
                     PriceMinus = purrr::map_dbl(PriceMinus, .f = as.numeric),
                     PricePlus = purrr::map_dbl(PricePlus, .f = as.numeric),
                     Delta = (PricePlus - PriceMinus) / (2 * StepSize()) / 10000,
                     Gamma = 0.5 * ((PricePlus - 2 * Price + PriceMinus) /  StepSize() ^ 2) / 10000 ^ 2
                     )

      Price.Local <- as.numeric(dplyr::filter(junk,Yield == 0)$Price)
      DeltaLocal <- as.numeric(dplyr::filter(junk,Yield == 0)$Delta)
      GammaLocal <- as.numeric(dplyr::filter(junk,Yield == 0)$Gamma)

      sens <- junk %>%
        dplyr::mutate(
          DeltaApprox = Price.Local + DeltaLocal * (Yield) * 10000,
          ActualPL = Price - Price.Local,
          DeltaPL = DeltaLocal * (Yield) * 10000,
          GammaPL = GammaLocal * ((Yield) * 10000) ^ 2,
          DeltaGammaPL = DeltaPL + GammaPL,
          UnexplainedPL = Price - (Price.Local + DeltaPL + GammaPL)
        )

      sens %>%
        plotly::plot_ly(x = ~Yield, y = ~ActualPL, name = "Actual PL", type = 'scatter', mode = 'lines') %>%
        #plotly::plot_ly(x = ~Yield, y = ~Price, name = "Portfolio Value", type = 'scatter', mode = 'lines') %>%
        #plotly::add_trace(y = ~ActualPL, name = 'Actual PL', mode = 'lines') %>%
        plotly::add_trace(y = ~DeltaPL, name = 'Delta PL', mode = 'lines') %>%
        plotly::add_trace(y = ~GammaPL, name = 'Gamma PL', mode = 'lines') %>%
        plotly::add_trace(y = ~UnexplainedPL, name = 'Unexplained PL', mode = 'lines') %>%
        plotly::layout(title = list(text = "PL Decomposition", x = 0.05),
                       xaxis = list(title = "Changes in YTM from Current Levels",separators = '.,',tickformat = ".2%"),
                       yaxis = list(title = "$",separators = '.,',tickformat = ",$"),
                       legend = list(x = 0.7, y = 0.5))
    })

  })
}
    
## To be copied in the UI
# mod_parallel_ui("parallel_ui_1")
    
## To be copied in the server
# mod_parallel_server("parallel_ui_1")
