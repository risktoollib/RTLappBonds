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
    tags$h5(tags$span(style = "color:lime;font-style: italic;font-size:1.0em", "Understanding the change in Delta (Gamma) as the YTMs changes")),
    tags$ul(
      tags$li("Create a portfolios with a risk profile in mind."),
      tags$li("Do results confirm your thinking?")
    ),
    tags$br(),
    shiny::radioButtons(ns("stepSize"), "Step size in basis points:", choices = c("1", "5", "10", "25", "50"), selected = "5", inline = TRUE),
    plotly::plotlyOutput(ns("chartParallel"), height = "600px"),
  )
}
    
#' parallel Server Functions
#'
#' @noRd 
mod_parallel_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    port <- Yield <- PricePlus <- PriceMinus <- Price <- DeltaPL <- GammaPL <- x <- NULL
    
    output$chartParallel <- plotly::renderPlotly({
      pp <- r$port
      s = 0 # initial ytm

      StepSize <- shiny::reactive(as.numeric(input$stepSize) / 10000)
      
      browser()
      
      # junk <-
      #   dplyr::tibble(
      #     Yield = round(seq(s - 0.03, s + 0.03, StepSize()), 4),
      #     Price = 0,
      #     x = tidyr::nest(.data = r$port, data = dplyr::everything())
      #   ) %>%
      #   dplyr::mutate(Price = purrr::pmap(.l = list(x = x$data, shockpar = Yield), .f = portfolioMTM),
      #                 PriceMinus = purrr::pmap(.l = list(x = x$data, shockpar = Yield - StepSize()), .f = portfolioMTM),
      #                 PricePlus = purrr::pmap(.l = list(x = x$data, shockpar = Yield + StepSize()), .f = portfolioMTM)
      #                 ) %>%
      #   dplyr::mutate(Price = purrr::map_dbl(Price, .f = as.numeric),
      #                PriceMinus = purrr::map_dbl(PriceMinus, .f = as.numeric),
      #                PricePlus = purrr::map_dbl(PricePlus, .f = as.numeric),
      #                Delta = (PricePlus - PriceMinus) / (2 * StepSize()) / 10000,
      #                Gamma = 0.5 * ((PricePlus - 2 * Price + PriceMinus) /  StepSize() ^ 2) / 10000 ^ 2
      #                )
      # 
      # Price.Local <- as.numeric(dplyr::filter(junk,Yield == 0)$Price)
      # DeltaLocal <- as.numeric(dplyr::filter(junk,Yield == 0)$Delta)
      # GammaLocal <- as.numeric(dplyr::filter(junk,Yield == 0)$Gamma)
      # 
      # sens <- junk %>%
      #   dplyr::mutate(
      #     DeltaApprox = Price.Local + DeltaLocal * (Yield) * 10000,
      #     ActualPL = Price - Price.Local,
      #     DeltaPL = DeltaLocal * (Yield) * 10000,
      #     GammaPL = GammaLocal * ((Yield) * 10000) ^ 2,
      #     DeltaGammaPL = DeltaPL + GammaPL,
      #     UnexplainedPL = Price - (Price.Local + DeltaPL + GammaPL)
      #   )
      

# cpp ---------------------------------------------------------------------

      for (i in 1:3) {
        x <-
          dplyr::tibble(
            Notional = r$port$Notional[i],
            Maturity = r$port$Maturity[i],
            Coupon = r$port$Coupon[i],
            YTM = r$port$YTM[i],
            Shock = r$port$Shock[i],
            Yield = round(seq(s - 0.03, s + 0.03, StepSize()), 4),
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
        
      Rcpp::sourceCpp("./src/rcppPortParallel.cpp")
      sens <- rcppPortParallel(x = base::as.matrix(sens), stepSize = StepSize()) %>% 
        dplyr::as_tibble() 
      
      local <- dplyr::filter(sens,Yield == 0)
    
      sens <- sens %>%
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
          DeltaApprox = PriceLocal + DeltaLocal * (Yield / StepSize()),
          ActualPL = Price - PriceLocal,
          DeltaPL = DeltaLocal * (Yield / StepSize()),
          GammaPL = GammaLocal * ((Yield / StepSize())) ^ 2,
          DeltaGammaPL = DeltaPL + GammaPL,
          UnexplainedPL = Price - (PriceLocal + DeltaPL + GammaPL)
        )
      
      sens %>% 
        dplyr::select(-1:-5) %>% 
        dplyr::group_by(Yield) %>% 
        dplyr::summarise_all(.funs = list(sum))
      
        
      sens %>%
        plotly::plot_ly(x = ~Yield, y = ~ActualPL, name = "Actual PL", type = 'scatter', mode = 'lines', line = list(dash = "solid")) %>%
        #plotly::plot_ly(x = ~Yield, y = ~Price, name = "Portfolio Value", type = 'scatter', mode = 'lines') %>%
        #plotly::add_trace(y = ~ActualPL, name = 'Actual PL', mode = 'lines') %>%
        plotly::add_trace(y = ~DeltaPL, name = 'Delta PL', mode = 'lines', line = list(dash = "dot")) %>%
        plotly::add_trace(y = ~GammaPL, name = 'Gamma PL', mode = 'lines', line = list(dash = "dot")) %>%
        plotly::add_trace(y = ~UnexplainedPL, name = 'Unexplained PL', mode = 'lines', line = list(dash = "dashdot")) %>%
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
