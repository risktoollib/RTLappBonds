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
    plotly::plotlyOutput(ns("chartParallel"), height = "600px"),
  )
}
    
#' parallel Server Functions
#'
#' @noRd 
mod_parallel_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    port <- Yield <- PricePlus <- PriceMinus <- Price <- DeltaPL <- GammaPL <- x <- PriceLocal <- DeltaLocal <- GammaLocal <- ActualPL <- Notional <- UnexplainedPL <- NULL
    
    output$chartParallel <- plotly::renderPlotly({
      r$sens %>% 
        dplyr::group_by(Yield) %>% 
        dplyr::summarise(Yield = max(Yield),
                         ActualPL = sum(ActualPL * Notional/100),
                         DeltaPL = sum(DeltaPL * Notional/100),
                         GammaPL = sum(GammaPL * Notional/100),
                         UnexplainedPL = sum(UnexplainedPL * Notional/100)) %>%
        plotly::plot_ly(x = ~Yield, y = ~ActualPL, name = "Actual PL", type = 'scatter', mode = 'lines', line = list(dash = "solid")) %>%
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
