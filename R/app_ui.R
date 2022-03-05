#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google

#' @noRd
app_ui <- function(request) {
  shiny::fluidPage(
    # if you want to use bootstrap 5 styling
    theme = bslib::bs_theme(version = 5,
                            bg = "#333333", # 626C70
                            fg = "White",
                            primary = "Cyan",
                            heading_font = bslib::font_google("Prompt"),
                            base_font = bslib::font_google("Prompt"),
                            code_font = bslib::font_google("JetBrains Mono"),
                            "progress-bar-bg" = "lime"),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    #<img src="https://i.imgur.com/XqpQZwi.png" width=300 />
    # UI logic
    shiny::fluidRow(shiny::column(8, titlePanel("Understanding IR Risk at Portfolio Level")),
                    shiny::column(4, align = "right",tags$h5(
                      tags$span(style = "color:White;;font-size:0.8em;font-style:italic", "created by pcote@ualberta.ca"),
                      tags$a(href = "https://www.linkedin.com/in/philippe-cote-88b1769/", icon("linkedin", "My Profile", target = "_blank"))
                    ))), 

    tags$ul(
      tags$li(tags$span(style = "color:lime;font-size:1.0em", "Create your own portfolio by modifying its bond positions.")),
      tags$li(tags$span(style = "color:lime;font-size:1.0em", "Initial Yield-To-Maturities are the latest Constant Maturity US Treasury yields from FRED.")),
      tags$li(tags$span(style = "color:lime;font-size:1.0em", "The central difference method is used for numerical sensitivites."))
      ),

        mod_instruments_ui("instruments_ui_1"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel("Parallel Shifts in YTM", mod_parallel_ui("parallel_ui_1")),
      shiny::tabPanel("Non-Parallel Shifts in YTM", mod_nonparallel_ui("nonparallel_ui_1"))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RTLappBonds'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

