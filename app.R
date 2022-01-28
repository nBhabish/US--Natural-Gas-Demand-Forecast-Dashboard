
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)
library(modeltime)

library(bslib)
library(thematic)

# Sourcing info_card.R ----

source("00_scripts/info_card.r")


# Theme w/ {bslib} & {thematic} -------------------------------------------

shinyOptions(bootstrapLib = TRUE)
thematic::thematic_shiny(font = "auto")

my_theme <- bslib::bs_theme(
  version    = 4,
  bootswatch = "flatly",
  fg         = "#1C3516",
  bg         = "white",
  base_font  = font_google("Rancho"),
  heading_font = font_google("Rancho"),
  font_scale  = 1,
  primary = "#4285F4",
  secondary = "#4285F4",
  success = "#1c3516",
  danger = "#DB4437",
  warning = "#0F9D58",
  info    = "#F4B400"
)

# Data Cleaning -----------------------------------------------------------

forecasted_data <- read_rds("00_data/forecast_data.rds")

models_renamed <- forecasted_data %>%
  mutate(
    .model_desc = case_when(
      .model_desc == "ARIMA(2,1,0)(4,1,1)[12]" ~ "ARIMA",
      .model_desc == "ETS(A,A,A)" ~ "ETS",
      .model_desc == "SEASONAL DECOMP: ETS(A,N,N)" ~ "ETS: SEASONAL DECOMPOSITION",
      .model_desc == "UPDATE: TBATS(0.714, {0,0}, -, {<12,5>, <24,1>, <36,1>})" ~ "TBATS",
      .model_desc == "SEASONAL DECOMP: ARIMA(4,1,1) WITH DRIFT" ~ "ARIMA: SEASONAL DECOMPOSITION",
      .model_desc == "UPDATE: REGRESSION WITH ARIMA(0,0,1)(0,1,1)[12] ERRORS" ~ "AUTO ARIMA",
      .model_desc == "PROPHET" ~ "PROPHET",
      .model_desc == "ACTUAL" ~ "ACTUAL"
    )
  )

residual_in_sample_acf <-
  read_rds("00_data/residuals_in_sample_tbl.rds") %>%
  mutate(
    .model_desc = case_when(
      .model_desc == "ARIMA(2,1,0)(4,1,1)[12]" ~ "ARIMA",
      .model_desc == "ETS(A,A,A)" ~ "ETS",
      .model_desc == "SEASONAL DECOMP: ETS(A,N,N)" ~ "ETS: SEASONAL DECOMPOSITION",
      .model_desc == "TBATS(0.282, {2,1}, -, {<12,5>, <24,1>, <36,1>})" ~ "TBATS",
      .model_desc == "SEASONAL DECOMP: ARIMA(4,1,1) WITH DRIFT" ~ "ARIMA: SEASONAL DECOMPOSITION",
      .model_desc == "REGRESSION WITH ARIMA(1,1,1)(2,1,1)[12] ERRORS" ~ "AUTO ARIMA",
      .model_desc == "PROPHET" ~ "PROPHET",
      .model_desc == "ACTUAL" ~ "ACTUAL"
    )
  )

metrics <- readRDS("00_data/metrics.rds")

residual_out_of_sample_acf <-
  read_rds("00_data/residuals_out_of_sample_tbl.rds") %>%
  mutate(
    .model_desc = case_when(
      .model_desc == "ARIMA(2,1,0)(4,1,1)[12]" ~ "ARIMA",
      .model_desc == "ETS(A,A,A)" ~ "ETS",
      .model_desc == "SEASONAL DECOMP: ETS(A,N,N)" ~ "ETS: SEASONAL DECOMPOSITION",
      .model_desc == "TBATS(0.282, {2,1}, -, {<12,5>, <24,1>, <36,1>})" ~ "TBATS",
      .model_desc == "SEASONAL DECOMP: ARIMA(4,1,1) WITH DRIFT" ~ "ARIMA: SEASONAL DECOMPOSITION",
      .model_desc == "REGRESSION WITH ARIMA(1,1,1)(2,1,1)[12] ERRORS" ~ "AUTO ARIMA",
      .model_desc == "PROPHET" ~ "PROPHET",
      .model_desc == "ACTUAL" ~ "ACTUAL"
    )
  )

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  title = h3("United States Natural Gas Demand Forecast"),
  inverse = FALSE,
  collapsible = TRUE,
  theme = my_theme,
  

# * CSS -------------------------------------------------------------------

  
  
  tags$body(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  shinyjs::useShinyjs(),

# * Tab Panel 1 -----------------------------------------------------------

  
  tabPanel(
    title = h4("Forecast"),
    
    div(
      class = "container",
      div(class = "header text-left",
          h3(HTML("<b>Accuracy Metrics</b>")))
    ),
    
    div(class = "container",
        fluidRow(
          # class = "container",
          id    = "favorite_cards",
          
          column(
            width = 3, 
            info_card(title     = "RMSE", bg_color = "danger", text_color = "white",
                      value     = verbatimTextOutput(outputId = "rmse_metric"), main_icon = "battery-three-quarters")
          ),
          
          
          column(
            width = 3, 
            info_card(title     = "MAE", bg_color = "primary", text_color = "white",
                      value     = verbatimTextOutput(outputId = "mae_metric"), main_icon = "lightbulb")
          ),
          
          
          column(
            width = 3, 
            info_card(title     = "RSQ", bg_color = "warning", text_color = "white",
                      value     = verbatimTextOutput(outputId = "rsq_metric"))
          ),
          
          column(
            width = 3, 
            info_card(title     = "MAPE", bg_color = "info", text_color = "white",
                      value     = verbatimTextOutput(outputId = "mape_metric"), main_icon = "percent")
        )
    ),
    div(class = "container",
        id    = "application_ui",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              div(
                id = "main_input",
                pickerInput(
                  inputId = "models",
                  label   = h3("Models"),
                  choices = c(
                    "ARIMA",
                    "ARIMA: SEASONAL DECOMPOSITION",
                    "AUTO ARIMA",
                    "ETS",
                    "ETS: SEASONAL DECOMPOSITION",
                    "PROPHET",
                    "TBATS"
                  ),
                  multiple = FALSE,
                  selected = "ALL",
                  option   = pickerOptions(
                    actionsBox = FALSE,
                    liveSearch = TRUE,
                    size       = 5
                  )
                )
              ),
              div(
                id = "input_buttons",
                actionButton(
                  inputId = "apply",
                  label = "Apply",
                  icon = icon("play")
                ),
                div(
                  class = "pull-right",
                  actionButton(
                    inputId = "reset",
                    label = "Reset",
                    icon = icon("sync")
                  )
                )
              )
            ),
            
            br(),
            br(),
            
            div(
              class = "well",
              h3("About the Data"),
              id = "lorem_ipsum",
              p(tags$small(
                "The data comes from",code("USgas"), "R package. It provides an overview of demand 
                for natural gas in the US in a time-series format.",
                a(
                  class = "btn btn-primary btn-sm",
                  href = "https://github.com/RamiKrispin/USgas",
                  target = "_blank",
                  "Learn More"
                )
              )),
            )
            
          ),
          column(width = 9,
                 div(
                   class = "panel",
                   div(
                     class = "panel-body",
                     style = "padding: 20px;",
                     plotlyOutput(outputId = "plot_forecast"),
                   )
                 ))
        )),
    br(),
    br(),
    hr()
  )),
  tabPanel(
    

# * TabPanel 2 ------------------------------------------------------------

    
    
    title = h4("Post-Forecast Diagnostics"),
    
    div(class = "container",
        id    = "application_ui",
        fluidRow(
          column(
            width = 3,
            wellPanel(div(
              id = "main_input",
              pickerInput(
                inputId = "models_2",
                label   = h3("Models"),
                choices = c(
                  "ARIMA",
                  "ARIMA: SEASONAL DECOMPOSITION",
                  "AUTO ARIMA",
                  "ETS",
                  "ETS: SEASONAL DECOMPOSITION",
                  "PROPHET",
                  "TBATS"
                ),
                multiple = FALSE,
                selected = "ALL",
                option   = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size       = 5
                )
              ),
              div(
                id = "input_buttons",
                actionButton(
                  inputId = "apply_1",
                  label = "Apply",
                  icon = icon("play")
                ),
                div(
                  class = "pull-right",
                  actionButton(
                    inputId = "reset_1",
                    label = "Reset",
                    icon = icon("sync")
                  )
                ),
                
              )
            )),
            
            # Line breaks ----
            br(),
            br(),
            
            div(
              class = "well",
              h3("About the Data"),
              id = "lorem_ipsum",
              p(tags$small(
                "The data comes from",code("USgas"), "R package. It provides an overview of demand 
                for natural gas in the US in a time-series format.",
                a(
                  class = "btn btn-primary btn-sm",
                  href = "https://github.com/RamiKrispin/USgas",
                  target = "_blank",
                  "Learn More"
                )
              )),
            )
          ),
          column(width = 9,
                 div(
                   class = "panel",
                   div(
                     class = "panel-body",
                     style = "padding: 20px;",
                     tabsetPanel(
                       type = "tabs",
                       
                       tabPanel(title = "In-Sample Residuals Plot",
                                plotlyOutput(outputId = "plotly_residuals_in_sample")),
                       
                       tabPanel(title = "Out-of-Sample Residuals Plot",
                                plotlyOutput(outputId = "plotly_residuals_out_of_sample")),
                       
                       tabPanel(title = "In-Sample Residuals ACF Plot",
                                plotlyOutput(outputId = "plotly_residuals_acf_in_sample")),
                       
                       tabPanel(title = "Out-of-Sample Residuals ACF Plot",
                                plotlyOutput(outputId = "plotly_residuals_acf_out_of_sample"))
                     )
                   )
                   
                 ))
        ))
  ),
  
  div(style = "height:50px;")
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer()
  

# * Server side for Tab Panel 1 -------------------------------------------



    
  observeEvent(eventExpr = input$reset, handlerExpr = {
    updatePickerInput(session = session,
                      inputId = "models",
                      selected = "ARIMA")
    
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply")
    })
    
  })
  
  
  models_renamed_reactive <-
    eventReactive(eventExpr = input$apply, {
      models_renamed %>%
        filter(.model_desc %in% c(input$models, "ACTUAL"))
      
    }, ignoreNULL = FALSE)
  

# * Metrics Reactivity ----------------------------------------------------

  
  
  rmse_metric <- 
    eventReactive(eventExpr = input$apply, {
      metrics %>%
        filter(.model_desc == input$models) %>% 
        select(rmse) %>% 
        mutate(rmse = rmse %>% scales::comma())
      
    }, ignoreNULL = FALSE)
  
  mae_metric <- 
    eventReactive(eventExpr = input$apply, {
      metrics %>%
        filter(.model_desc == input$models) %>% 
        select(mae) %>% 
        mutate(mae = mae %>% scales::comma())
      
    }, ignoreNULL = FALSE)
  
  rsq_metric <- 
    eventReactive(eventExpr = input$apply, {
      metrics %>%
        filter(.model_desc == input$models) %>% 
        select(rsq) %>% 
        round(3)
      
    }, ignoreNULL = FALSE)
  
  mape_metric <- 
    eventReactive(eventExpr = input$apply, {
      metrics %>%
        filter(.model_desc == input$models) %>% 
        select(mape) %>% 
        round(3)
      
    }, ignoreNULL = FALSE)
  

# * Metric OutputId -------------------------------------------------------

  
  
  output$rmse_metric <- renderText({
    rmse_metric()$rmse
  })  
  
  output$mae_metric <- renderText({
    mae_metric()$mae
  })
  
  output$rsq_metric <- renderText({
    rsq_metric()$rsq
  })  
  
  output$mape_metric <- renderText({
    mape_metric()$mape
  })
  
  output$plot_forecast <- renderPlotly({
    models_renamed_reactive() %>%
      plot_modeltime_forecast(.title = "Forecasting United States Monthly Gas Demand Until 2023-02-01")
    
  })
  
  

# * Server side for Tab Panel 2 -------------------------------------------

  
  
  observeEvent(eventExpr = input$reset_1, handlerExpr = {
    updatePickerInput(session = session,
                      inputId = "models_2",
                      selected = "ARIMA")
    
    shinyjs::delay(ms = 100, expr = {
      shinyjs::click(id = "apply_1")
    })
    
  })
  
  residuals_in_sample_acf <-
    eventReactive(
      eventExpr = input$apply_1,
      valueExpr = {
        residual_in_sample_acf %>%
          filter(.model_desc %in% c(input$models_2, "ACTUAL"))
        
      },
      ignoreNULL = FALSE
    )
  
  residuals_out_of_sample_acf <-
    eventReactive(
      eventExpr = input$apply_1,
      valueExpr = {
        residual_out_of_sample_acf %>%
          filter(.model_desc %in% c(input$models_2, "ACTUAL"))
        
      },
      ignoreNULL = FALSE
    )
  

# * Plot OutputId ---------------------------------------------------------

  
  
  output$plotly_residuals_out_of_sample <- renderPlotly({
    residuals_out_of_sample_acf() %>% 
      plot_modeltime_residuals(.title = "")
  })
  
  output$plotly_residuals_in_sample <- renderPlotly({
    residuals_in_sample_acf() %>%
      plot_modeltime_residuals(.title = "")
  })
  
  output$plotly_residuals_acf_in_sample <- renderPlotly({
    residuals_in_sample_acf() %>% 
      plot_modeltime_residuals(.type = "acf",
                               .title = "")
  })
  
  output$plotly_residuals_acf_out_of_sample <- renderPlotly({
    residuals_in_sample_acf() %>% 
      plot_modeltime_residuals(.type = "acf",
                               .title = "")
  })
  
  
}


shinyApp(ui = ui, server = server)