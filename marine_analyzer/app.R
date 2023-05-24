#### Intro ####
# Made by Henrique Oliveira
# May 2023

# t <- lapply(portfolio_stocks$code, get_stock_data, period = "1mo")
# tt = lapply(t, function(x) var(x[["adj_close"]]))
# 
# portfolio_stats <- data.frame(
#   'price' = unlist(lapply(t, function(x) tail(x[["close"]], 1))),
#   'variance' = unlist(lapply(t, function(x) round(var(x[["adj_close"]]), 2)))
# )

# Load R packages
library(fresh)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(bs4Dash)
library(plotly)
library(fontawesome)
library(yahoofinancer)
library(tidyquant)

source("functions.R")

# create a sample data frame
portfolio_stocks <- data.frame(
  code = c("VGIR11", "HGBS11", "BRCO11", "HGRU11"),
  quantity = c(425, 200, 100, 100),
  stringsAsFactors = FALSE
)

stocks_data <- read_csv("www/FI.csv")

# Join data
portfolio_stocks <- portfolio_stocks %>%
  join_portfolio_data(stocks_data)

#### Dashboard Header ####
header <- dashboardHeader(
  status = "secondary",
  tags$li(
    a(
      href = 'https://apps.hodatascience.com.br/',
      img(
        src = 'HO.gif',
        title = "Company Home",
        height = "70px"
      ),
      style = "padding-top:10px; padding-bottom:10px"
    ),
    class = "dropdown"
  ),
  h2("Real Estate Stocks Analyzer",
     style = "padding-left: 300px; font-style: italic; color: white")
)

#### Dashboard Sidebar ####
sidebar <- dashboardSidebar(#expandOnHover = FALSE,
  minified = FALSE,
  
  sidebarMenu(
    menuItem("Intro",
             tabName = "intro",
             icon = icon("cog")),
    menuItem("Stock Plot",
             tabName = "stocks_analysis",
             icon = icon("chart-line")),
    menuItem("Stock Data Table",
             tabName = "stocks_data_table",
             icon = icon("table")),
    menuItem("Set Portfolio",
             tabName = "portfolio",
             icon = icon("list")),
    menuItem("Portfolio Analysis",
             tabName = "portfolio_analysis",
             icon = icon("chart-pie")),
    
    tags$div(style = "height: 100px;"),
    
    h5(tags$strong("Select Stock:"), align = "center", style = "color: #FF50CA;"),
    select_stock("stock_code", stocks_data),
    
    tags$div(style = "height: 150px;"),
    
    created_by_msg(sidebar = TRUE)
  ))

#### Dashboard Body ####
body <- dashboardBody(tabItems(

  #### Intro Tab ####
  tabItem(tabName = "intro",
          fluidPage(
            p(h4("Welcome to the", tags$strong("Real Estate Stocks Analyzer"), " app.")),
            p(h4("This app allows users to analyze brazilian real estate stocks and manage their stocks portfolio. The app integrates various R packages such as ", tags$em("shiny"),", ", tags$em("bs4Dash"), ", ", tags$em("plotly"), ", ", tags$em("DT"), ", ", tags$em("yahoofinancer"), " and others.")),
            p(h4("Interact with the app's sidebar tabs to define what you want to analyze. Also in the sidebar of the app, use the stock selectize filter to select the stock you want to analyze.")),
            p(h4("Visit the website ", pink_words("apps.hodatascience.com.br", link = "https://apps.hodatascience.com.br/") , "for more apps like this, to know more about the app creator or to check out interesting data science reports projects")),
            br(),
            created_by_msg()
          )),

  #### Stocks Analysis Tab####
  tabItem(tabName = "stocks_analysis",
          fluidRow(
            column(
              offset = 1,
              width = 5,
              stock_analysis_period("plot_analysis_period"),
            ),
            column(
              offset = 1,
              width = 5,
              mavg_slider("mavg_period")
            )
          ),
          fluidRow(
            card_UI(
              plotlyOutput("stock_analysis"),
              "Stocks Data Visualization Analysis",
              width = 12,
              height = 550
            )
          )
  ),

  #### Stocks Data Table Tab ####
  tabItem(tabName = "stocks_data_table",
          card_UI(table_UI("stocks_table"),
                  "Stocks Data Table",
                  width = 12,
                  height = NULL)
  ),

  #### Set Portfolio Tab ####
  tabItem(tabName = "portfolio",
          sidebarLayout(
            # Stock inputs
            sidebarPanel(
              width = 3,
              style = "margin-right: 100px;",
              numericInput("new_quantity", "Quantity", value = 0),
              porfolio_buttons(),
            ),
            # Stocks output
            mainPanel(
              card_UI(
                table_UI("portfolio_table"),
                "Stocks Porfolio Data",
                width = 12,
                height = NULL
              )
            )
          )
  ),

  #### Portfolio Analysis Plot Tab ####
  tabItem(tabName = "portfolio_analysis",
          fluidPage(
            fluidRow(
            card_UI(plotlyOutput("portfolio_price"),
                    "Portfolio Stats",
                    width = 6),
            card_UI(plotlyOutput("portfolio_quantity"),
                    "Portfolio Quantity",
                    width = 6)
            )
          ))
))

# Create ui dashboardPage (front-end)
ui <- dashboardPage(freshTheme = bs4_theme() ,
                    dark = NULL,
                    header,
                    sidebar,
                    body)

# Server
server <- function(input, output, session) {

  #### Reactive Data ####

  portfolio_df <- reactiveVal(
    get_price_var(portfolio_stocks))

  stocks_df <- reactive({
    df <- get_stock_data(input$stock_code, "2y")
    df
  })

  # Stock data for plot
  stocks_analysis_df <- reactive({
    init_date <- tail(stocks_df()[["date"]], 1)

    df <- stocks_df() %>%
      filter(date >= init_date - days(input$plot_analysis_period))
    df
  })

  # Stock data for table
  stocks_table_df <- reactive({
    df <- stocks_df() %>%
      arrange(desc(date)) %>%
      mutate(date = as.Date(date)) %>%
      mutate_if(is.numeric, ~ round(., 2))
    df <- cbind(code = input$stock_code, df)
    df
  })

  portfolio_stats_df <- reactive({
    df <- portfolio_df() %>% portfolio_stats()
    df
  })

  #### Add Stock ####
  observeEvent(input$add_stock, {
    if (input$stock_code != "" && input$new_quantity > 0 && input$stock_code %in% portfolio_df()$code == FALSE) {
      new_row <- data.frame(
        code = input$stock_code,
        quantity = input$new_quantity,
        stringsAsFactors = FALSE
      ) %>%
        join_portfolio_data(stocks_data)

      stocks_add <- rbind(portfolio_df(), get_price_var(new_row))
      portfolio_df(stocks_add)
    }
  })

  #### Remove Stock ####
  observeEvent(input$remove_stock, {
    if (input$stock_code != "") {
      stocks_delete <-
        portfolio_df()[portfolio_df()$code != input$stock_code,]
      portfolio_df(stocks_delete)
    }
  })

  #### Update Stock ####
  observeEvent(input$update_stock, {
    if (input$stock_code != "" && input$new_quantity > 0) {
      index <- which(portfolio_df()$code == input$stock_code)
      if (length(index) > 0) {
        stocks_update <- portfolio_df()
        stocks_update$quantity[index] <- input$new_quantity
        portfolio_df(stocks_update)
      }
    }
    
    #print(modifiedData())
  })

  #### Render Table ####
  table_server("portfolio_table",
               portfolio_df,
               reactivity = TRUE,
               paging = FALSE,
               search = FALSE
  )
  table_server("stocks_table",
               stocks_table_df,
               reactivity = TRUE
  )
  table_server("portfolio_stats",
               portfolio_stats_df,
               ordering = FALSE,
               row_names = TRUE,
               reactivity = TRUE,
               search = FALSE,
               paging = FALSE
  )

  #### Render Plots ####
  output$portfolio_price <-
    renderPlotly(bar_plot(portfolio_df()))
  output$portfolio_quantity <-
    renderPlotly(pie_plot(portfolio_df()))
  output$stock_analysis <-
    renderPlotly(stock_plot(stocks_analysis_df(), input$mavg_period, input$stock_code))
}

# Run App
shinyApp(ui, server)
