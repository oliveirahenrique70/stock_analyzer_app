# WORLD ENERGY GENERATION APP
# Data source: https://ourworldindata.org/grapher/electricity-generation

# Packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(bslib)
library(shinyWidgets)

# Read data
df <- read_csv("www/WorldEnergy.csv")

#### UI ####
ui <- fluidPage(
  
  # Set theme and activate shiny dashboards
  theme = bs_theme(bootswatch = "cyborg"),
  useShinydashboard(),

  # Application title
  titlePanel(h1("", align = 'center'),
            windowTitle = "Countries Electricity Generation App"),
  
  # Header
  fluidRow(
    tags$style(HTML("div.row {background-color: #000000;}")),
    tags$style(HTML("img {margin-top: 23px;}")),
    
    # Logo image
    column(tags$img(src= "HO.gif", height=80, width=110), 
           width = 1),
    
    # App title
    column(h1(HTML("Countries Electricity Generation")),
           align = 'center',
           width = 8,
           offset = 1)
  ),
  
  # Intro
  fluidRow(
    tags$style(HTML("div.row {background-color: #000000;}")),
    p(h6("This app performs the data anlysis of the countries with highest electricity generation (in TWh) in the last years",
         align = 'center'),
      h6("Data source: https://ourworldindata.org/grapher/electricity-generation",
         align = 'center'))
  ),
  
  hr(),
  
  #### Sidebar ####
  sidebarLayout(
    sidebarPanel(
      
      tags$head(
        tags$style(
          HTML(".checkbox-inline {margin-bottom: 5px; margin-right: 16px;}")
        ) 
      ),
      
      # Year range filter
      sliderInput(
        "year_range",
        "Years Range:",
        min = 1985,
        max = 2021,
        value = c(1985, 2021)
      ),
      
      fluidRow(
        # Continents filter
        checkboxGroupInput("continents",
                           "Continents:",
                           c("Asia", "Europe", "Africa", "Oceania", "Americas"),
                           selected = c("Asia", "Europe", "Africa", "Oceania", "Americas"),
                           inline = T),
        
        # Submit button
        actionButton(inputId = "submit",
                     class = "btn btn-primary",
                     label = "Submit")
        ),
      
      hr(),
      
      #### Value Boxes ####
      tags$style(".small-box.bg-yellow {height: 103px; background-color: #ADAFAE !important; color: white !important; }"),
      
      valueBoxOutput("total_consumption", width =12),
      valueBoxOutput("max_consumption", width = 12),
      valueBoxOutput("min_consumption", width = 12),
      valueBoxOutput("test", width = 12),
      
      
      width = 3
    ),
    
    #### Plots ####
    mainPanel(
      
      # Countries energy consumption plot card
      card(
        height = 400,
        full_screen = TRUE,
        card_header("Countries Electricity Generation in TWh"),
        card_body_fill(plotlyOutput("world_plot",
                                    height = "550px"))
      ),
      
      # Energy consumption per time plot card
      card(
        height = 400,
        full_screen = TRUE,
        card_header("Top 5 Countries Electricity Generation in TWh"),
        card_body_fill(plotlyOutput("top_countries",
                                    height = "550px"))
      ),
      
      width = 9)
  )
)

# Define server logic
server <- function(input, output, session) {
  
  #bs_themer()
  
  #### Reactive Data ####
  
  # World countries total energy consumption
  world_df <- eventReactive(input$submit,{
    world_df <- df %>%
      filter(YEAR <= input$year_range[2]) %>%
      filter(YEAR >= input$year_range[1]) %>%
      filter(CONTINENT %in% input$continents) %>%
      group_by(COUNTRY, CODE) %>%
      summarise(ENERGY = sum(ENERGY))
  }, ignoreNULL = FALSE)
  
  # Top 5 countries with highest energy consumption
  top_countries <- reactive({
    top_countries <- world_df() %>%
      arrange(desc(ENERGY))
    
    top_countries <- top_countries[1:5, "COUNTRY"] %>%
      unlist()
  })

  df_top_countries <- eventReactive(input$submit,{
    df_top_countries <- df %>%
      filter(YEAR <= input$year_range[2]) %>%
      filter(YEAR >= input$year_range[1]) %>%
      filter(COUNTRY %in% top_countries()) %>%
      group_by(COUNTRY) %>%
      mutate(ENERGY = cumsum(ENERGY))
    
    colnames(df_top_countries)[c(1, 4, 5)] <-
      c("Country", "Year", "Energy")
    
    df_top_countries
  }, ignoreNULL = FALSE)
  
  #### Plots ####
  
  # World energy consumption plot
  output$world_plot <- renderPlotly({
    plot_ly(
      world_df(),
      type = 'choropleth',
      locations = world_df()[["CODE"]],
      z = world_df()[["ENERGY"]],
      text = world_df()[["COUNTRY"]],
      colorscale = "Viridis",
      showlegend = FALSE
    )
  })
  
  # World energy consumption plot
  output$top_countries <- renderPlotly({
    
    plot_df <- df_top_countries()
    plot_df[["Country"]] <- factor(plot_df[["Country"]], levels = top_countries())

    ggplotly(ggplot(plot_df, aes(x = Year, y = Energy, color = Country)) +
      geom_line() +
      theme_minimal() +
      scale_color_brewer(palette = "Set1"))
  })
  
  #### Value Boxes ####
  
  # Total electricity generation
  output$total_consumption <- renderValueBox({
      valueBox(
        formatC(
          sum(world_df()[["ENERGY"]]),
          format = "f",
          big.mark = ",",
          digits = 0
        ),
        "Countries Total Electricity Generation in TWh",
        icon = icon("flash", lib = "glyphicon"),
        color = "yellow"
        )
    })
  
  # Top 5 Electricity generation
  output$max_consumption <- renderValueBox({
      top_consumption <- world_df() %>%
        filter(COUNTRY %in% top_countries())
      
      valueBox(
        formatC(
          sum(top_consumption[["ENERGY"]]),
          format = "f",
          big.mark = ",",
          digits = 0
        ),
        "Top 5 Contries Total Electricity Generation in TWh",
        icon = icon("stats", lib = "glyphicon"),
        color = "blue"
        )
    })
  
  # Total period
  output$min_consumption <- renderValueBox({
      valueBox(
        length(unique(df_top_countries()[["Year"]])),
        "Total Period in Years",
        icon = icon("calendar", lib = "glyphicon"),
        color = "yellow"
      )
    })
  
  # Number of countries
  output$test <- renderValueBox({
      valueBox(
        length(unique(world_df()[["COUNTRY"]])),
        "Total Number of Countries",
        icon = icon("globe", lib = "glyphicon"),
        color = "blue"
      )
    })
  
}

# Run app
shinyApp(ui = ui, server = server)
