# Appsilon App
# Oct 2020
# MAde by Henrique Oliveira

# Load packages
library(lubridate)
library(leaflet)
library(tidyverse)
library(DT)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

# Read dataset
df <- read_csv('www/ShipsAppData.csv')

# Set arrival and departure
df$TYPE <- rep(c('arrival','departure'), nrow(df)/2)

# Set colors for graph 
df$LEGEND <- rep(c('red', 'green'), nrow(df)/2)

# Create table dataframe
dfTable <-
    data.frame(
        'Type' = df$ship_type[seq(2,nrow(df),2)],
        'Name' = df$SHIPNAME[seq(2,nrow(df),2)],
        'Departure' = as.character(df$DATETIME[df$TYPE == 'departure']),
        'Arrival' = as.character(df$DATETIME[df$TYPE == 'arrival']),
        'Time' = df$TIME[!is.na(df$TIME)],
        'Distance' = round(df$DISTANCE[!is.na(df$DISTANCE)], 0)
    )

# Define User Interface
ui <- fluidPage(
    
    # Set shinyapp theme
    theme = shinytheme("cyborg"),
    
    # Set tag style
    tags$head(tags$style(HTML("hr {border-top: 0.5px solid #C0C0C0;}"))),
    
    # First row 
    fluidRow(
        
        # Add image
        column(tags$img(src="HO.gif", height=80, width=110), 
               width = 1,
               offset = 0),
        
        # Add App title
        column(h1(HTML("<em> Marine App </em>")),
               align = 'center',
               width = 7,
               offset = 1)),
    
    # Add horizontal line
    tags$hr(),
    
    # Initiate shinydashboard
    useShinydashboard(),
    
    # Second row 
    fluidRow(
        
        # Values boxes 
        valueBoxOutput("distance", width = 3), 
        valueBoxOutput("time", width = 3),
        valueBoxOutput("departure", width = 3),
        valueBoxOutput("arrival", width = 3)),  

    # Third row 
    fluidRow(
        
        # Plot graph    
        column(leafletOutput("plot", height = 455, width = 1500),
               width = 12)),
    
    # Fourth row
    fluidRow(
        
        # Add space
        br(),
        
        # Add ship type filter
        column(selectInput("type",
                       "Select Ship Type:",
                       unique(df$ship_type)),
           width =2),
    
    
        # Add ship name filter
        column(selectInput("name",
                       "Select Ship Name:",
                       NA),
           width = 2)),
    
    # Add horizontal line
    tags$hr(),
    
    # Fifth row
    fluidRow(
        
        # Create table
        column(DTOutput("table"),
                  width = 12))
    
    ) # Close UI

# Define server 
server <- function(input, output, session) {
    
    # Create reactive graph data
    dataInput <- reactive({ if (input$name == 'All'){
                            dataInput <- df %>%
                            filter(ship_type == input$type)
                            } else {
                            dataInput <- df %>%
                            filter(ship_type == input$type) %>%
                            filter(SHIPNAME == input$name)
    }})
    
    # Create reactive table data
    dataTableInput <- reactive({if (input$name == 'All'){
                            dataInput <- dfTable %>%
                            filter(Type == input$type)
                            } else {
                            dataInput <- dfTable %>%
                            filter(Type == input$type) %>%
                            filter(Name == input$name)
    }})
    
    # Box values output
    output$distance <- renderValueBox({valueBox(formatC(sum(dataTableInput()$Distance),format="f", big.mark = ",", digits=0), 
                                                "Distance in Meters", 
                                                icon = icon("road", 
                                                lib = "glyphicon"),
                                                color = "fuchsia")
    })
    output$time <- renderValueBox({valueBox(formatC(sum(dataTableInput()$Time),format="f", big.mark = ",", digits=0), 
                                                "Time in Seconds", 
                                                icon = icon("time", 
                                                lib = "glyphicon"),
                                                color = "fuchsia")
    })
    output$departure <- renderValueBox({valueBox(min(date(dataTableInput()$Departure)), 
                                            "Departure Date", 
                                            icon = icon("calendar", 
                                            lib = "glyphicon"),
                                            color = "purple")
    })
    output$arrival <- renderValueBox({valueBox(max(date(dataTableInput()$Arrival)), 
                                                 "Arrival Date", 
                                                 icon = icon("calendar", 
                                                 lib = "glyphicon"),
                                                 color = "purple")
    })
    
    # Make filter reactive
    filterType <- reactive({
                    df %>% 
                    filter(ship_type == input$type) %>%
                    pull(SHIPNAME)})
    observe({updateSelectizeInput(session, "name", choices = c('All',filterType()))})
    
    # Plot graph
    output$plot <- renderLeaflet({
    plot <- leaflet(dataInput()) %>%
            addTiles() %>%
            addCircleMarkers(color = ~ LEGEND,
                             popup  = ~ SHIPNAME) %>%
            addLegend(colors = c('green','red'), 
                      labels = c('Departure','Arrival'))
    
    # Loop to plot line connecting observations
    for (i in unique(dataInput()$SHIPNAME)) {
    plot <- plot %>%
            addPolylines(data = dataInput()[dataInput()$SHIPNAME == i, ],
                         lng = ~ LON,
                         lat = ~ LAT,
                         color = 'grey')}
    
    plot})
    
    # Plot table
    output$table <- renderDataTable(dataTableInput(), rownames= FALSE)
}

# Run the App 
shinyApp(ui = ui, server = server)
