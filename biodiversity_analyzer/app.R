# Biodiversity Info App
# Created by Henrique Oliveira
# January 2023

# Load packages
library(bslib)
library(DT)
library(htmltools)
library(leaflet)
library(plotly)
library(shiny)
library(shinyWidgets)
library(tidyverse)

# Load app functions and vectors
source("app_functions_and_vectors.R")

# Read data
df <- read_csv("www/bio_poland.csv")

# Data pre-processing
df <- refine_df(df)
df_id_and_names <- create_df_id_names(df)
name_choices <- create_name_choices(df)

#### Define UI ####
ui <- fluidPage(

  # Set Bootstrap theme
  theme = bs_theme(bootswatch = "cerulean"),

  # CSS style
  tags$head(tags$style(HTML(
    ".shiny-options-group {
      padding-top: 5px
    }"
  ))),

  # App title
  titlePanel(h3(tags$b("Biodiversity App"),
                tags$small(tags$em(" created by Henrique Oliveira")),
                align = "left"),
             windowTitle = "Biodiversity App"),

  hr(),

  sidebarLayout(

    #### Sidebar Panel ####
    sidebarPanel(
      width = 2,

      sibebar_section("Analysis Period"),

      # Date filter
      datesUI(df),

      hr(),

      sibebar_section("Species Type"),

      # Checkbox filter
      checkboxUI("kingdom", "Kingdom:", kingdom_vec),
      checkboxUI("name_checkbox", "Name type:", names_vec),

      # Scpecie name filter and button
      selectInputUI(name_choices),
      
      hr(),
      
      buttonsUI()
    ),

    #### Main Panel ####
    mainPanel(

      width = 10,

      tabsetPanel(id = "tabSet",

        #### Tabs ####
        tabPanel(id = "tab1", "Map Plot",
                 plot_card("Biodiversity Map Plot",
                           leafletPlot("map_plot"))),

        tabPanel(id = "line_plot", "Timeline Line Plot",
                 plot_card("Individual Count Cumulative Sum Plot",
                           plotlyPlot("line_plot"))),

        tabPanel(id = "bar_plot", "Timeline Bar Plot",
                 plot_card("Individual Count Yearly Cumulative Sum Plot",
                           plotlyPlot("bar_plot"))),

        tabPanel(id = "data_table", "Data Table",
                 plot_card("Reactive Dataset Table",
                           dataTable("data_table")))
      )
    )
  )
)

server <- function(input, output, session) {

  #### Reactive Species Name Filter ####
  filter_name <- reactive({
    names <- df %>%
      filter(eventDate <= input$final_date) %>%
      filter(eventDate >= input$init_date) %>%
      filter(kingdom %in% input$kingdom)

    if (is.null(input$name_checkbox)) {
      "All"
    } else if (all(input$name_checkbox %in% "Scientific")) {
      names %>% pull(scientificName)
    } else if (all(input$name_checkbox %in% "Vernacular")) {
      names %>% pull(vernacularName)
    } else if (all(input$name_checkbox %in% c("Scientific", "Vernacular"))) {
      c(names$scientificName, names$vernacularName)
    }

  })

  # Update value of species name filter
  observe({
    updateSelectizeInput(
      session,
      "name",
      choices = c("All", filter_name()[filter_name() != "NA"]),
      selected = "All",
      server = TRUE
    )
  })

  observeEvent(input$clear, {
    updateSelectizeInput(
      session,
      "name",
      choices = c("All", filter_name()[filter_name() != "NA"]),
      selected = NULL,
      server = TRUE
    )
  })

  #### Reactive Map Plot Data ####
  df_react <- eventReactive(input$submit, {

    # Apply name filter
    df_react_name <- reactive(
    if ("All" %in% input$name) {
      df
    } else {
      ids <- get_ids(df_id_and_names, input$name)
      df %>% filter(id %in% ids)
    })

    # Apply date and kingdom filters
    df_react <- df_react_name() %>%
      filter(eventDate <= input$final_date) %>%
      filter(eventDate >= input$init_date) %>%
      filter(kingdom %in% input$kingdom)

  }, ignoreNULL = FALSE)

  #### Reactive Line Plot  Data ####
  df_line_react <- eventReactive(input$submit, {

    df_line_time_filtered <- reactive(
      df %>%
        filter(eventDate <= input$final_date) %>%
        filter(eventDate >= input$init_date)
    )

    if ("All" %in% input$name) {
      df_line_react <- df_line_time_filtered() %>%
        group_by(eventDate, kingdom) %>%
        summarize(cumulative_sum = sum(individualCount)) %>%
        group_by(kingdom) %>%
        summarize("cumulative sum" = cumsum(cumulative_sum),
                  date = eventDate) %>%
        ungroup()
    } else {

      ids <- get_ids(df_id_and_names, input$name)

      df_line_react <- df_line_time_filtered() %>%
        filter(id %in% ids) %>%
        arrange(eventDate) %>%
        group_by(scientificName) %>%
        summarize("cumulative sum" = cumsum(individualCount),
                  date = eventDate) %>%
        rename("scientific name" = scientificName) %>%
        ungroup()
    }
  }, ignoreNULL = FALSE)

  #### Reactive Bar Plot Data ####
  df_bar_react <- eventReactive(input$submit, {

    df_line_time_filtered <- reactive(
      df %>%
        filter(eventDate <= input$final_date) %>%
        filter(eventDate >= input$init_date)
    )

    # Apply date filter and summarise individual count
    if ("All" %in% input$name) {
      df_bar_react <- df_line_time_filtered() %>%
        mutate(date = factor(str_sub(eventDate, end = -7))) %>%
        group_by(date, kingdom) %>%
        summarise("cumulative sum" = sum(individualCount)) %>%
        ungroup()
    } else {
      ids <- get_ids(df_id_and_names, input$name)

      df_bar_react <- df_line_time_filtered() %>%
        filter(id %in% ids) %>%
        mutate(date = factor(str_sub(eventDate, end = -7))) %>%
        group_by(date, scientificName) %>%
        summarize("cumulative sum" = sum(individualCount)) %>%
        rename("scientific name" = scientificName) %>%
        ungroup()

    }
  }, ignoreNULL = FALSE)

  #### Render Plots and Table ####
  observeEvent(input$submit, {
  leafletPlotServer("map_plot", df_react())
  plotlyPlotServer("line_plot", df_line_react(), input$name, "line")
  plotlyPlotServer("bar_plot", df_bar_react(), input$name, "bar")
  dataTableServer("data_table", df_react())
  }, ignoreNULL = FALSE)
}

# Run app
shinyApp(ui = ui, server = server)
