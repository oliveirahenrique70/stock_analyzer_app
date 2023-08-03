#### INTRO ####

# Load packages
library(bs4Dash)
library(htmltools)
library(leaflet)
library(fresh)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)
library(sf)
library(fontawesome)
library(bslib)
library(shinycssloaders)
library(gt)

# Load app functions and vectors
source("app_functions_and_vectors.R")

# Read data
load("www/br_states.RData")
load("www/br_cities.RData")

df <- read_csv("www/data_2022.csv")
# df_filtered <- df %>%
#   filter(DthAtualizaCadastralEmpreend > "2022-01-01") %>%
#   filter(DthAtualizaCadastralEmpreend < "2023-01-01")
# 
# write.csv(df_filtered, "data_2022.csv", row.names = FALSE)

# # create the theme with a cyberpunk color palette
theme <- bs4_theme()

#### HEADER ####

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
  h2("Brazilian Renewable Energy",
     style = "padding-left: 300px;font: Geolica;; color: white")
)
  #title = "Renewable Energy App")

#### SIDEBAR ####

sidebar <- dashboardSidebar(
  
  includeCSS("www/sidebar_css.txt"),
  
  #expandOnHover = FALSE,
  minified = FALSE,

  sidebarMenu(
    menuItem("Intro",
             tabName = "intro",
             icon = icon("cog")
    ),
    menuItem("Data Visualization",
             tabName = "data_viz",
             icon = icon("globe")
    ),
    menuItem("Data Table",
             tabName = "data_table",
             icon = icon("th")
    )
  ),
    
  hr(),

  sibebar_section("Select State"),
  selectInputUI("state_name",
                c("All", sort(unique(br_states$name)))),
  br(),

  sibebar_section("Set Analysis Period"),
  analysisPeriodUI("analysis_period"),
  br(),

  sibebar_section("Energy Source"),
  checkboxUI("energy_source_checkbox"),
  hr(),

  # sibebar_section("Select Potency"),
  # buttonsUI(),
  # br(),


  submitUI()
)

#### BODY ####

body <- dashboardBody(
  
  #### INTRO TAB ####
  tabItems(
    tabItem(tabName = "intro",
            fluidPage(
              h4("The app is a data visualization tool that provides information on renewable energy sources in Brazil during the year of 2022. It allows users to explore and analyze data related to renewable energy generation across different states and cities in Brazil.",
                 style = "text-align: justify; margin-right: 100px; margin-left: 100px"),
              br(),
              h4("The data source is the official Brazilian government website ", a("gov.br", href="https://dados.gov.br/dados/conjuntos-dados/relacao-de-empreendimentos-de-geracao-distribuida", target = "blanck"), " which contais a description list of renewable generation projects",
                 style = "text-align: justify; margin-right: 100px; margin-left: 100px"),
              br(),
              h4("Enjoy exploring this interesting app and gaining insights into renewable energy generation in Brazil!",
                 style = "text-align: justify; margin-right: 100px; margin-left: 100px"),
              created_by_msg(),
              style = "height:750px"
            )
    ),
    
    #### VISUALIZATION TAB ####
    tabItem(tabName = "data_viz",
            fluidPage(
              bs4Card(
                id = "card_states",
                width = 12,
                #height = "650",
                title = strong("Total Values Box"),
                fluidRow(
                  valueBoxUI("count"),
                  valueBoxUI("potency"),
                  valueBoxUI("micro"),
                  valueBoxUI("mini")
                )
              ),
              
              
              bs4Card(
                id = "card_states",
                width = 12,
                height = "600",
                div(align = "center",
                    h4(strong("Geolocation Graph")),
                    uiOutput("graph_heading"),
                    br()),
                title = NULL,
                leafletPlot("plot"),
                #maximizable = TRUE,
                collapsible = FALSE
              )
            )),
    
    #### TABLE TAB ####
    tabItem(tabName = "data_table",
            fluidPage(
              bs4Card(
                id = "card_states",
                width = 12,
                #height = "650",
                title = strong("Energy Source Values Box"),
                fluidRow(
                  valueBoxUI("water"),
                  valueBoxUI("bio"),
                  valueBoxUI("wind"),
                  valueBoxUI("solar")
                )
              ),
              
              bs4Card(
                id = "card_states",
                width = 12,
                headerBorder = FALSE,
              height = "650",
              title = NULL,
              div(align = "center",
                  h4(strong("Data Table")),
                  uiOutput("table_heading")),
              gtTable("table"),
                maximizable = FALSE,
                collapsible = FALSE
              )
            )
    )
  )
)

#### DASHBOARD UI ####
ui <- dashboardPage(dark = NULL,
                    freshTheme = theme,
                    header,
                    sidebar,
                    body)

# Define server logic (back-end)
server <- function(input, output, session) {
  
  #### Loading Box ####
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Loading",
      div(align = "center",
          img(src = 'loading.gif', height = "70px")),
      br(),
      div("Please wait while the app is loading..."),
      footer = NULL#,
      #size = "small"
    ))
  }, ignoreNULL = FALSE)
  
  #### REACTIVE DATA ####

  #### Filtered Data ####
  df_filtered <- eventReactive(input$submit, {
    if (input$state_name == "All") {
      df %>%
        apply_filters(input)
    } else {
      UF_code <- br_states %>%
        filter(name == input$state_name) %>%
        pull(SigUF)

      df %>%
        apply_filters(input) %>%
        filter(SigUF == UF_code)
    }
  }, ignoreNULL = FALSE)

  #### Mini/Micro Data ####
  df_mini_micro <- eventReactive(input$submit, {
    df_filtered() %>%
      group_by(DscPorte) %>%
      summarise(micro_mini_count = n()) %>%
      ungroup()
  }, ignoreNULL = FALSE)
  
  df_mini_micro_table <- eventReactive(input$submit, {
    if (input$state_name == "All") {
      df_filtered() %>%
      group_by(SigUF, DscPorte) %>%
        summarise(n = n(),
                  potency = sum(MdaPotenciaInstaladaKW)) %>%
        ungroup()
    } else {
      df_filtered() %>%
        group_by(NomMunicipio, DscPorte) %>%
        summarise(n = n(),
                  potency = sum(MdaPotenciaInstaladaKW)) %>%
        mutate(nome = toupper(NomMunicipio)) %>%
        rename(name = NomMunicipio) %>%
        ungroup()
    }
  }, ignoreNULL = FALSE)

  observe({print(df_mini_micro_table())})
  #### Energy Source Data ####
  df_energy_sorce <- eventReactive(input$submit, {
      df_filtered() %>%
        group_by(DscFonteGeracao) %>%
        summarise(total_potency = sum(MdaPotenciaInstaladaKW),
                  n = n()) %>%
        ungroup()
  }, ignoreNULL = FALSE)
  
  #### DF React ####
  df_react <- eventReactive(input$submit, {
    if (input$state_name == "All") {
      df_states_count <- df_filtered() %>%
        group_by(SigUF) %>%
        summarise(n = n(),
                  potency = sum(MdaPotenciaInstaladaKW)) %>%
        filter(SigUF != "")
      
      df_react <- br_states %>%
        left_join(df_states_count, by = "SigUF") %>%
        drop_na() %>%
        add_label()
      
    } else {
      State_code <- br_states %>%
        filter(name == input$state_name) %>%
        pull(State)
      
      df_cities_count <- df_filtered() %>%
        group_by(NomMunicipio) %>%
        summarise(n = n(),
                  potency = sum(MdaPotenciaInstaladaKW)) %>%
        mutate(nome = toupper(NomMunicipio)) %>%
        rename(name = NomMunicipio) %>%
        ungroup()
      
      df_react <- br_cities %>%
        filter(State == State_code) %>%
        left_join(df_cities_count, by = "nome") %>%
        drop_na() %>%
        add_label()
    }
  }, ignoreNULL = FALSE)
  
  #### VALUE BOXES ####

  #### Total Values ####
  valueBoxServer_Total("count", reactive(df_react()), "n", "primary", "lightbulb", "Count")
  valueBoxServer_Total("potency", reactive(df_react()), "potency", "primary", "bolt", "Potency in KW")
  valueBoxServer_MicroMini("micro",reactive(df_mini_micro()), "Microgeracao", "info", "house", "Count Micro", footer_text = "< 75 KW")
  valueBoxServer_MicroMini("mini", reactive(df_mini_micro()), "Minigeracao", "info", "industry", "Count Mini", footer_text = "> 75 KW")

  # #### Energy Source ####
  valueBoxServer_energySource("water", reactive(df_energy_sorce()), "Hydraulic", "sub_test", "primary", "water", "Hydro")
  valueBoxServer_energySource("bio", reactive(df_energy_sorce()), "Biogas", "sub_test", "danger", "fire", "Biogas")
  valueBoxServer_energySource("wind", reactive(df_energy_sorce()),"Wind", "sub_test", "success", "wind", "Eolic")
  valueBoxServer_energySource("solar", reactive(df_energy_sorce()), "Solar", "sub_test", "warning", "sun", "Solar")

  #### UPDATE FILTERS ####
  observeEvent(df_react(), {
    states_choices <- df %>%
      apply_filters(input) %>%
      left_join(br_states %>% select(SigUF, name), by = "SigUF")

    updateSelectizeInput(session,
                         "state_name",
                         choices = c("All", sort(unique(states_choices$name)[!is.na(unique(states_choices$name))])),
                         selected = input$state_name)
  })

  
  #### RENDER ####
  observeEvent(input$submit, {
    stateName <- eventReactive(input$submit, {
      input$state_name
    }, ignoreNULL = FALSE)
    
    output$graph_heading <- renderUI({
      req(stateName())
      h6(em(str_c(stateName(), " State Data")), style = "color: gray;")
    })
    output$table_heading <- renderUI({
      req(stateName())
      h6(em(str_c(stateName(), " State Data")), style = "color: gray;")
    })
    
    leafletPlotServer("plot", df_react())
    gtTableServer("table", df_react(), df_mini_micro_table(), stateName())
  }, ignoreNULL = FALSE)

  #### Remove Loading Box ####
  observeEvent(df_react(), {
  removeModal()
  })
}

# Run app
shinyApp(ui = ui, server = server)
