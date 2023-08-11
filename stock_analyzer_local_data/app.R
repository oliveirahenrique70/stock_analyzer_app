# PACKAGES ----

library(tidyverse)
library(mongolite)
library(jsonlite)
library(config)
library(shiny)
library(plotly)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(tidyquant)

# FUNCTIONS AND DATA ----

source(file = "stock_analysis_functions.R")

stock_list_tbl <- get_stock_list("SP500")

user_base_tbl <- tibble(
    user = c("user1", "user2"),
    password = c("pass1", "pass2"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two"),
    favorites = list(c("AAPL", "GOOG", "NFLX"), c("TWTR")),
    last_symbol = c("NFLX", "TWTR"),
    user_settings = list(tibble(mavg_short = 20, mavg_long = 50, time_window = 180),
                         tibble(mavg_short = 30, mavg_long = 70, time_window = 365))
)

# UI ----

ui <- tagList(
    # CSS
    tags$head(
        tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = shinytheme("paper")
        )
    ),
    
    # JS
    useShinyjs(),
    
    # Website ----
    uiOutput(outputId = "website")
)

server <- function(input, output, session) {
    # USER LOGIN ----

    # User information ----
    reactive_values <- reactiveValues()
    
    observe({
        #if (credentials()$user_auth) {
        user_data_tbl <- user_base_tbl
        
        reactive_values$user_name <- user_data_tbl$name
        reactive_values$permissions <- user_data_tbl$permissions
        reactive_values$favorites_list <-
            user_data_tbl %>% pull(favorites) %>% pluck(1)
        reactive_values$last_symbol <- user_data_tbl$last_symbol
        reactive_values$user_settings <-
            user_data_tbl %>% pull(user_settings) %>% pluck(1)
        
        #}
    })
    
    # Moving averages and time window
    observeEvent(input$apply_and_save, {
        user_settings_tbl <- tibble(
            mavg_short = input$mavg_short,
            mavg_long = input$mavg_long,
            time_window = input$time_window
        )
    })
    
    # Toggle Input Settings ----
    observeEvent(input$settings_toggle, {
        toggle(id = "input_settings", anim = TRUE)
    })
    
    # Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)
    
    # User Input ----
    stock_selection_triggered <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE)
    
    # Apply Moving Averages ----
    mavg_short <- eventReactive(input$apply_and_save, {
        input$mavg_short
    },
    ignoreNULL = FALSE)
    
    mavg_long <- eventReactive(input$apply_and_save, {
        input$mavg_long
    },
    ignoreNULL = FALSE)
    
    # Apply Time Window ----
    time_window <- eventReactive(input$apply_and_save, {
        input$time_window
    },
    ignoreNULL = FALSE)
    
    selected_tab <- eventReactive(input$apply_and_save, {
        # Set Selected Tab
        if (is.character(input$stock_plot_tabset_panel)) {
            selected_tab <- input$stock_plot_tabset_panel
        } else {
            selected_tab <- "Last Analysis"
        }
    },
    ignoreNULL = FALSE)
    
    # Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>%
            get_stock_data(
                from = today() - time_window(),
                to   = today(),
                mavg_short = mavg_short(),
                mavg_long  = mavg_long()
            )
    })
    
    # PLOT OUTPUT ----
    
    # Plot Header ----
    output$plot_header <- renderText({
        stock_selection_triggered()
    })
    
    # Plotly Plot ----
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>% plot_stock_data()
    })
    
    # FAVORITES CARDS ----
    
    # Add Favorites Cards ----
    observeEvent(input$favorites_add, {
        new_stock_symbol <-
            get_symbol_from_user_input(input$stock_selection)
        
        new_stock_symbol_in_favorites <-
            new_stock_symbol %in% reactive_values$favorites_list
        
        if (!new_stock_symbol_in_favorites) {
            reactive_values$favorites_list <-
                c(reactive_values$favorites_list, new_stock_symbol) %>% unique()
            
            updateTabsetPanel(session = session,
                              inputId = "stock_plot_tabset_panel",
                              selected = new_stock_symbol)
        }
    })
    
    # Render Favorites Cards ----
    output$favorite_cards <- renderUI({
        if (length(reactive_values$favorites_list) > 0) {
            generate_favorite_cards(
                favorites = reactive_values$favorites_list,
                from = today() - time_window(),
                to   = today(),
                mavg_short = mavg_short(),
                mavg_long  = mavg_long()
            )
        }
    })
    
    # Delete Favorites Cards ----
    observeEvent(input$favorites_clear, {
        modalDialog(
            title = "Clear Favorites",
            size = "m",
            easyClose = TRUE,
            footer = modalButton("Exit"),
            p(h6(
                "Are you sure you want to delete favorites cards?"
            )),
            br(),
            div(
                selectInput(
                    inputId = "drop_list",
                    label = "Remove Single Favorite",
                    choices = reactive_values$favorites_list %>% sort()
                ),
                actionButton(
                    inputId = "remove_single_favorite",
                    label = "Clear Single",
                    class = "btn-warning"
                ),
                actionButton(
                    inputId = "remove_all_favorite",
                    label = "Clear All Favorites",
                    class = "btn-danger"
                )
            )
        ) %>%
            showModal()
    })
    
    # Clear Single Favorites Cards ----
    observeEvent(input$remove_single_favorite, {
        reactive_values$favorites_list <- reactive_values$favorites_list %>%
            .[reactive_values$favorites_list != input$drop_list]
        
        updateSelectInput(
            session = session,
            inputId = "drop_list",
            choices = reactive_values$favorites_list %>% sort()
        )
    })
    
    # Clear All Favorites Cards ----
    observeEvent(input$remove_all_favorite, {
        reactive_values$favorites_list <- vector()
        
        updateSelectInput(
            session = session,
            inputId = "drop_list",
            choices = reactive_values$favorites_list
        )
    })
    
    # Show/Hide Favorites Cards ----
    observeEvent(input$favorites_toggle, {
        toggle(id = "cards",
               anim = TRUE)
        toggle(id = "commentary_text",
               anim = TRUE)
    })
    
    
    # TABS PANEL ----
    
    output$stock_plot <- renderUI({
        #  Last Analysis Tab ----
        tab_last_analysis <- tabPanel(title = "Last Analysis",
                                      tab_panel(
                                          title = stock_symbol(),
                                          panel_body = plotlyOutput(outputId = "plotly_plot")
                                      ),
        )
        
        # Favorites Cards Tab ----
        tab_favorites_cards <- NULL
        
        if (length(reactive_values$favorites_list) > 0) {
            tab_favorites_cards <- reactive_values$favorites_list %>%
                map(
                    .f = function(x) {
                        tabPanel(
                            title = x,
                            tab_panel(
                                title = x,
                                panel_body =  x %>%
                                    get_stock_data(
                                        from = today() - time_window(),
                                        to   = today(),
                                        mavg_short = mavg_short(),
                                        mavg_long  = mavg_long()
                                    ) %>%
                                    plot_stock_data()
                            )
                        )
                    }
                )
        }
        
        # Create Tabs Set Panel ----
        do.call(
            tabsetPanel,
            list(tab_last_analysis) %>%
                append(tab_favorites_cards) %>%
                append(
                    list(
                        id = "stock_plot_tabset_panel",
                        type = "tabs",
                        selected = selected_tab()
                    )
                )
        )
    })
    
    # COMMENTARY SERVER ----
    
    output$analyst_commentary <- renderText({
        generate_commentary(data = stock_data_tbl(), user_input = stock_selection_triggered())
    })
    
    # RENDER WEBSITE ----
    
    output$website <- renderUI({
        #req(credentials()$user_auth, reactive_values$last_symbol)
        
        navbarPage(
            collapsible = TRUE,
            title = p("Stock Analyzer App"),
            theme = shinytheme("cyborg"),
            
            
            # APPLICATION UI -----
            fluidRow(
                column(
                    width = 12,
                    h2(class = "pull-left", "App UI")
                )
            ),
            
            fluidRow(
                
                # Plot Input ----
                column(width = 4,
                       wellPanel(
                           div(
                               id = "input-main",
                               pickerInput(
                                   inputId = "stock_selection",
                                   label = "Stock List (Pick One to Analyze)",
                                   choices = stock_list_tbl$label,
                                   multiple = FALSE,
                                   selected = stock_list_tbl %>% filter(label %>% grepl(
                                       pattern = str_c(reactive_values$last_symbol, ",")
                                   )) %>% pull(label),
                                   options = pickerOptions(
                                       actionsBox = FALSE,
                                       liveSearch = TRUE,
                                       size = 10
                                   )
                               )
                           ),
                           div(
                               id = "input_buttons",
                               actionButton(
                                   inputId = "analyze",
                                   label = "Analyze",
                                   icon = icon("download")
                               ),
                               div(
                                   class = "pull-right",
                                   actionButton(
                                       inputId = "favorites_add",
                                       label = NULL,
                                       icon = icon("heart")
                                   ),
                                   actionButton(
                                       inputId = "settings_toggle",
                                       label = NULL,
                                       icon = icon("cog")
                                   )
                               )
                           ),
                           div(
                               id = "input_settings",
                               hr(),
                               sliderInput(
                                   inputId = "mavg_short",
                                   label = "Short Moving Average (Days)",
                                   value = reactive_values$user_settings %>% pull(mavg_short),
                                   min = 5,
                                   max = 40
                               ),
                               sliderInput(
                                   inputId = "mavg_long",
                                   label = "Long Moving Average (Days)",
                                   value = reactive_values$user_settings %>% pull(mavg_long),
                                   min = 50,
                                   max = 120
                               ),
                               sliderInput(
                                   inputId = "time_window",
                                   label = "Time Window (Days)",
                                   value = reactive_values$user_settings %>% pull(time_window),
                                   min = 180,
                                   max = 730
                               ),
                               actionButton(
                                   inputId = "apply_and_save",
                                   label = "Apply and Save",
                                   icon = icon("save")
                               )
                           ) %>% hidden()
                       )),
                
                # Plot Output ----
                column(width = 8,
                       uiOutput(outputId = "stock_plot"))
            ),
            
            # FAVORITES CARD -----
            horizontal_line(),
            
            fluidRow(
                
                column(
                    width = 12,
                    h2(class = "pull-left",
                       "Favorites Cards & Comment"),
                    actionButton(class = "pull-right",
                                 inputId = "favorites_clear",
                                 "Clear Favorites")
                )
            ),
            
            br(),
            
            fluidRow(
                column(
                    width = 8,
                    #verbatimTex,Output(outputId = "favorite_print"),
                    uiOutput(outputId = "favorite_cards")
                ),
                column(width = 3,
                       class = "panel",
                       textOutput(outputId = "analyst_commentary")
                )
            ),
            br()
        )
    })
}

# RUN APP ----
shinyApp(ui = ui, server = server)
