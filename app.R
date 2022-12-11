# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# SET UP ----
button_theme_search <- "secondary"
button_theme_tags <- "info"
button_theme_cards <- "primary"

# APP CATALOG (META DATA) ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "MongoDB Atlas",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_local_data",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "MongoDB", "Auth"),
        color = c("info", "success", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app2 <- list(
    title = "Stock Analyzer",
    subtitle = "Local Data",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and uses internal data storage.",
    sub_directory = "stock_analyzer_local_data",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "Auth"),
        color = c("info", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app3 <- list(
    title = "Old Faithful",
    subtitle = NA,
    description = "A test application used to validate Shiny Server.",
    sub_directory = "test_app",
    tags = tibble(
        tag = c("Sample Apps", "Shiny", "AWS"),
        color = c("info", "success", "success")
    ) %>% list(),
    img = "test_app.jpg"
)

app_catalog_tbl <- bind_rows(app1, app2, app3, .id = "id")


# FUNCTIONS ----
navbar_page_with_inputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form navbar-right", inputs)
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <-
        htmltools::tagAppendChild(navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form)
    navbar
}

make_tags <- function(data) {
    data %>%
        mutate(tag = as_factor(tag)) %>%
        group_by(tag) %>%
        group_split() %>%
        map(.f = function(data) {
            span(class =  str_glue("label label-{data$color}"), data$tag)
        }) %>%
        tagList()
}

make_cards <- function(data) {
    data %>%
        mutate(id = as_factor(id)) %>%
        group_by(id) %>%
        group_split() %>%
        map(.f = function(data) {
                div(
                    class = "col-sm-4",
                    style = "display:flex",
                    div(
                        class = "panel panel-default",
                        div(class = "panel-heading",
                            make_tags(
                                data %>% pluck("tags", 1)
                            )),
                        div(
                            class = "panel-body",
                            style = "padding:20px;",
                            
                            img(class = "imh img-thumbnail",
                                src = data$img),
                            
                            br(),
                            br(),
                            h4(data$title),
                            br(),
                            if (!is.na(data$subtitle)) {
                                tags$small(data$subtitle)
                            }
                        ),
                        p(data$description,
                          style = "padding:20px;"),
                        a(
                            type = "button",
                            class = str_glue("btn btn-{button_theme_cards}"),
                            target = "_blank",
                            href = data$sub_directory,
                            "Open",
                            style = "margin:20px;"
                        )
                    )
                )
            }
        ) %>%
        tagList()
}

# UI ----

ui <- fluidPage(
    # Header of Web Page
    tagList(tags$head(
        HTML("<title>Henrique Oliveira Apps</title>")
    )),
    
    # Navbar header size
    tags$style(
        HTML(
            '.navbar-nav > li > a, .navbar-brand {
                   padding-top: 5px;
                   height: 50px;}'
        )
    ),
    
    style = "padding:0px",
    collapsible = TRUE,
    # themeSelector(),
    theme =  shinytheme("cyborg"),
    
    # Navbar page
    navbar_page_with_inputs(
        title = div(img(src = "HO.gif",
                        width = 40),
                    "Apps"),
        
        collapsible = TRUE,
        
        # Search box
        inputs = div(
            textInput(inputId = "search_box",
                      label = NULL,
                      width = 200,
                      placeholder = "Text"),
            actionButton(inputId = "search_button",
                         label = "Submit",
                         class = str_glue("btn-{button_theme_search}")),
            actionButton(inputId = "clear_button",
                         label = "Clear",
                         class = str_glue("btn-{button_theme_search}"))
        ),
        
        # Tabs
        tabPanel(
            div("Library",
            style = "padding-top:9px"),
            
            # Tag Filters
            div(
                class = "container", 
                id = "tag-filters",
                radioGroupButtons(
                    inputId = "input_tags", 
                    choices = c("All", app_catalog_tbl %>%
                                    select(tags) %>%
                                    unnest() %>%
                                    pull(tag) %>%
                                    unique() %>%
                                    sort()), 
                    justified = TRUE, 
                    status = button_theme_tags
                )
            ),
            
            # App Library
            div(
                class = "container", 
                id = "app-library",
                uiOutput(outputId = "output_cards")
            )
        )
        
    )
)

# SERVER ----
server <- function(session, input, output) {
    
    reactive_app_catalog_tbl <- reactiveValues(data = app_catalog_tbl)
    
    # Tag filters
    observeEvent(eventExpr = input$input_tags, {
        tag_selected <- str_to_lower(input$input_tags)
        
        if (tag_selected == "all") {
            reactive_app_catalog_tbl$data <- app_catalog_tbl
        } else {
            ids_selected <- app_catalog_tbl %>%
                unnest(tags) %>%
                filter(str_to_lower(tag) == tag_selected) %>%
                pull(id)
            
            reactive_app_catalog_tbl$data <- app_catalog_tbl %>%
                filter(id %in% ids_selected)
        }
    })
    
    # Search box & submit button
    observeEvent(eventExpr = input$search_button,{
        search_string <- str_to_lower(input$search_box)
        
        reactive_app_catalog_tbl$data <- app_catalog_tbl %>%
            filter(str_to_lower(title) %>% str_detect(search_string) |
                   str_to_lower(subtitle) %>% str_detect(search_string) |
                   str_to_lower(description) %>% str_detect(search_string)
            )
            
    })
    
    # Clear button
    observeEvent(eventExpr = input$clear_button,{
        updateTextInput(session = session,
                        inputId = "search_box",
                        value = "",
                        placeholder = "Search")
        
        updateRadioGroupButtons(session = session,
                                inputId = "input_tags",
                                selected = "All")
        
        reactive_app_catalog_tbl$data <- app_catalog_tbl
    })
    
    # Render App Cards
    output$output_cards <- renderUI({
        
        div(class = "container",
            div(
                class = "row",
                style = "display:-webkit-flex; flex-wrap:wrap;",
                
                # Cards
                reactive_app_catalog_tbl$data %>% make_cards()
            ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
