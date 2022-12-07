# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# APP CATALOG (META DATA) ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "MongoDB Atlas",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_mongo_atlas",
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

# UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Header of WebPage
    tagList(tags$head(
        HTML("<title>Henrique Oliveira Apps</title>")
    )),
    
    
    tags$style(
        HTML(
            '.navbar-nav > li > a, .navbar-brand {
                   padding-top:5px !important;
                   padding-bottom:0 !important;
                   height: 35px;
                 }
                 .navbar {min-height:60px !important;}'
        )
    ),
    
    style = "padding:0px",
    
    themeSelector(),
    
    # Navbar page
    navbar_page_with_inputs(
        title = div(img(src = "logo.gif",
                        width = 50),
                    "Henrique Oliveira Apps"),
        
        collapsible = TRUE,
        
        # Search box
        inputs = div(
            textInput(
                inputId = "search_box",
                label = NULL,
                width = 200
            ),
            actionButton(inputId = "search_button",
                         label = "Submit"),
            actionButton(inputId = "clear_button",
                         label = "Clear")
        ),
        
        # Tabs
        tabPanel(
            "Library",
            
            # 1.3.1 Tag Filters ----
            div(
                class = "container", 
                id = "tag-filters", 
                radioGroupButtons(
                    inputId = "input_tags", 
                    choices = c("Choice A", "Choice B", " Choice C", "Choice D"), 
                    justified = TRUE, 
                    status = "default"
                )
            ),
            
            # 1.3.2 App Library ----
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
    # Add App Cards
    output$output_cards <- renderUI({
        div(class = "container",
            div(
                class = "row",
                
                # First Card
                div(
                    class = "col-sm-4",
                    style = "display:flex",
                    div(
                        class = "panel panel-default",
                        div(class = "panel-heading",
                            span(class = "label label-info",
                                 "AWS")),
                        div(
                            class = "panel-body",
                            style = "padding:20px;",
                            
                            img(class = "imh img-thumbnail",
                                src = "stock_analyzer.jpg"),
                            
                            br(),
                            br(),
                            h4("Stock Analizer"),
                            br(),
                            tags$small("MongoDb Atlas")
                        ),
                        p(app_catalog_tbl$description[[1]],
                          style = "padding:20px;"),
                        a(type = "button",
                          class = "btn btn-primary",
                          target = "_blank",
                          href = str_c("/", app_catalog_tbl$sub_directory[[1]]),
                          "Open",
                          style = "margin:20px;"
                        )
                    )
                )
            ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
