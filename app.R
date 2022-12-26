# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(emayili)
library(shinyalert)

# SET UP ----
button_theme_search <- "secondary"
button_theme_tags <- "info"
button_theme_cards <- "primary"

# APP CATALOG (META DATA) ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "Financial data analysis",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_mongo_atlas",
    tags = tibble(
        tag = c("AWS", "Auth", "MongoDB", "Plotly"),
        color = c("primary", "dark", "success", "info")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app2 <- list(
    title = "Marine App",
    subtitle = "Geolocation Data Analysis",
    description = "A marine application for analyzing boat travel data on departure and arrival dates, journey distance and duration. Leverages AWS EC2 and uses internal data storage.",
    sub_directory = "marine_analyzer",
    tags = tibble(
        tag = c("AWS", "Leaflet", "DT"),
        color = c("primary", "danger", "warning")
    ) %>% list(),
    img = "marine_app.jpg"
)

app3 <- list(
    title = "Old Faithful",
    subtitle = NA,
    description = "A test application used to validate Shiny Server.",
    sub_directory = "test_app",
    tags = tibble(
        tag = c("AWS", "Plotly"),
        color = c("primary", "info")
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
                            
                            img(src = data$img,
                                height="170px",
                                width="320px"),
                            
                            br(),
                            br(),
                            h4(data$title),
                            br(),
                            if (!is.na(data$subtitle)) {
                                tags$em(data$subtitle)
                            }
                        ),
                        p(data$description,
                          style = "padding:20px;"),
                        a(
                            type = "button",
                            class = str_glue("btn btn-{button_theme_cards}"),
                            target = "_blank",
                            href = str_c("/", data$sub_directory, "/"),
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
    
    #### Header of Web Page ####
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
    

    
    #### Navbar Page ####
    navbar_page_with_inputs(
        title = div(img(src = "HO.gif",
                        width = 40),
                    "Apps"),
        
        collapsible = TRUE,
        
        #### Search Box ####
        inputs = div(
            textInput(inputId = "search_box",
                      label = NULL,
                      width = 200,
                      placeholder = "Search for App"),
            actionButton(inputId = "search_button",
                         label = "Submit",
                         class = str_glue("btn-{button_theme_search}")),
            actionButton(inputId = "clear_button",
                         label = "Clear",
                         class = str_glue("btn-{button_theme_search}"))
        ),
        
        #### Tabs ####
        tabPanel(
            div("Library",
            style = "padding-top:15px"),
            
            div(
                class = "jumbotron",
                div(
                    class = "container",
                    style = "background-color:black;",
                    column(
                        width = 10,
                        h1(strong("Henrique Oliveira's Apps")),
                        h6("These Apps were created by",
                        tags$b('HO'),
                        ". Check out my profiles or send me a direct message"),
                        "LinkedIn" %>% a(target="_blank", class = "btn btn-lg btn-secondary", href = "https://www.linkedin.com/in/henrique-meira-de-oliveira-4b381232/"),
                        "Upwork" %>% a(target="_blank", class = "btn btn-lg btn-secondary", href = "https://www.upwork.com/freelancers/~0121d225d384034e92"),
                        "RPubs" %>% a(target="_blank", class = "btn btn-lg btn-secondary", href = "https://rpubs.com/oliveirahenrique70"),
                        "GitHub" %>% a(target="_blank", class = "btn btn-lg btn-secondary", href = "https://github.com/oliveirahenrique70"),
                        actionButton(inputId = "contact_me",
                                            class = "btn btn-lg",
                                            style = "color: white; background-color: #9933CC; border-color: black",
                                            label = "Contact me!"),
                    ),
                    column(
                        width = 2,
                        style = "padding-top: 15px",
                        img(
                            class = "thumbnail img-responsive",
                            src = "profile.jpeg",
                            style = "width:150px;"
                        )
                    )
                    )
                )
            ,
            
            #### Tag Filters ####
            div(
                class = "container", 
                id = "tag-filters",
                
                h5("Select Apps feature"),
                
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
            
            #### App Library ####
            div(
                id = "app-library",
                uiOutput(outputId = "output_cards")
            )
        )
        
    )
)

# SERVER ----
server <- function(session, input, output) {
    
    observeEvent(input$contact_me, {
        showModal(modalDialog(
            tags$h3("Please share your info!"),
            textInput("email",
                      "Enter e-mail:",
                      placeholder = 'Enter text here'),
            textAreaInput(inputId = "message",
                          label = "Enter message:",
                          height = "200px",
                          width = "300px"),
            size = "s",
            easyClose = TRUE,
            footer = tagList(actionButton('submit_form', 'Submit'),
                             modalButton('Cancel')
            )
        ))
        
    })
    
    output$output1 <- renderText({
        input$input1
    })
    
    observeEvent(input$submit_form, {
        if (input$email == ""){
            shinyalert(title = "Please, add your email", type = "error")
            
        } else {
        
            from <- isolate(input$email)
            msg <- isolate(input$message)
            
            # Create email body
            email <- envelope(
                to = "oliveirahenrique70@gmail.com",
                from = "oliveirahenrique70@gmail.com",
                subject = str_c("HO Apps Contact - ", from),
                text = msg
            )
            
            # Create smtp server port
            smtp <- emayili::server(
                host = "smtp.gmail.com",
                port = 465,
                username = "oliveirahenrique70@gmail.com",
                password = "mkjppkgwvnfqrzfh"
            )
            
            # Send email
            #smtp(email)
            
            shinyalert(title = "Message sent!", type = "success")
            print(email)
        }
    })
    
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
                        placeholder = "Search for App")
        
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
