# Biodiversity Info App Functions and Vectors
# Created by Henrique Oliveira
# January 2023

#### Vectors ####

energy_source <- c("Hydraulic",
                   "Biogas",
                   "Wind",
                   "Solar")
output_height <- 450

#### UI Functions ####
bs4_theme <- function() {
  create_theme(
     # Change this to the desired color,
    bs4dash_vars(
      card_bg = "#F7F7F7",
      navbar_light_color = "#FFF",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#FFF"
    ),
    bs4dash_layout(
      main_bg = "#FFF"
    ),
    bs4dash_sidebar_dark(
      bg = "#FFF",
      # color = "#bec5cb",
      hover_color = "black"
      # submenu_bg = "#FFF",
      # submenu_color = "#FFF",
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#FFF", secondary = "black"
    ),
    bs4dash_color(
      white = "#FFF"
    )
  )
}

buttonsUI <- function() {
  checkboxGroupButtons("potency",
                       selected = c("Micro", "Mini"),
                       choices =  c("Micro", "Mini"),
                       #status = "info",
                       checkIcon = list(
                         yes = tags$i(class = "fa fa-check-square", 
                                      style = "color: steelblue"),
                         no = tags$i(class = "fa fa-square-o", 
                                     style = "color: steelblue")),
                       justified = TRUE
                       )
}

created_by_msg <- function(sidebar = FALSE) {
  if (sidebar) {
    div(
      h5("created by", style = "font-style: italic", align = "center"),
      p("Henrique Oliveira",
        align = "center",
        style = "color: #FF50CA; font-size: 23px; font-family: 'Lucida Console'; margin-bottom: 10px;"),
      p(
        media_icon(href = "https://www.upwork.com/freelancers/~0121d225d384034e92",
                   icon = "up"),
        media_icon(
          href = "https://www.linkedin.com/in/henrique-meira-de-oliveira-4b381232",
          icon = icon("linkedin", lib = "font-awesome", class = "fa-lg")
        ),
        media_icon(
          href = "https://github.com/oliveirahenrique70",
          icon = icon("github", lib = "font-awesome", class = "fa-lg")
        )
      ),
      align = "center"
    )
  } else {
    tagList(
      #hr(style = "border-top: 2px solid #FF50CA;"),
      fluidRow(
        style = "text-align:center; background-color:#FFF",
        class = "jumbotron",
        column(1, offset = 4, img(src = "profile.jpeg", width = 100, style = "padding-top: 12px;")),
        column(3, h5("created by", style = "font-style: italic"),
               p("Henrique Oliveira",
                 align = "center",
                 style = "color: #FF50CA; font-size: 23px; font-family: 'Lucida Console'; margin-bottom: 10px;"),
               p(media_icon(href = "https://www.upwork.com/freelancers/~0121d225d384034e92",
                            icon = "up"),
                 media_icon(href = "https://www.linkedin.com/in/henrique-meira-de-oliveira-4b381232",
                            icon = icon("linkedin", lib = "font-awesome", class = "fa-lg")),
                 media_icon(href = "https://github.com/oliveirahenrique70",
                            icon = icon("github", lib = "font-awesome", class = "fa-lg")))),
      )
    )
  }
}

media_icon <- function(href, icon) {
  tags$a(
    href = href,
    icon,
    style = "color: black; padding-top: 0px; padding-left: 10px;",
    target = "_blank"
  )
}

checkboxUI <- function(id) {
  awesomeCheckboxGroup(
    inputId = id,
  #inline = TRUE,
    label = NULL,
    choices = energy_source,
    selected = energy_source)
}

analysisPeriodUI <- function(id) {
  dateRangeInput(id,
                 start  = "2022-01-01",
                 end = "2022-12-31",
                 label = NULL)
}

selectInputUI <- function(id, vec) {
  selectizeInput(id,
                 label = NULL,
                 multiple = FALSE,
                 choices = vec)
}

submitUI <- function() {
  actionButton("submit",
               style = "font-size: 20px;; margin:auto; background-color: green; color:white",
               label = "Submit")
}

plot_card <- function(title, card_body) {
  output <- card(
    height = output_height + 70,
    full_screen = FALSE,
    card_header(title,
                class = "text-center"),
    card_body_fill(card_body))
  return(output)
}

sibebar_section <- function(title) {
  return(h5(tags$em(title), align = "center"))
}

#### Server Functions ####

get_color_palette <- function(df) {
  color_palette <- colorNumeric(palette = "RdYlBu",
                                domain = as.integer(df$n),
                                reverse = TRUE)
  return(color_palette)
}

apply_filters <- function(df, input) {
  df <- df %>%
    filter(DthAtualizaCadastralEmpreend >= input$analysis_period[[1]]) %>%
    filter(DthAtualizaCadastralEmpreend <= input$analysis_period[[2]]) %>%
    #filter(DscPorte %in% str_c(input$potency, "geracao")) %>%
    filter(DscFonteGeracao %in% input$energy_source_checkbox)
  return(df)
}

#### Shiny Modules ####

valueBoxUI <- function(id) {
  ns <- NS(id)
  valueBoxOutput(ns("box"), width = 3)
}

valueBoxServer_MicroMini <- function(id, df_react, var, color, icon_id, subtitle, footer_text = "") {
  moduleServer(id,
               function(input, output, session) {
                 output$box <- renderValueBox({
                   
                  df <- as.data.frame(df_react()) %>% 
                    filter(DscPorte == var)
                   
                   value <- sum(df["micro_mini_count"])
                   
                   valueBox(
                     value = value,
                     subtitle = subtitle,
                     color = color,
                     icon = icon(icon_id),
                     footer = div(footer_text)
                   )
                 })
               })
}

valueBoxServer_Total <- function(id, df_react, var, color, icon_id, subtitle) {
  moduleServer(id,
               function(input, output, session) {
                 output$box <- renderValueBox({
                   df <- as.data.frame(df_react())
                   value <- sum(df[var])
                   
                   if (var == "potency") {
                     value <- str_c(value, " KW")
                   }
                   
                   valueBox(
                     value = value,
                     subtitle = subtitle,
                     color = color,
                     icon = icon(icon_id)
                   )
                 })
               })
}

valueBoxServer_energySource <- function(id, df_react, energy_type, subtitle, color, icon_id, footer_text) {
  moduleServer(id,
               function(input, output, session) {
                 output$box <- renderValueBox({
                   
                   df <- df_react() %>%
                     filter(DscFonteGeracao == energy_type)
                   
                   count <- df %>%
                     pull(n)
                   
                   potency <- df %>%
                     pull(total_potency)
                   
                   if (length(count) == 0) {
                     count <- potency <- 0
                   }
                   
                   valueBox(
                     value = str_c("Count: ", count),
                     subtitle = str_c("Potency: ", potency, " KW"),
                     color = color,
                     icon = icon(icon_id),
                     footer = div(footer_text)
                   )
                 })
               })
}
               
               
leafletPlot <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("plot"), height = "100%")
}

add_label <- function(df) {
    df <- df %>%
      mutate(label = sprintf("%s<br/>%s<br/>%s",
                             name,
                             str_c("count: ", n),
                             str_c("potency: ", round(potency/1000,1), " MW")) %>%
               lapply(HTML))
}

leafletPlotServer <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 
                 color_palette <-   colorNumeric(palette = "RdYlBu",
                                                 domain = as.integer(df$n),
                                                 reverse = TRUE)

                 output$plot <- renderLeaflet({
                   leaflet(df) %>%
                     addProviderTiles(providers$CartoDB.Positron) %>%
                     addPolygons(
                       data = df,
                       smoothFactor = 0.5,
                       fillOpacity = 0.5,
                       weight = 0.5,
                       opacity = 0.8,
                       color = ~color_palette(n),
                       label = ~label) %>%
                     addLegend(pal = color_palette,
                               values = ~n,
                               title = "Count")
                 })
               })
}

gtTable <- function(id) {
  ns <- NS(id)
  gt_output(ns("table"))
}

gtTableServer <- function(id, df_react, df_mini_micro_table, state_name) {
  moduleServer(id,
               function(input, output, session) {
                 
                 if (state_name != "All") {
                   df <- df_react %>%
                     select(-c("City", "MicroRegion", "MesoRegion")) %>%
                     mutate(state = state_name, .before = 1) %>%
                     rename(`total count` = n,
                            `total potency` = potency)
                   
                   print(df)

                   df <- df %>%
                     left_join(df_mini_micro_table %>%
                                 filter(DscPorte == "Microgeracao"), by = "name")
                   df <- df %>%
                     left_join(df_mini_micro_table %>%
                                 filter(DscPorte == "Minigeracao"), by = "name")

                   df <- df %>%
                     select(-c("State", "nome", "nome.y", "nome.x", "label", "DscPorte.x", "DscPorte.y", "Region")) %>%
                     rename(city = name,
                            `mini count` = n.y,
                            `micro count` = n.x,
                            `mini potency` = potency.y,
                            `micro potency` = potency.x)
                 } else {
                   df <- df_react %>%
                     rename(state = name)
                   
                   df <- df %>%
                     left_join(df_mini_micro_table %>%
                                 filter(DscPorte == "Microgeracao"), by = "SigUF")
                   df <- df %>% 
                     left_join(df_mini_micro_table %>%
                                 filter(DscPorte == "Minigeracao"), by = "SigUF")
                   
                   df <- df %>%
                     rename(UF = SigUF,
                            `mini count` = n.y,
                            `micro count` = n.x,
                            `mini potency` = potency.y,
                            `micro potency` = potency.x,
                            `total count` = n,
                            `total potency` = potency) %>%
                     select(-c("DscPorte.x", "DscPorte.y")) %>%
                     select(UF, state, `total count`, `total potency`, `micro count`, `micro potency`, `mini count`, `mini potency`)
                 }
                 
                 df <- as.data.frame(df) %>%
                   select(-c("geometry")) %>%
                   arrange(desc(`total count`))
                 
                df <-  replace(df, is.na(df), 0)
                 
                output$table <- render_gt({
                  gt(df) %>%
                    opt_interactive(
                      use_search = TRUE,
                      use_filters = TRUE,
                      use_resizers = TRUE,
                      use_highlight = TRUE,
                      use_compact_mode = TRUE,
                      use_text_wrapping = FALSE,
                      use_page_size_select = TRUE
                    ) %>%
                    data_color(columns = `total count`, palette = "Oranges") %>%
                    data_color(columns = `total potency`, palette = "Oranges") %>%
                    data_color(columns = `micro count`, palette = "Purples") %>%
                    data_color(columns = `micro potency`, palette = "Purples") %>%
                    data_color(columns = `mini count`, palette = "Greens") %>%
                    data_color(columns = `mini potency`, palette = "Greens")
                  #tab_header(title = md("**Population** and **Density** Data"))
                })
          })
}


load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}