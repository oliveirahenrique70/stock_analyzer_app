# Biodiversity Info App Functions and Vectors
# Created by Henrique Oliveira
# January 2023

#### Vectors ####

kingdom_vec <- c("Plantae", "Fungi", "Animalia", "NA")
names_vec <- c("Scientific", "Vernacular")
output_height <- 640

#### Data Pre-Processing Functions ####

refine_df <- function(df) {
  df <- df %>%
    select(-1) %>%
    select(-stateProvince) %>%
    mutate(eventTime = as.character(eventTime)) %>%
    mutate_at("eventTime", ~replace(., is.na(.), "NA")) %>%
    mutate_at("kingdom", ~replace(., is.na(.), "NA")) %>%
    mutate_at("vernacularName", ~replace(., is.na(.), "NA")) %>%
    mutate(kingdom = factor(kingdom, levels = kingdom_vec))
  return(df)
}

create_name_choices <- function(df) {
  choices <- c("All", unique(df$scientificName), unique(df$vernacularName))
  choices <- choices[choices != "NA"]
}

create_df_id_names <- function(df) {
  output <- data.frame(
    "id" = rep(df$id, 2),
    "name" = c(df$scientificName, df$vernacularName))
  return(output)
}

#### UI Functions ####

checkboxUI <- function(id, title, vec) {
  awesomeCheckboxGroup(id, title, vec, vec, inline = FALSE)
}

datesUI <- function(df) {
  tagList(
  dateInput("init_date",
            value = min(df$eventDate),
            label = "Inital Date:"),
  dateInput("final_date",
            value = max(df$eventDate),
            label = "Final Date:")
  )
}

selectInputUI <- function(name_choices) {
  selectizeInput("name",
                 "Select Name:",
                 multiple = TRUE,
                 choices = name_choices,
                 selected = "All")
}

buttonsUI <- function() {
  div(actionButton("submit",
                   class = "btn btn-success",
                   label = "Submit"),
      actionButton("clear",
                   class = "btn btn-danger",
                   label = "Clear"),
      align = "center")
}

plot_card <- function(title, card_body) {
  output <- card(
    height = output_height,
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

get_ids <- function(df_id_and_names, input_name) {
  ids <- df_id_and_names %>%
    filter(name %in% input_name) %>%
    pull(id)

  return(ids)
}

bar_plot <- function(df, species_name) {
  if ("All" %in% species_name) {
    plot <- ggplot(df, aes(date, `cumulative sum`, fill = kingdom)) +
      labs(title = "Kingdom")
  } else {
    plot <- ggplot(df, aes(date, `cumulative sum`, fill = `scientific name`)) +
      labs(title = "Species")
  }

  plot <- ggplotly(
    plot +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  )

  return(plot)
}

line_plot <- function(df, specie) {
  if ("All" %in% specie) {
    plot <- ggplot(df, aes(x = date, y = `cumulative sum`, color = kingdom)) +
      labs(title = "Kingdom")
  } else {
    plot <- ggplot(df, aes(x = date, y = `cumulative sum`, color = `scientific name`)) +
      labs(title = "Species")
  }

  plot <- ggplotly(
    plot +
      geom_line() +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
  )

  return(plot)
}

#### Shiny Modules ####

plotlyPlot <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot"), height = output_height)
}

plotlyPlotServer <- function(id, df, input_name, plot_type) {
  moduleServer(id,
               function(input, output, session) {
                 output$plot <- renderPlotly({

                   input$submit
                   species_name <- isolate(input_name)

                   if (plot_type == "line") {
                     line_plot(df, species_name)
                   } else if (plot_type == "bar") {
                     bar_plot(df, species_name)
                   }
                 })
               })
}

leafletPlot <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("plot"), height = output_height)
}

leafletPlotServer <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {

                 output$plot <- renderLeaflet({
                   leaflet(df) %>%
                     addTiles() %>%
                     addMarkers(~longitudeDecimal,
                                ~latitudeDecimal,
                                popup = ~str_c(
                                  "<br>Kingdom: ", df$kingdom,
                                  "<br>Scientific Name: ", df$scientificName,
                                  "<br>Vernacular Name: ", df$vernacularName,
                                  "<br>Individual Count: ", df$individualCount,
                                  "<br>Date: ", df$eventDate,
                                  "<br>Time: ", df$eventTime,
                                  "<br><a href=https://observation.org/observation/", str_sub(df$id, end = -5), ">Click for more info</a>"),
                                clusterOptions = markerClusterOptions(singleMarkerMode = TRUE))
                 })
               })
}

dataTable <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("table"))
}

dataTableServer <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 output$table <- renderDataTable(df)
               })
}
