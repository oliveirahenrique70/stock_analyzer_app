#### UI functions ####
bs4_theme <- function() {
  create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    bs4dash_layout(
      main_bg = "#353c42"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30", secondary = "black"
    ),
    bs4dash_color(
      gray_900 = "#FFF", white = "#FFF"
    )
  )
}

bullet_point <- function(){
  fa("fas fa-caret-right", fill = "#FF50CA", margin_right = "0.7em")
}

card_UI <- function(card_body,
                    card_title,
                    width = 6,
                    height = 500) {
  if (is.null(height)) {
    bs4Card(
      width = width,
      card_body,
      title  = card_title,
      solidHeader = TRUE,
      status = "gray",
      collapsible = FALSE
    )
  } else {
    bs4Card(
      style = str_c("height: ", height, "px"),
      width = width,
      card_body,
      title  = card_title,
      solidHeader = TRUE,
      status = "gray",
      collapsible = FALSE
    )
  }
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
        style = "text-align:center; background-color:#353C42",
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

porfolio_buttons <- function() {
  div(
    actionButton("add_stock", "Add", style = "color: white; background-color: #5E81AC; border-color: #5E81AC;"),
    actionButton("remove_stock", "Remove", style = "color: white; background-color: #5E81AC; border-color: #5E81AC;"),
    actionButton("update_stock", "Update", style = "color: white; background-color: #5E81AC; border-color: #5E81AC;"),
    align = "center"
  )
}

select_stock <- function(id, df, label = NULL) {
  selectizeInput(
    id,
    label,
    choices = unique(df$code),
    selected = "VGIR11"
  )
}

mavg_slider <- function(id) {
  sliderInput(
    id,
    label = "Select MAVG Period (in days):",
    value = 30,
    max = 60,
    min = 7,
    step = 7,
    animate = TRUE
  )
}

stock_analysis_period <- function(id) {
  radioGroupButtons(
    inputId = id,
    label = "Select Analysis Period:",
    choices = list("3 months" = 90,
                   "6 months" = 180,
                   "1 year" = 360,
                   "2 years" = 720),
    status = "primary"
  )
}

submit_button <- function(id) {
  actionButton(id,
               "Submit",
               style = "margin-top: 30px")
}

media_icon <- function(href, icon) {
  tags$a(
    href = href,
    icon,
    style = "color: white; padding-top: 0px; padding-left: 10px;",
    target = "_blank"
  )
}

pink_words <- function(words, link = NA){
  if (is.na(link)) {
    tags$span(tags$b(words), style = "color: #FF50CA;")
  } else {
    a(
      tags$span(tags$b(words), style = "color: #FF50CA;"),
      href = link,
      target = "_blank"
    )
  }
}

ranking_buttons <- function(id) {
    radioGroupButtons(
    inputId = id,
    label = "Rank App:",
    choices = list("★" = 1, "★★" = 2, "★★★" = 3, "★★★★" = 4, "★★★★★" = 5),
    status = "primary")
}

#### Server Functions ####
get_stock_data <- function(index, period) {
  index_sa <- str_c(index, ".SA")
  indice <- Index$new(index_sa)
  indice$set_index(index_sa)
  output <- indice$get_history(period = period, interval = "1d", start = NULL, end = NULL)
  return(output)
}

get_price_var <- function(df) {
  stocks_data <- lapply(df$code, get_stock_data, period = "1mo")

  df <- cbind(df,
              `stock price` = unlist(lapply(stocks_data, function(x) round(tail(x[["close"]], 1), 2))),
              `30 days variance` = unlist(lapply(stocks_data, function(x) round(var(x[["adj_close"]]), 2)))) %>%
      mutate(`total value in R$` = quantity * `stock price`)
  return(df)
}


join_portfolio_data <- function(df, stocks_data) {
  df %>%
    left_join(
      stocks_data %>% select(
        code,
        sector
      ),
      by = "code"
    )
}

bar_plot <- function(df) {
  # Define the colors
  colors <- c(
    rgb(211,94,96, maxColorValue = 255),
    rgb(128,133,133, maxColorValue = 255),
    rgb(144,103,167, maxColorValue = 255),
    rgb(171,104,87, maxColorValue = 255),
    rgb(114,147,203, maxColorValue = 255),
    rgb(200, 1, 0, maxColorValue = 255),
    rgb(1, 200, 0, maxColorValue = 255),
    rgb(1, 0, 200, maxColorValue = 255))

  # Create the plot
  plot <- ggplot(df, aes(code,
                         `total value in R$`,
                         fill = code,
                         text = `total value in R$`)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors[1:length(unique(df$code))],
                      guide = guide_legend(title = "Legend")) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = NULL)
  
  ggplotly(plot, tooltip = "text") %>%
      layout(height = 550)
}

pie_plot <- function(df) {
  plot_ly(
    df %>% arrange(code),
    labels = ~ code,
    values = ~ quantity,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'value+percent',
    insidetextfont = list(color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~ str_c(code),
    marker = list(
      colors = c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)',
        'rgb(200, 1, 0)',
        'rgb(1, 200, 0)',
        'rgb(1, 0, 200)'
      ),
      line = list(color = '#FFFFFF', width = 1)
    ),
    #The 'pull' attribute can also be used to create space between the sectors
    showlegend = TRUE
  ) %>%
    layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      height = 550
    )
}

stock_plot_hover_text <- function(date, open, close, high, low) {
  str_c(
    "date: ", format(date, "%d-%b-%Y") , "<br>",
    "open: ", round(open, 2), "<br>",
    "close: ", round(close, 2), "<br>",
    "high: ", round(high, 2), "<br>",
    "low: ", round(low, 2)
  )
}

stock_plot_mavg_hover_text <- function(date, mavg) {
  str_c(
    "date: ", format(date, "%d-%b-%Y") , "<br>",
    "mavg: ", round(mavg, 2)
  )
}

stock_plot <- function(df, mavg_period, stock_code) {
  df <- df %>% 
    mutate(color = ifelse(open - close > 0, "red", "green"),
           mavg = rollmean(adj_close, k = mavg_period, na.pad = TRUE, align = "right"))
  
  plot <- ggplot(df, aes(x = date)) +

    # Add candlestick
    geom_segment(aes(xend = date, y = open, yend = close, colour = color, text = stock_plot_hover_text(date, open, close, high, low)),
      size = 2) +

    # Add candlesticks high lines
    geom_linerange(aes(ymin = ifelse(open - close > 0, open, close), ymax = high, colour = color, text = stock_plot_hover_text(date, open, close, high, low)),
      width = 1) +
    
    # Add candlesticks low lines
    geom_linerange(aes(ymin = low, ymax = ifelse(open - close > 0, close, open), colour = color, text = stock_plot_hover_text(date, open, close, high, low)),
      width = 1) +

    # Add mavg line
    geom_line(aes(y = mavg),
              color = "black",
              linetype = "dashed",
              alpha = 0.5) +

    # Add mavg hover text
    geom_point(aes(y = mavg, text = stock_plot_mavg_hover_text(date, mavg)),
               color = "black",
               size = 0.1,
               alpha = 0.1) +

    theme_minimal() +
    scale_x_datetime(date_labels = "%y-%b-%d") +
    labs(title = stock_code) +
    theme(legend.position = "none", plot.title = element_text(size = 16, face = "bold")) +
    scale_fill_manual(values = c("green", "red")) +
    scale_color_manual(values = c("green", "red"))
  
  ggplotly(plot, tooltip = "text") %>%
    layout(height = 500)
}

#### Modules #####
table_UI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("table"))
}

table_server <- function(id, df, reactivity, ordering = TRUE, row_names = FALSE, search = TRUE, paging = TRUE) {
  options_list <- list(
    ordering = ordering,
    searching = search,
    paging = paging,
    scrollX = T,
    lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "All")),
    pageLength = 10,
    columnDefs = list(list(targets = "_all", className = "dt-center")),
    language = list(info = "")
  )
  
  moduleServer(id,
               function(input, output, session) {
                 if (reactivity) {
                   output$table <- renderDT({
                     datatable(df(),
                               rownames = row_names,
                               options = options_list
                     )
                   })
                  } else {
                     output$table <- renderDT({
                       datatable(df,
                                 rownames = row_names,
                                 options = options_list
                       )
                   })
                  }
                 })
}