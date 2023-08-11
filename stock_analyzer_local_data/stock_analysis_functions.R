get_stock_list <- function(stock_index = "SP500") {
  tq_index(stock_index) %>%
    select(symbol, company) %>%
    arrange(symbol) %>%
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    select(label)
}

get_symbol_from_user_input <- function(user_input) {
  user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)
}

get_stock_data <- function(stock_symbol,
                           from = today() - days(180),
                           to   = today(),
                           mavg_short = 20,
                           mavg_long = 50) {
  stock_symbol %>%
    tq_get(get = "stock.prices", from = from, to = to) %>%
    select(date, adjusted) %>%
    mutate(mavg_short = rollmean(
      adjusted,
      k = mavg_short,
      na.pad = TRUE,
      align = "right"
    )) %>%
    mutate(mavg_long  = rollmean(
      adjusted,
      k = mavg_long,
      na.pad = TRUE,
      align = "right"
    ))
  
}

generate_commentary <- function(data, user_input) {
  warning_signal <- data %>%
    tail(1) %>%
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    pull(mavg_warning_flag)
  
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long  <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  if (warning_signal) {
    str_glue(
      "In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends"
    )
  } else {
    str_glue(
      "In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends"
    )
    
  }
}

get_stock_mavg_info <- function(data) {
  n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  data %>%
    tail(1) %>%
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    mutate(
      n_short = n_short,
      n_long = n_long,
      pct_chg = (mavg_short - mavg_long) / mavg_long
    )
}

generate_favorite_card <- function(data) {
  column(
    width = 3,
    info_card(
      title = as.character(data$stock),
      value = str_glue("{data$n_short}-day <small>vs {data$n_long}-day</small>") %>% HTML(),
      sub_value = data$pct_chg %>% scales::percent(),
      sub_text_color = ifelse(data$mavg_warning_flag, "danger", "success"),
      sub_icon = ifelse(data$mavg_warning_flag, "arrow-down", "arrow-up")
    )
  )
}

generate_favorite_cards <- function(favorites,
                                    from = today() - days(180),
                                    to = today(),
                                    mavg_short = 30,
                                    mavg_long = 50) {
  favorites %>%
    # Step 1 - Get data from stocks
    map(
      .f = function(x) {
        x %>%
          get_stock_data(
            from = from,
            to = to,
            mavg_short = mavg_short,
            mavg_long = mavg_long
          )
      }
    ) %>%
    # Step 2 - Get info card data from stock data
    set_names(favorites) %>%
    map(
      .f = function(data) {
        data %>%
          get_stock_mavg_info()
      }
    ) %>%
    # Step 3 - Refine info card data
    bind_rows(.id = "stock") %>%
    mutate(stock = as_factor(stock)) %>%
    split(.$stock) %>%
    # Set 4 - Generate info card
    map(
      .f = function(data) {
        data %>%
          generate_favorite_card()
      }
    ) %>%
    # Step 5 - Make tag list
    tagList()
}

horizontal_line <- function() {
  div(hr(style = "border-top: 2.5px solid #004455;"))
}

info_card <- function(title,
                      value,
                      sub_value,
                      main_icon = "chart-line",
                      sub_icon = "arrow-up",
                      bg_color = "default",
                      text_color = "default",
                      sub_text_color = "success") {
  div(class = "panel panel-default",
      div(
        class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
        style = "padding: 2px",
        p(class = "pull-right", icon(class = "fa-3x", main_icon)),
        h4(title),
        h5(value),
        p(
          class = str_glue("text-{sub_text_color}"),
          icon(sub_icon),
          tags$small(sub_value)
        )
      ))
  
}

mongo_connect <- function(collection,
                          database,
                          host = config$host,
                          username = config$username,
                          password = config$password) {
  mongo(
    collection = collection,
    url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
  )
}

mongo_read_user_base <- function(database,
                                 collection,
                                 host,
                                 username,
                                 password) {
  # Connect to database
  mongo_connection <- mongo_connect(
    database   = database,
    collection = collection,
    host = host,
    username = username,
    password = password
  )
  
  user_base_tbl <<- mongo_connection$find() %>%
    as_tibble()
  
  mongo_connection$disconnect()
}

mongo_update_and_write_user_base <- function(user_name,
                                             col_name,
                                             assign_input,
                                             database,
                                             collection,
                                             host,
                                             username,
                                             password) {
  user_base_tbl[user_base_tbl$user == user_name,][[col_name]] <<-
    assign_input
  
  # Connect to database
  mongo_connection <- mongo_connect(
    database   = database,
    collection = collection,
    host = host,
    username = username,
    password = password
  )
  
  # Query String
  query_string <- str_c('{"user": "', user_name , '"}')
  
  # Update String
  update_string <- user_base_tbl %>%
    filter(user == user_name) %>%
    select(-user,-password,-permissions,-account_created) %>%
    toJSON(POSIXt = "mongo") %>%
    str_remove_all(pattern = "^\\[|\\]$")
  
  # Update
  mongo_connection$update(query = query_string,
                          update = str_c('{"$set": ', update_string, '}'))
  
  mongo_connection$disconnect()
}

plot_stock_data <- function(data) {
  g <- data %>%
    gather(
      key = "legend",
      value = "value",
      adjusted:mavg_long,
      factor_key = TRUE
    ) %>%
    
    ggplot(aes(date, value, color = legend, group = legend)) +
    geom_line(aes(linetype = legend)) +
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
    scale_color_tq() +
    labs(y = "Adjusted Share Price", x = "")

  ggplotly(g, tooltip = c("date", "value")) %>%
    layout(legend = list(title = list(text = ""),
                         orientation = "h",
                         x = 0.2,
                         y = -0.2))
}

read_user_base <- function() {
  user_base_tbl <<- read_rds("database/user_base_tbl.rds")
}

tab_panel <- function(title, panel_body, footer = NULL) {
  div(
    class = "panel",
    div(
      class = "panel-header",
      style = "padding-top: 10px",
      h4(title,
         align = "center")
    ),
    div(class = "panel-body",
        panel_body),
    if (!is.null(footer)) {
      div(class = "panel-footer",
          footer)
    }
  )
}

update_and_write_user_base <-
  function(user_name, col_name, assing_input) {
    user_base_tbl[user_base_tbl$user == user_name,][[col_name]] <<-
      assing_input
    write_rds(user_base_tbl, path = "database/user_base_tbl.rds")
  }
