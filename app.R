# Load packages
library(shiny)
library(bslib)
library(shinyjs)
library(fontawesome)
library(emayili)
library(shinyalert)

# Load fucntions
source("functions.R")

#### UI ####
ui <- navbarPage(
  useShinyjs(),
  id = "navbar_page",
  windowTitle = "HO making data accessible",
  title = "Making data accessible",
  theme = bs_theme(
    bootswatch = "darkly",
    primary = "#C60089",
    bg = "rgb(0, 0, 0)",
    fg = "#fff"
  ),
  
  #### CSS ####
  
  tags$head(
    tags$style(HTML("
      p{font-size: 20px; font-family: 'brandon-text';}
      a{text-decoration: none; color: white;}"))
  ),
  
  #### Home ####
  
  tabPanel(
    class = "container",
    "Home",
    
    HO_logo(),
    
    fluidPage(
      
      # First section - Data Potency
      section_title("Get the Full Potency of your Data", line_position = "up"),
      br(),
      p("Welcome to the", pink_words("HO"), "portfolio webpage! 🧑‍💻 As a data scientist, my goal is to help you get the most out of your information. Here's how I can help:"),
      fluidRow(
          column(width = 7,
                 p(bullet_point(), "Manipulate large", pink_words("data"), ", create ", pink_words("data visualization"), " and ", pink_words("machine learning models"), "."),
                 p(bullet_point(), "Develop intuitive and captivating ", pink_words("data science reports"), "."),
                 p(bullet_point(), "Build ", pink_words("interactive apps"), " that enable real-time exploration of your data.")
                 ),
          column(width = 5,
                 align = "center",
                     h4(tags$strong("What porfolio are you interested in?")),
                     actionButton("link_to_ds_report", "Data Science Reports", style = "background-color: purple; font-size: 20px;  padding: 20px 25px"),
                     actionButton("link_to_apps", "Interactive Apps", style = "background-color: purple; font-size: 20px;  padding: 20px 25px;")
                    
                 )
      ),
      p("My goal is to create tools that facilitate data analysis for both experts and amateur users. Let's work together to get the full potential of your data."),
      #br(),

      # Fourth section - Created by
      created_by_msg(),
      #br()
    )
  ),
  
  #### About ####
  
  tabPanel("About",
    class = "container",
    
    fluidPage(
      
      # First section - Intro text and about info
      section_title("Known more About Me"),
      fluidRow(column(7,
                      video_thumbnail("https://www.youtube.com/embed/hDPFZeTEboE")),
               column(5,
                      about_info(),
                      style = "padding-top: 35px")),
      br(),
      p("Greetings! 🤝 As a skilled Brazilian engineer with 5 years of expertise in developing data science projects and providing valuable business insights to clients, I am here to help you unleash the full potential of your data."),
      p("Through my proficiency in data manipulation, visualization, machine learning models, statistical tests and interactive apps, I can assist you in optimizing your data and obtain important business insigths. Don't hesitate to contact me to get the best out of your data! 🙂"),

      # Technologies
      actionButton("technologies_button", HTML("&nbsp;&nbsp; Show Technologies"), icon = icon("chevron-down")),
      p(img(src="technologies.png", width = "90%", style="display: none;", id = "technologies"), align = "center"),

      # Second section - Academical Experience
      section_title("Academic Experience"),
      p(bullet_point(), "Master in Energy Technology at the University of Pernambuco"),
      p(bullet_point(), "Post-grad in Renewable Energy at the University of Pernambuco"),
      p(bullet_point(), "Graduation in Chemical Engineering at the Federal University of Pernambuco"),

      # Third section - Work Experience
      section_title("Professional Experience"),
      p(bullet_point(), "Data Scientist at", a("Upwork", href = "https://www.upwork.com/", target = "_blank"), ". From November 2018 to Nowadays"),
      p(bullet_point(), "Data Scientist at", a("Arctoris", href = "https://www.arctoris.com/", target = "_blank"), ". From May 2021 to November 2022"),
      p(bullet_point(), "Data Scientist at Truss Investments. From December 2020 to May 2021"),

      section_title("Clients Feedback"),
      fluidRow(
        column(3, feedback_msg("It was a pleasure working with Henrique",
                               "Arctoris Ltd",
                               "https://www.arctoris.com")),
        column(3, feedback_msg("Henrique delivered good work on this python development project and I enjoyed working with him. His communication was top-notch, he met all deadlines, and his skills were reasonably strong. I enjoyed working with Henrique and will likely have additional jobs for him in the future.",
                               "Truss Investments",
                               "#")),
        column(3, feedback_msg("Excellent communication and expert in R. He solved my problem in one hour!",
                               "Spyridon Hab",
                               "https://www.upwork.com/jobs/~019ee484b55d962498")),
        column(3, feedback_msg("Henrique is a fantastic data scientist. He helped me with a project that was specifically challenging for me. He not only was able to complete it on time but took the time to explain things to me in a way that I understood. I left more knowledgeable in R and machine learning as a whole. Thanks so much!",
                               "Axios Enterprises, LLC",
                               "https://www.upwork.com/jobs/~01f7163e68621b674f"))
      ),

      # Third section - Created by
      created_by_msg()
    )
  ),

#### Portfolio ####

  navbarMenu(
    "Portfolio",
    
    #### Apps Portfolio ####
    
    tabPanel(
      "Apps",
      id = "apps_portfolio",
      fluidPage(
        class = "container",
        section_title("Interactive Apps"),
        p("An interactive app is a type of software application that allows users to interact with data dynamically."),
        p("One of the key benefits of interactive apps is that they enable users to explore and analyze data in real time, allowing for a more in-depth understanding of the underlying trends and patterns. With interactive apps, users can filter and sort data based on specific criteria, and then generate custom reports or visualizations that highlight the most important insights."),
        br(),
        p(tags$em(tags$strong("Table of Contents:"))),

        # Table of Content
        bullet_point_toc("Real Estate Stock Analyzer", "#real_estate_stock_analyzer"),
        bullet_point_toc("Biodiversity Analyzer", "#biodiversity_analyzer"),
        bullet_point_toc("Stock Analyzer", "#stock_analyzer"),

        # First App
        section_subtitle("Real Estate Stock Analyzer", small = "created in May 2023", id = "real_estate_stock_analyzer"),
        p("The app allows users to analyze Brazilian real estate stocks and manage their stock portfolio. The app contains interactive plots and a table of the available stock data. Furthermore, the user can add stocks to a portfolio by specifying the stock code and quantity."),
        packages_bagde(c("tidyverse", "shiny", "yahoofinancer", "bs4Dash", "DT", "fontawesome")),
        img_with_link("real_estate_stocks_analyzer_app.png", "/real_estate_stock_analyzer/", rpubs = FALSE),
        app_link_buttons("real_estate_stock_analyzer"),

        # Second App
        section_subtitle("Biodiversity Analyzer", small = "created in January 2023", id = "biodiversity_analyzer"),
        p("The app displays information about the biodiversity in Poland. Allows the user to filter the data by date range, species type (kingdom), and species name (scientific or vernacular). To display the info the app contains four tabs: a map plot, a timeline line plot, a timeline bar plot, and a data table."),
        packages_bagde(c("tidyverse", "shiny", "shinyWidgets", "plotly", "DT", "leaflet")),
        img_with_link("biodiversity_app.png", "/biodiversity_analyzer/", rpubs = FALSE),
        app_link_buttons("biodiversity_analyzer", "biodiversity-info-app", github = T),

        # Third App
        section_subtitle("Stock Analyzer", small = "created in July 2022", id = "stock_analyzer"),
        p("A financial app for analyzing trends in your favourite SP 500 stocks. The app uses an API to get the stock data and allows users to analyze and visualize stock data. Users can customize settings such as moving averages and analysis time windows. Furthermore, users can also add or remove favorite stocks."),
        packages_bagde(c("tidyverse", "tidyquant", "shiny", "shinyWidgets", "plotly", "shinyjs")),
        img_with_link("stocks_analyzer_app.png", "/stock_analyzer_local_data/", rpubs = FALSE),
        app_link_buttons("stock_analyzer_local_data"),

        created_by_msg()
      )
    ),
    
    #### Reports Portfolio ####
    
    tabPanel(
      "DS Reports",
      id = "reports_portfolio",
      fluidPage(
        class = "container",
        section_title("Data Science Reports"),
        p("A data science report is a document that provides a detailed analysis of a dataset using visualizations, tables, statistical analysis and machine learning techniques."),
        p("The report typically includes a variety of visualizations, tables, and text that help to convey insights and conclusions drawn from the data. These reports can be used for a variety of purposes, such as identifying trends, predicting future outcomes, or making data-driven decisions."),
        br(),
        p(tags$em(tags$strong("Table of Contents:"))),

        # Table of Content
        bullet_point_toc("Pooling Data Analysis", "#pooling_data_analysis"),
        bullet_point_toc("Solar Panels ROI Analysis", "#solar_panels_roi_analysis"),
        bullet_point_toc("Books Text Analysis", "#books_text_analysis"),

        # First Report
        section_subtitle("Pooling Data Analysis", small = "created in July 2020", id = "pooling_data_analysis"),
        p("This report analyzes a polling dataset that includes information on two election candidates, their images, ballot results and voters' demographics data."),
        packages_bagde(c("tidyverse", "rmarkdown", "plotly", "kableExtra")),
        img_with_link("ds_polling_data_analysis_2.png", "Polling_Data_Analisys", rpubs = TRUE),
        ds_report_link("Polling_Data_Analisys"),

        # Second Report
        section_subtitle("Solar Panels ROI Analysis", small = "created in October 2022", id = "solar_panels_roi_analysis"),
        p("The project objective is to analyzes Solar Panels Return on Investment (ROI) in U.S.A. states, which is calculated by considering the investment cost, the money saved on energy, the money made by selling extra energy, and the incentives of federal/state tax credits."),
        packages_bagde(c("tidyverse", "rmarkdown", "plotly", "DT", "htmltools")),
        img_with_link("ds_solar_panels_analysis_2.png", "solar_panels_ROI_analysis", rpubs = TRUE),
        ds_report_link("solar_panels_ROI_analysis"),

        # Third Report
        section_subtitle("Books Text Analysis", small = "created in July 2020", "books_text_analysis"),
        p("The article discusses the analysis of a dataset containing information on books written in English and Spanish. The dataset includes the title, category, and the total count of different types of word classes in the books."),
        packages_bagde(c("tidyverse", "rmarkdown", "plotly", "tidytext", "stopwords", "wordcloud2")),
        img_with_link("ds_text_analysis.png", "text_analysis", rpubs = TRUE),
        ds_report_link("text_analysis"),

        created_by_msg()
      )
    )
  ),

  #### Media Icons ####

  # Upwork
  tabPanel(
    media_icon(href = "https://www.upwork.com/freelancers/~0121d225d384034e92",
               icon = "up")
  ),
  # Linkedin
  tabPanel(
    media_icon(href = "https://www.linkedin.com/in/henrique-meira-de-oliveira-4b381232",
               icon = icon("linkedin", lib = "font-awesome", class = "fa-lg"))
  ),
  # Github
  tabPanel(
    media_icon(href = "https://github.com/oliveirahenrique70",
               icon = icon("github", lib = "font-awesome", class = "fa-lg"))
  )
)

server <- function(input, output, session) {

  # Link to reports and apps portfolio
  observeEvent(input$link_to_ds_report, {
    updateNavbarPage(session, "navbar_page", "DS Reports")
  })
  observeEvent(input$link_to_apps, {
    updateNavbarPage(session, "navbar_page", "Apps")
  })

  observeEvent(input$contact_me, {
    showModal(modalDialog(
      textInput("email",
                "Enter e-mail:"),
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
        subject = paste0("HO Apps Contact - ", from),
        text = msg)
            
      # Create smtp server port
      smtp <- emayili::server(
        host = "smtp.gmail.com",
        port = 465,
        username = "oliveirahenrique70@gmail.com",
        password = "mkjppkgwvnfqrzfh")
            
      # Send email
      smtp(email)
            
      shinyalert(title = "Message sent!", type = "success")
    }
  })

  # Technologies image button
  showImage <- reactiveVal(FALSE)
  observeEvent(input$technologies_button, {
    showImage(!showImage())
    if (showImage()) {
      updateActionButton(
        session,
        "technologies_button",
        label = HTML("&nbsp;&nbsp; Hide Technologies"),
        icon = icon("chevron-up")
      )
    } else {
      updateActionButton(
        session,
        "technologies_button",
        label = HTML("&nbsp;&nbsp; Show Technologies"),
        icon = icon("chevron-down")
      )
    }
  })

  observeEvent(input$technologies_button, {
    shinyjs::toggle("technologies", anim = TRUE, animType = "fade")
  })
  
  shinyjs::addClass(id = "navbar_page", class = "navbar-right")
  #bslib::bs_themer()
}

# Run the application
shinyApp(ui = ui, server = server)
