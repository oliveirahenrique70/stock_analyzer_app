# Load packages
library(shiny)
library(bslib)
library(shinyjs)
library(fontawesome)

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
      section_title("Get all Potency from your Data"),
      p("Welcome to the HO data science consultancy webpage 🧑‍💻. Here is how I can help you extract all potency from your data:"),
      p(bullet_point(), "Create, read, update and manipulate ", pink_words("large data"), "."),
      p(bullet_point(), "Create clear, concise and easy-to-interpret ", pink_words("data visualization"),
        " that accurately represents the data and avoids any distortions or misrepresentations."),
      p(bullet_point(), "Build intuitive and captivating ", pink_words("data science reports"),
        " to help you understand important insights from your data."),
      p(bullet_point(), "Develop ", pink_words("interactive apps"),
        " that allows users to explore the data in real time, generate custom reports or visualizations, and perform unique analyses."),
      p("Throughout my work, my goal is to create tools that help both experts and amateur users to interact with the data available to them."),
      section_title("Use Case Examples"),
      
      # Second section - DS Report
      section_subtitle("Data Science Reports", hr = FALSE),
      p("A data science report is a document that provides a detailed analysis of a dataset using visualizations, tables, statistical analysis and machine learning techniques."),
      p("The report typically includes a variety of visualizations, tables, and text that help to convey insights and conclusions drawn from the data. These reports can be used for a variety of purposes, such as identifying trends, predicting future outcomes, or making data-driven decisions."),
      p(img(src="ds_report.gif"), align = "center"),
      actionLink("link_to_ds_report", "Click here to access Data Science Reports portfolio"),
      
      # Third section - Apps
      section_subtitle("Interactive Apps"),
      p("An interactive app is a type of software application that allows users to interact with data or information in a dynamically."),
      p("One of the key benefits of interactive apps is that they enable users to explore and analyze data in real time, allowing for a more in-depth understanding of the underlying trends and patterns. With interactive apps, users can filter and sort data based on specific criteria, and then generate custom reports or visualizations that highlight the most important insights."),
      p(img(src="app.gif"), align = "center"),
      actionLink("link_to_apps", "Click here to access Apps portfolio"),
      br(),

      # Fourth section - Created by
      created_by_msg(),
      br()
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
                      style = "padding-top: 65px")),
      br(),
      p("Hello! 👋 I'm a 33 years old Brazilian engineer with 5 years of experience developing data science projects and getting valuable business insights to clients. My projects use data manipulation & visualization, machine learning models, statistical tests and interactive apps to get the full potential from the data."),
      p("I would love to help you get all potency from your data. Please get in touch! 🙂"),

      # Second section - Technologies
      section_title("Academical and Work Experience"),
      p("I have experience with the following:"),

      # Technologies
      actionButton("technologies_button", HTML("&nbsp;&nbsp; Show Technologies"), icon = icon("chevron-down")),
      p(img(src="technologies.png", width = 1000, heigth = 1000, style="display: none;", id = "technologies"), align = "center"),

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
        section_title("Interactive Apps Porfolio"),
        p("In this section you can access the following interactive apps:"),
        
        # Table of Content
        bullet_point_toc("Stock Analyzer", "#stock_analyzer"),
        bullet_point_toc("Maritime Transport Analyzer", "#maritime_transport_analyzer"),
        
        # First App
        section_subtitle("Stock Analyzer", id = "stock_analyzer"),
        p("A financial app for analyzing trends in your favourite SP 500 stocks. The app uses an API to get the stock data and allows users to analyze and visualize stock data. Users can customize settings such as moving averages and analysis time windows. Furthermore, users can also add or remove favorite stocks and view the corresponding plots"),
        packages_bagde(c("tidyverse", "tidyquant", "shiny", "shinyWidgets", "plotly", "shinyjs")),
        img_with_link("app.gif", "/stock_analyzer_local_data/", rpubs = FALSE),
        app_link("stock_analyzer_local_data", "Stock Analyzer"),

        # Second App
        section_subtitle("Maritime Transport Analyzer", id = "maritime_transport_analyzer"),
        p("An app developed in 2020, it reads in a dataset with maritime transport information, and creates a table and an interactive map graph of the data. The user interface includes value boxes, ship data filters. The server allows for reactive data inputs"),
        packages_bagde(c("tidyverse", "shiny", "shinydashboard", "leaflet", "DT")),
        img_with_link("marine_app.gif", "/marine_analyzer/", rpubs = FALSE),
        app_link("marine_analyzer", "Maritime Transport Analyzer"),
  
        created_by_msg()
      )
    ),
    
    #### Reports Portfolio ####
    
    tabPanel(
      "Reports",
      id = "reports_portfolio",
      fluidPage(
        class = "container",
        section_title("Data Science Reports Porfolio"),
        p("In this section you can access the following data science reports:"),
        
        # Table of Content
        bullet_point_toc("Pooling Data Analysis", "#pooling_data_analysis"),
        bullet_point_toc("Solar Panels ROI Analysis", "#solar_panels_roi_analysis"),
        bullet_point_toc("Books Text Analysis", "#books_text_analysis"),

        # First Report
        section_subtitle("Pooling Data Analysis", id = "pooling_data_analysis"),
        p("This report analyzes a polling dataset that includes information on two election candidates, their images, ballot results and voters' demographics data."),
        p("The project tasks include candidates image comparison, ballot results according to candidate A image, voters demographics data analysis, and a significance test between ballot results and voters age."),
        packages_bagde(c("tidyverse", "rmakdown", "plotly", "kableExtra")),
        img_with_link("polling_data_analysis.gif", "PollingDataAnalisys", rpubs = TRUE),
        ds_report_link("PollingDataAnalisys", "Pooling Data Analysis"),

        # Second Report
        section_subtitle("Solar Panels ROI Analysis", id = "solar_panels_roi_analysis"),
        p("The project objective is to analyzes Solar Panels Return on Investment (ROI) in U.S.A. states, which is calculated by considering the investment cost, the money saved on energy, the money made by selling extra energy, and the incentives of federal/state tax credits."),
        p("The ROI score value will be calculated using a formula that includes electricity price increase, system cost per watt, and solar radiation, to determine which U.S.A. state has the highest ROI."),
        packages_bagde(c("tidyverse", "rmakdown", "plotly", "DT", "htmltools")),
        img_with_link("solar_panel_ROI_analysis.gif", "solar_panels_ROI_analysis", rpubs = TRUE),
        ds_report_link("solar_panels_ROI_analysis", "Solar Panels ROI Analysis"),

        # Third Report
        section_subtitle("Books Text Analysis", "books_text_analysis"),
        p("The article discusses the analysis of a dataset containing information on books written in English and Spanish. The dataset includes the title, category, and the total count of different types of word classes in the books."),
        p("The project will create graphs to show the relationship between adjectives and verbs also a word cloud of the book titles."),
        packages_bagde(c("tidyverse", "rmakdown", "plotly", "tidytext", "stopwords", "wordcloud2")),
        img_with_link("ds_report.gif", "text_analysis", rpubs = TRUE),
        ds_report_link("text_analysis", "Books Text Analysis"),

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
    updateNavbarPage(session, "navbar_page", "Reports")
  })
  observeEvent(input$link_to_apps, {
    updateNavbarPage(session, "navbar_page", "Apps")
  })

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
