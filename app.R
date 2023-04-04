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
      p("Welcome to the HO data science consultancy webpage 🧑‍💻"),
      p("Here you can know more about how to extact all potency from your data. As a experience data science I can help you build data visualization 📈, use machine learning 🤖 and interactive apps ⚙️ to get the best info from your data."),
      p("Here is how I can help you:"),
      p(bullet_point(), "Create, read, update and maninupulate ", pink_words("large data"), "."),
      p(bullet_point(), "Create clear, concise and easy to interpret ", pink_words("data visualisation"),
        " that accurately represent the data and avoid any distortions or misrepresentations."),
      p(bullet_point(), "Build intuitive and captivating ", pink_words("data science report"),
        " to help you undestand important insigths from your data."),
      p(bullet_point(), "Develop ", pink_words("interactive app"),
        " that allows users to explore the data in real-time, generate custom reports or visualizations, and perform unique analyses."),
      p("I'd love to help you get all potency from your data. Please get in touch! 🙂"),
      section_title("Here are Some Examples of My Work"),
      p("Throughout my work, my goal is to create tools that help both experts and end users interact well with the data available to them."),
      
      # Second section - DS Report
      section_subtitle("Data Science Reports"),
      p("A data science report is a document that provides a detailed analysis of a dataset using visualizations, tables, statistical analysis and machine learning techniques."),
      p("The report typically includes a variety of visualizations, tables, and text that help to convey insights and conclusions drawn from the data. These reports can be used for a variety of purposes, such as identifying trends, predicting future outcomes, or making data-driven decisions."),
      p(img(src="ds_report.gif"), align = "center"),
      p("To build a data science report specialized tools and languages such as R & RMarkdown and Python & Jupyter Notebook are used. By using these tools together, data scientists can create reports that are not only informative but also visually appealing and easy to understand."),
      actionLink("link_to_ds_report", "Click here to access Data Science Reports portfolio"),
      
      # Third section - Apps
      section_subtitle("Interactive Apps"),
      p("An interactive app is a type of software application that allows users to interact with data or information in a dynamic way. These apps are often used to visualize complex data sets, create custom reports, or perform complex calculations. Interactive apps can be used in a wide range of industries, including finance, healthcare, marketing, and more."),
      p("One of the key benefits of interactive apps is that they enable users to explore and analyze data in real-time, allowing for a more in-depth understanding of the underlying trends and patterns. With interactive apps, users can filter and sort data based on specific criteria, and then generate custom reports or visualizations that highlight the most important insights."),
      p("In addition to providing insights into data, interactive apps can also be used to optimize business processes and streamline workflows."),
      p(img(src="app.gif"), align = "center"),
      p("Interactive apps can be built using a variety of programming languages and tools, but one popular choice is R Shiny. R Shiny is an open-source web framework that allows developers to build interactive apps using the R programming language. This makes it easy to create custom visualizations and reports using R's extensive library of data visualization and analysis tools."),
      actionLink("link_to_apps", "Click to access Apps portfolio"),
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
      p("Hello! 👋 I´m a 33 years old Brazilian engineer with 5 years of experience developing data science projects and getting valuable business insights to clients. My projects use data manipulation & visualization, machine learning models, statistical tests and interactive apps to get the full potential from the data."),
      p("I have worked in projects envolving data scraping, manipulation of large data, data visualization with unique chart and user interactivity, machine learning models (supervised, unsupervised and deep learning) and interactive apps."),
      
      # Second section - Technologies
      section_title("Technologies"),
      p("I have experience using the following:"),
      p(img(src="technologies.png", width = 1000, heigth = 1000), align = "center"),

      # Third section - Created by
      created_by_msg()
    )
  ),
  
  navbarMenu(
    "Portfolio",
    
    #### Apps Portfolio ####
    
    tabPanel(
      "Apps",
      id = "apps_portfolio",
      fluidPage(
        class = "container",
        section_title("Interactive Apps Porfolio"),
        p("In this section you can access apps created by HO"),
        
        # Table of Content
        bullet_point_toc("Stock Analyzer", "#stock_analyzer"),
        
        # First App
        section_subtitle("Stock Analyzer", id = "stock_analyzer"),
        p("A financial application for analyzing trends in your favourite SP 500 stocks."),
        p(img(src = "app.gif"), align = "center"),
        app_link("stock_analyzer_local_data", "Stock Analyzer"),

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
        p("In this section you can access the data science reports. There are the following reports:"),
        
        # Table of Content
        bullet_point_toc("Pooling Data Analysis", "#pooling_data_analysis"),
        bullet_point_toc("Solar Panels ROI Analysis", "#solar_panels_roi_analysis"),
        bullet_point_toc("Books Text Analysis", "#books_text_analysis"),
        
        # First Report
        section_subtitle("Pooling Data Analysis", id = "pooling_data_analysis"),
        p("This report analyzes a polling dataset that includes information on two election candidates, their images, ballot results and voters' demographics data."),
        p("The project tasks include candidates image comparison, ballot results according to candidate A image, voters demographics data analysis, and a significance test between ballot results and voters age."),
        img_with_link("polling_data_analysis.gif", "PollingDataAnalisys", rpubs = TRUE),
        ds_report_link("PollingDataAnalisys", "Pooling Data Analysis"),

        # Second Report
        section_subtitle("Solar Panels ROI Analysis", id = "solar_panels_roi_analysis"),
        p("The project is a test case study for MoneyGeek. The objective is to analyzes Solar Panels Return on Investment (ROI) in U.S.A., which is calculated by considering the investment cost, the money saved on energy, the money made by selling extra energy, and the incentives of federal/state tax credits."),
        p("The project will also discuss electricity cost, investment cost, and solar radiation, which varies by time of day, location, and climate. Finally, the ROI score value will be calculated using a formula that includes electricity price increase, system cost per watt, and solar radiation, to determine which U.S.A. state has the highest ROI."),
        img_with_link("solar_panel_ROI_analysis.gif", "solar_panels_ROI_analysis", rpubs = TRUE),
        ds_report_link("solar_panels_ROI_analysis", "Solar Panels ROI Analysis"),

        # Third Report
        section_subtitle("Books Text Analysis", "books_text_analysis"),
        p("The article discusses the analysis of a dataset containing information on books written in English and Spanish. The dataset includes the title, category, and the total count of different types of words in the books."),
        p("The project will create graphs to show the relationship between adjectives and verbs and a word cloud of the book titles."),
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
  
  shinyjs::addClass(id = "navbar_page", class = "navbar-right")
  #bslib::bs_themer()
}

# Run the application
shinyApp(ui = ui, server = server)
