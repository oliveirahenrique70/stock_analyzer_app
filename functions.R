app_link_buttons <- function(app_name, git_link = "", github = FALSE) {
  if (github) {
    div(a(target = "_blank",
          href = paste0("/", app_name, "/"),
          class = "btn btn-primary",
          "Click to Access App"),
        a(target = "_blank",
          href = paste0("https://github.com/oliveirahenrique70/", git_link),
          class = "btn btn-secondary",
          "Click to Access Apps GitHub"),
        align = "center")
  } else {
    div(a(target = "_blank",
          href = paste0("/", app_name, "/"),
          class = "btn btn-primary",
          "Click to Access App"),
        align = "center")
  }
}

about_info <- function() {
  tagList(
    p(fa("fas fa-lightbulb", fill = "purple", width = "1.7em", margin_right = "1em"),
      "Data-driven Mindset", style = "font-size: 19px; font-family: 'brandon-text'"),
    p(fa("fas fa-user-graduate", fill = "purple", width = "2em", margin_right = "0.7em"),
      "BSc Chemical Engenieer & MS Energy Technology", style = "font-size: 19px; font-family: 'brandon-text'"),
    p(fa("fas fa-language", fill = "purple", width = "1.7em", margin_right = "1em"),
      "Potuguese & Spanish & English", style = "font-size: 19px; font-family: 'brandon-text'"),
    p(fa("fas fa-at", fill = "purple", width = "2em", margin_right = "0.7em"),
      "oliveirahenrique70@gmail.com", style = "font-size: 19px; font-family: 'brandon-text'"),
    p(fa("fas fa-phone", fill = "purple", width = "2em", margin_right = "0.7em"),
      "+55 (81) 99991-0182", style = "font-size: 19px; font-family: 'brandon-text'"),
    actionButton(inputId = "contact_me", class = "btn btn-lg", style = "background-color: #9933CC", label = "Contact me!")
  )
}

bullet_point <- function(){
  fa("fas fa-caret-right", fill = "#FF50CA", margin_right = "0.7em")
}

bullet_point_toc <- function(text, id){
  p(fa("fas fa-caret-right", fill = "#FF50CA", margin_right = "0.5em"),
    HTML(paste0("<a href='", id, "'>", text, "</a>"))
  )
}

created_by_msg <- function() {
  tagList(
    hr(style = "border-top: 2px solid #FF50CA;"),
    fluidRow(
      style = "text-align:center; background-color:#000000",
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

ds_report_link <- function(link) {
  div(a(target = "_blank",
        href = paste0("https://rpubs.com/oliveirahenrique70/", link),
        class = "btn btn-primary",
        paste0("Click to Access Report")),
      align = "center")
}

feedback_msg <- function(text, author, link) {
  HTML(
    paste0(
      "<blockquote style='font-style: italic; font-size: 17px; border-left: 5px solid #FF50CA; padding-left: 10px;'>",
      text,
      "</blockquote> <a target = _blank, href=",
      link,
      " style='font-size: 17px; display: block; text-align: right;'>",
      author,
      "</a>"
    )
  )
}

HO_logo <- function() {
  fluidRow(
    style = "text-align:center; background-color:#000000",
    class = "jumbotron",
    column(1, offset = 4, img(src = "HO.gif", width = 100, style = "padding-top: 12px;")),
    column(4, h2("Henrique Oliveira"),
           p("MAKING DATA ACCESSIBLE",
             align = "center",
             style = "color: #FF50CA; font-size: 23px; font-family: 'Lucida Console'"))
  )
}

img_with_link <- function(img, link, rpubs) {
  p(tags$a(
    href = ifelse(rpubs, paste0("https://rpubs.com/oliveirahenrique70/", link), link),
    target = "_blank",
    img(src = img, width = "80%")),
    align = "center"
    )
}

media_icon <- function(href, icon) {
  tags$a(
    href = href,
    icon,
    style = "color: white; padding-top: 0px; padding-left: 10px;",
    target = "_blank"
  )
}

packages_bagde <- function(packages) {
  tags$ul(lapply(packages,
    function(pkg) {
      tags$li(class = "badge",
              style = "background-color: #AB029C; font-size: 14px;",
              pkg)
    }
  ), align = "center")
}


pink_words <- function(words){
  tags$span(tags$b(words), style = "color: #FF50CA;")
}

section_title <- function(title, line_position = "down") {
    if (line_position == "down") {
        tagList(
            h2(tags$i(title)),
            hr(style = "border-top: 2px solid #FF50CA;")
        )
    } else if (line_position == "up") {
        tagList(
            hr(style = "border-top: 2px solid #FF50CA;"),
            h2(tags$i(title))
        )
    }

}

section_subtitle <- function(title, small = NULL, id = NULL, hr = TRUE) {
  if (hr) {
    tagList(
      hr(style = "border-top: 2px solid #FF50CA;"),
      div(
      h3(tags$i(title), id = id, style = "color: #FF50CA; display: inline"),
      h5(tags$i(small), style = "color: #FF50CA; display: inline")
      )
    )
  } else {
    h3(tags$i(title), id = id, style = "color: #FF50CA")
  }
}

video_thumbnail <- function(video_url) {
  div(class = "thumbnail",
      div(class = "embed-responsive embed-responsive-16by9",
          tags$iframe(class = "embed-responsive-item",
                      src = video_url,
                      allowfullscreen = NA,
                      width = "100%",
                      height = "400")
      )
  )
}
