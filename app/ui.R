######### Import of scripts #########
source('ui_tools.R', local = TRUE)
source('ui_exploratoryAnalysis.R', local = TRUE)
source('ui_twoGroupComp.R', local = TRUE)
source('ui_gsea.R', local = TRUE)
source('ui_multipleComp.R', local = TRUE)
source('ui_information.R', local = TRUE)
shinyjs::useShinyjs()
shinyFeedback::useShinyFeedback()



######### Header #########
header <- dashboardHeader(
  title = tags$div(
    class = "header-title",
    img(src = "RG_logo_only_2018_high_resolution.png", class = "header-title-logo"),
    "riken genesis"
  )
)



sidebar <- dashboardSidebar(

  chooseSliderSkin(
    skin = "Flat",
    color = "SkyBlue"
  ),
  minified = FALSE,
  
  sidebarMenu(
    id = "tabs",
    tags$h4(class = "sidebar-menu-title", "Analysis menu"),
    menuItem(
      "Statistical analysis", icon = icon("bar-chart"), startExpanded = T,
      menuSubItem("Exploratory analysis", tabName = "exploratoryAnalysis"),
      menuSubItem("Two-group comparison", tabName = "twoGroupComp"),
      menuSubItem("GSEA", tabName = "gsea"),
      menuSubItem("Multiple comparison", tabName = "multipleComp")
    ),
    menuItem("Tools", tabName = "tools", icon = icon("gear")),
    menuItem("Information", tabName = "information", icon = icon("info-circle")),
    # tags$hr(class = "sidebar-starting-line"),
    tags$h4(class = "sidebar-setting-title", "Analysis settings"),
    selectInput(
      inputId = "panel_input",
      label = h5(class = "label-sidebar", "Choose a panel"),
      choices = panels
    ),
    selectizeInput(
      inputId = "groupToAnalyze",
      label = h5(class = "label-sidebar", "Choose a groupset"),
      options = list(placeholder = "select a groupset"),
      choice = NULL
    ),
    tags$div(
      class = "sidebar-message",
      textOutput("group_num_message")
    )
   
  ),
  
  ### load CSS file
  includeCSS("www/styles.css")
)



######### Body #########
body <- dashboardBody(
  
  tabItems(
    tabItem_exploratoryAnalysis,
    tabItem_twoGroupComp,
    tabItem_gsea,
    tabItem_multipleComp,
    tabItem_information,
    tabItem_tools
  ),
  ### load CSS file
  includeCSS("www/styles.css")
)



######### Footer #########
footer <- dashboardFooter(
  right = "RIKEN GENESIS CO.,LTD.",
  left = HTML(paste0("<span class='project-title'>", app_name, "</span>",  HTML("&emsp;"), "version: ", version))
)


shinydashboardPlus::dashboardPage(header, sidebar, body, footer = footer, title = "Viewer", skin = "blue")
