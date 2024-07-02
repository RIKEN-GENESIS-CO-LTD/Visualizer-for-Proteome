tabItem_information <- tabItem(
  
  tabName = "information",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "Information"),
  ),
  
  fluidRow(
    shinydashboard::box(
      title = "Version History",
      solidHeader = TRUE,
      width = 7,
      collapsible = F,
      status = "primary",
      suppressMessages(timelineBlock(
        width = 12,
        icon = NULL,
        timelineItem(
          title = paste(app_name, ":", version),
          icon = NULL,
          time = "now",
          HTML(paste(tags$span(style="color: silver", "release: 2024-04-15"),
                     tags$br(),
                     "This update marks a big advancement for", app_name, ", and it brings some notable changes compared to the previous version. The highlight of this release is the addition of reanalysis mode.",
                     sep = "")
          )
        ),
        timelineLabel("April 2024", color = "gray", icon = NULL),
        timelineItem(
          title = paste(app_name, ":", "v0.5.0-alpha"),
          icon = NULL, 
          HTML(paste(tags$span(style="color: silver", "release: 2024-01-03"),
                     tags$br(),
                     "Released by Riken Genesis.",
                     sep = "")
          ),
        ),
        timelineLabel("January 2024", color = "gray", icon = NULL)
      ))
    ),
    shinydashboard::box(
      title = "R packages",
      solidHeader = TRUE,
      width = 5,
      collapsible = F,
      status = "primary",
      tags$ul(
        id = "package-list",
        lapply(packages, function(pkg) {
          tags$li(
            tags$a(
              href = paste0("https://cran.r-project.org/web/packages/", pkg),
              target = "_blank",
              class = "package-link",
              HTML(
                paste(
                  "<span class='pkg'>", pkg, "</span>",
                  "<span class='pkg-version'>", packageVersion(pkg), "</span>",
                  tags$br(),
                  sep = ""
                )
              )
            )
          )
        })
      )
    )
  ),
  
  fluidRow(
    shinydashboard::box(
      title = "Softwares",
      solidHeader = TRUE,
      width = 7,
      collapsible = F,
      status = "primary",
      tags$div(
        HTML(
          paste0(
            "<span class='tools-title'>R", HTML("&nbsp;"), R.version[["major"]], ".", R.version[["minor"]], "</span>",
            HTML("&emsp;"),
            a(href="https://www.r-project.org/",
              target="_blank",
              "Jump to the official website for R language."
            )
          )
        )
      )
    ),
    shinydashboard::box(
      title = "Others",
      solidHeader = TRUE,
      width = 5,
      collapsible = F,
      status = "primary",
      tags$ul(
        id = "project-id",
        tags$li(
          HTML(
            paste(
              "<span class='tools-title'>Product information</span>",
              HTML("&emsp;"),
              "<span class='project-title'>Project ID</span>",
              "<span class='project-number'>", projectID, "</span>",
              tags$br(),
              sep = ""
            )
          )
        )
      ),
      tags$div(
        HTML(
          paste(
            "<span class='tools-title'>Riken genesis</span>",
            HTML("&emsp;"),
            a(href="https://www.rikengenesis.jp/",
              target="_blank",
              "Visit our website for more details"
            )
          )
        )
      )
    )
  ),
  tags$div(
    class = "image-logo-information-container",
    img(src = "RG_logo_2018_high_resolution.png", width = 480, height = 80)
  )
)
