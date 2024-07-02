tabItem_exploratoryAnalysis <- tabItem(
  tabName = "exploratoryAnalysis",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "Exploratory analysis")
  ),

  fluidRow(
    shinydashboardPlus::box(
      title="PCA",
      solidHeader=TRUE,
      width=6,
      collapsible=TRUE,
      status="primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_pca",
        startOpen = T,
        width = 25,
        h4(class = "exportTitle", "Export settings",
           style = "font-weight: bold;"),
        sliderInput(inputId = "height_pcaplot", 
                    label = HTML("Height"), 
                    min = 2, max = 10, value = 4),
        sliderInput(inputId = "width_pcaplot", 
                    label = HTML("Width"),
                    min = 2, max = 10, value = 6),
        sliderInput(inputId = "dpi_pcaplot", 
                    label = HTML("dpi"), 
                    min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          3,
          tags$div(
            class = "switch-modeInteractivePcaplot",
            materialSwitch(
              inputId = "modeInteractivePcaplot",
              label = HTML("Interactive mode"), 
              status = "primary"
            )
          ),
          custom_downloadButton(id = "savePcaplot")
        ),
        column(
          9,
          uiOutput("pcaplot")
        )
      )
    ),
    
    shinydashboardPlus::box(
      title="UMAP",
      solidHeader=TRUE,
      width=6,
      collapsible=TRUE,
      status="primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_umap",
        startOpen = T,
        width = 25,
        h4(class="exportTitle", "Export settings",
           style = "font-weight: bold;"),
        sliderInput(inputId = "height_umapplot", 
                    label = HTML("Height"), 
                    min = 2, max = 10, value = 4),
        sliderInput(inputId = "width_umapplot",
                    label = HTML("Width"), 
                    min = 2, max = 10, value = 6),
        sliderInput(inputId = "dpi_umapplot", 
                    label = HTML("dpi"), 
                    min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          3,
          materialSwitch(
            inputId = "modeInteractiveUmapplot",
            label = HTML("Interactive mode"), 
            status = "primary"
          ),
          custom_downloadButton(id = "saveUmapplot")
        ),
        column(
          9,
          uiOutput("umapplot")
        )
      )
    )
  )
)
