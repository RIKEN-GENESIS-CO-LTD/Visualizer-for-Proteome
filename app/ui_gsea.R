tabItem_gsea <- tabItem(
  
  tabName = "gsea",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "GSEA"),
  ),
  fluidRow(
    shinydashboardPlus::box(
      title = "Table",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      status = "primary",
      fluidRow(
        column(
          2,
          custom_downloadButton(id = "savePathwayEnrichment")
        ),
        column(
          10,
          dataTableOutput("tablePathwayEnrichment")
        )
      )
    )
  ),
  
  fluidRow(
    shinydashboardPlus::box(
      title = "Heatmap for pathway enrichment",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      status = "primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_heatmap_gsea",
        startOpen = T,
        width = 25,
        h4(class="exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(inputId = "height_pathwayHeatmap", label = HTML("Height"), 
                    min = 2, max = 20, value =  6, step = 2),
        sliderInput(inputId = "width_pathwayHeatmap", label = HTML("Width"), 
                    min = 10, max = 49, value = 30, step = 10),
        sliderInput(inputId = "dpi_pathwayHeatmap", label = HTML("dpi"),
                    min = 100, max = 1000, value = 300, step = 100),
      ),
      
      fluidRow(
        column(
          2,
          tags$div(
            class = "selectize-namePathToShow_heatmap",
            checkboxInput(
              inputId = "checkbox1",
              label = HTML("Pathway name<br>to display"),
              value = FALSE
            ),
            selectizeInput(
              inputId = "namePathToShow_heatmap",
              label = NULL, 
              choices = NULL,
              selected = NULL,
              multiple = FALSE, 
              options = list(
                'plugins' = list('remove_button'),
                placeholder = 'Enter/Select pathway names'
              )
            )
          ),
          checkboxInput(
            inputId = "checkbox2",
            label = HTML("Number of pathways<br>to display"),
            value = TRUE
          ),
          numericInput(
            inputId = "numPathToShow_heatmap", 
            label = NULL, 
            value = 5, min = 1, max = 20
          ),
          custom_downloadButton(id = "savePathwayHeatmap")
        ),
        column(
          10,
          plotOutput("pathwayHeatmap")
        )
      )
    )
  ),

  fluidRow(
    shinydashboardPlus::box(
      title = "Bargraph for pathway enrichment",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      status = "primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_bargraph_gsea",
        startOpen = T,
        width = 25,
        h4(class="exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(inputId = "height_pathwayBargraph", label = HTML("Height"),
                    min = 2, max = 20, value =  6, step = 2),
        sliderInput(inputId = "width_pathwayBargraph", label = HTML("Width"),
                    min = 2, max = 20, value = 8, step = 2),
        sliderInput(inputId = "dpi_pathwayBargraph", label = HTML("dpi"),
                    min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          2,
          numericInput(
            inputId = "numPathToShow_bargraph", 
            label = HTML("Number of pathways<br>to display"), 
            value = 10, min = 1, max = 50
          ),
          custom_downloadButton(id = "savePathwayBargraph")
        ),
        column(
          10,
          plotOutput("pathwayBargraph")
        )
      )
    )
  )
)
  
