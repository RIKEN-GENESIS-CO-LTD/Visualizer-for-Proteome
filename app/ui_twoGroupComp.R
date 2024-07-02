tabItem_twoGroupComp <- tabItem(
  
  tabName = "twoGroupComp",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "Two-group comparison"),
  ),
  
  fluidRow(
    shinydashboardPlus::box(
      title = "Table",
        solidHeader = TRUE,
        width = 12,
        height = "12px",
        collapsible = TRUE,
        status = "primary",
        fluidRow(
          column(
            2, 
            selectInput(
              inputId = "twoGroupCompMethod", 
              label = HTML("Select a method"), 
              choices = c("t_test", "wilcoxon_test"), 
              selected = "t_test"
            ),
            custom_downloadButton(id = "saveTwoGroupComp")
          ),
          column(
            10,
            title = "Two-group comparison",
            status = "primary",
            uiOutput("tableTwoGroupComp")
          )
        )
      )
  ),

  fluidRow(
    shinydashboardPlus::box(
      title = "Volcano plot",
      solidHeader = TRUE,
      width = 6,
      collapsible = TRUE,
      status = "primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_volcanoplot",
        startOpen = F,
        width = 25,
        h4(class = "exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(inputId = "height_volcanoplot", label = HTML("Height"),
                    min = 2, max = 20, value =  6, step = 2),
        sliderInput(inputId = "width_volcanoplot", label = HTML("Width"),
                    min = 2, max = 20, value = 8, step = 2),
        sliderInput(inputId = "dpi_volcanoplot", label = HTML("dpi"),
                    min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          3,
          selectizeInput(
            inputId = "genesToLabel_volcano",
            label = HTML("Genes to label"), 
            choices = NULL,
            multiple = TRUE, 
            options = list(
              'plugins' = list('remove_button'),
              placeholder = 'Enter/Select gene names'
            )
          ),
          numericInput(
            inputId = "numGenesToLabel_volcano",
            label = "Number of genes to label",
            value = 0, 
            min = 0,
            max = 20
          ),
          materialSwitch(
            inputId = "modeInteractiveVolcanoplot",
            label = HTML("Interactive mode"), 
            status = "primary"
          ),
          custom_downloadButton(id = "saveVolcanoplot")
        ),
        column(
          9,
          uiOutput("volcanoplot")
        )
      )
    ),

    shinydashboardPlus::box(
      title = "Box plot",
      solidHeader = TRUE,
      width = 6,
      collapsible = TRUE,
      status = "primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_boxplot_twogroup",
        startOpen = T,
        width = 25,
        h4(class = "exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(
          inputId = "height_boxplot_twogroup",
          label = HTML("Height"),
          min = 2, max = 20, value =  6, step = 2),
        sliderInput(
          inputId = "width_boxplot_twogroup", 
          label = HTML("Width"),
          min = 2, max = 20, value = 8, step = 2),
        sliderInput(
          inputId = "dpi_boxplot_twogroup",
          label = HTML("dpi"),
          min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          3,
          selectizeInput(
            inputId = "geneToComp",
            label = HTML("Gene to compare"),
            choices = NULL,
            multiple = FALSE,
            options = list(
              'plugins' = list('remove_button'),
              placeholder = 'Enter/Select a gene name'
            )
          ),
          custom_downloadButton(id = "saveBoxplot_twogroup")
        ),
        column(
          9,
          plotOutput("boxplot_twogroup")
        )
      )
    )
  ),
  
  fluidRow(
    shinydashboardPlus::box(
      title = "NPX heatmap",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      status = "primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_cluHeatmap",
        startOpen = T,
        width = 25,
        h4(class = "exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(
          inputId = "height_cluHeatmap",
          label = HTML("Height"),
          min = 2, max = 20, value = 6, step = 2),
        sliderInput(
          inputId = "width_cluHeatmap", 
          label = HTML("Width"),
          min = 2, max = 20, value = 8, step = 2),
        sliderInput(
          inputId = "dpi_cluHeatmap", label = HTML("dpi"),
          min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          2,
          numericInput(
            inputId = "numGenesToLabel_cluHeatmap",
            label = "Number of genes to display", 
            value = 25, min = 2),
          custom_downloadButton(id = "saveClusteringHeatmap")
        ),
        column(
          10,
          plotOutput("clusteringHeatmap")
        )
      )
    )
  )
)