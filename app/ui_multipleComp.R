tabItem_multipleComp <- tabItem(
  
  tabName = "multipleComp",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "Multiple comparsion"),
  ),
  
  fluidRow(
    shinydashboardPlus::box(
      title="One-way ANOVA",
      solidHeader=TRUE,
      width=12,
      collapsible=TRUE,
      status="primary",
      
      fluidRow(
        column(
          2,
          custom_downloadButton(id = "saveOnewayAnova")
        ),
        column(
          10,
          dataTableOutput("tableOnewayAnova")
        )
      )
    )
  ),
  
  fluidRow(
    shinydashboardPlus::box(
      title="Post-hoc test",
      solidHeader=TRUE,
      width=12,
      collapsible=TRUE,
      status="primary",
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "multipleCompMethod", 
            label = HTML("Choose a method"),
            choices = c("Tukey-Kramer test"), 
            # choices = c("Tukey-Kramer test", "Kruskal-Wallis test"), 
            selected = "Tukey-Kramer test"
          ),
          custom_downloadButton(id = "saveMultipleComp")
        ),
        column(
          10,
          uiOutput("tableMultipleComp")
        )
      )
    )
  ),

  fluidRow(
    shinydashboardPlus::box(
      title="Box plot",
      solidHeader=TRUE,
      width=12,
      collapsible=TRUE,
      status="primary",
      
      sidebar = custom_boxSidebar(
        id = "sidebar_boxplot_multiple",
        startOpen = T,
        width = 25,
        h4(class="exportTitle", "Export settings", style = "font-weight: bold;"),
        sliderInput(inputId = "height_boxplot_multiple", label = HTML("Height"),
                    min = 2, max = 10, value =  6, step = 2),
        sliderInput(inputId = "width_boxplot_multiple", label = HTML("Width"),
                    min = 2, max = 20, value = 6, step = 2),
        sliderInput(inputId = "dpi_boxplot_multiple", label = HTML("dpi"),
                    min = 100, max = 1000, value = 300, step = 100)
      ),
      
      fluidRow(
        column(
          2,
          selectizeInput(
            inputId="geneToComp_multiple",
            label=HTML("Gene to compare"),
            choices=NULL,
            multiple=FALSE,
            options=list('plugins' = list('remove_button'),
                         placeholder = 'Enter/Select a gene name')
          ),
          custom_downloadButton(id = "saveBoxplot_multiple")
        ),
        column(
          9,
          plotOutput("boxplot_multiple")
        )
      )
    )
  )
)
