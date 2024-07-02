tabItem_tools <- tabItem(
  
  ### ui.Rとは別にsource関数でui_tools.Rが実行されるためかui制御域内の先頭でリロードする必要がある。
  useShinyFeedback(),
  useShinyjs(),
  
  tabName = "tools",
  tags$div(
    class = "content-tabItem",
    tags$span(class = "content-tabItem-title", "Tools"),
  ),
  
  fluidRow(
    tabBox(
      width = 12,
      tabPanel(
        title = "Reanalysis",
        icon = icon("repeat"),
        fluidRow(
          column(
            6, 
            p(class = "tools-reanalysis-title", HTML("Reanalysis mode")),
            materialSwitch(
              inputId = "modeAnalysis",
              label = div(
                class = "tools-reanalysis-text",
                # br(),
                p("To perform reanalysis, turn on the switch and click the reanalysis button displayed below."),
                p("If there are any samples to exclude, select their names from dropdown menu.")
              ),
              value = FALSE,
              status = "danger"
            ),
            br(),
            selectizeInput(
              inputId = "speciesInput",
              label = HTML("Select your species"),
              choices = c("Human"), #c("Human", "Mouse")
              multiple = FALSE,
              selected = "Human"
            ),
            br(),
            textOutput("title_selectize_reanalysis"),
            lapply(names(df.sampleInfo.default)[-1], function(colname) {
              tags$div(
                class = "selectize-reanalysis",
                selectizeInput(
                  inputId = paste0("select_", colname),
                  label = HTML(paste0(
                    "<span class='label-colname'>Target groupset : ", colname, "</span>")
                  ),
                  choices = unique(df.sampleInfo.default[[1]]),
                  multiple = TRUE,
                  width = '100%',
                  options = list(
                    'plugins' = list('remove_button'),
                    placeholder = "Enter the sample names you want to exclude from reanalysis"
                  )
                )
              )
            }),
            br(),
            actionButton(inputId = "startReanalysis", label = "Start reanalysis")
          ),
          column(
            6,
            tags$div(
              class = "tools-sampleinfo-title",
              p("Sample information")
            ),
            dataTableOutput("sample_info_table")
          )
        )
      ),
      tabPanel(
        title = "Export",
        icon = icon("download"),
        h3("Export format"),
        selectInput(
          inputId = "tableFormat",
          label = HTML("Select table format"),
          choices = c("csv", "tsv")
        ),
        selectInput(
          inputId = "plotFormat", 
          label = HTML("Select plot format"), 
          choices = c("png", "jpeg", "tiff", "bmp", "eps", "pdf") # "svg"
        )
      )
    )
  )
)