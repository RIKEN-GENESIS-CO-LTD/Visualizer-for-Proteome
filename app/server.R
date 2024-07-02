#### Function for server process ####
customConfig <- function(.) {
  config(
    .,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list(
      'pan', 'select2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 
      'lasso2d', 'lassoSelect', 'lassoUnselect', 'boxSelect', "zoom",
      'boxZoom', 'toggleSpikelines', 'autoScale2d'
    )
  )
}

formatStyle_threshold <- function(.) {
  DT::formatStyle(
    .,
    columns = c('Threshold'),
    color = styleEqual(c("Significant"), c('red'))
  )
}

function(input, output, session) {
  
 
  
  ### panel
  tmpPanel <- reactive({
    return(input$panel_input)
  })
  
  ### group
  tmpGroup <- reactive({
    return(input$groupToAnalyze)
  })
  

    
  observe({
    if (input$modeAnalysis) {
      feedbackWarning(
        inputId = "groupToAnalyze",
        text = "The switch for reanalysis mode is currently turned ON.",
        show = TRUE)
    } else {
      hideFeedback("groupToAnalyze")
    }
  })
  output$group_num_message <- renderText({
    paste0('"', input$groupToAnalyze, '"', " consists of ", length_unique_tmpGroup(), " groups.")
  })
  
  
  
  ### sample information
  df.sampleInfo <- reactive({
    if (!input$modeAnalysis) {
      return(df.sampleInfo.default)
    } else {
      return(df.sampleInfo.default)
      # return(df.sampleInfo.reanal)
    }
  })
  
  df.mergedData <- reactive({
    data <- merge(df.npxData, df.sampleInfo(), by="SampleID") %>% 
      dplyr::filter(!grepl('CONTROL', SampleID, ignore.case=T))
    return(data)
  })
  
  observe({
    if (!input$modeAnalysis) {
      choices_for_groupToAnalyze <- names(li.ttest)
    }
    else {
      choices_for_groupToAnalyze <- colnames(df.sampleInfo())[-c(1)]
    }
    updateSelectizeInput(session, "groupToAnalyze", choices = choices_for_groupToAnalyze, server = TRUE)
  })
  
  ### number of group sets
  length_unique_tmpGroup <- reactive({
    if (!input$modeAnalysis) {
      length(unique(na.omit(df.sampleInfo.default[[tmpGroup()]])))
    } else {
      length(unique(na.omit(df_sampleinfo_reanal_filt()[[tmpGroup()]])))
    }
  })

  
  
  #### Script loading ####
  source('server_tools.R', local = TRUE)
  source('server_exploratoryAnalysis.R', local = TRUE)
  source('server_twoGroupComp.R', local = TRUE)
  source('server_gsea.R', local = TRUE)
  source('server_multipleComp.R', local = TRUE)

}