################################################################################
#----------------------------- Reactive variables -----------------------------#

react.onewayanova.panel <- reactive({
  if (tmpPanel() == "All"){
    data <- react.onewayanova() %>% custom_arrange(p.value)
  } else {
    data <- react.onewayanova() %>% filter(Panel == tmpPanel()) %>% custom_arrange(p.value)
  }
  return(data)
})

react.tukeyKramertest.panel <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.tukeyKramertest.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      nrow(react.tukeyKramertest()) > 0,
      "We didn't conduct a posthoc test because we didn't find any genes
      with significant expression changes in the ANOVA analysis."
    )
  )

  if (tmpPanel() == "All"){
    react.tukeyKramertest() %>% custom_arrange(Adjusted_pval)
    } else {
    react.tukeyKramertest() %>% filter(Panel == tmpPanel()) %>% custom_arrange(Adjusted_pval)
  }
})



################################################################################
#------------------------------- Update choices -------------------------------#

observe({
  if (tmpPanel() == "All"){
    filtered_choices_multipleComp_boxplot <- unique(react.onewayanova()$Assay)
  } else {
    filtered_choices_multipleComp_boxplot <- unique(react.onewayanova()$Assay[react.onewayanova()$Panel == tmpPanel()])
  }
  updateSelectizeInput(session, 'geneToComp_multiple', choices = NULL, server = TRUE)
  updateSelectizeInput(session, 'geneToComp_multiple', choices = filtered_choices_multipleComp_boxplot, server = TRUE)
})



################################################################################
#------------------------------------ ANOVA -----------------------------------#

##### Visualization
react.tableOnewayAnova <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.onewayanova.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() > 2,
      twogroup_category_error_message
    )
  )
  react.onewayanova.panel() %>%
    select(-c("term", "sumsq", "meansq", "statistic", "df" ))
})

output$tableOnewayAnova <- renderDT({
  datatable(
    react.tableOnewayAnova(),
    rownames = FALSE,
    extensions = 'Scroller',
    options=list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0,2),
          render = JS(js_link_render)
        ),
        list(
          targets = "_all", 
          render = JS(js_digits_render)
        )
      )
    )
  ) %>% formatStyle_threshold()
})

##### Saving
output$saveOnewayAnova <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_One-way_ANOVA_results.", input$tableFormat)
  },
  content = function(file) {
    switch(
      input$tableFormat,
      "csv" = write.csv(react.onewayanova.panel(), file, row.names = FALSE),
      "tsv" = write.table(react.onewayanova.panel(), file, sep = "\t", quote = F, row.names = FALSE)
    )
  }
)



################################################################################
#---------------------------------- post hoc ----------------------------------#

##### Visualization
output$tableMultipleComp <- renderUI({
  switch(
    input$multipleCompMethod,
    "Tukey-Kramer test" = dataTableOutput("tableTukeyKramertest"),
    "Kruskal-Wallis test" = dataTableOutput("tableKruskalWallistest")
  )
})

react.tableTukeyKramertest <- reactive({
  react.tukeyKramertest.panel() %>%
    select(-c("term", "conf.low", "conf.high"))
})

react.signifGenes.anova <- reactive({
  react.onewayanova.panel() %>% filter(Threshold == "Non-significant")
})

output$tableTukeyKramertest <- renderDT({
  datatable(
    react.tableTukeyKramertest(),
    rownames = FALSE,
    extensions = 'Scroller',
    options=list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0,2),
          render = JS(js_link_render)
        ),
        list(
          targets = "_all", 
          render = JS(js_digits_render)
        )
      )
    )
  ) %>% formatStyle_threshold()
})


##### Settings for saving
observe({
  updateBoxSidebar("sidebar_boxplot_multiple")
})

##### Saving
output$saveMultipleComp <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_", input$multipleCompMethod, "_results.", input$tableFormat)
  },
  content = function(file) {
    switch(
      input$tableFormat,
      "csv" = write.csv(react.tukeyKramertest.panel(), file, row.names = FALSE),
      "tsv" = write.table(react.tukeyKramertest.panel(), file, sep = "\t", quote = F, row.names = FALSE)
    )
  }
)



################################################################################
#---------------------------------- Box plot ----------------------------------#

##### Reactive function
react.geneToComp_multiple <- reactive({
  if (grepl("^OID", input$geneToComp_multiple)) {
    input$geneToComp_multiple
  } else {
    result <- unique(df.mergedData()$OlinkID[df.mergedData()$Assay %in% input$geneToComp_multiple])
    return(result)
  }
})


react.boxplot_multiple <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      react.geneToComp_multiple() != "", "Enter a gene name"
    )
  )
  if ( is.null(input$geneToComp_multiple) ) {
    return(NULL)
  } else {
    gg <- react.mergedData.panel() %>% 
      dplyr::filter(!is.na(.data[[tmpGroup()]])) %>%
      olink_boxplot(
        variable = tmpGroup(), 
        olinkid_list = react.geneToComp_multiple()
      )
    gg[[1]] +
      theme(legend.title = element_text(size = 16),
            text = element_text(size = 18)) +
      labs(fill = tmpGroup())
  }
})

##### Visualization
output$boxplot_multiple <- renderPlot({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.onewayanova.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() > 2, 
      twogroup_category_error_message
    )
  )
  react.boxplot_multiple()
})

##### Saving
output$saveBoxplot_multiple <- downloadHandler(
  filename = function() {
    paste0(input$geneToComp_multiple, "_", react.geneToComp_multiple(), "_", tmpPanel(), "_", tmpGroup(), "_boxplot_multiple_comparison.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.boxplot_multiple()
    ggsave(file, plot = gg, width = input$width_boxplot_multiple, height = input$height_boxplot_multiple, dpi = input$dpi_boxplot_multiple)
  }
)