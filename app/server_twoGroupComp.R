################################################################################
#----------------------------- Reactive variables -----------------------------#

react.ttest.panel <- reactive({
  if (tmpPanel() == "All"){
    data <- df.ttest() %>% custom_arrange(p.value)
  } else {
    data <- df.ttest() %>% custom_filter(Panel == tmpPanel()) %>% custom_arrange(p.value)
  }
  return(data)
})

react.wilcoxontest.panel <- reactive({
  if (tmpPanel() == "All"){
    data <- df.wilcoxontest() %>% custom_arrange(p.value)
  } else {
    data <- df.wilcoxontest() %>% custom_filter(Panel == tmpPanel()) %>% custom_arrange(p.value)
  }
  return(data)
})



################################################################################
#------------------------------- Update choices -------------------------------#

observe({
  if (tmpPanel() == "All"){
    filtered_choices_volcano <- unique(df.ttest()$Assay)
  } else {
    filtered_choices_volcano <- unique(df.ttest()$Assay[df.ttest()$Panel == tmpPanel()])
  }
  updateSelectizeInput(session, 'genesToLabel_volcano', choices = NULL, server = TRUE)
  updateSelectizeInput(session, 'genesToLabel_volcano', choices = filtered_choices_volcano, server = TRUE)
})

observe({
  if (tmpPanel() == "All"){
    filtered_choices_twoGroupComp_box <- unique(df.ttest()$Assay)
  } else {
    filtered_choices_twoGroupComp_box <- unique(df.ttest()$Assay[df.ttest()$Panel == tmpPanel()])
  }
  updateSelectizeInput(session, 'geneToComp', choices = NULL, server = TRUE)
  updateSelectizeInput(session, 'geneToComp', choices = filtered_choices_twoGroupComp_box, server = TRUE)
})

# observeEvent(df.mergedData.filt(), {
observe({
  updateNumericInput(
    session,
    inputId = "numGenesToLabel_cluHeatmap",
    max = length(unique(react.mergedData.panel()$Assay))
  )
})


################################################################################
#---------------------------- Two-group comparison ----------------------------#

##### Visualization
output$tableTwoGroupComp <- renderUI({
  switch(
    input$twoGroupCompMethod,
    "t_test" = dataTableOutput("tableTtest"),
    "wilcoxon_test" = dataTableOutput("tableWilcoxontest")
  )
})

react.tableTtest <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
      )
    )
  react.ttest.panel() %>%
    custom_select(-c("statistic", "conf.low", "conf.high", "parameter", "method", "alternative"))
})

output$tableTtest <- renderDT({
  datatable(
    react.tableTtest(),
    rownames = FALSE,
    extensions = 'Scroller',
    options = list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "400px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0, 2),
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

react.tableWilcoxontest <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    )
  )
  react.wilcoxontest.panel() %>% 
    custom_select(-c("statistic", "conf.low", "conf.high", "method", "alternative"))
})

output$tableWilcoxontest <- renderDT({
  datatable(
    react.tableWilcoxontest(),
    rownames = FALSE,
    extensions = 'Scroller',
    options=list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "400px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0, 2),
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
  updateBoxSidebar("sidebar_volcanoplot")
})

##### Saving
output$saveTwoGroupComp <- downloadHandler(
  filename = function() {
    switch(
      input$twoGroupCompMethod,
      "t_test" = paste0(tmpPanel(), "_", tmpGroup(), "_ttest_results.", input$tableFormat),
      "wilcoxon_test" = paste0(tmpPanel(), "_", tmpGroup(), "_wilcoxontest_results.", input$tableFormat)
    )
  },
  content = function(file) {
    react.twoGroupComp <- switch(
      input$twoGroupCompMethod,
      "t_test" = react.ttest.panel(),
      "wilcoxon_test" = react.wilcoxontest.panel()
    )
    switch(
      input$tableFormat,
      "csv" = write.csv(react.twoGroupComp, file, row.names = FALSE),
      "tsv" = write.table(react.twoGroupComp, file, sep = "\t", quote = F, row.names = FALSE)
    )
  }
)



################################################################################
#-------------------------------- Volcano plot --------------------------------#

##### Reactive function
gene_names <- reactive({
  input_genes <- input$genesToLabel_volcano
  return(input_genes)
})

react.volcanoplot <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    ) %then%
    need(
      input$numGenesToLabel_volcano != "" && 
      input$numGenesToLabel_volcano >= 0 &&
      input$numGenesToLabel_volcano <= 20 && 
      length(gene_names()) <= 20,
      "Specify that the number of genes to label is 20 or less."
    )
  )
  dataset <- react.ttest.panel()
  xmax <- max(abs(dataset$estimate))
  gg <- ggplot(dataset, aes(x=estimate, y=-log10(p.value), text=Assay))
  if (input$modeInteractiveVolcanoplot) {
    gg <- gg + geom_point(aes(color = ifelse(Adjusted_pval < 0.05, "Significant", "Not Significant")), size=0.8) +
      scale_color_manual(values = c("Significant"="#FF6065", "Not Significant"="grey70")) +
      labs(x="Estimate", y="-log10(P-Value)") +
      set_plot_theme() +
      theme(
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12)
      )
  } else {
    gg <- gg + 
      geom_point(aes(color = ifelse(Adjusted_pval < 0.05, "Significant", "Not Significant")), size=2) +
      scale_color_manual(values = c("Significant" = "#FF6065", "Not Significant" = "grey70")) +
      labs(x = "Estimate", y = "-log10(P-Value)") +
      set_plot_theme() +
      theme(
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
  }
  gg <- gg + ggtitle(tmpPanel()) +
    xlim(-xmax, xmax) +
    geom_text_repel(data = dataset[dataset$Assay %in% gene_names(), ], aes(label = Assay),
                    hjust = 0, vjust = 0, color = "black", max.overlaps = 20)
    # geom_hline(yintercept=0.05, linetype="dashed")
  if (input$numGenesToLabel_volcano > 0 || input$numGenesToLabel_volcano != "") {
    gg <- gg + 
      geom_text_repel(
        data = dataset[0:input$numGenesToLabel_volcano,], 
        aes(label = Assay),
        hjust = 0, vjust = 0, color = "black", max.overlaps = 20
      )
  }
  suppressWarnings(gg)

})

##### Visualization
output$volcanoplot <- renderUI({
  if (input$modeInteractiveVolcanoplot) {
    plotlyOutput("interactiveVolcanoplot")
  } else {
    plotOutput("staticVolcanoplot")
  }
})

# Interactive modeの場合のplotlyOutput
output$interactiveVolcanoplot <- renderPlotly({
  ggplotly(react.volcanoplot(), tooltip = c("text")) %>%
    customConfig() %>%
    layout(legend = list(title = list(text = ""), x = 100, y = 0.5))
})

# Non-Interactive modeの場合のplotlyOutput
output$staticVolcanoplot <- renderPlot({
  react.volcanoplot()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_volcanoplot")
})

##### Saving
observe({
  if (input$modeInteractiveVolcanoplot) {
    disable("saveVolcanoplot")
  } else {
    enable("saveVolcanoplot")
  }
})

output$saveVolcanoplot <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_volcano_plot.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.volcanoplot()
    ggsave(file, plot = gg, width = input$width_volcanoplot, height = input$height_volcanoplot, dpi = input$dpi_volcanoplot)
  }
)



################################################################################
#--------------------------------- Box plot -----------------------------------#

##### Reactive function
react.geneToComp <- reactive({
  if (grepl("^OID", input$geneToComp)) {
    input$geneToComp
  } else {
    result <- unique(df.mergedData()$OlinkID[df.mergedData()$Assay %in% input$geneToComp])
    return(result)
  }
})

react.boxplot <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.ttest.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    ) %then%
    need(
      react.geneToComp() != "", "Enter a gene name."
    )
  )
  if ( is.null(input$geneToComp) ) {
    return(NULL)
  } else {
    # react.li.mergedData.panel.reanal
    gg <- react.mergedData.panel() %>%
      filter(!is.na(.data[[tmpGroup()]])) %>%
      olink_boxplot(
        variable = tmpGroup(),
        olinkid_list = react.geneToComp(),
        ttest_results = df.ttest()
      )
    gg1 <- gg[[1]] +
      theme(
        legend.title = element_text(size = 14),
        text = element_text(size = 16)
      ) +
      labs(fill = tmpGroup())
    gg1
  }
})

##### Visualization
output$boxplot_twogroup <- renderPlot({
  react.boxplot()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_boxplot_twogroup")
})

##### Saving
output$saveBoxplot_twogroup <- downloadHandler(
  filename = function() {
    paste0(input$geneToComp, "_", react.geneToComp(), "_", tmpPanel(), "_", tmpGroup(), "_boxplot.", input$plotFormat)
  },
  content = function(file) {
    ggsave(file, plot = react.boxplot(), width = input$width_boxplot_twogroup, height = input$height_boxplot_twogroup, dpi = input$dpi_boxplot_twogroup)
  }
)



################################################################################
#---------------------------- Clustering heatmap ------------------------------#

##### Reactive function
numTopAssay <- reactive({
  data <- input$numGenesToLabel_cluHeatmap
  return(data)
})

df.mergedData.filt <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      !is.null(react.mergedData.panel()) || !input$modeAnalysis || !is.null(react.ttest.panel()), 
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    ) %then%
    need(
      numTopAssay() != "", 
      "Enter the number of genes to display."
    ) %then%
    need(
      numTopAssay() >= 2,
      "Enter the number of genes larger than or equal to 2."
    ) %then%
    need(
      numTopAssay() <= length(unique(react.mergedData.panel()$Assay)),
      "Enter a number less than or equal to the number of genes contained in the specified panel."
    )
  )
  data_filtered <- react.mergedData.panel() %>% 
    custom_filter(!is.na(.data[[tmpGroup()]])) %>% 
    custom_filter(Assay %in% react.ttest.panel()$Assay[1:numTopAssay()])
  data_filtered
})



react.clusteingHeatmap <- reactive({
  gg <- olink_heatmap_plot(
    df.mergedData.filt(),
    variable_row_list = tmpGroup(),
    fontsize = 12
  ) +
  labs(
    title = tmpPanel(),
    fill = tmpGroup()
  ) +
  theme(
    legend.title = element_text(size = 14, color = ggplot_text_color),
    text = element_text(size = 16, color = ggplot_text_color),
    plot.title = element_text(hjust = 0.5, color = ggplot_text_color)
  )
  gg
})

##### Visualization
output$clusteringHeatmap <- renderPlot({
  react.clusteingHeatmap()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_cluHeatmap")
})

##### Saving
output$saveClusteringHeatmap <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_clustering_heatmap.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.clusteingHeatmap()
    ggsave(file, plot = gg, width = input$width_cluHeatmap, height = input$height_cluHeatmap, dpi = input$dpi_cluHeatmap)
  }
)
