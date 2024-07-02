################################################################################
#------------------------------- Update choices -------------------------------#

observe({
  # if (tmpPanel() == "All"){
  pathways_choices_heatmap <- unique(df.gsea()$Description) %>% gsub("_", " ", .)
  # } else {
  #   pathways_choices_heatmap <- unique(df.gsea.split$Description[df.gsea.split()$Panel == tmpPanel()])
  # }
  updateSelectizeInput(session, 'namePathToShow_heatmap', choices = NULL, server = TRUE)
  updateSelectizeInput(session, 'namePathToShow_heatmap', choices = pathways_choices_heatmap, server = TRUE)
})



################################################################################
#--------------------------------- GSEA Table ---------------------------------#

##### JavaScript 
js_link_render_gsea <- '
function(data, type, row, meta) {
  if (type === "display") {
    var url;
    var dataArray = data.split("/");
    var linkHTML = "";
    for (var i = 0; i < dataArray.length; i++) {
      var currentData = dataArray[i];
      if (i !== 0) {
        linkHTML += "/";
      }
      url = "https://www.ncbi.nlm.nih.gov/gene/?term=" + currentData;
      linkHTML += "<a href=\'" + url + "\' target=\'_blank\'>" + currentData + "</a>";
    }
    return linkHTML;
  } else {
    return data;
  }
}
'

##### Visualization
data_forTable <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(unique(na.omit(df.sampleInfo()[[tmpGroup()]]))) == 2,
      multiple_category_error_message
      )
    )
  df.gsea() %>%
    custom_select(-c("ID", "qvalue", "leading_edge", "enrichmentScore")) %>%
    custom_mutate(Description=gsub("_", " ", Description))
})

output$tablePathwayEnrichment <- renderDT({
  datatable(
    data_forTable(),
    rownames = FALSE,
    extensions = 'Scroller',
    options = list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(-1),
          render = JS(js_link_render_gsea)
        ),
        list(
          targets = "_all", 
          render = JS(js_digits_render)
        )
      )
    )
  )
})

##### Saving
output$savePathwayEnrichment <- downloadHandler(
  filename = function() {
    # paste0(tmpPanel(), "_", tmpGroup(), "_pathway_enrichment_results.", input$tableFormat)
    paste0("All_", tmpGroup(), "_pathway_enrichment_results.", input$tableFormat)
  },
  content = function(file) {
    switch(
      input$tableFormat,
      "csv" = write.csv(df.gsea() %>% custom_mutate(Description=gsub(" ", "_", Description)), file, row.names = FALSE),
      "tsv" = write.table(df.gsea() %>% custom_mutate(Description=gsub(" ", "_", Description)), file, sep = "\t", quote = F, row.names = FALSE)
    )
  }
)



################################################################################
#------------------------------ Pathway Heatmap -------------------------------#
observeEvent(input$checkbox2, {
  if (input$checkbox2){
    updateCheckboxInput(session, "checkbox1", value = FALSE)
  } else {
    updateCheckboxInput(session, "checkbox1", value = TRUE)    
  }
})

observeEvent(input$checkbox1, {
  if (input$checkbox1){
    updateCheckboxInput(session, "checkbox2", value = FALSE)
  } else {
    updateCheckboxInput(session, "checkbox2", value = TRUE)
  }
})

##### Reactive function
react.pathwayHeatmap <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      !is.null(react.li.gsea.reanal()) || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    ) %then%
    need(
      input$numPathToShow_heatmap != "" && input$numPathToShow_heatmap > 0, 
      "Enter the name of the pathway to display, or the number of pathways to display."
    ) %then%
    need(
      input$numPathToShow_heatmap <= 20, 
      "Enter a number less than or equal to 20."
    )
  )

  olink_pathway_heatmap(
    enrich_results = df.gsea() %>% custom_mutate(Description=gsub("_", " ", Description)),
    test_results = df.ttest(),
    number_of_terms = floor(input$numPathToShow_heatmap),
    keyword = if (input$checkbox1) {input$namePathToShow_heatmap} else {NULL}
  ) +
  labs(title = "All") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 8)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20))
})

##### Visualization
output$pathwayHeatmap <- renderPlot({
  react.pathwayHeatmap()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_heatmap_gsea")
})

##### Saving
output$savePathwayHeatmap <- downloadHandler(
  filename = function() {
    paste0("All_", tmpGroup(), "_pathway_heatmap.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.pathwayHeatmap()
    ggsave(file, plot = gg, width = input$width_pathwayHeatmap, height = input$height_pathwayHeatmap, dpi = input$dpi_pathwayHeatmap)
  }
)



################################################################################
#------------------------------ Pathway bargraph ------------------------------#

##### Reactive function
react.pathwayBargraph <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      !is.null(react.li.gsea.reanal()) || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need(
      length_unique_tmpGroup() == 2,
      multiple_category_error_message
    ) %then%
    need(
      input$numPathToShow_bargraph != "" && input$numPathToShow_bargraph > 0, 
      "Enter the number of pathways to display."
    ) %then%
    need(
      input$numPathToShow_bargraph <= 50, 
      "Enter a number less than or equal to 50."
    )
  )
  olink_pathway_visualization(enrich_results = df.gsea(),
                              number_of_terms = floor(input$numPathToShow_bargraph)) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      text = element_text(color = ggplot_text_color),
      axis.text.x = element_text(color = ggplot_text_color),
      axis.text.y = element_text(color = ggplot_text_color),
      legend.title = element_text(size = 14, color = ggplot_text_color),
      plot.title = element_text(size = 16, hjust = 0.5, color = ggplot_text_color)
    ) +
    labs(title = "All")
})

##### Visualization
output$pathwayBargraph <- renderPlot({
  react.pathwayBargraph()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_bargraph_gsea")
})

##### Saving
output$savePathwayBargraph <- downloadHandler(
  filename = function() {
    paste0("All_", tmpGroup(), "_pathway_bargraph.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.pathwayBargraph() 
    ggsave(file, plot = gg, width = input$width_pathwayBargraph, height = input$height_pathwayBargraph, dpi = input$dpi_pathwayBargraph)
  }
)