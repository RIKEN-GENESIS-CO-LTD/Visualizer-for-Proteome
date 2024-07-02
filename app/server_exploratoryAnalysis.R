################################################################################
#------------------------------------ PCA -------------------------------------#

##### Reactive function
react.pcaplot <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      !is.null(react.li.pca.reanal()) || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  
  li.pca.data <- if (tmpPanel() == "All") df.pca_all() else df.pca()
  labels <- li.pca.data[["labels"]]
  
  p <- ggplot(data = li.pca.data[["data"]], aes(text=SampleID)) + 
    set_plot_theme() +
    labs(x = labels$x, y = labels$y, title = tmpPanel(), colour = tmpGroup())
  if (input$modeInteractivePcaplot) {
    p <- p + geom_point(aes(x = PCX, y = PCY, colour = colors), size = 1) +
      theme(
        legend.title = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 10)
      )
  } else {
    p <- p + geom_point(aes(x = PCX, y = PCY, colour = colors), size = 2.5) +
      theme(
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
  }
  p + olink_color_discrete()
})

##### Visualization
output$pcaplot <- renderUI({
  if (input$modeInteractivePcaplot) {
    plotlyOutput("interactivePcaplot")
  } else {
    plotOutput("staticPcaplot")
  }
})

output$interactivePcaplot <- renderPlotly({
  ggplotly(react.pcaplot(), tooltip = c("text")) %>% 
    customConfig() %>%
    layout(legend = list(x = 100, y = 0.5))
})

output$staticPcaplot <- renderPlot({
  react.pcaplot()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_pca")
})

##### Saving
observe({
  if (input$modeInteractivePcaplot) {
    disable("savePcaplot")
  } else {
    enable("savePcaplot")
  }
})

output$savePcaplot <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_PCA_plot.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.pcaplot()
    ggsave(file, plot = gg, width = input$width_pcaplot, height = input$height_pcaplot, dpi = input$dpi_pcaplot)
  }
)



################################################################################
#------------------------------------ UMAP ------------------------------------#

##### Reactive function
react.umapplot <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.umap.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    ) %then%
    need( # for default mode
      nrow(df.umap()) > 1 || nrow(df.umap_all()) > 1, 
      "Sample size is too small"
    )
  )
  
  df.umap.data <- if (tmpPanel() == "All") df.umap_all() else df.umap()
  p <- ggplot(data = df.umap.data, aes(text = SampleID)) + 
    set_plot_theme() +
    labs(x = "UMAP1", y = "UMAP2", title = tmpPanel(), colour = tmpGroup())
  if (input$modeInteractiveUmapplot) {
    p <- p + geom_point(aes(x = umapX, y = umapY, colour = .data[[tmpGroup()]]), size = 1) +
      theme(
        legend.title = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 10)
      )
  } else {
    p <- p + geom_point(aes(x = umapX, y = umapY, colour = .data[[tmpGroup()]]), size = 2.5) +
      theme(
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16)
      )
  }
  p + olink_color_discrete()
})

##### Visualization
output$umapplot <- renderUI({
  if (input$modeInteractiveUmapplot) {
    plotlyOutput("interactiveUmapplot")
  } else {
    plotOutput("staticUmapplot")
  }
})

# Interactive modeの場合のplotlyOutput
output$interactiveUmapplot <- renderPlotly({
  ggplotly(react.umapplot(), tooltip = c("text")) %>% 
    customConfig() %>% 
    layout(legend = list(x = 100, y = 0.5))
})

# Non-Interactive modeの場合のOutput
output$staticUmapplot <- renderPlot({
  react.umapplot()
})

##### Settings for saving
observe({
  updateBoxSidebar("sidebar_umap")
})

##### Saving
observe({
  if (input$modeInteractiveUmapplot) {
    disable("saveUmapplot")
  } else {
    enable("saveUmapplot")
  }
})

output$saveUmapplot <- downloadHandler(
  filename = function() {
    paste0(tmpPanel(), "_", tmpGroup(), "_UMAP_plot.", input$plotFormat)
  },
  content = function(file) {
    gg <- react.umapplot()
    ggsave(file, plot = gg, width = input$width_umapplot, height = input$height_umapplot, dpi = input$dpi_umapplot)
  }
)