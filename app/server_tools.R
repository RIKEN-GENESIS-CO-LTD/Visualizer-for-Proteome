################################################################################
#----------------- Update UI dependent on modeAnalysis switch -----------------#
observe({
  if (input$modeAnalysis) {
    shinyjs::show("speciesInput")
  } else {
    shinyjs::hide("speciesInput")
  }
})

observe({
  if (input$modeAnalysis) {
    shinyjs::show("title_selectize_reanalysis")
  } else {
    shinyjs::hide("title_selectize_reanalysis")
  }
})

output$title_selectize_reanalysis <- renderText({
  "Groupsets"
})

observe({
  if (input$modeAnalysis) {
    shinyjs::show("startReanalysis")
  } else {
    shinyjs::hide("startReanalysis")
  }
})

df_sampleinfo_reanal_filt <- reactive({
  data <- df.sampleInfo.default
  for (col_name in names(df.sampleInfo.default)[-1]) {
    selected_rows <- df.sampleInfo.default[["SampleID"]] %in% input[[paste0("select_", col_name)]]
    data[[col_name]][selected_rows] <- NA
  }
  data
})



observe({
  if (any(sapply(df_sampleinfo_reanal_filt()[-1], function(col) length(unique(col[!is.na(col)]))) <= 1) | 
      any(sapply(names(df_sampleinfo_reanal_filt())[-1], function(col) sum(table(na.omit(df_sampleinfo_reanal_filt()[[col]])) < 2)))) {
    shinyjs::disable("startReanalysis")
  } else {
    shinyjs::enable("startReanalysis")
  }
})



observe({
  lapply(names(df.sampleInfo.default)[-1], function(col_name) {
    if (input$modeAnalysis) {
      shinyjs::show(paste0("select_", col_name))
    } else if (!input$modeAnalysis) {
      shinyjs::hide(paste0("select_", col_name))
    }
  })
})

observe({
  lapply(names(df.sampleInfo.default)[-1], function(col_name) {
    if (any(sapply(col_name, function(col) length(unique(na.omit(df_sampleinfo_reanal_filt()[[col]]))) < 2))) {
      feedbackDanger(
        inputId = paste0("select_", col_name),
        text = "The number of groups within a groupset must be at least 2",
        show = TRUE
      )
    } else if (any(sapply(col_name, function(col) sum(table(na.omit(df_sampleinfo_reanal_filt()[[col]])) < 2)))) {
      feedbackDanger(inputId = paste0("select_", col_name),
                     text = "Each group within a groupset must contain at least two samples.",
                     show = TRUE)
    } else {
      hideFeedback(paste0("select_", col_name))
    }
  })
})

output$sample_info_table <- renderDT({
  df <- df_sampleinfo_reanal_filt() %>% mutate(Index = as.numeric(rownames(.))) %>% relocate(Index, .before = 1)
  df[is.na(df)] <- "NA"
  server = TRUE
  
  datatable(
    df,
    rownames = FALSE,
    extensions = 'Scroller',
    options=list(
      pageLength = 50,
      scrollX = TRUE,
      scrollY = "600px",
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = "_all",
          render = JS(js_digits_nonexpo_render)
        )
      )
    )
  ) %>%
    formatStyle(
      columns = setdiff(names(df), "SampleID"), 
      color = styleEqual("NA", "lightgray")
    )
})



################################################################################
# ----------- Reactive variables (dependent on input$startReanalysis) ----------#
react.li.mergedData.panel.reanal <- reactiveVal(list())
react.li.pca.reanal <- reactiveVal(list())
react.li.pca_all.reanal <- reactiveVal(list())
react.li.umap.reanal <- reactiveVal(list())
react.li.umap_all.reanal <- reactiveVal(list())
react.li.ttest.reanal <- reactiveVal(list())
react.li.wilcoxontest.reanal <- reactiveVal(list())
react.li.gsea.reanal <- reactiveVal(list())
react.li.onewayanova.reanal <- reactiveVal(list())
react.li.tukeyKramertest.reanal <- reactiveVal(list())


observeEvent(input$startReanalysis, {
  showModal(
    modalDialog(
      title = "Message",
      "Reanalysis ongoing, please wait.",
      easyClose = TRUE,
      footer = NULL
    )
  )
})
  
observeEvent(input$startReanalysis, {
  if (input$modeAnalysis && any(sapply(df_sampleinfo_reanal_filt(), function(col) length(unique(col[!is.na(col)]))) > 1)) {
    
    li.mergedData.panel.reanal <- list()
    li.pca.reanal <- list()
    li.pca_all.reanal <- list()
    li.umap.reanal <- list()
    li.umap_all.reanal <- list()
    li.ttest.reanal <- list()
    li.wilcoxontest.reanal <- list()
    li.gsea.reanal <- list()
    li.onewayanova.reanal <- list()
    li.tukeyKramertest.reanal <- list()

    groups.reanal <- setdiff(colnames(df.sampleInfo()), "SampleID")
    totalGroups <- length(groups.reanal)
    inc <- totalGroups * 8

    # disable buttons
    shinyjs::disable("startReanalysis")
    shinyjs::disable("savePcaplot")
    shinyjs::disable("saveUmapplot")
    shinyjs::disable("saveTwoGroupComp")
    shinyjs::disable("saveVolcanoplot")
    shinyjs::disable("saveBoxplot_twogroup")
    shinyjs::disable("saveClusteringHeatmap")
    shinyjs::disable("savePathwayEnrichment")
    shinyjs::disable("savePathwayHeatmap")
    shinyjs::disable("savePathwayBargraph")
    shinyjs::disable("saveOnewayAnova")
    shinyjs::disable("saveMultipleComp")
    shinyjs::disable("saveBoxplot_multiple")

    withProgress(message = 'Processing...', value = 0, {
      for (tmpGroup.reanal in groups.reanal) {

        #### removal of samples from analysis ####
        # If a particular row (sample) in a particular column with group information is blank, the row is excluded from the analysis. This allows certain samples to be excluded from the analysis.
        if (any(is.na(df.mergedData()[[tmpGroup.reanal]]))) {
          npxtmp <- df.mergedData() %>% filter(!is.na(.data[[tmpGroup.reanal]])) %>%
            filter(!SampleID %in% input[[paste0("select_", tmpGroup.reanal)]])
        } else {
          npxtmp <- df.mergedData() %>% filter(!SampleID %in% input[[paste0("select_", tmpGroup.reanal)]])
        }
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": PCA"))

        for (paneltmp in panels){
          if (paneltmp == "All"){
            li.mergedData.panel.reanal[[tmpGroup.reanal]][[paneltmp]] <- npxtmp
          } else {
            li.mergedData.panel.reanal[[tmpGroup.reanal]][[paneltmp]] <- npxtmp %>% dplyr::filter(Panel == paneltmp)
          }
        }
     


        #### PCA ####
        # each panel
        li.pca_group.reanal <- list()
        pca_results <- olink_pca_plot(df = npxtmp, color_g = tmpGroup.reanal, byPanel = T)
        for (paneltmp in names(pca_results)){
          # OlinkAnalyzeのバージョンによって？olink_pca_plotの実行結果のデータフレームにおいてSampleID列とcolor列が存在しない場合と存在する場合がある。
          pca_results_data <- as.data.frame(pca_results[[paneltmp]]$data)
          if (!"SampleID" %in% colnames(pca_results_data)){
            pca_results_data <- as.data.frame(pca_results[[paneltmp]]$data) %>% 
              mutate(SampleID = rownames(.)) %>%
              merge(., df.sampleInfo.default, by.x = "SampleID", by.y = "SampleID", all = F) %>%
              rename("colors" = tmpGroup.reanal)
          }
          li.pca_group.reanal[[paneltmp]][["data"]] <- pca_results_data
          li.pca_group.reanal[[paneltmp]][["labels"]] <- as.data.frame(pca_results[[paneltmp]]$labels)
        }
        li.pca.reanal[[tmpGroup.reanal]] <- li.pca_group.reanal
        
        # all panels
        pca_results <- olink_pca_plot(df = npxtmp, color_g = tmpGroup.reanal, byPanel = F)
        pca_results_data <- as.data.frame(pca_results[[1]]$data)
        if (!"SampleID" %in% colnames(pca_results_data)){
          pca_results_data <- as.data.frame(pca_results[[1]]$data) %>% 
            mutate(SampleID = rownames(.)) %>%
            merge(., df.sampleInfo.default, by.x = "SampleID", by.y = "SampleID", all = F) %>%
            select(-contains("colors")) %>%  # 「start reanalysis」押下時にcolors列が生成される場合とされない場合が存在するので生成された場合は除外する。
            rename("colors" = tmpGroup.reanal)
        }
        li.pca_all.reanal[[tmpGroup.reanal]][["data"]] <- pca_results_data 
        li.pca_all.reanal[[tmpGroup.reanal]][["labels"]] <- as.data.frame(pca_results[[1]]$labels)
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": UMAP"))



        #### UMAP ####
        sampleInfo_grouptmp <- df.sampleInfo() %>% select(all_of(c("SampleID", tmpGroup.reanal)))
        # each panel
        li.umap_group.reanal <- list()

        ### function
        try_olink_umap_plot <- function(...) {
          res <- try(OlinkAnalyze::olink_umap_plot(...), silent = T)
          if (class(res) == "try-error") {
            return(NULL)
          } else {
            return(res)
          }
        }

        umap_results <- try_olink_umap_plot(df = npxtmp, color_g = tmpGroup.reanal, byPanel = T)
        for (paneltmp in names(umap_results)){
          if (is.null(umap_results)) {
            li.umap_group.reanal[[paneltmp]] <- data.frame()
          } else {
            umap_results_dataOnly <- as.data.frame(umap_results[[paneltmp]]$data) %>% mutate(SampleID = rownames(.))
            li.umap_group.reanal[[paneltmp]] <- merge(umap_results_dataOnly, sampleInfo_grouptmp, by.x="SampleID", by.y="SampleID", all=F) %>% select(-Panel)
          }
        }
        li.umap.reanal[[tmpGroup.reanal]] <- li.umap_group.reanal
        # all panels
        umap_results <- try_olink_umap_plot(df = npxtmp, color_g = tmpGroup.reanal, byPanel = F)
        if (is.null(umap_results)) {
          li.umap_all.reanal[[tmpGroup.reanal]] <- NULL
        } else {
          umap_results_dataOnly <- as.data.frame(umap_results[[1]]$data) %>% mutate(SampleID = rownames(.))
          li.umap_all.reanal[[tmpGroup.reanal]] <- merge(umap_results_dataOnly, sampleInfo_grouptmp, by.x="SampleID", by.y="SampleID", all=F)
        }
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": t-test"))



        #### ttest ####
        if ( length(unique(na.omit(df_sampleinfo_reanal_filt()[,tmpGroup.reanal]))) == 2 ) {
          ttest_results <- olink_ttest(df = npxtmp, variable = tmpGroup.reanal, alternative = 'two.sided')
        } else {
          ttest_results <- data.frame()
        }
        li.ttest.reanal[[tmpGroup.reanal]] <- ttest_results
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": wilcoxon test"))



        #### wilcoxon test ####
        if ( length(unique(na.omit(df_sampleinfo_reanal_filt()[,tmpGroup.reanal]))) == 2 ) {
          wilcoxontest_results <- olink_wilcox(df = npxtmp, variable = tmpGroup.reanal, alternative = 'two.sided')
        } else {
          wilcoxontest_results <- data.frame()
        }
        li.wilcoxontest.reanal[[tmpGroup.reanal]] <- wilcoxontest_results
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": GSEA"))



        #### pathway analysis ####
        set.seed(seed_value)
        if ( length(unique(na.omit(df_sampleinfo_reanal_filt()[,tmpGroup.reanal]))) == 2 ) {
          gsea_results <- olink_pathway_enrichment(data = npxtmp,
                                                   test_results = ttest_results,
                                                   organism = tolower(input$speciesInput)
                                                   )
        } else {
          gsea_results <- data.frame()
        }
        li.gsea.reanal[[tmpGroup.reanal]] <- gsea_results
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": ANOVA"))



        #### one-way ANOVA ####
        if ( length(unique(na.omit(df_sampleinfo_reanal_filt()[,tmpGroup.reanal]))) > 2 ) {
          onewayanova_results <- olink_anova(df = npxtmp, variable = tmpGroup.reanal)
        } else {
          onewayanova_results <- data.frame()
        }
        li.onewayanova.reanal[[tmpGroup.reanal]] <- onewayanova_results
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": post-hoc test"))



        #### one-way ANOVA post-hoc (Tukey-Kramer) ####
        if ( length(unique(na.omit(df_sampleinfo_reanal_filt()[,tmpGroup.reanal]))) > 2 ) {
          onewayanova_results_signifcant <- onewayanova_results %>% filter(Threshold=="Significant") %>% pull(OlinkID)
          if ( length(onewayanova_results_signifcant) > 0 ){
            tukeyKramertest_results <- olink_anova_posthoc(df = npxtmp,
                                                           olinkid_list = onewayanova_results_signifcant,
                                                           variable = tmpGroup.reanal,
                                                           effect = tmpGroup.reanal)
          } else {
            tukeyKramertest_results <- data.frame()
          }
        } else {
          tukeyKramertest_results <- data.frame()
        }
        li.tukeyKramertest.reanal[[tmpGroup.reanal]] <- tukeyKramertest_results
        incProgress(1/inc, detail = paste(tmpGroup.reanal, ": completed"))
      }
    })
    #### Setting data in reactiveVal ####
    react.li.mergedData.panel.reanal(li.mergedData.panel.reanal)
    react.li.pca.reanal(li.pca.reanal)
    react.li.pca_all.reanal(li.pca_all.reanal)
    react.li.umap.reanal(li.umap.reanal)
    react.li.umap_all.reanal(li.umap_all.reanal)
    react.li.ttest.reanal(li.ttest.reanal)
    react.li.wilcoxontest.reanal(li.wilcoxontest.reanal)
    react.li.gsea.reanal(li.gsea.reanal)
    react.li.onewayanova.reanal(li.onewayanova.reanal)
    react.li.tukeyKramertest.reanal(li.tukeyKramertest.reanal)
    
    show_alert(
      title = "Success",
      text = "You can now browse reanalyzed data.",
      type = "success"
    )
    
    # enable buttons
    shinyjs::enable("startReanalysis")
    shinyjs::enable("savePcaplot")
    shinyjs::enable("saveUmapplot")
    shinyjs::enable("saveTwoGroupComp")
    shinyjs::enable("saveVolcanoplot")
    shinyjs::enable("saveBoxplot_twogroup")
    shinyjs::enable("saveClusteringHeatmap")
    shinyjs::enable("savePathwayEnrichment")
    shinyjs::enable("savePathwayHeatmap")
    shinyjs::enable("savePathwayBargraph")
    shinyjs::enable("saveOnewayAnova")
    shinyjs::enable("saveMultipleComp")
    shinyjs::enable("saveBoxplot_multiple")
    
  }
})
set.seed(seed_value)

react.mergedData.panel <- reactive({
  if (tmpPanel() == "All"){
    data <- df.mergedData()
  } else {
    data <- df.mergedData() %>% dplyr::filter(Panel == tmpPanel())
  }
  if (input$modeAnalysis && length(react.li.mergedData.panel.reanal()) > 0) {
    data <- react.li.mergedData.panel.reanal()[[tmpGroup()]][[tmpPanel()]]
  }
  data 
})



df.ttest <- reactive({
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.ttest)) {
      if ( tmpGroup() != "" ) {
        li.ttest[[tmpGroup()]]
      }
    }
  } else {
    if ( tmpGroup() != "" && length_unique_tmpGroup() == 2) {
      react.li.ttest.reanal()[[tmpGroup()]]
    }
  }
})



#### PCA ####
df.pca <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need( # for reanalysis mode 
      length(react.li.ttest.reanal()) == 0 || length(react.li.pca.reanal()) > 0, 
      "Sample size is too small"
    ) %then%
    need(
      length(react.li.pca.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (input$modeAnalysis) {
    react.li.pca.reanal()[[tmpGroup()]][[tmpPanel()]]
  } else {
    li.pca[[tmpGroup()]][[tmpPanel()]]
  }
})
df.pca_all <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need( # for reanalysis mode 
      length(react.li.ttest.reanal()) == 0 || length(react.li.pca_all.reanal()) > 0, 
      "Sample size is too small"
    ) %then%
    need(
      length(react.li.pca_all.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (input$modeAnalysis) {
    react.li.pca_all.reanal()[[tmpGroup()]]
  } else {
    li.pca_all[[tmpGroup()]]
  }
})



#### UMAP ####
df.umap <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need( # for reanalysis mode 
      length(react.li.ttest.reanal()) == 0 || length(react.li.umap.reanal()) > 0, 
      "Sample size is too small"
    ) %then%
    need(
      length(react.li.umap.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.umap)) {
      if ( tmpGroup() != "" ) {
        li.umap[[tmpGroup()]][[tmpPanel()]]
      }
    }
  } else {
    react.li.umap.reanal()[[tmpGroup()]][[tmpPanel()]]
  }
})
df.umap_all <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need( # for reanalysis mode 
      length(react.li.ttest.reanal()) == 0 || length(react.li.umap_all.reanal()) > 0, 
      "Sample size is too small"
    ) %then%
    need(
      length(react.li.umap_all.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.umap_all)) {
      if ( tmpGroup() != "" ) {
        li.umap_all[[tmpGroup()]]
      }
    }
  } else {
    react.li.umap_all.reanal()[[tmpGroup()]]
  }
})



#### ttest ####
df.ttest <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.ttest.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.ttest)) {
      if ( tmpGroup() != "" ) {
        li.ttest[[tmpGroup()]]
      }
    }
  } else {
    if ( tmpGroup() != "" && length_unique_tmpGroup() == 2) {
      react.li.ttest.reanal()[[tmpGroup()]]
    }
  }
})



#### wilcoxontest ####
df.wilcoxontest <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.wilcoxontest.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.gsea)) {
      if ( tmpGroup() != "" ) {
        li.wilcoxontest[[tmpGroup()]]
      }
    }
  } else {
    if ( tmpGroup() != "" && length_unique_tmpGroup() == 2) {
      react.li.wilcoxontest.reanal()[[tmpGroup()]]
    }
  }
})



#### GSEA ####
df.gsea <- reactive({
  validate(
    need(
      tmpGroup() != "",
      "Enter a group name."
    ) %then%
    need(
      length(react.li.gsea.reanal()) > 0 || !input$modeAnalysis,
      check_reanalysis_mode_message
    )
  )
  if (!input$modeAnalysis) {
    if (tmpGroup() %in% names(li.gsea)) {
      if ( tmpGroup() != "" ) {
        li.gsea[[tmpGroup()]]
      }
    }
  } else {
    if ( tmpGroup() != "" && length_unique_tmpGroup() == 2) {
      react.li.gsea.reanal()[[tmpGroup()]]
    }
  }
})



#### ANOVA ####
react.onewayanova <- reactive({
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
  if (!input$modeAnalysis && tmpGroup() != "" ) {
    li.onewayanova[[tmpGroup()]]
  } else {
    react.li.onewayanova.reanal()[[tmpGroup()]]
  }
})



#### post-hoc one-way ANOVA ####
react.tukeyKramertest <- reactive({
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
      length_unique_tmpGroup() > 2,
      twogroup_category_error_message
    )
  )
  if (!input$modeAnalysis && tmpGroup() != "" ) {
    li.tukeyKramertest[[tmpGroup()]]
  } else {
    react.li.tukeyKramertest.reanal()[[tmpGroup()]]
  }
})
