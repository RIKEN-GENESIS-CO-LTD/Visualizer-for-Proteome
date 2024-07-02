########################################################################################
# Name:  ProteomeView_v0.9.0
# Created:  2023/11/29
# Last Modified:  2024/04/05
# Version:  0.9.0
# Author: Daiki Shikata
# Description:  Add a brief or detailed description of viewer-related scripts here.
# Change Log:
# - 2023/11/29: Version 0.1.0 - made scripts.
# - 2023/12/08: Version 0.2.0 - changed the display format of viewer to a dashboard format.
# - 2023/12/13: Version 0.3.0 - modified scripts to allow analysis to be run with both statistically analysed and pre-statistical data as input.
# - 2024/01/10: Version 0.4.0 - added function of displaying error messages.
# - 2024/01/26: Version 0.5.0-alpha - Alpha version for projectID: PE_RXXX
# - 2024/03/25: Version 0.8.0 - modified almost all functions.
# - 2024/04/02: Version 0.8.4 - removed R libaray "BH" form library folder.
# - 2024/04/05: Version 0.8.5 - added footer. added version variable. updated args fo updateSelectizeInput().
# - 2024/04/11: Version 0.9.0 - used this version for CSV. This version is scheduled to be released.
#########################################################################################

version <- "v0.9.0"
app_name <- "Proteome View"



options(warn = -1)
################################################################################
#---------------------------------- library -----------------------------------#
packages <- c(
  "shiny",
  "shinyjs", 
  "shinyWidgets",
  "shinyFeedback",
  "shinydashboard", 
  "shinydashboardPlus",
  "tidyr",
  "dplyr", 
  "stringr",
  "DT",
  "ggplot2", 
  "ggrepel", 
  "ggtext", 
  "plotly",
  "pheatmap",
  "ggplotify",
  "OlinkAnalyze"
)

lapply(packages, function(pkg){
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
})



################################################################################
#---------------------------------- function ----------------------------------#

##### this function is used for validate function
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

##### this customized filter function deals with null input
custom_filter <- function(data, ...) {
  if (!is.null(data)) {
    return(dplyr::filter(data, ...))
  } else {
    return(NULL)
  }
}

custom_arrange <- function(data, ...) {
  if (!is.null(data)) {
    return(dplyr::arrange(data, ...))
  } else {
    return(NULL)
  }
}

custom_mutate <- function(data, ...) {
  if (!is.null(data)) {
    return(dplyr::mutate(data, ...))
  } else {
    return(NULL)
  }
}

custom_select <- function(data, ...) {
  if (!is.null(data)) {
    return(dplyr::select(data, ...))
  } else {
    return(NULL)
  }
}



custom_downloadButton <- function(id) {
  tags$div(
    class = "export-button",
    downloadButton(id, label=" Export")
  )
}



custom_boxSidebar <- function (..., id = NULL, width = 50, background = "#333a40", 
                               startOpen = FALSE, icon = shiny::icon("cogs")) {
  stopifnot(width >= 25 && width <= 100)
  toolbarTag <- shiny::tags$button(id = id, `data-background` = background, 
                                   `data-width` = width, `data-widget` = "chat-pane-toggle", 
                                   `data-toggle` = "tooltip", `data-original-title` = "Export", 
                                   `data-start-open` = tolower(startOpen), type = "button", 
                                   icon)
  contentTag <- shiny::tags$div(style = "z-index: 1; height: inherit;", 
                                class = "direct-chat-contacts", shiny::tags$ul(class = "contacts-list", 
                                                                               shiny::tags$li(...)))
  shiny::tagList(toolbarTag, contentTag)
}



################################################################################
#-------------------------------- basic setting -------------------------------#
check_reanalysis_mode_message <- "Warnings: The switch for reanalysis mode is turned on!!\nIf you want to display the default results, turn off the switch. If you want to display reanlyzed results, keep the switch on and click the 'Start reanalysis' button to wait for the reanalysis to complete."
multiple_category_error_message <- "The selected groupset consists of more than 2 groups. It must be exactly 2"
twogroup_category_error_message <- "The selected groupset consists of only 2 groups. It must be more than 2."
ggplot_text_color <- "#737373"
seed_value <- 1
set.seed(seed_value)



################################################################################
#-------------------------------- Data loading --------------------------------#

#### Setting for sample information ####
dir.data <- paste0(dirname(getwd()), "/data/")
tname.sampleInfo <- list.files(path=dir.data, pattern=glob2rx("*_sample_info.rds"), full.names=F)
projectID <- sub("_sample_info.rds", "", tname.sampleInfo)



##### for default mode ####
li.pca <- readRDS(file = paste0(dir.data, projectID, "_pca.rds"))
li.pca_all <- readRDS(file = paste0(dir.data, projectID, "_pca_all.rds"))
li.umap <- readRDS(file = paste0(dir.data, projectID, "_umap.rds"))
li.umap_all <- readRDS(file = paste0(dir.data, projectID, "_umap_all.rds"))
li.ttest <- readRDS(file = paste0(dir.data, projectID, "_dea_ttest.rds"))
li.ttest <- readRDS(file = paste0(dir.data, projectID, "_dea_ttest.rds"))
li.wilcoxontest <- readRDS(file = paste0(dir.data, projectID, "_dea_wilcoxontest.rds"))
li.gsea <- readRDS(file = paste0(dir.data, projectID, "_pathway_analysis_ttest_gsea.rds"))
li.onewayanova <- readRDS(file = paste0(dir.data, projectID, "_table_onewayanova.rds"))
li.tukeyKramertest <- readRDS(file = paste0(dir.data, projectID, "_table_tukeyKramertest.rds"))
df.sampleInfo.default <- readRDS(file = paste0(dir.data, projectID, "_sample_info.rds"))



#### for re-analysis mode ####
tname.npxFile <- list.files(path=dir.data, pattern=paste0(projectID, ".*\\.csv$"), full.names=T)[1]
df.npxData <- read.table(tname.npxFile, header=T, sep=";")
panels <- c(unique(df.npxData$Panel), "All")



#### javascript ####
js_link_render <- '
  function(data, type, row, meta) {
    if (type === "display" && data !== "NT-proBNP" && data !== "NTproBNP") {
      var url;
      var dataArray = data.split("_");
      var linkHTML = "";
      for (var i = 0; i < dataArray.length; i++) {
        var currentData = dataArray[i];
        if (i !== 0) {
          linkHTML += "_";
        }
        if (meta.col === 0){
          url = "https://www.ncbi.nlm.nih.gov/gene/?term=" + currentData;
        } else if (meta.col === 2){
          url = "https://www.uniprot.org/uniprot/" + currentData;
        }
        linkHTML += "<a href=\'" + url + "\' target=\'_blank\'>" + currentData + "</a>";
      }
      return linkHTML;
    } else {
      return data;
    }
  }
'

js_digits_render <- '
  function(data, type, row, meta) {
    if (type === "display" && typeof data === "number") {
      return parseFloat(data).toExponential(2);
    } else {
      return data;
    }
  }
'

js_digits_nonexpo_render <- '
  function(data, type, row, meta) {
    if (type === "display" && typeof data === "number") {
      return parseFloat(data);
    } else {
      return data;
    }
  }
'