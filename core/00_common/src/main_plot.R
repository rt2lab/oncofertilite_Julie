#####################################################################################
################################ Libraries ##########################################
#####################################################################################
library(ggsci)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(memisc)
library(plotly)
library(survival)
library(survminer)
library("optparse")
library("readxl")
library("stringr")
library("crayon")
library(shiny)
library(shinyjs)
library(processx)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

options = list(
  make_option(c("-d", "--db_name"), action="store", type='character',
              help="Name of the database folder, e.g: 06_implants_seintinelles"),
  make_option(c("-f", "--file_name"), action="store", type='character',
              default="",
              help="Name of the database, optional"),
  make_option(c("-o", "--output_folder"), action="store", type='character',
              default="",
              help="Path to where the plot is exported"),
  make_option(c("-p", "--general_plot"), action="store_true", default=FALSE,
              help="Run the Shiny app to create the general plot"),
  make_option(c("-s", "--sunburst_plot"), action="store_true", default=FALSE,
              help="Run and save a legend version of the sunburst plot only")
)

opt = parse_args(OptionParser(option_list=options))

if (opt$output_folder == ""){
  output_folder <- file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "docs")
} else {
  output_folder <- opt$output_folder
}

if (opt$file_name == ""){
  file_name <- sprintf("%s_preprocessed.RData", opt$db_name)
} else {
  file_name <- opt$file_name
}

#####################################################################################
################################ Parameters #########################################
#####################################################################################
colors_palette <- pal_jama("default")(7)
colors <- c(colors_palette[4],
            colors_palette[1],
            colors_palette[2],
            colors_palette[7],
            colors_palette[6],
            "white")
colors_sunburst = c(colors_palette[1:2],colors_palette[4],colors_palette[6],colors_palette[5])
colors_sunburst <- c("#A50026","#FDAE61","#ABD9E9" , "#74ADD1","#4575B4")

font_size_legend= 3
font_size_title = 5

#####################################################################################
################################ Import data ########################################
#####################################################################################
source(file.path(
  Sys.getenv("PROJECT_PATH"),
  "core/00_common/src/mappingNames.R"))
source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/general_plot_function.R"))


extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv("PROJECT_PATH"),"core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) #Get largest version name
print(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs", last_version_data_dict))
dictionnary = read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs", last_version_data_dict), 1)

load(file.path(
  Sys.getenv("PROJECT_PATH"),
  "core",
  opt$db_name,
  "data",
  file_name))

#####################################################################################
################################ Prepare plot list ##################################
#####################################################################################
d_dictionnary <- as.data.frame(database_preprocessed)
#d_dictionnary <- database_preprocessed[,intersect(dictionnary$var,colnames(database_preprocessed))]
#ncohort <- length(unique(d_dictionnary$numdos_curie))
#nrows <- nrow(d_dictionnary)

#####################################################################################
################################ Launch app #########################################
#####################################################################################
source(file.path(
  Sys.getenv("PROJECT_PATH"),
  "core/00_common/src/app_plot/ui.R"))
source(file.path(
  Sys.getenv("PROJECT_PATH"),
  "core/00_common/src/app_plot/server.R"))
if(opt$general_plot){
  runApp(list(ui=ui,server=server), launch.browser = TRUE)
}

#####################################################################################
###################### Run sunburst plot on its own ################################# 
#####################################################################################
if(opt$sunburst_plot){
  print("Sunburst plot")
  label_ttt <- ggdraw() + draw_label("Treatment path",x=0,hjust=-0.1,colour=colors[2])
  legend_plot_ttt <- sunburst_ttt(d_dictionnary,colors_sunburst,n_seq_max_plot=Inf,height = 15,width = 15)
  file_name = file.path(
    Sys.getenv("PROJECT_PATH"),
    "core",
    opt$db_name,
    "docs",
    sprintf("%s_sunburst_treatment.pdf",opt$db_name))
  plot <- magick::image_read_pdf(file_name)
  plot_ttt <- ggdraw(clip="on") + draw_image(plot)
  sunburst_total <- plot_grid(plot_ttt,legend_plot_ttt,
                              ncol=1,
                              align="h",
                              rel_heights = c(10,1))
  pdf(file.path(output_folder,sprintf("%s_sunburst_plot_legend.pdf", opt$db_name)), paper = "a4r", width=10, height=10)
  print(sunburst_total)
  dev.off()
}
