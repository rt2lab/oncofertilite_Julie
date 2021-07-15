# The variable PROJECT_PATH is required,
# e.g: PROJECT_PATH = "/path/to/databases"

library("optparse")
library("readxl")
library("stringr")
library("crayon")
library("tidyverse")
library("readxl")
#library("plyr'")

options(nwarnings = 10000)

options = list(
  make_option(c("-d", "--db_name"), action="store", type='character',
              help="Name of the database folder, e.g: 06_implants_seintinelles"),
  make_option(c("-f", "--file_name"), action="store", type='character', default = 'comedic_generic.RData',
              help="Name of the database containing generic variables of comedications, with file extension. Can be csv, RData or xlsx"),
  make_option(c("-i", "--id_file_name"), action="store", type='character', default = 'id_total.RData',
              help="Name of the database containing id of patients, even patients with no comedication. Can be csv, RData or xlsx"),
  make_option(c("-l", "--long"), action="store_true", default=TRUE,
              help="Create the preprocessed database in long format (one row per patient-comedic)"),
  make_option(c("-w", "--wide"), action="store_true", default=TRUE,
              help="Create the preprocessed database in wide format (one row per patient)"),
  make_option(c("-o", "--output_folder"), action="store", type='character',
              default="",
              help="Path to where the data is exported")
)

opt = parse_args(OptionParser(option_list=options))
if (opt$output_folder == ""){
  output_folder <- file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data")
} else {
  output_folder <- opt$output_folder
}

#Load data dictionary (last version)
extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv("PROJECT_PATH"),"core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) #Get largest version name
data_dict = read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs/", last_version_data_dict), sheet = "comedic_dict")
print(paste0("Loading data dictionnary, version : ",last_version_data_dict))

######################################
# Step 1: Create data in long format #
######################################


if (opt$long){

  message(green("Running long format."))
  
  # Load database related classes
  if(grepl("RData", opt$file_name)){
    #Load RData
    env <- new.env()
    nm <- load(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name), env)[1]
    db <-env[[nm]]
  }else if(grepl("csv", opt$file_name)){
   #Load csv 
    db <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name), sep = ",")
    if(ncol(db) != 2){
      db <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name), sep = ";")
      print(ncol(db))
    }
    if(ncol(db) != 2){
      db <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name), sep = ",", row.names = 1)
    }
    if(ncol(db) != 2){
      db <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name), sep = ";", row.names = 1)
    }
  }else if(grepl("xlsx", opt$file_name)){
    #Load xlsx
    db <- readxl::read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$file_name))
  }
  
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/preprocessing_comedic_long.R"))
  
  #Reorder variables in order data dictionary
  in_data_dict = intersect(data_dict$var, colnames(db))
  db <- db %>% 
    dplyr::select(setdiff(colnames(db),in_data_dict), all_of(in_data_dict))
  
  #Change name database
  assign(paste0(sub(".*?_", "", opt$db_name),"_long") , db)
  # Save database mapped
  ## csv
  write.csv(
    db,
    file.path(
      output_folder,
      sprintf("%s_comedic_long.csv", opt$db_name)
    )
  )
  
  ## RData
  text_to_parse <- paste0("save(",
                          paste0(sub(".*?_", "", opt$db_name),"_long"),
                          ",file=file.path(output_folder,sprintf('%s_comedic_long.RData', opt$db_name)))")
  eval(parse(text = text_to_parse))

}


######################################
# Step 1: Create data in wide format #
######################################

if (opt$wide){
  
  message(green("Running wide format."))
  
  # Load database related classes
  load(file=file.path(output_folder,sprintf("%s_comedic_long.RData", opt$db_name)), verbose = T)
  db <- eval(parse(text = paste0(sub(".*?_", "", opt$db_name),"_long")))
  
  # Load database with total id
  if(grepl("RData", opt$id_file_name)){
    #Load RData
    env <- new.env()
    nm <- load(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name), env)[1]
    id <-env[[nm]]
  }else if(grepl("csv", opt$id_file_name)){
    #Load csv 
    id <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name), sep = ",")
    if(! "base_cletri" %in% colnames(id)){
      id <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name), sep = ";")
      print(ncol(id))
    }
    if(! "base_cletri" %in% colnames(id)){
      id <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name), sep = ",", row.names = 1)
    }
    if(! "base_cletri" %in% colnames(id)){
      id <- read.csv(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name), sep = ";", row.names = 1)
    }
  }else if(grepl("xlsx", opt$id_file_name)){
    #Load xlsx
    id <- readxl::read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data",opt$id_file_name))
  }
  id <- id %>% select(base_cletri) %>% unique()
  
  #Create wide dataset
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/preprocessing_comedic_wide.R"))
  
  #Change name database
  assign(paste0(sub(".*?_", "", opt$db_name),"_wide") , db_wide)
  # Save database mapped
  ## csv
  write.csv(
    db_wide,
    file.path(
      output_folder,
      sprintf("%s_comedic_wide.csv", opt$db_name)
    )
  )
  ## RData
  text_to_parse <- paste0("save(",
                          paste0(sub(".*?_", "", opt$db_name),"_wide"),
                          ",file=file.path(output_folder,sprintf('%s_comedic_wide.RData', opt$db_name)))")
  eval(parse(text = text_to_parse))
  
  assign(paste0(sub(".*?_", "", opt$db_name),"_very_wide") , db_very_wide)
  write.csv(
    db_very_wide,
    file.path(
      output_folder,
      sprintf("%s_comedic_very_wide.csv", opt$db_name)
    )
  )
  
  ## RData
  text_to_parse <- paste0("save(",
                          paste0(sub(".*?_", "", opt$db_name),"_very_wide"),
                          ",file=file.path(output_folder,sprintf('%s_comedic_very_wide.RData', opt$db_name)))")
  eval(parse(text = text_to_parse))
  
}
