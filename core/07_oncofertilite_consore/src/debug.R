#This file is intended to help you debugging your mapping-preprocessing
#It will allow you to run the mappinng family var by family var
#And to inspect the results

########################################################################
#Step 1 : Set parameters
opt            <- NULL
opt$db_name    <- "07_oncofertilite_consore" #TODO : the name of your database
opt$class_name <- "MappingOncofertilite" #TODO : the name of your class
opt$output_folder <- ""
Sys.setenv("PROJECT_PATH"="/Users/ahamypet/RT2Lab/databases") #TODO : your PROJECT_PATH
if (opt$output_folder == ""){
  output_folder <- file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data")
} else {
  output_folder <- opt$output_folder
}
########################################################################

########################################################################
#Step 2 : Libraries
library("optparse")
library("readxl")
library("stringr")
library("crayon")
library("lubridate")

########################################################################

########################################################################
#Step 3 : Load files and methods

#General mapping class
source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/mapping.R")) 
#Quality check
source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/quality_check.R")) 
#Your own mapping file
source(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "src/mapping.R")) 
#Load data dictionary
extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv("PROJECT_PATH"),"core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) #Get largest version name
data_dict = read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs", last_version_data_dict), 1)
#Create an object of your mapping class
self = eval(parse(text=paste0(opt$class_name, "()"))) 
########################################################################

########################################################################
#Step 4 : Check the import of your databases
dim(self@database)
head(self@database)
########################################################################

########################################################################
#Step 5 : If relevant, check your mapping initial

#Run mapping initial
self <- mapping_initial(self)

#Check that your database was updated 
dim(self@database) #Example
#TODO : your checking code here
########################################################################

########################################################################
#Step 6 : check your mapping methods
#Option 1 : run all the methods one by one 
#Be careful, you should respect the order in which they are run 
#And update your dataset each time

#Ordered list of methods
list_mapping_methods <- list(
  mapping_patient_id=mapping_patient_id,
  mapping_patient_char=mapping_patient_char,
  mapping_bc_diagnosis=mapping_bc_diagnosis,
  mapping_bc_biology=mapping_bc_biology,
  mapping_surgery=mapping_surgery,
  mapping_treatments_binary=mapping_treatments_binary,
  mapping_neoadj_or_not=mapping_neoadj_or_not,
  mapping_neoadjuvant_ct_antiher2=mapping_neoadjuvant_ct_antiher2,
  mapping_adjuvant_ct_antiher2=mapping_adjuvant_ct_antiher2,
  mapping_treatments=mapping_treatments,
  mapping_tumor_char_surg=mapping_tumor_char_surg,
  mapping_tumor_char_neo=mapping_tumor_char_neo,
  mapping_events_and_censor=mapping_events_and_censor,
  mapping_comedication=mapping_comedication,
  mapping_comorbidity=mapping_comorbidity,
  mapping_fertility_pregnancy_diag = mapping_fertility_pregnancy_diag,
  mapping_fertility_preservation = mapping_fertility_preservation,
  mapping_fertility_after_cancer = mapping_fertility_after_cancer,
  mapping_pregnancy_after_bc = mapping_pregnancy_after_bc
)

#Loop on all your methods
#Each time print a summary of the variables which are returned
#If you are satisfied with the summary type ok, the loop will go to the next method
#Otherwise, if you want to exit the for loop, type exit
#If you identify an error, go and correct your mapping file 
#Be careful to re-source your mapping file each time you do a correction
#(which means to re run line 29)
#Useful function to print variable summary (to run)
print_variable <- function(x,cols){
  #Fin variable type in data dict
  variable_type = data_dict$var_type[data_dict$var == x]
  print(paste0("Variable name : ", x))
  if(length(variable_type) == 0){
    print("Beware, variable not in data dictionnary")
    var_info= summary(as.factor(cols[[x]]))
  }else{
    print(paste0("Variable type (from data dict) : ", variable_type))
    if(variable_type == "character"){
      var_info = summary(as.factor(cols[[x]]))
    }
    if(variable_type %in% c("integer","numeric")){
      var_info = summary(as.numeric(cols[[x]]))
    }
    if(variable_type == "date"){
      var_info = summary(as.Date(cols[[x]]))
    }
  }
  print(var_info)
  print("")
  print("")
  print("")
  return()
}

#Printing variable summary
for (i in 1:length(list_mapping_methods)){
  
  cols = NULL
  method <- list_mapping_methods[[i]]
  method_name <- names(list_mapping_methods[i])
  
  print(paste0("Method name: ", method_name))
  
  tryCatch(
    {
      # try to compute the new variables
      cols <- method(self)  # cols is a list of variables
    },
    error = function(e) {}
  )
  
  printing = lapply(names(cols),function(x) print_variable(x,cols)) 
  stop_or_ok <- readline(prompt="Type ok to proceed to next family var. Type exit to exit : \n")
  if(stop_or_ok == "exit"){break}
  if (!is.null(cols)) {self <- update(self, cols)} #Update the dataset
}

########################################################################


########################################################################
#Step 6 : check your mapping methods
#Option 2 : run only one method

#In this case, use the mapped database (because of dependencies which may exist with previous family var)
load(file.path(output_folder, "base_mapped.RData"), verbose=TRUE)
self <- db

#Run the method your want to test and examine the results
#Example : 
cols = mapping_patient_id(self)
print(cols)
#TODO : Your checking code hereD
########################################################################

# Then run preprocessing
# export PROJECT_PATH="/Users/ahamypet/RT2Lab/databases"    
# cd $PROJECT_PATH;
# Rscript core/00_common/src/main.R --db_name=07_oncofertilite_consore --class_name=MappingOncofertilite --mapping --preprocessing 

