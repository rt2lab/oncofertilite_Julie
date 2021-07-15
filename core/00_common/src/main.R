# The variable PROJECT_PATH is required,
# e.g: PROJECT_PATH = "/path/to/databases"

library("optparse")
library("readxl")
library("stringr")
library("crayon")
library("tidyverse")

options(nwarnings = 10000)


options = list(
  make_option(c("-d", "--db_name"), action="store", type='character',
              help="Name of the database folder, e.g: 06_implants_seintinelles"),
  make_option(c("-c", "--class_name"), action="store", type='character',
              help="Name of the database class, e.g: BaseSein"),
  make_option(c("-m", "--mapping"), action="store_true", default=FALSE,
              help="Run the mapping step"),
  make_option(c("-p", "--preprocessing"), action="store_true", default=FALSE,
              help="Run the preprocessing step"),
  make_option(c("-q", "--disable_quality_check"), action="store_true", default=FALSE,
              help="Run the quality check step"),
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

###################
# Step 1: Mapping #
###################

if (opt$mapping){
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/mapping.R"))
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/quality_check.R"))

  message(green("Running mapping."))

  # Load database related classes
  source(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "src/mapping.R"))

  # Create a mapping object
  db = eval(parse(text=paste0(opt$class_name, "()")))

  # load the data dictionary (last version)
  extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv("PROJECT_PATH"),"core","00_common","docs")),
                                        "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
  last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) #Get largest version name
  data_dict = read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs/", last_version_data_dict), 1)
  print(paste0("Loading data dictionnary, version : ",last_version_data_dict))

  # List all the methods
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
    mapping_evol=mapping_evol,
    mapping_comedication=mapping_comedication,
    mapping_comorbidity=mapping_comorbidity,
    mapping_fertility_pregnancy_diag = mapping_fertility_pregnancy_diag,
    mapping_fertility_preservation = mapping_fertility_preservation,
    mapping_fertility_after_cancer = mapping_fertility_after_cancer,
    mapping_pregnancy_after_bc = mapping_pregnancy_after_bc
  )

  # Run the initial mapping before running the other methods
  tryCatch(
    {
      # try to compute the new variables
      db = mapping_initial(db)
    },
    error = function(e) {
      # code ran on error, e.g the function does not exist
    }
  )

  message(yellow("Quality check will be skipped, as requested."))

  # Run all methods
  for (i in 1:length(list_mapping_methods)){
      cols = NULL
      method <- list_mapping_methods[[i]]
      method_name <- names(list_mapping_methods[i])

      tryCatch(
          {
              # try to compute the new variables
              cols <- method(db)  # cols is a list of variables
          },
          error = function(e) {
              # code ran on error, e.g the function does not exist
              warning(yellow(str_interp("[${method_name}] ${e}")))
          }
      )

      # skip if no columns computed
      if (is.null(cols) | length(cols)==0) {
          next
      }

      # Print family var being mapped
      message(green(str_interp("Finished mapping for variable '${method_name}'")))

      # quality check (if not disabled)
      if (!opt$disable_quality_check){

        # mapping_patient_id -> patient_id
        family_var <- substring(method_name, 9)

        # check variable presence and levels
        for (i in 1:length(cols)) {
          # check cols has the same length than the database
          if (!(length(cols[[i]]) == dim(db@database)[1])) {
            stop(red(str_interp("'${names(cols[i])}' column and database do not have the same shape. Stopping.")))
          }

          errors <- quality_check_for_variable(
            row=cols[i],
            var_name=names(cols)[i],
            data_dict=data_dict,
            db_name=opt$db_name,
            family_var=family_var
          )
          # show errors for debugging
          if (!is.null(errors)){
            warning(yellow(str_interp("[Quality Check, ${names(cols[1])}] Warning: ${errors}")))
          }
        }

      }

      # Update the database
      db <- update(db, cols)
  }

  # Save database mapped
  ## csv
  write.csv(
    db@database,
    file.path(
      output_folder,
      sprintf("%s_mapped.csv", opt$db_name)
    )
  )

  ## RData
  save(
    db,
    file=file.path(
      output_folder,
      sprintf("%s_mapped.RData", opt$db_name)
    )
  )
}

#########################
# Step 2: Preprocessing #
#########################

if (opt$preprocessing){
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/preprocessing.R"))

  message(green("Running preprocessing."))

  load(
    file.path(
      output_folder,
      sprintf("%s_mapped.RData", opt$db_name)
    ),
    verbose=TRUE
  )

  db = Preprocessing(
    database=db@database
  )

  # List all the methods
  list_preprocessing_methods <- c(
      preprocessing_patient_id,
      preprocessing_patient_char,
      preprocessing_bc_diagnosis,
      preprocessing_bc_biology,
      preprocessing_surgery,
      preprocessing_treatments_binary,
      preprocessing_neoadj_or_not,
      preprocessing_tumor_char_surg,
      preprocessing_tumor_char_neo,
      preprocessing_events_and_censor,
      preprocessing_settings_and_regimen,
      preprocessing_pre_post_neo,
      preprocessing_delays_pathways,
      preprocessing_evol,
      preprocessing_fertility_pregnancy_diag
  )

  # Run all methods
  for (method in list_preprocessing_methods){
      cols <- method(db)
      
      # prepare a vector for saving the position elements that are saved to NULL, we want to remove them
      to_remove = c()
      for(i in 1:length(cols)){
        if(is.null(cols[[i]]) | length(cols[[i]]) == 0){
          to_remove = c(to_remove, i)
        }
      }
      if(length(to_remove)>0)
        cols = cols[-to_remove]; 

      # if there are columns to add
      if(length(cols) > 0){
        # Update the database
        db = update(db, cols)
      }
  }

  #Reorder columns according to data dict 
  in_data_dict = intersect(data_dict$var, colnames(db@database))
  db@database <- db@database %>% 
    dplyr::select(setdiff(colnames(db@database),in_data_dict), all_of(in_data_dict))
  
  # Save database preprocessed
  ## csv
  write.csv(
    db@database,
    file.path(
      output_folder,
      sprintf("%s_preprocessed.csv", opt$db_name)
    )
  )

  ## RData
  database_preprocessed <- db@database
  save(
    database_preprocessed,
    file=file.path(
      output_folder,
      sprintf("%s_preprocessed.RData", opt$db_name)
    )
  )

  #Save database with labels instead of levels
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/mappingNames.R"))
  database_preprocessed_labels <- mappingNames(database_preprocessed, data_dict,map_var_names = FALSE)
  save(
    database_preprocessed_labels,
    file=file.path(
      output_folder,
      sprintf("%s_preprocessed_labels.RData", opt$db_name)
    )
  )
  
  #Save database with labels instead of levels + complete colnames
  source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/mappingNames.R"))
  database_preprocessed_labels_colnames <- mappingNames(database_preprocessed, data_dict,map_var_names = TRUE)
  save(
    database_preprocessed_labels_colnames,
    file=file.path(
      output_folder,
      sprintf("%s_preprocessed_labels_colnames.RData", opt$db_name)
    )
  )
  
  #Save database short and identified
  print("Creating database short deident")
  database_preprocessed_labels_deident <- database_preprocessed_labels %>%
    #dplyr::select(one_of(data_dict$var)) %>% #Finally we keep all variable iinn deident. 
    dplyr::select(-one_of(data_dict$var[data_dict$deident == "x" & !is.na(data_dict$deident)])) 
  save(
    database_preprocessed_labels_deident,
    file=file.path(
      output_folder,
      sprintf("%s_preprocessed_labels_deident.RData", opt$db_name)
    )
  )
  
  #Save database in Redcap format
  print("Creating database Redcap format")
  database_preprocessed_redcap <- database_preprocessed %>%
    dplyr::select(one_of(data_dict$var)) %>%
    dplyr::mutate(record_id = ifelse(rep("numdos_curie" %in% colnames(database_preprocessed),n()), 
                                           database_preprocessed$numdos_curie, 
                                           database_preprocessed$cletri)) %>%
    dplyr::select(record_id, everything()) %>% #Column record id at the beginning
    dplyr::mutate_all(as.character) %>% 
    replace(.,is.na(.), "")
  write.csv(
    database_preprocessed_redcap,
    file.path(
      output_folder,
      sprintf("%s_preprocessed_redcap.csv", opt$db_name)
    ),
    row.names = FALSE
  )
  
  #Save base_cletri only (if the variable exists) : useful for comedic_mapping.
  print("Save unique base cletri")
  if("base_cletri" %in% colnames(database_preprocessed)){
    database_preprocessed_base_cletri <- database_preprocessed %>%
      dplyr::select(base_cletri) %>% 
      unique()
    save(
      database_preprocessed_base_cletri,
      file=file.path(
        output_folder,
        "id_total.RData"
      )
    )
  }
  
}

print(warnings())

message(green("Finished running."))
