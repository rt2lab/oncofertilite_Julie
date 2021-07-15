#This file is intended to help you debugging your mapping-preprocessing
#It will allow you to run the mappinng family var by family var
#And to inspect the results

########################################################################
#Step 1 : Set parameters
opt            <- NULL
opt$db_name    <- "15_base_sein_appasur" 
opt$class_name <- "MappingSein" 
opt$output_folder <- ""
Sys.setenv("PROJECT_PATH"="/Users/elisedumas/Code/databases")
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
########################################################################

########################################################################
#Step 3 : Load files and methods

#General mapping class
source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/mapping.R")) 
#Quality check
source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/quality_check.R")) 
#Your own mapping file
source(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "src/mapping.R")) 
#Load data dictionary (newest version)
extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv("PROJECT_PATH"),"core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) #Get largest version name
data_dict = read_excel(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/docs/", last_version_data_dict), 1)
self = eval(parse(text=paste0(opt$class_name, "()"))) #Create an object of your mapping class
########################################################################

########################################################################
#Step 4 : Check the import of your databases
dim(self@database) #10442 * 873
length(unique(self@database$NUMDOS)) #10298 (au départ : 10410 ; 112 patientes retirées car opposition utilisation données)
########################################################################

########################################################################
#Step 5 : If relevant, check your mapping initial

#Run mapping initial
self <- mapping_initial(self)

#Check that your database was updated 
dim(self@database) #10286 *  409
length(unique(self@database$NUMDOS)) # 10286 OK
#Patiens with more than one row : 
#144 bilateral patients. 
#View(self@database %>% group_by(NUMDOS) %>% summarise(count = n()) %>% arrange(desc(count)) %>% filter(count > 1))
#test_to_remove_contro <- self@database %>% filter(numcs == 1) #Ok cela retire bien tous les contro. 
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
      var_info = summary(as.numeric(as.character(cols[[x]])))
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


######














##################################################################

#Save list of nips
#nip_appasur2 <- unique(self@database$numdos_curie)
#save(nip_appasur2, file = file.path(output_folder, "nip_appasur2.RData"))
#write.csv(nip_appasur2, file = file.path(output_folder, "nip_appasur2.csv"),row.names = F)

########################################################################

########################################################################
#Step 6 : check after preprocessing

#Load preprocessed database
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed.RData")), verbose=TRUE)
dim(database_preprocessed)

#Save birth year only for CASD
# appasur_preprocessed_birth_year <- database_preprocessed %>% dplyr::select(numdos_curie,year_birth)
# write.csv(appasur_preprocessed_birth_year , 
#           file.path(output_folder,"base_sein_appasur_preprocessed_birth_year.csv"),
#           row.names = F)

#Check pCR
summary(factor(database_preprocessed$pCR))
summary(factor(database_preprocessed$pcr))
summary(factor(database_preprocessed$pcr_old))
summary(factor(database_preprocessed$breast_res_infiltr_raw))

#Check news preprocessed variables : neorep, appasur1, appasur2, etc.. 
summary(factor(database_preprocessed$is_base_sein)) #Il manque 3 NIPs : pourquoi???
summary(factor(database_preprocessed$is_neorep))
summary(factor(database_preprocessed$is_appasur1))
summary(factor(database_preprocessed$is_appasur2))
summary(factor(database_preprocessed$is_neocheck))
summary(factor(database_preprocessed$is_appasur_snds))
summary(factor(database_preprocessed$is_comedic_snds))
summary(factor(database_preprocessed$is_base_sein_ybcp_florence)) #Ok, everything's fine. 

#Check pCR in function of subtype
pcr_per_subtype <- database_preprocessed %>% 
  filter(!is.na(pcr)) %>%
  group_by(subtype,pcr) %>% 
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent_full = count/sum(count)*100)

#check variable multifocality_clin_histo
summary(database_preprocessed$multifocality_histo)
table(database_preprocessed$multifocality_histo,database_preprocessed$neo_ct, exclude = NULL) #Ok pas de valeurs histo pour neaodj patients.
summary(database_preprocessed$multifocality_clin)
table(database_preprocessed$multifocality_clin,database_preprocessed$neo_ct, exclude = NULL)
summary(database_preprocessed$multifocality_clin_histo) 
database_preprocessed %>% group_by(neo_ct,multifocality_histo,multifocality_clin,multifocality_clin_histo) %>% summarise(count = n()) #Ok, cohrent

#Load preprocessed database with labels
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels.RData")), verbose=TRUE)
dim(database_preprocessed_labels)
summary(database_preprocessed_labels$grade_2cl)
summary(database_preprocessed_labels$histo_2cl)
summary(database_preprocessed_labels$histo_3cl)
summary(database_preprocessed_labels$ev_meta) #Probleme ici car mauvaise lectures des labels dans data dict. 
summary(database_preprocessed_labels$ev_meta_txt)

#Check issues with delay_rfs and delay_rfs_diag
summary(database_preprocessed$delay_rfs)
summary(database_preprocessed$delay_rfs_diag)
summary(database_preprocessed$dat_last_news_censor)
summary(database_preprocessed$dat_censor_database)
summary(database_preprocessed$dat_last_news) #same
#Pourquoi date de rfs négative? #TODO l: check des 3 dossiers
database_preprocessed %>% filter(delay_rfs < 0) %>% select(numdos_curie,dat_last_news_censor,dat_rfs,dat_first_surg, dat_bc_diagnosis, is_appasur1)
# numdos_curie dat_last_news_censor    dat_rfs dat_first_surg dat_bc_diagnosis appasur1
#      0982367           2017-06-20 2010-05-31     2010-07-15       2009-03-09       NA

#Check evol delays (in months, rounded?)
head(database_preprocessed$delay_efs_diag)
head(database_preprocessed$delay_rfs_diag)
head(database_preprocessed$delay_drfs_diag)
head(database_preprocessed$delay_dss_diag)
head(database_preprocessed$delay_os_diag)

head(database_preprocessed$delay_rfs_diag)
head(database_preprocessed$delay_drfs_diag)
head(database_preprocessed$delay_dss_diag)
head(database_preprocessed$delay_os_diag)

#TODO : pourquoi est-ce que les chiffres commentés sont bcp plus élevés. 
summary(factor(database_preprocessed$ev_meta)) #751
summary(factor(database_preprocessed$ev_recloc)) #370
summary(factor(database_preprocessed$ev_recreg)) #153
summary(factor(database_preprocessed$ev_secondk)) #486
summary(factor(database_preprocessed$ev_contro)) #371

#Une personne qui a une date de ev_meta mais qui est ev_meta 0 :
database_preprocessed %>% filter(ev_meta == 0 & !is.na(dat_meta)) %>% select(dat_meta, numdos_curie) #1112667 dat_meta : 15/02/2013
#Une patiente qui a ev_meta = 0 mais une dat_meta (la même que celle d'avant, certainement un mauvais copier-coller); en attente de la réponse d'Aullène.  #Ok erreur modifiée
database_preprocessed %>% filter(ev_recloc == 0 & !is.na(dat_recloc)) %>% select(dat_recloc, numdos_curie)  # Personne
database_preprocessed %>% filter(ev_recreg == 0 & !is.na(dat_recreg)) %>% select(dat_recreg, numdos_curie)  # Personne
database_preprocessed %>% filter(ev_contro == 0 & !is.na(dat_contro)) %>% select(dat_contro, numdos_curie)  # Personne
database_preprocessed %>% filter(ev_secondk == 0 & !is.na(dat_secondk)) %>% select(dat_secondk, numdos_curie)  # Personne

#Check delays
summary(database_preprocessed$delay_diag_to_neo_ct) # -461 to -50 
database_preprocessed_neo_ct =  database_preprocessed %>% filter(neo_ct == 1)
dim(database_preprocessed_neo_ct) #858 #10288-585 = 9430 donc nombre de NA est ok.

head(database_preprocessed$dat_bc_diagnosis)
head(database_preprocessed$dat_first_surg)
head(database_preprocessed$delay_diag_to_surg_day)
head(database_preprocessed$delay_diag_to_surg_month)
summary(database_preprocessed$delay_diag_to_surg_day) 
summary(database_preprocessed$delay_diag_to_surg_month) 
database_preprocessed$numdos_curie[database_preprocessed$delay_diag_to_surg > 24]
#37 mois de délai entre diag et chir (1007573) 
database_preprocessed$dat_bc_diagnosis[database_preprocessed$numdos_curie == "1007573"] #Erreur base sein diagnostic au 15/09/2010 done
#0803259
database_preprocessed$dat_bc_diagnosis[database_preprocessed$numdos_curie == "0803259"] #Diag ok
database_preprocessed$dat_first_surg[database_preprocessed$numdos_curie == "0803259"] #"2010-04-09" #Ok pas d'erreur. 
database_preprocessed[database_preprocessed$delay_diag_to_surg_day < 0, c("numdos_curie","delay_diag_to_surg_day","dat_first_surg","dat_bc_diagnosis")] #Possible si on opère et qu'on se rend compte après que c'est une tumeur en fait? 

head(database_preprocessed_neo_ct$dat_bc_diagnosis)
head(database_preprocessed_neo_ct$dat_first_neo_ct)
head(database_preprocessed_neo_ct$delay_diag_to_neo_ct)
summary(database_preprocessed_neo_ct$delay_diag_to_neo_ct) #Max 7,5 mois
database_preprocessed_neo_ct$numdos_curie[database_preprocessed_neo_ct$delay_diag_to_neo_ct > 5] 
#"0880200"
database_preprocessed$dat_bc_diagnosis[database_preprocessed$numdos_curie == "0880200"] #"2008-01-14"
database_preprocessed$dat_first_neo_ct[database_preprocessed$numdos_curie == "0880200"] #"2008-08-11"
# "1091613" "1203517" Est-ce que c'est bizarre? 
#1107092
database_preprocessed$dat_bc_diagnosis[database_preprocessed$numdos_curie == "1107092"] #"2011-05-04"

head(database_preprocessed_neo_ct$dat_end_neo_ct)
head(database_preprocessed_neo_ct$dat_first_surg)
head(database_preprocessed_neo_ct$delay_end_neo_ct_to_surg)
summary(database_preprocessed_neo_ct$delay_end_neo_ct_to_surg) #Max 7,5 mois
database_preprocessed_neo_ct$numdos_curie[database_preprocessed_neo_ct$delay_end_neo_ct_to_surg > 7] #"0882187"  "0982367" "1091152" "1091422"
#0982367 : plus de 7 mois de délai : complications + rt neo adj. 

database_preprocessed_adj_ct <- database_preprocessed %>% filter(adj_ct == 1)
head(database_preprocessed_adj_ct$dat_first_surg)
head(database_preprocessed_adj_ct$dat_first_adj_ct)
head(database_preprocessed_adj_ct$delay_surg_to_adj_ct)
summary(database_preprocessed_adj_ct$delay_surg_to_adj_ct) #Max 14 mois
database_preprocessed_adj_ct$numdos_curie[database_preprocessed_adj_ct$delay_surg_to_adj_ct > 13] 
#1086120 : 
database_preprocessed_adj_ct$dat_first_surg[database_preprocessed_adj_ct$numdos_curie == "1086120"] #"2010-07-21"
database_preprocessed_adj_ct$dat_first_adj_ct[database_preprocessed_adj_ct$numdos_curie == "1086120"]  #Erreur à corriger : 02/09/2010 et pas 02/09/2011 : done
#1102481 : 
database_preprocessed_adj_ct$dat_first_surg[database_preprocessed_adj_ct$numdos_curie == "1102481"] #"2011-07-27"
database_preprocessed_adj_ct$dat_first_adj_ct[database_preprocessed_adj_ct$numdos_curie == "1102481"] #"2012-09-27" : ok mais c'est de la chimio pour méta pas chimio adjuvante. 

database_preprocessed_adj_ct_rt <- database_preprocessed %>% filter(adj_ct == 1 & rt == 1) #3023 patientes
head(database_preprocessed_adj_ct_rt$dat_end_first_ct)
head(database_preprocessed_adj_ct_rt$dat_first_rt)
head(database_preprocessed_adj_ct_rt$delay_end_first_ct_to_first_rt)
summary(database_preprocessed_adj_ct_rt$delay_end_first_ct_to_first_rt) #-24 mois : probleme avec les neo_rt? 
database_preprocessed_adj_ct_rt$numdos_curie[database_preprocessed_adj_ct_rt$delay_ct_to_rt > 10] 
database_preprocessed_adj_ct_rt$numdos_curie[database_preprocessed_adj_ct_rt$neo_rt == 0 &!is.na(database_preprocessed_adj_ct_rt$delay_ct_to_rt) & database_preprocessed_adj_ct_rt$delay_ct_to_rt <0] #Neo rt == 0
#Exemples : "0704679" "0706992" "0780833"

#0786907 
database_preprocessed_adj_ct_rt$dat_end_adj_ct[database_preprocessed_adj_ct_rt$numdos_curie == "0786907"] #"2008-02-28"
database_preprocessed_adj_ct_rt$dat_first_rt[database_preprocessed_adj_ct_rt$numdos_curie == "0786907"] #"2009-01-15" #En fait c'est la fin des rayons.  NA sur la date?  
#0903620
database_preprocessed_adj_ct_rt$dat_end_adj_ct[database_preprocessed_adj_ct_rt$numdos_curie == "0903620"] #"2009-07-23"
database_preprocessed_adj_ct_rt$dat_first_rt[database_preprocessed_adj_ct_rt$numdos_curie == "0903620"] #"2010-05-27"
#9485504
database_preprocessed_adj_ct_rt$dat_end_adj_ct[database_preprocessed_adj_ct_rt$numdos_curie == "9485504"] #"2011-03-08"
database_preprocessed_adj_ct_rt$dat_first_rt[database_preprocessed_adj_ct_rt$numdos_curie == "9485504"] #"2012-05-01" => pas de date de début de RT mais date de fin de RT au 06/2011.

database_preprocessed_adj_rt <- database_preprocessed %>% filter(rt == 1) #3023 patientes
head(database_preprocessed_adj_ct_rt$dat_first_surg)
head(database_preprocessed_adj_ct_rt$dat_first_rt)
head(database_preprocessed_adj_ct_rt$delay_surg_to_first_rt_day)
head(database_preprocessed_adj_ct_rt$delay_surg_to_first_rt_month)
summary(database_preprocessed_adj_ct_rt$delay_surg_to_first_rt_day)
summary(database_preprocessed_adj_ct_rt$delay_surg_to_first_rt_month)
database_preprocessed_adj_ct_rt$numdos_curie[database_preprocessed_adj_ct_rt$delay_surg_to_rt > 15]
database_preprocessed_adj_rt$numdos_curie[database_preprocessed_adj_rt$neo_rt == 0 &!is.na(database_preprocessed_adj_rt$delay_surg_to_first_rt) & database_preprocessed_adj_rt$delay_surg_to_first_rt <0] #Neo rt == 0

summary(database_preprocessed$delay_rfs)
database_preprocessed$numdos_curie[database_preprocessed$delay_rfs > 800]
database_preprocessed$dat_last_news[database_preprocessed$numdos_curie == "1112088"]
database_preprocessed$dat_last_news[database_preprocessed$numdos_curie == "1112958"]


########################################################################

#Prepare database for Marcel

load(file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels.RData")), verbose=TRUE)
dim(database_preprocessed_labels)
length(colnames(database_preprocessed_labels))
intersect(colnames(database_preprocessed_labels),data_dict$var)

database_preprocessed_labels_prep <- database_preprocessed_labels %>% 
  dplyr::select(intersect(colnames(database_preprocessed_labels),data_dict$var)) %>%
  dplyr::select(cletri:delay_os) %>%
  dplyr::select(-contains("dat_"))
write.csv(database_preprocessed_labels_prep,
          file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_small.csv")))
save(database_preprocessed_labels_prep,
        file =  file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_small.RData")))

load(file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_deident.RData")), verbose=TRUE)
dim(database_preprocessed_labels_deident)

          