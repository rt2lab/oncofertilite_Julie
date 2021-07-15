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
#Step 2 : load preprocessed dataset
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed.RData")), verbose=TRUE)
dim(database_preprocessed)
########################################################################

########################################################################
#Step 3 : select columns
#Save dataset with events for Redcap
base_sein_appasur_event <- database_preprocessed %>% #TODO : change name of database
  dplyr::select(numdos_curie,
                neorep, appasur1, appasur2, base_sein_ybcp_florence,
                ev_prog_neo, dat_prog_neo,
                ev_recloc, dat_recloc,
                ev_recreg, dat_recreg, 
                ev_meta, dat_meta,
                ev_contro, dat_contro,
                ev_secondk, dat_secondk,
                status_vital,cause_death,
                status_dfs,
                dat_last_news,
                dat_last_update, 
                dat_bc_diagnosis
  )
########################################################################

#Death dates export Julien

#Check death_date Curie for Julien's file (23 juin 2020) matches with Curie's death dates from Linda's APPASUR2 export
# consore_death_date <- read_excel(file.path(Sys.getenv("PROJECT_PATH"), 
#                                            "core",
#                                            opt$db_name,
#                                            "data",
#                                            "Rapport_APPASUR2_Date_Décès_22062020.xls"))
# consore_death_date$numdos_curie = as.character(formatC(consore_death_date$NIP,width = 7, flag = "0",format="d"))
# test_death_date_curie_matches <- consore_death_date %>% 
#   dplyr::select(numdos_curie,date_deces_curie) %>% 
#   left_join(base_sein_appasur_event %>% dplyr::select(numdos_curie,status_vital,dat_last_news))
#Case 1 : patients dead in APPASUR2 first export but not in Julien's export.
#test_death_date_curie_matches %>% filter(is.na(date_deces_curie) & status_vital == 1) #10 rows
#Case 2 : patients dead in Julien's export but not in APPASUR2 first export.
#test_death_date_curie_matches %>% filter(!is.na(date_deces_curie) & status_vital == 0) #245 rows
#Case 3 : patients dead in the two databases but with different death dates.
#test_death_date_curie_matches %>% filter(!is.na(date_deces_curie) & status_vital == 1 & dat_last_news != as.Date(date_deces_curie)) #4
#Case 4 : deaths in the two databases match exactly
#test_death_date_curie_matches %>% filter(!is.na(date_deces_curie) & status_vital == 1 & dat_last_news == as.Date(date_deces_curie)) #333
#24th June decision taken with Aullene : creation of the column death_insee : 1 if death insee but not death date_deces_curie for Julien's database. 
#Save 14 NIPS for Case 1 and Case 3 (they are going to be checked manually by Aullène). 
# test_death_date_curie_matches_case1 <- test_death_date_curie_matches %>% filter(is.na(date_deces_curie) & status_vital == 1) %>% mutate(error_type = "Missing death in Julien's database")
# test_death_date_curie_matches_case3 <- test_death_date_curie_matches %>% filter(!is.na(date_deces_curie) & status_vital == 1 & dat_last_news != as.Date(date_deces_curie)) %>% mutate(error_type = "Different death dates")
# test_death_date_curie_matches_error <- rbind(test_death_date_curie_matches_case1,test_death_date_curie_matches_case3)
# write.csv(test_death_date_curie_matches_error,
#           file.path(output_folder,"death_date_curie_matches_error.csv"),
#           row.names = F)

#Difference between Julien's and Thomas' export for Consore's death. 
#Thomas : 
# consore <- read_excel("Code/APPASUR2/data/extraction_20200529/Appasur2_exportBMDX_LA_29052020.xlsx")
# consore$numdos_curie <- as.character(formatC(consore$patient_ipp,width = 7, flag = "0",format="d"))
# consore_thomas <- unique(consore %>% dplyr::select(numdos_curie, statut_deces,statut_matching_insee_deces))
# consore_julien <- consore_death_date <- read_excel(file.path(Sys.getenv("PROJECT_PATH"), 
#                                                              "core",
#                                                              opt$db_name,
#                                                              "data",
#                                                              "Rapport_APPASUR2_Date_Décès_22062020.xls"))
# consore_julien$numdos_curie = as.character(formatC(consore_julien$NIP,width = 7, flag = "0",format="d"))
# consore_julien <- consore_julien %>% dplyr::select(numdos_curie, date_deces_insee,level_insee)
# test_consore_julien_thomas <- consore_julien %>% left_join(consore_thomas)
#Attention : pas mal de status et surtout de level insee ne correspondent pas entre les deux bases de donnnées;
#A signaler a Aullene mais pour l'instant on considere que l'export de Julien est le plus fiable.

########################################################################
#Step 4 : Add death dates from CONSORE
consore_death_date <- read_excel(file.path(Sys.getenv("PROJECT_PATH"),
                                           "core",
                                           opt$db_name,
                                           "data",
                                           "Rapport_APPASUR2_Date_Décès_22062020.xls"))
consore_death_date <- consore_death_date %>%
  mutate(numdos_curie = as.character(formatC(consore_death_date$NIP,width = 7, flag = "0",format="d")),
         death_insee = ifelse(!is.na(date_deces_insee) & is.na(date_deces_curie), 1, NA)) %>%
  dplyr::select(numdos_curie, death_insee, level_insee)
#Number of 1 if column death_insee : 576
#summary(factor(consore_death_date$level_insee))
# 1     2     3    4    5    6  NA's 
# 176  764   10   54   25    5  9264 
base_sein_appasur_event <- base_sein_appasur_event %>% left_join(consore_death_date)
########################################################################

########################################################################
#Step 4 : Change date format for REDCAP
base_sein_appasur_event$dat_prog_neo <- format(as.Date(base_sein_appasur_event$dat_prog_neo, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_recloc <- format(as.Date(base_sein_appasur_event$dat_recloc, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_recreg <- format(as.Date(base_sein_appasur_event$dat_recreg, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_meta <- format(as.Date(base_sein_appasur_event$dat_meta, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_contro <- format(as.Date(base_sein_appasur_event$dat_contro, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_secondk <- format(as.Date(base_sein_appasur_event$dat_secondk, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_last_news <- format(as.Date(base_sein_appasur_event$dat_last_news, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_last_update <- format(as.Date(base_sein_appasur_event$dat_last_update, "%Y-%m-%d"),"%d/%m/%Y")
#base_sein_appasur_event$dat_last_news_actu <- format(as.Date(base_sein_appasur_event$dat_last_news_actu, "%Y-%m-%d"),"%d/%m/%Y")
base_sein_appasur_event$dat_bc_diagnosis <- format(as.Date(base_sein_appasur_event$dat_bc_diagnosis, "%Y-%m-%d"),"%d/%m/%Y")
########################################################################

########################################################################
#Step 5 : Check error meta nips
load(file = paste0("/Users/elisedumas/Code/SNDS_APPASUR1/","/data/preprocessed/base_sein_appasur_error_numdos", ".RData"))
base_sein_appasur_event$nip_error_meta <- factor(ifelse(base_sein_appasur_event$numdos_curie %in% nip_error_meta, 1, NA))
########################################################################

########################################################################
#Step 6 : Sorted nips .Add number of row as a column record_id
sorted <- read.csv(file.path(output_folder,"base_sein_appasur_event_sorted_raw.csv"), row.names = 1,colClasses = "character" ) %>% dplyr::select(numdos_curie)
sorted_dat_diag <- sorted %>% 
  left_join(base_sein_appasur_event) %>%
  tibble::rownames_to_column(var = "record_id")
#Comment elise 24/07/2020 : on laisse la patiente de 2012 dans l'actualisation même si elle a été retirée de l'étude appasur2 car trop complique a supprimer. 

#save(base_sein_appasur_event,file = file.path(output_folder,"base_sein_appasur_event.RData"))
#write.csv(base_sein_appasur_event,file.path(output_folder,"base_sein_appasur_event.csv")) 
#Ajouter record_id pour numero de ligne et virer export automatique des numeros de ligne par R.

write.csv(sorted_dat_diag,
          file.path(output_folder,"base_sein_appasur_event_sorted.csv"),
          row.names = FALSE)


########################################################################
#Process 2 : Step 1 
########################################################################
#Create a column in Curationbaseseinappa_DATA_2020-10-12_1905 with 1 if in NUMDOS matched APPASUR2 in CASD

export_actu_eva_complet = read.csv(paste(Sys.getenv("PROJECT_PATH"),
                                         'core/',
                                         opt$db_name,
                                         '/data',
                                         "Curationbaseseinappa_DATA_2020-10-12_1905.csv", sep="/"))
#10298 * 48

numdos_appasur2_matched = read.csv("/Users/elisedumas/Code/APPASUR2/data/nips_matching_appasur2_casd_after_2009.csv",
                                   row.names = 1)
numdos_appasur2_matched$numdos_curie <- as.character(formatC(as.integer(as.character(numdos_appasur2_matched$NUMDOS )),
                                                             width = 7, flag = "0",format="d"))
#5897* 7

export_actu_eva_complet <- export_actu_eva_complet %>% 
  mutate(appasur2_matched = ifelse(numdos_curie %in% numdos_appasur2_matched$NUMDOS & export_actu_eva_complet$dat_last_update_actu =="", 1 , 0))

write.csv(export_actu_eva_complet,
          file.path(output_folder,"base_sein_appasur_with_matched.csv"),
          row.names = FALSE)

########################################################################
#Process 3 : After all updates, select matched patient only
########################################################################
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed.RData")), verbose=TRUE)
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels.RData")), verbose=TRUE)
load(file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_colnames.RData")), verbose=TRUE)

numdos_appasur2_matched = read.csv("/Users/elisedumas/Code/APPASUR2/data/nips_matching_appasur2_casd_after_2009.csv",
                                   row.names = 1)
numdos_appasur2_matched$numdos_curie <- as.character(formatC(as.integer(as.character(numdos_appasur2_matched$NUMDOS )),
                                                             width = 7, flag = "0",format="d"))

database_preprocessed_matched <- database_preprocessed %>% 
  filter(numdos_curie %in% numdos_appasur2_matched$numdos_curie) # 5892 patients
database_preprocessed_labels_matched <- database_preprocessed_labels %>% 
  filter(numdos_curie %in% numdos_appasur2_matched$numdos_curie) # 5892 patients
database_preprocessed_labels_colnames_matched <- database_preprocessed_labels_colnames %>% 
  filter(numdos_curie %in% numdos_appasur2_matched$numdos_curie) # 5892 patients


#Ok pour le nombre d'events en sortie, ça paraît cohérent. 
summary(factor(database_preprocessed_labels_matched$ev_meta)) #453
summary(factor(database_preprocessed_labels_matched$ev_recloc)) #206
summary(factor(database_preprocessed_labels_matched$ev_recreg)) #99
summary(factor(database_preprocessed_labels_matched$ev_secondk)) #310
summary(factor(database_preprocessed_labels_matched$ev_contro)) #233
#Median follow-up
summary(database_preprocessed_labels_matched$delay_rfs_diag)
#94.3 mois : entre 7 et 8 ans. 

setdiff(numdos_appasur2_matched$numdos_curie,database_preprocessed_matched$numdos_curie) 
#Il manque 5 NUMDOS mais ok car ils ont été supprimés entre temps d'APPASUR2 (CS non incident)

write.csv(database_preprocessed_matched,
          file.path(output_folder, paste0(opt$db_name, "_preprocessed_matched.csv")))
write.csv(database_preprocessed_labels_matched,
          file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_matched.csv")))
write.csv(database_preprocessed_labels_colnames_matched,
          file.path(output_folder, paste0(opt$db_name, "_preprocessed_labels_colnames_matched.csv")))

