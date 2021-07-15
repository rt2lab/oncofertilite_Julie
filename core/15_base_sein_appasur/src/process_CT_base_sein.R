process_CT_base_sein <- function(d1,path_mapping_ct){
  magic_file_chemotherapy <- process_magic_file_chemotherapy(d1, read.csv(path_mapping_ct,sep=";",stringsAsFactors = F))
  list_dates_chemo <- process_df_dates_ct(d1)
  df_nb_cy_ct <- process_nb_chemo(d1)
  df_all_chemo <- process_df_all_chemo(d1,list_dates_chemo$df_dates_ct_start, list_dates_chemo$df_dates_ct_end,df_nb_cy_ct,magic_file_chemotherapy)
  df_all_chemo_wide <- process_df_all_chemo_wide(df_all_chemo)
  df_all_chemo_wide_2 <- process_df_all_chemo_wide_2(df_all_chemo_wide)
  return(list(
    df_all_chemo = df_all_chemo,
    df_all_chemo_wide = df_all_chemo_wide,
    df_all_chemo_wide_2 = df_all_chemo_wide_2
  ))
}

process_magic_file_chemotherapy <- function(d1,mapping_ct){
  #########################################################################################################################
  #Create a file to map chemotherapy treatment in free text to a chemotherapy code
  #Input : d1 the sein database ; mapping_ct : the auxilliary mapping file
  #Output : magic_file_chemotherapy
  #########################################################################################################################
  
  #TYPCT : que des NA's pour .f3 

  magic_file_chemotherapy <- d1 %>% 
                             filter(ct==1) %>% 
                             select(starts_with("TYPCT"), neo_ct) %>%
                             gather(key,value,-neo_ct) %>%
                             filter(!is.na(value)) %>% 
                             group_by(neo_ct,key,value) %>% 
                             count() %>% 
                             as.data.frame()
  
  #magic_file_chemotherapy[which(magic_file_chemotherapy$value == "TAXOTERE XÉLODA"),"value"] <- "TAXOTERE XELODA" #1 ligne
  #Pas de XELODA REMAGUS : normal???? 
  #magic_file_chemotherapy[which(magic_file_chemotherapy$value == "XÉLODA REMAGUS"),"value"] <- "XELODA REMAGUS" #0
  
  magic_file_chemotherapy$chemo_code <- mapping_ct[match(magic_file_chemotherapy$value,mapping_ct$value),"code"]

  magic_file_chemotherapy <- magic_file_chemotherapy %>% mutate(chemo_clear = case_when(chemo_code== 1 ~"anthra-taxanes",
                                   chemo_code== 2 ~"anthra",
                                   chemo_code== 3 ~"taxanes",
                                   chemo_code== 4 ~"Others")) %>%
                             mutate(chemotherapy_number = case_when(stringr::str_detect(key,"f1")  ~"chemot_1",
                                    stringr::str_detect(key,"f2")  ~ "chemot_2",
                                    stringr::str_detect(key,"f3")  ~ "chemot_3"))
  return(magic_file_chemotherapy)
}

process_df_dates_ct <- function(d1){
  #########################################################################################################################
  #Process start and end dates of chemotherapy
  #Input : d1 the sein database
  #Output : a list containing df_dates_ct (all chemotherapy dates);
  #                           df_dates_ct_start (start dates of all chemotherapies)
  #                           df_dates_ct_end (end dates of all chemotherapies)
  #########################################################################################################################
  variables_dat_chimio <- colnames(d1)[stringr::str_detect( colnames(d1), paste0 (c("DATDCT","DATFCT") ,collapse = "|"))]
  #Je retire les colonnes qui sont complètement NA (sinon problème de conversion pour les dates)
  variables_dat_chimio = setdiff(variables_dat_chimio,names(which(sapply(d1[,variables_dat_chimio], function(x)all(is.na(x))) == TRUE)))
  all_variable_date_collapse <- paste0(c("DATDCT.","DATFCT."), collapse = "|")
  
  df_dates_ct  <-  d1 %>% 
                   filter(ct==1, !is.na(dat_first_surg)) %>% #3915 patients with chemotherapy and surgery
                   select(numdos_curie,side,one_of(variables_dat_chimio)) %>%
                   pivot_longer( cols = starts_with("DAT"),
                                 names_to = "date_chimio",
                                 values_to = "date") %>%
                   filter(!is.na(date)) %>% #3915 dates of beginning, only 3891 dates of end. 
                   arrange(numdos_curie) %>%
                   mutate(start_or_end      = case_when(stringr::str_detect(date_chimio, "DC") ~"start", stringr::str_detect(date_chimio, "FC") ~"end"),
                          number_chemo_f    = stringr::str_replace(date_chimio, all_variable_date_collapse, ""),
                          numdos_curie_side = paste0(numdos_curie,"_",side))  %>%
                   select(numdos_curie_side,number_chemo_f,start_or_end,date_chimio,date)
  return(list(
    df_dates_ct       = df_dates_ct,
    df_dates_ct_start = df_dates_ct %>% filter(start_or_end=="start"),
    df_dates_ct_end   = df_dates_ct %>% filter(start_or_end=="end")
  ))
}

process_nb_chemo <- function(d1){
  #########################################################################################################################
  #Process number of chemotherapies per patient
  #Input : d1 the sein database
  #Output : a database containing the number of cycles per chemo, patients and side
  #########################################################################################################################
  variables_nbcy_chimio      <- colnames(d1)[stringr::str_detect( colnames(d1), "NBCYCT")]
  all_variable_nbcy_collapse <- paste0(c("DATDCT.","DATFCT."), collapse = "|")

  ###QC
  # View(d1 %>% filter(ct ==1, !is.na(NBCYCT.q1.f2)) %>%
  #        dplyr::select(numdos_curie, one_of(variables_nbcy_chimio), starts_with("TYPCT")))
  #0780096 , 0780833 , 0780888 , 0780924
  #q1 et q2 souvent inversées : grave?? 
  #Pas de raison apparente pour les f2. 
  
  df_nb_chemo <- d1  %>%   filter(ct==1,!is.na(dat_first_surg)) %>% #3915 patients
                   select(numdos_curie,side,one_of(variables_nbcy_chimio)) %>%
                   gather(nbcy_chimio, nb_cycle_each_q, -numdos_curie,-side) %>% #3915 * 25 rows
                   filter(!is.na(nb_cycle_each_q)) %>% #Plus que 3857 patients
  ###Probleme ici : il y a des NBCYCT.q1.f2 (117) et des NBCYCT.q2.f2 (7) = toutes les valeurs sont à 1 dans ce cas. ###
  ###Alors que très peu de 1 pour les autres NBCYCT.q1.f1, ..., NBCYCT.q3.f2
                   mutate(nb_cycle_each_q = as.integer(nb_cycle_each_q)) %>%
  ###De 1 a 60 cycles avec grosse majorite à 6 #Bizarre d'avoir des valeurs si elevées (au dessus de 9)
  ###Valeur a 60 pour 0988621 : erreur car 3 normalement. 
                   arrange(numdos_curie) %>%
                   mutate(number_chemo_q_f  = stringr::str_replace(nbcy_chimio, "NBCYCT.", ""),
                          numdos_curie_side = paste0(numdos_curie,"_",side)) %>%
                   select(-numdos_curie,-side)
  return(df_nb_chemo)
}

process_df_all_chemo <- function(d1,df_dates_ct_start,df_dates_ct_end,df_nb_cy_ct,magic_file_chemotherapy){
  #########################################################################################################################
  #Build the dataframe with all the data on initial chemotherapy
  #We select only the variables relative to patients who had CT, who had surgery,
  #and only variables relative to doses, dates, schedules etc... (No other variable from the data base).
  #We also build :
  #  - the class of chemotherapy, chemo_class : anthra/ anthra-taxanes/ Others/ taxanes
  #  - the xxx of a chemotherapy (@ASHP move to setting?? ), sequence_ct_each_f: adjuvant / neo and adjuvant/ neoadjuvant/ NA
  #  - the sequential type (whether it is composed of one or several sequences), chimio_sequentielle_or_not: "sequential","non sequential"
  #  - dates of start and end; + length of chemo
  #Input : d1 : the sein database,
  #        df_dates_ct_start : start dates of all chemotherapies
  #        df_dates_ct_end  : end dates of all chemotherapies
  #        df_nb_cy_ct      : a database containing the number of cycles per chemo, patients and side
  #        magic_file_chemotherapy : a database mapping chemotherapy treatment in free text to a chemotherapy code
  #Output : df_all_chemo
  #########################################################################################################################
  variables_chimio <- colnames(d1)[stringr::str_detect( colnames(d1), paste0 (c("TYPCT","TYPCTCL"),collapse = "|"))]
  variables_chimio = setdiff(variables_chimio,names(which(sapply(d1[,variables_chimio], function(x)all(is.na(x))) == TRUE)))
  all_variable_chemo          <- c("DATDCT.","TYPCT.","NBCYCT.","TYPCTCL.","DATFCT.")
  all_variable_chemo_collapse <- paste0(all_variable_chemo, collapse = "|")
  all_q_collapse              <- paste0(c("q1.","q2.","q3.","q4.","q5."), collapse = "|")
  
  #Create df_all_chemo
  df_all_chemo     <- d1 %>%
                      filter(ct==1,!is.na(dat_first_surg)) %>% #3915 patients
                      select(numdos_curie,side,neo_ct,dat_first_ct,dat_first_surg, one_of(variables_chimio)) %>% #859 neo_ct among 3915 patients
                      mutate(numdos_curie_side = paste0(numdos_curie,"_",side) ) %>% 
                      select(-side, -numdos_curie) %>%
                      gather(quelle_chimio, typ_chimio_clair,
                             -numdos_curie_side, -neo_ct,-dat_first_ct, -dat_first_surg) %>% #3915*7 rows
                      filter(!is.na(typ_chimio_clair))  %>% #Still 3915 patients, 7058 rows. 
                      mutate(chemo_class = magic_file_chemotherapy$chemo_clear[match(typ_chimio_clair, magic_file_chemotherapy$value)],
                            number_sequence = case_when(stringr::str_detect(quelle_chimio,"q1") ~"q1", 
                                                        stringr::str_detect(quelle_chimio, "q2") ~"q2",
                                                        stringr::str_detect(quelle_chimio, "q3") ~"q3",
                                                        stringr::str_detect(quelle_chimio, "q4") ~"q4",
                                                        stringr::str_detect(quelle_chimio, "q5") ~"q5"),
                             f_or_q_tmp       = stringr::str_replace(quelle_chimio, all_variable_chemo_collapse, ""),
                             number_chemo_q_f = stringr::str_replace(f_or_q_tmp, "L.", ""), 
                             number_chemo_f   = stringr::str_replace(number_chemo_q_f, all_q_collapse, "")) %>% 
                      select(-f_or_q_tmp) %>%
                      arrange(numdos_curie_side,number_chemo_f,number_sequence) %>%
                      left_join(.,df_dates_ct_start %>% select(-start_or_end) ,
                                by = c("numdos_curie_side","number_chemo_f")) %>%
                      rename(date_start = date) %>%
                      left_join(.,df_dates_ct_end %>% select(-start_or_end,-date_chimio),
                                by = c("numdos_curie_side","number_chemo_f")) %>%
                      rename(date_end = date) %>% #47 dates de fin manquantes (attention moins de chimio car plusieurs lignes par chimio)
                      mutate(ct_start_before_surg = ifelse(as.Date(date_start) <= as.Date(dat_first_surg),"yes","no"),
                             ct_end_before_surg   = ifelse(as.Date(date_end) <= as.Date(dat_first_surg),"yes","no"), 
                      ###ct_end_before_surg is NA for chemo where date_end is missing###
                             all_before_surg      = ifelse(ct_start_before_surg =="yes" & ct_end_before_surg =="yes","yes","no"),
                             all_after_surg       = ifelse(ct_start_before_surg =="no" & ct_end_before_surg =="no","yes","no"),
                             both_before_and_after_surg  = ifelse(ct_start_before_surg =="yes" &
                                                                    ct_end_before_surg =="no","yes","no"),
                             length_chemo = as.integer(as.Date(date_end)-as.Date(date_start))) %>%
                      ###Entre 0 et 847 jours. Médiane 106 jours. 
                      ###Que faire des chimio de 0 ou 1 jour?? 
                      left_join(.,df_nb_cy_ct , by = c("numdos_curie_side","number_chemo_q_f"))  %>%
                      #Il y a aussi des chimio qui ne sont pas renseignées dans df_nb_cy_ct (186 lignes, notamment toutes les polychimio).
                      unique() #Pas de duplicats : normal. 

      #Process neo adjuvant and adjuvant
      df_all_chemo$sequence_ct_each_f    <- NA
      df_all_chemo[which(df_all_chemo$all_before_surg == "yes" &
                           df_all_chemo$all_after_surg  == "no" &
                           df_all_chemo$both_before_and_after_surg  == "no" ) ,"sequence_ct_each_f"] <- "neoadjuvant"
      df_all_chemo[which(df_all_chemo$all_before_surg == "no" &
                           df_all_chemo$all_after_surg  == "yes"&
                           df_all_chemo$both_before_and_after_surg  == "no" ) ,"sequence_ct_each_f"] <- "adjuvant"
      df_all_chemo[which(df_all_chemo$all_before_surg == "no" &
                           df_all_chemo$all_after_surg  == "no"&
                           df_all_chemo$both_before_and_after_surg  == "yes" ) ,"sequence_ct_each_f"] <- "neo and adjuvant"
      # All those with ct_start_before_surg == no => are ADJUVANT chemotherapy  
      df_all_chemo[which(df_all_chemo$ct_start_before_surg == "no"  ) ,"sequence_ct_each_f"]        <- "adjuvant" 
      #Il reste 8 NAs : celles qui commencent avant la chirurgie mais date de fin non renseignées. 

      #Error correction 
      #On perd 3 chimios
      #Exemple :
      #0981480_2  TAXOTERE en q1.f1 et q2.f1 même date debut et de fin. Mais pas même nombre de cycles (2 puis 4)
      df_all_chemo    <- df_all_chemo %>% distinct(neo_ct,dat_first_ct,dat_first_surg,
                                                   numdos_curie_side,typ_chimio_clair, chemo_class,
                                                   date_start,date_end,.keep_all = TRUE)
      
      #Add number of chemotherapies
      #Number of f (lines) per patient ==> one for all patients. 
      df_all_chemo_nb_f             <-  df_all_chemo %>%  
                                        select(numdos_curie_side,number_chemo_f) %>% #7155 rows
                                        unique() %>%  
                                        group_by(numdos_curie_side,number_chemo_f) %>% 
                                        summarise(count=n()) %>% 
                                        summarise(nb_f = sum(count)) #130 patientes with f2, 3785 patientes with f1 only. 
      
      #Number of q (sequences) per patient
      df_all_chemo_nb_q_by_sequence <-  df_all_chemo %>%  
                                        select(numdos_curie_side,number_chemo_f,number_sequence) %>% 
                                        unique() %>% #Pas de duplicats à retirer : normal
                                        group_by(numdos_curie_side,number_chemo_f,number_sequence) %>%
                                        summarise(count=n()) %>%  
                                        summarise(nb_q = sum(count)) #3915 #Between 1 and 4, median is 2. 
 
      df_all_chemo                  <- df_all_chemo %>% 
                                       left_join(df_all_chemo_nb_f) %>%
                                       rename(nb_f_for_patient_side = nb_f) %>% 
                                       left_join(df_all_chemo_nb_q_by_sequence) %>%
                                       mutate(chimio_sequentielle_or_not = ifelse( nb_q > 1, "sequential","non sequential") )

      #Error corrections (pourquoi virer les TYPCTCL?) 
      #On vire tous les NIPS avec POLYCHIMIO #Ou alors juste les polychimio???
      #Car fait bugger code d'après (car pas de .q pour ces chimio)
      nip_chimio_TYPCTCL        <- df_all_chemo[stringr::str_detect(df_all_chemo$quelle_chimio,"TYPCTCL"), "numdos_curie_side"]
      df_all_chemo <-  df_all_chemo %>% filter(! numdos_curie_side %in% nip_chimio_TYPCTCL) #Plus que 3860 NIPs
      
      return(df_all_chemo)
}

process_df_all_chemo_wide <- function(df_all_chemo){
  #########################################################################################################################
  #This is the dataframe with each line of chemotherapy in a single row
  #Here, we make a pivot by sequence , meaning that the sequences of a chemotherapy are pivoted from long to wide.
  #Before, we had one row per sequence, and there we have 1 to 5 sequences in a wide format.
  #All sequences q3 to q5 are concatenated in a single column (chemo_q3_q4_q5).
  #Then we build
  #- the drugs regimen for a line (f) , recap_regimen_each_f : anthra anthra-taxanes others taxanes
  #- whether composed of one or several sequences, pluriseq_ct:1_or_2_seq,pluriseq.
  # Input : df_all_chemo : the output of the function process_df_all_chemo
  # Output : df_all_chemo_wide
  #########################################################################################################################
  #Create df_all_chemo_wide
  df_all_chemo_wide <- df_all_chemo %>%
                       select(-nbcy_chimio,-number_chemo_q_f,-quelle_chimio, -date_chimio) %>%
                       pivot_wider(names_from = number_sequence,
                                   values_from = c("nb_cycle_each_q","typ_chimio_clair","chemo_class")  , 
                                   names_sep="_" ) %>%
                       mutate(chemo_q3_q4_q5 = paste (nb_cycle_each_q_q3,typ_chimio_clair_q3)) %>% #Pas de q4 et de q5 dans mon cas
                       select(-nb_cycle_each_q_q3,-typ_chimio_clair_q3,-chemo_class_q3) %>%
                       as.data.frame()
  
    df_all_chemo_wide$sum_nbcy_q1_q2   <- rowSums(df_all_chemo_wide[, c("nb_cycle_each_q_q1","nb_cycle_each_q_q2")],na.rm = TRUE )
    df_all_chemo_wide                  <- df_all_chemo_wide %>%
                        mutate  (nb_cy_q1q2 = paste(nb_cycle_each_q_q1, nb_cycle_each_q_q2, sep = "|"))
    df_all_chemo_wide$nb_cy_q1q2 <- gsub("\\|NA","",df_all_chemo_wide$nb_cy_q1q2)

    df_all_chemo_wide$interval_2cycles <- df_all_chemo_wide$length_chemo  / df_all_chemo_wide$sum_nbcy_q1_q2
    df_all_chemo_wide$class_q1q2       <- paste(df_all_chemo_wide$chemo_class_q1,"-",df_all_chemo_wide$chemo_class_q2)
    #Remarque : on laisse les NA's pour class_q1q2 car utiles lignes suivantes (260 à 265).
    df_all_chemo_wide$typ_chimio_clair_q1q2 <- paste(df_all_chemo_wide$typ_chimio_clair_q1,"-",df_all_chemo_wide$typ_chimio_clair_q2)
    df_all_chemo_wide$typ_chimio_clair_q1q2 <- gsub("\\ - NA","",df_all_chemo_wide$typ_chimio_clair_q1q2)

    #Regroup anthra_taxanes, taxanes, anthra and others
    anthra_taxanes <- c("anthra - taxanes","anthra-taxanes - NA","anthra-taxanes - taxanes","anthra - anthra-taxanes","taxanes - anthra")
    anthra         <- c("NA - anthra","anthra - Others","Others - anthra","anthra - anthra","anthra - NA")
    taxanes        <- c("Others - taxanes","taxanes - taxanes","taxanes - Others","taxanes - NA")
    others         <- c("Others - Others","Others - NA")
    input_regroup <- list(anthra_taxanes,anthra,taxanes,others)
    output_regroup <- c("anthra-taxanes","anthra","taxanes","others")
    df_all_chemo_wide$recap_regimen_each_f <- regroup(df_all_chemo_wide$class_q1q2, input_regroup,output_regroup)
    #Results : 777 anthra, 2862 anthra - taxanes, 116 others, 231 taxanes. 

    #Chemo_q3_q4_q5
    df_all_chemo_wide$chemo_q3_q4_q5 <- ifelse(df_all_chemo_wide$chemo_q3_q4_q5 != "NA NA",df_all_chemo_wide$chemo_q3_q4_q5,NA)
    #Lignes suivantes sont inutiles car par de q4 et q5 dans la base APPASUR2
    #df_all_chemo_wide$chemo_q3_q4_q5 <- gsub("| NA NA ","",df_all_chemo_wide$chemo_q3_q4_q5)
    #df_all_chemo_wide$chemo_q3_q4_q5 <- gsub("| NA NA","",df_all_chemo_wide$chemo_q3_q4_q5)
    #df_all_chemo_wide$chemo_q3_q4_q5 <- gsub("\\||","",df_all_chemo_wide$chemo_q3_q4_q5)
    df_all_chemo_wide$chemo_q3_q4_q5 <- gsub("NA ","",df_all_chemo_wide$chemo_q3_q4_q5)

    #Number of sequences
    df_all_chemo_wide$pluriseq_ct    <- ifelse(is.na(df_all_chemo_wide$chemo_q3_q4_q5), "1_or_2_seq","3 or more sequences")
    #Results : 3925 1 or 2 sequences, 61 3 or more sequences (1 de plus que dans pluriseq_ct (car il doit y avoir des NA's j'imagine)). 
    df_all_chemo_wide[df_all_chemo_wide$nb_q == 1,"pluriseq_ct"] <- "monosequential"
    #1028 mono-sequential
    df_all_chemo_wide[df_all_chemo_wide$nb_q == 2,"pluriseq_ct"] <- "bi-sequential"
    #2898 bi-sequential
    return(df_all_chemo_wide)
}

process_df_all_chemo_wide_2 <- function(df_all_chemo_wide){
  #########################################################################################################################
  #This is the dataframe with a single row for all chemotherapies , given a patient side
  #Here, we make a pivot by lines of chemotherapy , meaning that all the lines of chemotherapy are pivoted from long to wide.
  #Before, we had one row per line, and now we have the 1 or 2 lines of chemotherapy in a wide format.
  #We then create a global summary of:
  #  - the final chemotherapy setting, seq_ct_each_pat_side :   seq_ct_each_pat_side: adjuvant/ neoadjuvant/ neoadjuvant and adjuvant
  #  - the final chemotherapy regimen, regimen_ct_each_pat_side :   anthra/anthra-taxanes/ others/ taxanes
  #Input : df_all_chemo_wide : the output of the function process_df_all_chemo_wide
  #Output : df_all_chemo_wide_2
  #########################################################################################################################

  #Create df_all_chemo_wide_2
  df_all_chemo_wide_2 <- df_all_chemo_wide %>%
                         select(numdos_curie_side,neo_ct, dat_first_ct,dat_first_surg, number_chemo_f,
                               dat_first_surg,date_start,date_end,
                               length_chemo,sequence_ct_each_f,
                               sum_nbcy_q1_q2,nb_cy_q1q2,typ_chimio_clair_q1q2,
                               recap_regimen_each_f,pluriseq_ct) %>%
                         pivot_wider(names_from=number_chemo_f,
                                    values_from = c("date_start","date_end","recap_regimen_each_f",
                                                    "sequence_ct_each_f",
                                                    "sum_nbcy_q1_q2","nb_cy_q1q2","typ_chimio_clair_q1q2",
                                                    "length_chemo","pluriseq_ct"),
                                    names_sep="_" ) %>%  
                         as.data.frame()  %>%
                         mutate(typ_chimio_clair_q1q2_f1f2 = paste(typ_chimio_clair_q1q2_f1, typ_chimio_clair_q1q2_f2, sep="-"), #Pas de f2 dans APPASUR2. 
                               sum_nbcy_q1q2_f1f2 = NA, #Nombre de cycles pas fiables : on met NA partout. 
                               nb_cy_q1q2_f1f2 = paste(nb_cy_q1q2_f1, nb_cy_q1q2_f2, sep="|")) %>% 
                        select(numdos_curie_side, neo_ct, dat_first_ct, dat_first_surg,
                               sequence_ct_each_f_f1,date_start_f1,date_end_f1,recap_regimen_each_f_f1,
                               sum_nbcy_q1_q2_f1,nb_cy_q1q2_f1,typ_chimio_clair_q1q2_f1,length_chemo_f1,pluriseq_ct_f1,
                               sequence_ct_each_f_f2,date_start_f2,date_end_f2,recap_regimen_each_f_f2,
                               sum_nbcy_q1_q2_f2,nb_cy_q1q2_f2,typ_chimio_clair_q1q2_f2,length_chemo_f2,pluriseq_ct_f2,
                               nb_cy_q1q2_f1f2,sum_nbcy_q1q2_f1f2,typ_chimio_clair_q1q2_f1f2) %>%
                        mutate(seq_ct_each_pat_side_tmp = paste(sequence_ct_each_f_f1,sequence_ct_each_f_f2, sep="|"),
                               regimen_ct_each_pat_side_tmp = paste(recap_regimen_each_f_f1,recap_regimen_each_f_f2, sep="|"),
                               pluriseq_ct_f1f2_tmp =  paste(pluriseq_ct_f1,pluriseq_ct_f2, sep="|"),
                               date_start_f1f2 = date_start_f1,
                               date_end_f1f2   = date_end_f1)
  
  df_all_chemo_wide_2[which(!is.na(df_all_chemo_wide_2$date_end_f2)),"date_end_f1f2"] <-
                               df_all_chemo_wide_2[which(!is.na(df_all_chemo_wide_2$date_end_f2)),"date_end_f2"]
  df_all_chemo_wide_2$sum_nbcy_q1q2_f1f2 <-rowSums(df_all_chemo_wide_2[,c("sum_nbcy_q1_q2_f1","sum_nbcy_q1_q2_f2")],
                                                    na.rm=TRUE)

  #Adjuvant or neoadjuvant
  adjuvant      <- c("adjuvant|NA","adjuvant|adjuvant")
  neo_and_adj   <- c("neo and adjuvant|NA","neo and adjuvant|adjuvant","neoadjuvant|neo and adjuvant","neoadjuvant|adjuvant")
  neoadjuvant   <- c("neoadjuvant|NA","neoadjuvant|neoadjuvant")
  input_regroup <- list(adjuvant,neo_and_adj,neoadjuvant)
  output_regroup <- c("adjuvant","neoadjuvant and adjuvant","neoadjuvant")
  df_all_chemo_wide_2$seq_ct_each_pat_side <- regroup(df_all_chemo_wide_2$seq_ct_each_pat_side_tmp, input_regroup,output_regroup)

  #Error corrections
  df_all_chemo_wide_2$nb_cy_q1q2_f1f2            <-  gsub("\\|NA","",df_all_chemo_wide_2$nb_cy_q1q2_f1f2)
  #Ajout Elise : a faire valider par ASHP
  df_all_chemo_wide_2$nb_cy_q1q2_f1f2            <-  gsub("NA\\|","",df_all_chemo_wide_2$nb_cy_q1q2_f1f2)
  #Trick not to remove NAVELBINE
  df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2 <-  gsub("NAVELBINE","titi",df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2) 
  df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2 <-  gsub("-NA","",df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2)
  df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2 <-  gsub("titi","NAVELBINE",df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2) 
  #df_all_chemo_wide_2$typ_chimio_clair_q1q2_f1f2 %>% unique()

  #CT regimen
  anthra_taxanes <- c("anthra-taxanes|anthra","anthra-taxanes|NA","anthra-taxanes|others","anthra-taxanes|taxanes","anthra|taxanes",
                      "others|anthra-taxanes","taxanes|anthra")
  anthra         <- c("anthra|anthra","anthra|NA","anthra|others")
  taxanes        <- c("taxanes|NA","taxanes|others")
  others         <- c("others|NA")
  input_regroup <- list(anthra_taxanes,anthra,taxanes,others)
  output_regroup <- c("anthra-taxanes","anthra","taxanes","others")
  df_all_chemo_wide_2$regimen_ct_each_pat_side  <- regroup(df_all_chemo_wide_2$regimen_ct_each_pat_side_tmp, input_regroup,output_regroup)

  #monosequential" = "1", "bi-sequential"="2", "3 or more sequences"="3
  monosequential  <- c("monosequential|NA")
  bi_sequential   <- c("bi-sequential|NA","monosequential|monosequential")
  plurisequential <- c("bi-sequential|monosequential","3 or more sequences|NA","bi-sequential|bi-sequential",
                       "3 or more sequences|monosequential","monosequential|bi-sequential","bi-sequential|3 or more sequences")
  input_regroup <- list(monosequential,bi_sequential,plurisequential)
  output_regroup <- c("monosequential","bi-sequential","plurisequential")
  df_all_chemo_wide_2$pluriseq_ct_each_pat_side <- regroup(df_all_chemo_wide_2$pluriseq_ct_f1f2_tmp, input_regroup,output_regroup)

  return(df_all_chemo_wide_2)
}

process_df_all_tcibl_adj <- function(d1){
  #########################################################################################################################
  #Process adjuvant tc
  #Input : d1 the sein database
  #Output : df_all_tcibl_adj
  #########################################################################################################################
  
    #847 patients with filled DATDTCIBL.f1
    #827 patient with filled DATFTCIBL.f1 (20 are missing) #est-ce que l'on impute à 1 an la date de fin des ttt?  
    #185 patients with filled DATDCTIBL.f2 
    #160 patients with filled DATFTCIBL.f2 (25 are missing)
    #Check 1: all patients with filled DATFTCIBL.f1 has filled DATDTCIBL.f1 (Done, ok)
    #Check 2: all patients with filled DATFTCIBL.f2 has filled DATDTCIBL.f2 (Done, ok )
    #Check 3: all patients with filled start date .f2 have filled start date .f1 (Done, ok)
    #Check 4: all patients with filled start date .f2 have filled end date .f1 (Done, not ok 6 patients in error "0889483_1" "0984989_1" "1080862_2" "1082419_1" "1117256_2" "1119074_2")
    ########Question : est-ce que l'on doit remplir la date de fin de .f1 avec la date de début de .f2??? #######
    
    d1$numdos_curie_side  <- paste0(as.character(d1$numdos_curie),"_",d1$side)
    df_all_tcibl_adj <- d1 %>% select(numdos_curie_side, dat_first_surg, contains("DATDTCIBL"),contains("DATFTCIBL")) %>%
                        rename(first_surg = dat_first_surg ) %>%
                        pivot_longer( cols = starts_with("DAT"),
                                      names_to = "which_date",
                                      values_to = "date") %>%
                        filter(!is.na(date)) %>% arrange(numdos_curie_side,date) %>%
                        mutate(date_after_surg = ifelse(as.Date(date)> as.Date(first_surg), 1,0)) %>%
                        filter(date_after_surg==1) %>%
                        mutate(antiher2_1year_post_surgery = ifelse(as.Date(date)-as.Date(first_surg)<365,1,0)) %>%
                        distinct(numdos_curie_side, first_surg,date_after_surg,.keep_all = TRUE) %>%
                        rename(dat_first_antiher2_adj = date) %>%
                        select(-which_date,-first_surg, - date_after_surg)
    return(df_all_tcibl_adj)
}

