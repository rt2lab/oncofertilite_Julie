options(stringsAsFactors=FALSE)
library(tidyverse)

load("/Users/ahamypet/RT2Lab/NEOREP/general/data/processed/neorep_2019.RData")
d1 <- data.frame(numdos7 = as.character(neorep_2019$numdos7))
head(d1)
str(d1)
nrow(d1)

# patient_id

# patient_char 
  ## BRCA
  load("/Users/ahamypet/RT2Lab/NeoBRCA/data/processed/neorep_BRCA1_2.RData")
  head(neorep_BRCA1_2)
  neorep_BRCA1_2_tmp  <- neorep_BRCA1_2 %>% rename(BRCA_status = BRCA_status.x,
                                                   BRCA_mutation_type = TYPE)

  neorep_BRCA1_2_tmp$brca_screen  <- ifelse(is.na(neorep_BRCA1_2_tmp$BRCA_status), 0,1)
  neorep_BRCA1_2_tmp$brca_mut     <- ifelse(neorep_BRCA1_2_tmp$BRCA_status == "No BRCA mutation", 0,1)
  neorep_BRCA1_2_tmp$brca_1_2_mut <- case_when(neorep_BRCA1_2_tmp$BRCA_mutation_type == "BRCA1" ~ 1,
                                               neorep_BRCA1_2_tmp$BRCA_mutation_type == "BRCA2"  ~ 2,
                                               neorep_BRCA1_2_tmp$BRCA_mutation_type == "BRCA1/BRCA2" ~ 3,
                                               neorep_BRCA1_2_tmp$BRCA_status == "No BRCA mutation"~ 4)

  ## Tabac
  load("/Users/ahamypet/RT2Lab/NEOREP/general/data/processed/neo_tabac_part.RData")
  head(neo_tabac_part)
  neo_tabac_part$smoking_3cl <- case_when(neo_tabac_part$tabac_3_classes == "never" ~ 1,
                                          neo_tabac_part$tabac_3_classes == "ever"  ~ 3,
                                          neo_tabac_part$tabac_3_classes == "current" ~ 2)
  neo_tabac_part$smoking    <- case_when(neo_tabac_part$actif_at_diag == "non" ~ 0,
                                          neo_tabac_part$actif_at_diag == "oui"  ~ 1) 

  table(neo_tabac_part$smoking,neo_tabac_part$actif_at_diag,exclude=NULL)

## comedication and comorbidity

  # See preprocessing in Preprocess_neorep_medic_final.r
  load("/Users/ahamypet/RT2Lab/NEOREP/medic_NEOREP/medic_new/data/processed/neorep_medic_final.RData")
  head(neorep_medic_final)
  neorep_medic_final_tmp <-   neorep_medic_final %>%   
              rename(
                comedic                      = comedic,
                comedic_n_nervous_system     = Nervous_system,
                comedic_c_cardiovascular     = Cardiovascular,
                comedic_a_alimentary_metabo  = Alimentary_metabo,
                comedic_h_hormonal_prep      = Hormonal_prep,
                comedic_others               = Others,
                comorbidity_bin                   = comorbidity_bin,
                comor_hypertension_heart_disease  = Hypertension_heart_disease,
                comor_depression_anxiety          = depression_anxiety,
                comor_dyslipidemia                = dyslipidemia,
                comor_diabete                     = diabete,
                comor_ulcere_gastritis            = ulcere_gastritis,
                comor_thyroid_disorders           = thyroid_disorders,
                comor_others_grouped              = Others_coded)

  ##  neoadjuvant_ct_antiher2	
  # cf preprocessing refined_type_NAC_NEOREP_v2.R
  
  # neo_ct_regimen
  load("/Users/ahamypet/RT2Lab/TILs/TILs_all_subtypes/data/processed/neorep_TILs_proto.RData"); head(neorep_TILs_proto)
  neorep_TILs_proto_tmp <- neorep_TILs_proto %>% 
                          mutate(neo_ct_regimen   = case_when(typneo_corrected == "Anthracyclines-taxanes regimens" ~ 1 ,
                                 typneo_corrected == "Anthracyclines based regimens" ~ 2 ,
                                 typneo_corrected == "Taxanes-based regimens" ~ 3 ,
                                 typneo_corrected == "Others" ~ 4 )) 

    # nb_cycles_neo_ct_taxanes	nb_cycles_neo_ct_anthra	
  load("/Users/ahamypet/RT2Lab_archive/embols/data/processed/neorep_2019_typ_NAC_refined_new.RData")
  head(neorep_2019_typ_NAC_refined_new)
  
neorep_2019_typ_NAC_refined_new_tmp <- neorep_2019_typ_NAC_refined_new %>%
                                mutate(neo_ct_sequence = case_when( nb_sequences_neo == 1 ~  1,
                                                                    nb_sequences_neo == 2 ~  2,
                                                                    nb_sequences_neo > 2 ~  3) ,
                                       adj_ct_sequence = nb_sequences_adj,  
                                       nb_cycles_adj_ct = nb_total_cycles_adj) 
head(neorep_2019_typ_NAC_refined_new_tmp)

  # mapping_adjuvant_ct_antiher2

  #  Raf : 
 # adj_ct_sequence = nb_sequences_adj  
  # nb_cycles_adj_ct = nb_total_cycles_adj
  # adj_ct_regimen 
  # nb_cycles_adj_ct_taxanes
  # nb_cycles_adj_ct_anthra
    
# bc_biology & tumor_char_neo
      ## tils 
            # TILs : Preprocess in TILs_merge_three_database_v8.Rmd
              load("/Users/ahamypet/RT2Lab/TILs/TILs_all_subtypes/data/processed/neorep_TILs.RData")
              head(neorep_TILs)
              neorep_TILs$perc_stromal_lymphocyte2
              
              neorep_TILs_tmp <- neorep_TILs %>% 
                                  rename( str_til_perc = perc_stromal_lymphocytes,
                                          it_til_perc  = perc_TIL) %>%
                                  mutate( str_til_perc_postneo = as.integer(perc_stromal_lymphocyte2),
                                          it_til_perc_postneo  = as.integer(perc_TIL2))   
            # in situ : preprocessed in TIls_in_situ_v2
            load("/Users/ahamypet/RT2Lab_archive/collab_Vassili/signatures_Th/data/processed/base_test_charlotte.RData")
            head(base_test_charlotte)
            in_situ_tmp <- base_test_charlotte %>%
                          rownames_to_column(., var = "numdos7")  %>%
                          rename(cis_infiltrant_than     = cis_infiltrant_bin)
                                 # breast_res_insitu  = cis_infiltrant2_bin)

            # LVI : Preprocessed in : 
              load("/Users/ahamypet/RT2Lab/NEOREP/general/data/processed/neo_embols_part.RData")
              head(neo_embols_part)
              neo_embols_part_tmp <- neo_embols_part %>% 
                                      rename( lvi_biop     = embols_biopsie,
                                              lvi_postneo  = embols_sein)   

            #  RCB
            load("/Users/ahamypet/RT2Lab_archive/RCB_validation/data/processed/neorep_RCB.RData")
            head(neorep_RCB)
            neorep_RCB_tmp <- neorep_RCB %>% rename( RCB_index = RCB,
                                                     cis_infiltrant_marick = cis_infiltrant)
            head(neorep_RCB_tmp)
            # We will also take other variables even if not in data dictionary.
            # neorep_RCB_tmp$cis_infiltrant
            # neorep_RCB_tmp$cis_infiltrant2
            # neorep_RCB_tmp$Index_mitotique
            # neorep_RCB_tmp$Index_mitotique2
            # neorep_RCB_tmp$perc_cellules_tumorales
            # neorep_RCB_tmp$perc_cellules_tumorales2

  # Old useful variables
  load("/Users/ahamypet/RT2Lab/NEOREP/general/data/processed/neorep_2019.RData")
  head(neorep_2019)
  neorep_old_var  <-  neorep_2019 %>% select(numdos7,
                                             perc_TIL,perc_TIL2,
                                             perc_stromal_lymphocytes,perc_stromal_lymphocyte2,
                                             # str_TILS_med.f,str_TILS_tert.f,str_TILS_quart.f,
                                             # str_TILS_10_perc,str_TILS_cutoff_60.f,str_TILS_3classes.f,
                                             RCH_2.f, RCH4.f, RCH3.f, RCH5.f,
                                             delder.m , delder2.m, #etader,
                                             delDFS.m , delDFS2.m,DFS,
                                             delay_RFS.m , delay_RFS2.m,
                                             delay_DRFS.m , delay_DRFS2.m,
                                             delay_os, delay_os.m,
                                             delay_os2, delay_os2.m,
                                             tabac_3_classes)
            
            
            
# events_and_censor (all integrated in update_survival_preprocessing_delays.r )
            load("/Users/ahamypet/RT2Lab/NEOREP/general/data/processed/neorep_2019.RData")
            head(neorep_2019)
            
            neorep_new_tmp <-  neorep_2019 %>% 
                 rename(#ev_recloc     = status_rec,
                        #dat_recloc    = date_rec_actu,
                        # ev_recreg     = NA, a revoir plus tard; (sont dans les locales...)
                        # dat_recreg    = NA,
                        ev_meta       = status_met,
                        dat_meta      = date_met_actu,
                        ev_contro     = status_contro,
                        dat_contro    = date_contro_actu,
                        ev_secondk    = status_deuk,
                        dat_secondk   = date_deuk_actu, 
                        status_vital  = vital_status,
                        # cause_death   = NA à revoir plus tard car n'a pas été actualisé 
                        dat_last_news =   date_der_actu)  %>% mutate(ev_recloc  = 0,
                                                                     dat_recloc = as.Date(NA),
                                                                     ev_recreg  = 0, 
                                                                     dat_recreg = as.Date(NA))
head(neorep_new_tmp)            
dim(neorep_new_tmp)            

# distinguish recloc and rec reg            
load("/Users/ahamypet/RT2Lab/databases/core/02_neorep/data/recidives_elodie_gauroy/data/processed/localisation_recidive_neorep.RData")
head(localisation_recidive_neorep)

neorep_new_tmp_2  <- left_join(neorep_new_tmp,localisation_recidive_neorep)
head(neorep_new_tmp_2)
dim(neorep_new_tmp_2)
neorep_new_tmp_2 %>% group_by(srec.factor) %>% count()
# srec.factor                      n
# <fct>                        <int>
# 1 1- mammaire                     63
# 2 2- mammaire + ganglionnaire      5
# 3 3- 3,3- ganglionnaire isolee    25
# 4 4- parietale                    21
# 5 5- parietale + ganglionnaire     5
# 6 NA                            1080

rec_locale     <- c("1- mammaire","2- mammaire + ganglionnaire","4- parietale","5- parietale + ganglionnaire")
rec_regionale  <- c("2- mammaire + ganglionnaire","3- 3,3- ganglionnaire isolee","5- parietale + ganglionnaire")

neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_locale ),"ev_recloc"]        <- 1
neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_locale ),"dat_recloc"]       <- neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_locale ),"date_rec_actu"]
neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_regionale ),"ev_recreg"]     <- 1
neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_regionale ),"dat_recreg"]    <- neorep_new_tmp_2[which(neorep_new_tmp_2$srec.factor %in% rec_regionale ),"date_rec_actu"]
neorep_new_tmp <- neorep_new_tmp_2

# @ASHP @EVA Recheck prog_neo ??       
            
base_satellite_d1 <- left_join(d1, neorep_old_var) %>%
                    left_join(.,neorep_BRCA1_2_tmp    %>% select(numdos7,brca_screen,brca_mut,brca_1_2_mut))%>%
            # left_join(.,neorep_BRCA1_2_tmp    %>% select(numdos7,brca_screen,brca_mut,brca_1_2_mut))%>%
            left_join(.,neo_tabac_part         %>% select(numdos7,smoking_3cl,smoking)  )%>%
            left_join(.,neorep_medic_final_tmp    %>% select(numdos7,comedic,
              comedic_n_nervous_system,comedic_c_cardiovascular,comedic_a_alimentary_metabo,comedic_h_hormonal_prep,comedic_others,
              comorbidity_bin,
              comor_hypertension_heart_disease,comor_depression_anxiety,comor_dyslipidemia,comor_diabete,
              comor_ulcere_gastritis,comor_thyroid_disorders,comor_others_grouped)) %>% 
  
            left_join(.,in_situ_tmp               %>% select(numdos7,cis_infiltrant_than ))%>% # ,dcis_component
            left_join(.,neorep_TILs_proto_tmp     %>% select(numdos7,neo_ct_regimen))%>%
            left_join(.,neorep_2019_typ_NAC_refined_new_tmp   ) %>% 
                      # %>% select(numdos7,
                      #                        nb_cycles_neo_ct_anthra,nb_cycles_neo_ct_taxanes,
                      #                               nb_cycles_neo_ct,nb_sequences_neo,neo_ct_sequence,
                                             # nb_sequences_adj,
                                             # adj_ct_sequence,nb_cycles_adj_ct,nb_total_cycles_adj))%>%
            left_join(.,neorep_TILs_tmp       %>% select(numdos7,str_til_perc,it_til_perc, 
                                                         str_til_perc_postneo, it_til_perc_postneo ))%>%
            left_join(.,neo_embols_part_tmp   %>% select(numdos7,lvi_biop,lvi_postneo))%>%
            left_join(.,neorep_RCB_tmp        %>% select(numdos7,RCB_index,
                                                         cis_infiltrant_marick,cis_infiltrant2,
                                                         Index_mitotique,Index_mitotique2,
                                                         perc_cellules_tumorales,perc_cellules_tumorales2))%>%
            left_join(.,neorep_new_tmp        %>% select(numdos7,ev_recloc,dat_recloc,
                                      ev_recreg,dat_recreg,
                                      ev_meta,dat_meta,ev_contro,dat_contro,ev_secondk,dat_secondk,
                                      status_vital,
                                      # cause_death,
                                      dat_last_news))

base_satellite_d1$cis_infiltrant_than_bin    <- ifelse(base_satellite_d1$cis_infiltrant_than >0,1,0)
base_satellite_d1$cis_infiltrant_marick_bin  <- ifelse(base_satellite_d1$cis_infiltrant_marick >0,1,0)
base_satellite_d1$cis_infiltrant2_bin        <- ifelse(base_satellite_d1$cis_infiltrant2 >0,1,0)
table(base_satellite_d1$cis_infiltrant_bin,base_satellite_d1$cis_infiltrant)
table(base_satellite_d1$cis_infiltrant2_bin,base_satellite_d1$cis_infiltrant2)

# head(base_satellite_d1)
save(base_satellite_d1, file="~/RT2Lab/databases/core/02_neorep/data/base_satellite_d1.RData")

# base_satellite_d1 %>% group_by(dcis_component) %>% count()
# 1199 - 176
# 
# base_satellite_d1 
base_satellite_d1 %>% group_by(neo_ct_sequence) %>% count()
base_satellite_d1 %>% group_by(ev_recreg) %>% count()

 base_satellite_d1 %>% group_by(tabac_3_classes) %>% count()
 base_satellite_d1 %>% group_by(smoking_3cl) %>% count()
 base_satellite_d1 %>% group_by(RCH5.f) %>% count()
 

# base_satellite_d1$RCH5.f

