library(dplyr)
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/07_oncofertilite_consore_preprocessed_labels.RData")
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/07_oncofertilite_consore_preprocessed_labels_deident.RData")
head(database_preprocessed_labels)
database_preprocessed_labels$delay
database_preprocessed_labels$delay_diag_to_neo_ct
database_preprocessed_labels$amh
database_preprocessed_labels %>% select(bmi) %>% filter(!is.na(bmi)) %>% range()
database_preprocessed_labels %>% filter(bmi == 0.4)  %>% select(numdos_curie,weight_corrected, size, bmi)
database_preprocessed_labels$ct

database_preprocessed_labels$cfa
# database_preprocessed_labels <- database_preprocessed_labels_deident
# head(database_preprocessed_labels_deident)
nrow(database_preprocessed_labels)
database_preprocessed_labels$tclin_old
class(database_preprocessed_labels$tclin)
class(database_preprocessed_labels$tclin)

database_preprocessed_labels$cn
database_preprocessed_labels$ct_setting_5cl

nip_oncofertilite_1357 <- database_preprocessed_labels %>% select(numdos_curie) 
save(nip_oncofertilite_1357, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/nip_oncofertilite_1357.RData")
database_preprocessed_labels %>% select(-contains("old"), - contains ("corrected"), -contains("is_")) %>% head()
database_preprocessed_labels %>% select(contains("cort")) %>% head()
database_preprocessed_labels %>% select(reuse_frozen_cortex) %>% filter(!is.na(reuse_frozen_cortex))

database_preprocessed_labels %>% select(contains("dat")) %>% head()
database_preprocessed_labels %>% select(contains("rfs")) %>% head()
database_preprocessed_labels %>% select(contains("_txt")) %>% head()
database_preprocessed_labels %>% select(status_vital) %>% head()
head(database_preprocessed_labels)

library(dplyr)

base_julie <- database_preprocessed_labels %>% select(numdos_curie:delay_diag_to_surg_month, #) %>% #head()
                                        numdos_curie:nbggpos_postneo,
# base_marcel_oncofertilite <- database_preprocessed_labels %>% select(year_diag:delay_diag_to_surg_month, #) %>% #head()
                                        # numdos_curie:nbggpos_postneo,
                                        status_rfs:status_rfs_txt,delay_rfs,status_vital,  delay_os,
                                        ev_recloc_txt ,ev_recreg_txt ,ev_meta_txt ,ev_contro_txt ,ev_secondk_txt ,status_vital_txt,
                                        center_fpp:frozen_embryos,reuse_frozen_oocytes_nbr:pregnancy_post_art_after_cancer_nbr,
                                        preg_dg:comment_additional_pregnancies) %>% 
                                        select(  - contains("_old"), - contains ("corrected"),- contains ("is_"))
head(base_julie)

# save(base_julie, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/base_julie.RData")
# write.csv2(base_julie, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/base_julie.csv")




"database"                                "numdos_curie"                           
[37] "cletri"                                  "dat_birth"                              
[39] "dat_bc_diagnosis"                        "center_curie"                           
[41] "nb_child"                                "brca_screen"                            
[43] "brca_mut"                                "weight"                                 
[45] "size"                                    "smoking"                                
[47] "bilat_bc"                                "inflammatory_BC"                        
[49] "tclin"                                   "cnuicc_4cl"                             
[51] "muicc"                                   "dat_first_biopsy"                       
[53] "grade_3cl"                               "dat_first_surg"                         
[55] "ct"                                      "dat_first_ct"                           
[57] "neo_ct"                                  "adj_ct"                                 
[59] "nbggpos"                                 "histo_size"                             
[61] "nbggpos_postneo"                         "dat_last_update"                        
[63] "ev_recloc"                               "dat_recloc"        
