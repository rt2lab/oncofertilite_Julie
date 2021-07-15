# Get list with nip no refusal
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/raw/nip_oncofertilite_aullene_no_refusal.RData") # 1366

# Get data preprocessed   
## cf oncofertility_preprocess_NEW_v2
load("/Users/ahamypet/RT2Lab/oncofertilite/oncofertilite_2011_2017/oncofertility_NEW/data/processed/mat_aullene_to_clean_eva.RData")
head(mat_aullene_to_clean_eva)
nrow(mat_aullene_to_clean_eva) # 1382
mat_aullene_to_clean_eva$numdos_curie
mat_aullene_to_clean_eva$numdos_curie [!mat_aullene_to_clean_eva$numdos_curie %in% nip_oncofertilite_aullene_no_refusal]
# [1] "1107764" "1110584" "1203281" "1210878" "1211276" "1214002" "1219261" "1301979" "1308054" "1315828" "1317556" "1401596" "1403916" "1404946"
# [15] "1406600" "1407596" "1407799" "1414472" "1415283" "1416190" "1502631" "1510593" "1520091" "1602776" "1608424" "1618299" "1620473" "1709378"
# [29] "1709907"  
nip_oncofertilite_aullene_no_refusal[!nip_oncofertilite_aullene_no_refusal %in% mat_aullene_to_clean_eva$numdos_curie]
# [1] "1114699" "1202828" "1216387" "1307059" "1318724" "1407846" "1501245" "1504432" "1517106" "1609630" "1615924" "1700364" "1703079"


mat_aullene_to_clean_eva_1366 <- mat_aullene_to_clean_eva %>% #filter(numdos_curie %in% nip_oncofertilite_aullene_no_refusal) %>% 
                                      select(-neo_ct,-adj_ct)  %>% # We remove neo_ct & adj_ct because corrected afterwards by Alice and Victoire
                                      select(-brca_screen , -brca_mut,- brca_1_2_mut,- brca_ongoing)  %>%  # We remove because we add later ongoing brca status
                                      select(numdos_curie : frozen_embryos, reason_no_PF,reason_no_PF_2)
head(mat_aullene_to_clean_eva_1366)
nrow(mat_aullene_to_clean_eva_1366)

# il faudra rajouter les données du nb d'embryons et d'ocvocytes congelés
# load("/Users/ahamypet/RT2Lab/oncofertilite/oncofertilite_2011_2017/oncofertility_NEW/data/processed/mat_pf_and_material.RData")



# Get initial clinical data curated by alice and victoire 
##  cf import_raw_data_from_file_Alice_Victoire
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/data_victoire_alice_clean_to_merge.RData")
mat_aullene_to_clean_eva_1366_with_alice_vic <- left_join(mat_aullene_to_clean_eva_1366,data_victoire_alice_clean_to_merge)
head(mat_aullene_to_clean_eva_1366_with_alice_vic)
mat_aullene_to_clean_eva_1366_with_alice_vic$reason_no_pf3
  
## Reintegrate information in the right variable
### pf_discussion (cf file all doctors.)
# mat_aullene_to_clean_eva_1366_with_alice_vic   <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% 
#                                                             mutate(pf_discussion = case_when(PF_2 == "yes" ~ 1, 
#                                                                   PF_2 == "no" ~ 0,
#                                                                   PF_3 == 1  ~  1  ))    %>% 
#                                                             select(-PF_discussion)
# mat_aullene_to_clean_eva_1366_with_alice_vic %>% group_by(PF_2, PF_3) %>% count()

### Preg / post partum                                                                                                          
mat_aullene_to_clean_eva_1366_with_alice_vic[which(mat_aullene_to_clean_eva_1366_with_alice_vic$preg_diag3 == 1),"preg_dg"]          <- 1
mat_aullene_to_clean_eva_1366_with_alice_vic[which(mat_aullene_to_clean_eva_1366_with_alice_vic$post_partum3 == 1),"post_partum_dg"] <- 1

# Ventilate reasons refusal
mat_aullene_to_clean_eva_1366_with_alice_vic <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% mutate(no_preg_des = case_when(reason_no_PF == 3 ~ 1 ,
                                                                                                                                no_preg_des3 == 1 ~ 1 ))
mat_aullene_to_clean_eva_1366_with_alice_vic <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% mutate(med_refusal = case_when(#reason_no_PF == 3 ~ 1 ,
                                                                                                                                med_refusal3 == 1 ~ 1 ))
mat_aullene_to_clean_eva_1366_with_alice_vic <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% mutate(pat_refusal = case_when(reason_no_PF %in% c(2,3) ~ 1 ,
                                                                                                                                pat_refusal3 == 1 ~ 1 ))
mat_aullene_to_clean_eva_1366_with_alice_vic %>% group_by(no_preg_des, reason_no_PF,no_preg_des3) %>% count()

mat_aullene_to_clean_eva_1366_with_alice_vic <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% 
                                                select(-preg_diag3 ,-post_partum3,- no_preg_des3,
                                                   - med_refusal3 ,-pat_refusal3,- reason_no_pf3)

# Get data on BRCA
## cleaned
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/df_brca_completed.RData")
mat_aullene_to_clean_eva_1366_with_alice_vic <- left_join(mat_aullene_to_clean_eva_1366_with_alice_vic, 
                                                          df_brca_completed %>% select(-brca_ongoing))
mat_aullene_to_clean_eva_1366_with_alice_vic %>% select(brca_mut, brca_1_2_mut) %>% head()
nrow(mat_aullene_to_clean_eva_1366_with_alice_vic)

# Get data on doctors and if they discussed PF => pf_proposal
mat_aullene_to_clean_eva_1366_with_alice_vic %>% group_by(PF_discussion,pf_proposal) %>% count()
mat_aullene_to_clean_eva_1366_with_alice_vic <- mat_aullene_to_clean_eva_1366_with_alice_vic %>% select(-PF_discussion)
head(mat_aullene_to_clean_eva_1366_with_alice_vic)

# Get data on pregnancy and relapse
data_preg_relapse  <- read.csv('~/RT2Lab/databases/core/07_oncofertilite_consore/data/raw/ASHP_export_17_11_2020/Actualisationoncofer_DATA_2020-11-18_1544.csv') %>% 
                      select(-prenom, -nom, -record_id,- dat_birth, 
                             # -fertil_preserv, 
                             -center_fpp,
                             - ivm, -cos, -ovarian_cryopreservation,
                             -frozen_mat_available ,-frozen_oocytes,
                             -frozen_embryos,- any_keyword, -keyword_enceinte,- keyword_accouch,
                             -is_preg_no_rec,- is_rec_no_preg,- dead_2018_do_not_update,
                             -actualisation_oncofertilite_complete,
                             -preg_outcome_preg_4, - dat_start_preg_4,-spontan_art_preg_4,-mat_reuse_preg_4,- comment_preg_4) %>% 
                      mutate(numdos_curie = as.character(numdos_curie)) %>% 
                      filter(refused_data_use == 0) 
head(data_preg_relapse)
data_preg_relapse$numdos_curie <-  formatC(as.integer(data_preg_relapse$numdos_curie),width = 7, flag = "0")
data_preg_relapse %>% group_by(return_center_pf) %>% count()

# Process 
library(lubridate)
data_preg_relapse$dat_start_preg_1 <- as.Date(data_preg_relapse$dat_start_preg_1)
data_preg_relapse$dat_start_preg_2[which( data_preg_relapse$dat_start_preg_2== "01032017")] <- "2017-03-01"
data_preg_relapse$dat_start_preg_2[which( data_preg_relapse$dat_start_preg_2== "10092018")] <- "2018-09-10"
data_preg_relapse$dat_start_preg_2[which( data_preg_relapse$dat_start_preg_2== "19-11-2019")] <- "2019-11-19"
data_preg_relapse$dat_start_preg_2 <- as.Date(data_preg_relapse$dat_start_preg_2)

data_preg_relapse$dat_start_preg_3[which( data_preg_relapse$dat_start_preg_3== "15072019")] <- "2019-07-15"
data_preg_relapse$dat_start_preg_3[which( data_preg_relapse$dat_start_preg_3== "05-09-2018")] <- "2018-09-05"
data_preg_relapse$dat_start_preg_3 <- (ymd(data_preg_relapse$dat_start_preg_3))

head(data_preg_relapse)

data_preg_relapse[which(is.na(data_preg_relapse$status_dfs_actu)
                        & !is.na(data_preg_relapse$ev_recloc)),c("ev_recloc",
                                                                    "ev_recreg",
                                                                    "ev_meta",
                                                                    "ev_contro",
                                                                    "ev_secondk")]
data_preg_relapse[which(is.na(data_preg_relapse$status_dfs_actu)
                        & !is.na(data_preg_relapse$ev_recloc)),c("status_dfs_actu")] <- 0

data_preg_relapse[which(is.na(data_preg_relapse$status_dfs_actu)),]
data_preg_relapse[which(data_preg_relapse$status_dfs_actu == 0),c("ev_recloc",
                                                                  "ev_recreg",
                                                                  "ev_meta",
                                                                  "ev_contro",
                                                                  "ev_secondk")] <- 0

data_preg_relapse[which(data_preg_relapse$status_dfs_actu == 1),c("ev_recloc",
                                                                  "ev_recreg",
                                                                  "ev_meta",
                                                                  "ev_contro",
                                                                  "ev_secondk")] <- data_preg_relapse[which(data_preg_relapse$status_dfs_actu == 1),c("ev_recloc",
                                                                  "ev_recreg",
                                                                  "ev_meta",
                                                                  "ev_contro",
                                                                  "ev_secondk")] %>%   mutate_all(tidyr :: replace_na,0)

head(data_preg_relapse)

data_preg_relapse %>% group_by(ev_recloc) %>% count() # 23 NA
data_preg_relapse %>% group_by(ev_recreg) %>% count() # 23 NA
data_preg_relapse %>% group_by(ev_meta) %>% count() # 8 NA
data_preg_relapse %>% group_by(ev_secondk) %>% count() # 21 NA

data_preg_relapse %>% filter(is.na(ev_recloc))
# numdos_curie 
# 1      1117939
# 2      1209382
# 3      1412599
# 4      1413129
# 5      1614722       

nrow(data_preg_relapse) # 1357

# Fusion 
mat_aullene_to_clean_eva_1366_with_alice_vic$numdos_curie
data_preg_relapse$numdos_curie

aullene_with_preg_relapse <- right_join(mat_aullene_to_clean_eva_1366_with_alice_vic, data_preg_relapse) %>% 
                                        select(-status_dfs_actu,-refused_data_use) %>%  
                                        rownames_to_column( var = "record_id") 

head(aullene_with_preg_relapse)
nrow(aullene_with_preg_relapse) # 1357
write.csv2(aullene_with_preg_relapse, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/raw/aullene_with_preg_relapse.csv",row.names = FALSE) # format OK for redcap?
