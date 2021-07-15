# rm(list = ls())
library(dplyr)
# Original data coming from file sent by Lisa Belin;  cf mail 9 mars ; 
# Only an agregated file with raw data from neorep; with macro run on it 
##load('~/RT2Lab/NEOREP/general/data/raw/NEOREP20160309.Rdata')##Code ASHP
load("/Users/ahamypet/RT2Lab/databases/core/02_neorep/data/NEOREP20160309.Rdata")
neorep$numdos7		<- formatC(neorep$numdos,width = 7, flag = "0")

source('~/RT2Lab/databases/core/02_neorep/src/1_Corrections_wrong_numdos_NEOREP.R', local = TRUE)
# Correct baseline errors in the raw datas
neorep <- correction_wrong_numdos_neorep(neorep)
head(neorep)
"0887512" %in% neorep$numdos7 
"0887513" %in% neorep$numdos7 # OK , is corrected

source('~/RT2Lab/databases/core/02_neorep/src/2_Corrections_added_in_NEOREP_root.r', local = TRUE)
neorep <- correction_root_neorep(neorep)
neorep$datexam[neorep$numdos7=="0886480"] # "2008-08-19" => ok is the good one

# Add satellites databases
# Preprocessed in rajout_bases_satellites.R
load("/Users/ahamypet/RT2Lab/databases/core/02_neorep/data/base_satellite_d1.RData")
head(base_satellite_d1)

d1                <- left_join(neorep, base_satellite_d1)

# Add more variables, including correction after merge

        ## DCIS pre NAC : dcis_component
              # data absent from initial database
              # 2 sources, one retrieved by than (and stored in base charlotte) : cis_infiltrant_bin
              # 1 reread by Marick on 718 patients cis_infiltrant2_bin : cis_infiltrant_bin
              d1$cis_infiltrant_than_bin  
              d1$cis_infiltrant_marick_bin  
              d1 <- d1 %>% mutate(dcis_component = case_when(!is.na(cis_infiltrant_marick_bin) ~ cis_infiltrant_marick_bin,
                                                              is.na(cis_infiltrant_marick_bin) ~ cis_infiltrant_than_bin))
              d1 %>% group_by(dcis_component,cis_infiltrant_marick_bin,cis_infiltrant_than_bin) %>% count() # Check OK

        ## DCIS post NAC : breast_res_insitu
              # 2 sources, one initiale in database absins
              # 1 reread by Marick on 718 patients cis_infiltrant2_bin
              d1 <- d1 %>% mutate(breast_res_insitu = case_when(!is.na(cis_infiltrant2_bin) ~ cis_infiltrant2_bin,
                                                                 (is.na(cis_infiltrant2_bin) & absins == 0) ~ 1 ,
                                                                 (is.na(cis_infiltrant2_bin) & absins == 1) ~ 0 ))
              d1 %>% group_by(breast_res_insitu,cis_infiltrant2_bin,absins) %>% count() # Check OK
        
      ## Index mitotic pre NAC
              # 2 sources, 
              # one initiale in database absins
              # 1 reread by Marick on 718 patients cis_infiltrant2_bin
              d1 <- d1 %>% mutate(mitotic_index    = case_when(!is.na(Index_mitotique) ~ as.integer(Index_mitotique),
                                                                is.na(Index_mitotique) ~ indexm )  )
              d1 %>% select(indexm,Index_mitotique,mitotic_index) %>% slice (1:50) # Check OK

      ## Index mitotic post NAC
              # data absent from initial database
              # 1 reread by Marick on 718 patients cis_infiltrant2_bin
              # d1$indexm
              d1 <- d1 %>% mutate(mitotic_index_postneo   = Index_mitotique2)

neorep_before_mapping <- d1
save(neorep_before_mapping, file = "~/RT2Lab/databases/core/02_neorep/data/neorep_before_mapping.RData")
                    
neorep_before_mapping %>% group_by(dcis_component) %>% count()
neorep_before_mapping %>% group_by(neo_ct_sequence) %>% count()
