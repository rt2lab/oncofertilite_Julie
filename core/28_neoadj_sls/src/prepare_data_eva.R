library(readxl)
library(dplyr)

Base_de_donnees_v33 <- read_excel("/Users/ahamypet/RT2Lab/comedic_SLS/data/Base de donnees_v33.xlsx", 
                                  na = c("NA","","NC"),
                                  sheet = "Feuil1", col_types = c("text", 
                                                                  "text", "text", "text", "numeric", 
                                                                  "text", "numeric", "date", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "numeric", "text", "text", 
                                                                  "text", "text", "numeric", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "date", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "numeric", 
                                                                  "text", "numeric", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "date", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "text", "text", 
                                                                  "text", "text", "text", "date", "text", 
                                                                  "text", "text", "date", "text", "text", 
                                                                  "date", "text", "text", "date", "text", 
                                                                  "text", "text", "date", "date", "text", 
                                                                  "text")) %>% as.data.frame()
head(Base_de_donnees_v33)
nrow(Base_de_donnees_v33) # 591

Base_de_donnees_v33$IPP
Base_de_donnees_v33$NIP <- as.character(Base_de_donnees_v33$NIP)

# Build subset with comedications only
head(Base_de_donnees_v33)
Base_de_donnees_v33_comedic_only <- Base_de_donnees_v33 %>% select(IPP,NIP,Comédications,DCI, ATC) %>% 
                                                            rename(comedications = Comédications, 
                                                                   comedications_concaten = ATC)
head(Base_de_donnees_v33_comedic_only)

base_sls_v1 <-  read_excel("data/base_sls_v1.xls", 
                           na = c("NA","","NC","NP","np"),
                           col_types = c("text", "text", "date", 
                                         "text", "numeric", "numeric", "text", 
                                         "numeric", "text", "date", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "date", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "date", "text","date", 
                                         "text", "date", "text", "date", "date", 
                                         "text", "text")) %>% as.data.frame()  
nrow(base_sls_v1)
head(base_sls_v1)
base_sls_v1  <- base_sls_v1 %>% filter(!is.na(NIP))
nrow(base_sls_v1) # 782
head(base_sls_v1)

Base_de_donnees_v33$NIP %in% base_sls_v1$NIP
base_sls_v1$NIP %in% Base_de_donnees_v33$NIP

to_complete_eva <-  base_sls_v1 %>% filter(! NIP %in% Base_de_donnees_v33$NIP) %>% # nrow()# 209
                    select(NOM,NIP,`date naissance`)
head(to_complete_eva)

library(readr)
write_excel_csv2(to_complete_eva,  "/Users/ahamypet/RT2Lab/comedic_SLS/data/to_complete_eva.csv")

# Back from sls 
library(readxl)
# Eva did not find : 
library(readxl)
not_found <- read_excel("~/RT2Lab/databases/core/28_neoadj_sls/raw/20210127_to_complete_.xlsx", 
                                     sheet = "to_complete_v1")
head(not_found)
not_found %>% group_by(`Aucune venue ne peut être affichée pour ce patient(soit il n'a aucune venue, soit celles-ci sont supérieures à 10)`) %>% count()

X20210127_to_complete_brut <- read_excel("~/RT2Lab/databases/core/28_neoadj_sls/raw/20210127_to_complete_.xlsx", 
                                     sheet = "to_complete_v1", col_types = c("text", 
                                                                             "text", "date", "text", "numeric", 
                                                                             "text", "numeric", "numeric", "numeric", 
                                                                             "numeric", "text")) %>% slice(-1)
head(X20210127_to_complete_brut)
nrow(X20210127_to_complete_brut)
X20210127_to_complete_brut <- X20210127_to_complete_brut %>% select_if(~!(all(is.na(.)) )) 

X20210127_to_complete_ <- read_excel("~/RT2Lab/databases/core/28_neoadj_sls/raw/20210127_to_complete_.xlsx")
head(X20210127_to_complete_)
nrow(X20210127_to_complete_)

library(stringr)
 X20210127_to_complete_$NIP <- as.character(X20210127_to_complete_$NIP) 
 X20210127_to_complete_$NIP == X20210127_to_complete_brut$NIP
 
 X20210127_to_complete_$comedications_concat <-  str_c(X20210127_to_complete_$comedications1,",", X20210127_to_complete_$comedications2,",",
                                                       X20210127_to_complete_$comedications3,",", X20210127_to_complete_$comedications4,",",
                                                       X20210127_to_complete_$comedications5,",", X20210127_to_complete_$comedications6,",",
                                                       X20210127_to_complete_$comedications7,",", X20210127_to_complete_$comedications8,",",
                                                       X20210127_to_complete_$comedications9,",", X20210127_to_complete_$comedications10,",",
                                                       X20210127_to_complete_$comedications11) 
 X20210127_to_complete_$comedications_concaten <- str_replace(X20210127_to_complete_$comedications_concat,",0","" )
 X20210127_to_complete_$comedications_concaten <- str_replace(X20210127_to_complete_$comedications_concaten,",0","" )
 X20210127_to_complete_$comedications_concaten <- str_replace(X20210127_to_complete_$comedications_concaten,"ths,","" )
 X20210127_to_complete_$comedications_concaten <- str_replace(X20210127_to_complete_$comedications_concaten,",ths","" )
 X20210127_to_complete_$comedications_concaten <- str_replace(X20210127_to_complete_$comedications_concaten,",R05X","" )
 
compil_sls_comedic_208 <- full_join(X20210127_to_complete_brut,X20210127_to_complete_ %>% select(NIP,comedications_concaten)) %>% 
                          as.data.frame() #%>% select(-NOM, -'date naissance')
head(compil_sls_comedic_208)
tail(compil_sls_comedic_208)

# Combine with those previously curated by Clementine.
head(compil_sls_comedic_208)
head(Base_de_donnees_v33_comedic_only)
nrow(compil_sls_comedic_208) # 210
nrow(Base_de_donnees_v33_comedic_only) # 591

Base_de_donnees_v33_comedic_only$NIP %in% compil_sls_comedic_208$NIP
Base_de_donnees_v33_comedic_only$IPP %in% compil_sls_comedic_208$NIP # 2 patients have a match in IPP
compil_sls_comedic_208$NIP %in% Base_de_donnees_v33_comedic_only$NIP

comedic_sls_801_tmp <- bind_rows(Base_de_donnees_v33_comedic_only, compil_sls_comedic_208)
head(comedic_sls_801_tmp)
tail(comedic_sls_801_tmp)

comedic_sls_801 %>% slice(228:238)

comedic_sls_801 <- comedic_sls_801_tmp %>% rename(numdos_other_center            = NIP,
                                              comedications_raw              = comedications,
                                              comedications_dci_concat        = DCI,
                                              comedications_atc_concat       = comedications_concaten)
head(comedic_sls_801)

library(readxl)
fichier_corrige_laeti <- read_excel("data/fichier_corrigé_ laeti.xls", 
                                    col_types = c("numeric", "text", "numeric", 
                                                  "text", "text", "numeric"))
# Some nip in comedic_sls_801 are not ok;
# laetitia corrects the file
head(fichier_corrige_laeti)
comedic_sls_801$NIP_corrected           <- comedic_sls_801$numdos_other_center
comedic_sls_801$colonne_marc_corrected  <- fichier_corrige_laeti$nip_corrected[match(comedic_sls_801$numdos_other_center,fichier_corrige_laeti$numero_orange_correspondant)]
comedic_sls_801$colonne_801_corrected   <-  ifelse( is.na(comedic_sls_801$colonne_marc_corrected),comedic_sls_801$numdos_other_center, comedic_sls_801$colonne_marc_corrected  )
head(comedic_sls_801)
comedic_sls_801 %>% slice(700:800)

comedic_sls_801$numdos_other_center <- comedic_sls_801$colonne_801_corrected
comedic_sls_801 <- comedic_sls_801 %>% select(-NOM,- `date naissance`, -colonne_marc_corrected,- colonne_801_corrected,-NIP_corrected) 
comedic_sls_801 %>% 

head(comedic_sls_801)
comedic_sls_801 %>% filter(duplicated(numdos_other_center) |duplicated(numdos_other_center,fromLast=TRUE))  # 50
comedic_sls_801 <- comedic_sls_801 %>% distinct()
comedic_sls_801 %>% filter(duplicated(numdos_other_center) |duplicated(numdos_other_center,fromLast=TRUE))  # 0
nrow(comedic_sls_801) # 775
comedic_sls <-  comedic_sls_801 # 775

save(comedic_sls, file = "/Users/ahamypet/RT2Lab/comedic_SLS/data/comedic_sls.RData")

# cross control : 
load("/Users/ahamypet/RT2Lab/databases/core/28_neoadj_sls/data/28_neoadj_sls_preprocessed_labels.RData")
load("/Users/ahamypet/RT2Lab/comedic_SLS/data/comedic_sls.RData")

database_preprocessed_labels$numdos_other_center %in% comedic_sls$numdos_other_center  
comedic_sls$numdos_other_center  %in% database_preprocessed_labels$numdos_other_center 

duplicated(database_preprocessed_labels$numdos_other_center)  %>% table() # 7
duplicated(database_preprocessed_labels$numdos_other_center)  %>% table() # 7
database_preprocessed_labels %>% group_by(bilat_bc) %>% count()
duplicated(comedic_sls_801$numdos_other_center)  %>% table() # 26

sls_no_bilat <- database_preprocessed_labels %>% filter(bilat_bc == "No")  %>% select(-NOM)
nrow(sls_no_bilat) # 775
head(sls_no_bilat) # 775

database_preprocessed_labels$numdos_other_center %in% comedic_sls$numdos_other_center  
comedic_sls %>% filter(numdos_other_center %in% database_preprocessed_labels$numdos_other_center) %>% nrow()

save(sls_no_bilat, file = "/Users/ahamypet/RT2Lab/comedic_SLS/data/sls_no_bilat.RData")

sls_no_bilat$numdos_other_center %in% comedic_sls$numdos_other_center  # 775
comedic_sls$numdos_other_center  %in% sls_no_bilat$numdos_other_center  # 775



