################################
#patients_concomit_split
#################################


#############
# Read data # 
#############

setwd("~/Pro/RT2Lab")
library(haven)
library(readr)
library(readxl)
library (plyr)
library(dplyr)
library(tidyr)


DATA_PATH <- "/Users/beatrizgrandal/Database_curie/Neoaltto/wetransfer-05428a"

DATA_PATH_2 <- "/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/final_v1"

# Funtion NADIR, mapping ATC 
source ("/Users/beatrizgrandal/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_comedications.R")

######################################################################################################################################
#### LOAD drug list ##############################################################################################################
######################################################################################################################################

whole_name_medical        <- read.csv(file.path(DATA_PATH_2,"dataComeds_mapped_whole_name_medical.csv"),sep = ";", row.names = 1)
mapped_first_word_medical <- read.csv(file.path(DATA_PATH_2,"dataComeds_mapped_first_word_medical.csv"),sep = ";", row.names = 1)
not_mapped_medical        <- read.csv(file.path(DATA_PATH_2,"dataComeds_not_mapped_medical.csv"),sep = ";", row.names = 1)
multiple_ATC_unique       <- readxl::read_excel (file.path(DATA_PATH_2,"dataComeds_multiple_ATC_unique.xls"))

#######################################################################################################################################
#### Load list of virgin molecules and medicinal history ##############################################################################
#######################################################################################################################################
patients_concomit <- read_sas(file.path(DATA_PATH, "dataset4_concomitant.sas7bdat"), NULL)  # 7,781 x 3
patients_comorb   <- read_sas(file.path(DATA_PATH, "dataset5_comorbidities.sas7bdat"), NULL)

      #### All dataset with SPLIT, UNIQUE DRUGS 
      # patients_concomit_split   <-read.csv ("/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v1/dataComeds.csv")
      # dim (patients_concomit_split) #[1] 1087   12
      # head(patients_concomit_split)
      #View (patients_concomit_split )

### All dataset with SPLIT, ALL DRUGS 
patients_concomit_split   <-read.csv ("/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds.csv")
dim (patients_concomit_split) #[1] 8810   13
head(patients_concomit_split)
View (patients_concomit_split )

###############################################################################################################################
## Prepare the different forms ( drugs list) of mapping  ########################################################################
###############################################################################################################################

#### Direct molecule mapping 
whole_name_medical <- whole_name_medical %>% filter (is.na(multipleATC)) %>% 
  select(trpt,ATC ) %>% mutate(source ="direct_atc") 

#### First word mapping 
mapped_first_word_medical  <- mapped_first_word_medical %>% 
  filter (is.na(multipleATC)) %>%
  mutate(ATC = ifelse(medical_opinion =="1, Yes", ATC,final_atc_2 )) %>%
  select(trpt,ATC ) %>% mutate(source ="first_word_medical") 

#### Medical mapping
not_mapped_medical  <- not_mapped_medical %>% 
  #filter (is.na(multipleATC)) %>%
  select(trpt, ATC = final_atc_3) %>% mutate(source ="manual_mapped") %>%
  mutate (ATC =gsub (" |\xca", "", ATC))

#### Multiple ATC mapping
  ### Not medical relevant
multiple_ATC_not_relevant  <- multiple_ATC_unique %>% 
  filter (medicinal_relevant == "0, No")  %>%
  #filter (is.na(final_atc_4_without_disease)) %>%
  #mutate(ATC = ifelse(medical_opinion =="1, Yes", ATC,final_atc_2 )) %>%
  select(trpt, ATC = final_atc_4_without_disease ) %>% mutate(source ="multiple_atc_discarded") 

  ### with medical history, to stack
multiple_ATC_disease   <- multiple_ATC_unique %>% 
  filter (medicinal_relevant == "1, Yes" & final_atc_4_without_disease == "NA")  %>%
  #filter (is.na(final_atc_4_without_disease)) %>%
  #mutate(ATC = ifelse(medical_opinion =="1, Yes", ATC,final_atc_2 )) %>%
  select(trpt, ATC = final_atc_4_without_disease ) %>% mutate(source ="multiple_atc_disease") 
  
 ### with medical history, to check
multiple_ATC_disease2   <- multiple_ATC_unique %>% 
  filter (medicinal_relevant == "1, Yes" & final_atc_4_without_disease == "NA")  %>%
  #filter (is.na(final_atc_4_without_disease)) %>%
  #mutate(ATC = ifelse(medical_opinion =="1, Yes", ATC,final_atc_2 )) %>%
  select(trpt, multipleATC, ATC = final_atc_4_without_disease) %>% mutate(source ="multiple_atc_disease") 
   
  # Save multiple ATC to check (41 drugs)
  write.csv (multiple_ATC_disease2, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/final_v1/dataComeds_multiple_ATC_tocheck.csv")

  ### decision by frequency or large group 
multiple_ATC_unique  <- multiple_ATC_unique %>% 
  filter (medicinal_relevant == "1, Yes" & ! (final_atc_4_without_disease == "NA"))  %>%
  #filter (is.na(final_atc_4_without_disease)) %>%
  #mutate(ATC = ifelse(medical_opinion =="1, Yes", ATC,final_atc_2 )) %>%
  select(trpt,  ATC = final_atc_4_without_disease ) %>% mutate(source ="multiple_atc_unique") 



######################################################################################################################################
### Stack all working groups #########################################################################################################
######################################################################################################################################

atc_mapping_molecule <-do.call(rbind,list(whole_name_medical,mapped_first_word_medical, not_mapped_medical,multiple_ATC_disease, multiple_ATC_unique, multiple_ATC_not_relevant ))

  #### Check that there are no molecules repeated by multiple forms of mapping 
#View(atc_mapping_molecule %>% group_by(trpt) %>% filter(n()>1)) #%>% arrange(trpt)
#View (atc_mapping_molecule)


table(atc_mapping_molecule$ATC)
dim(atc_mapping_molecule ) #[1] 1098    3
View (atc_mapping_molecule)

######################################################################################################################################
### Choose ATCs in those molecules with multiple ATCs and medical history ############################################################
######################################################################################################################################
  
  ###### Clean original dataset (multiple repeat molecules per patient)###########
 
 # Unique molecule, the same thing, 2 version :)
unique_molecule  <- patients_concomit[!duplicated(patients_concomit$trpt),]  #[1] 1093    3 
unique_molecule2 <- length(unique(patients_concomit$trpt)) #[1] 1093  
 
 # In many cases repeated drugs per patient 
a <- patients_concomit %>% group_by(pt) %>% dplyr::summarise(count=n()) # 403 x 2
# --> Lot of patients have many medications (up to 125 medications the most) 
#View (a)

 #--> Clean initial dataset, not repeat drug by patient
nrow(distinct(patients_concomit))  #[1] 5833

patients_concomit_not_repeat <- distinct(patients_concomit) # 5,833 x 3
#View (patients_concomit_not_repeat)
write.csv (patients_concomit_not_repeat, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/final_v1/patients_concomit_not_repeat.csv")
a <- patients_concomit_not_repeat %>% group_by(pt) %>% dplyr::summarise(count=n()) 
#View (a)

patients_concomit_not_repeat2 <- patients_concomit %>% distinct(pt,trpt)   # 5,780 x 2
#View (patients_concomit_not_repeat2)
write.csv (patients_concomit_not_repeat2, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/final_v1/patients_concomit_not_repeat2.csv")
b <- patients_concomit_not_repeat2 %>% group_by(pt) %>% dplyr::summarise(count=n()) 
#View (b)


  ## Choose only molecules and NIP with multiple ATC
nip_multiple_ATC <- patients_concomit_not_repeat2 %>% 
  right_join(multiple_ATC_disease2, by = c("trpt" = "trpt"))

dim(nip_multiple_ATC) # [1] 1] 615   5 # times taht 1 treatment with multiple ATC appears
#View(nip_multiple_ATC)
#View (patients_concomit)
head(nip_multiple_ATC)


 # Prepare vector with medical history 
patients_comorb #--> A tibble: 609 x 6
c <- patients_comorb %>% group_by(pt) %>% dplyr::summarise(count=n())  # A tibble: 266 x 2
length(unique(patients_comorb$pt)) #[1] 266
nrow(distinct(patients_comorb))  #[1] 266

patients_comorb <- patients_comorb %>% 
  group_by(pt) %>% 
  mutate(diagpt_one_row = paste0(diagpt, collapse = "/"))  %>% 
  select (pt,diagpt_one_row) #%>% 

patients_comorb2 <- distinct(patients_comorb) # 266 x 2
#View (patients_comorb2)

  ## Merge with medical history
nip_multiple_ATC_disease <- nip_multiple_ATC %>% 
  left_join(patients_comorb2, by = c("pt" = "pt"))

dim (nip_multiple_ATC_disease) #[1] [1] 615   6
#View(nip_multiple_ATC_disease)
head(nip_multiple_ATC_disease)

    # Save to check 41 molecule
  write.csv (nip_multiple_ATC_disease, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/final_v1/dataComeds_multiple_ATC_tocheck_medicalhistory.csv")
dim(nip_multiple_ATC_disease)
head(nip_multiple_ATC_disease)

        
##################################################
### Mapping ATC to BASE DATASET ##################
################################################
    # Choose no repeat drugs
 # dim(patients_concomit_not_repeat2) #5780    2


# *******************************************************************************************************************************************************************
#*******************************************************************************************************************************************************************
#********************************************** USE dataset already SPLIT, and after that, remove REPEAT rows *****************************************************
## LOAD multiple ATC with medical history and already final ATC    

# atc_mapping_molecule2  <- patients_concomit_not_repeat2 %>% 
#   left_join(atc_mapping_molecule, by = c("trpt" = "trpt"))
# 
# dim(atc_mapping_molecule2 )  #[1] 5806    4
# View(atc_mapping_molecule2)


    head(patients_concomit_split)
    patients_concomit_split <- patients_concomit_split %>% select(pt, trpt)
    
    atc_mapping_molecule2  <- patients_concomit_split  %>% 
      left_join(atc_mapping_molecule, by = c("trpt" = "trpt"))
    
    dim(atc_mapping_molecule2 )  #[1] 8849    4
    View(atc_mapping_molecule2)

    # Remove all repeat LINES -
    # TODO Check version B
      #Version 1
    atc_mapping_molecule2 <- distinct(atc_mapping_molecule2)
    nrow (atc_mapping_molecule2) #[1] 6587
    dim (atc_mapping_molecule2)
    #View ( a) #[1] 6587    4
    head(atc_mapping_molecule2)
    class(atc_mapping_molecule2$pt)
    atc_mapping_molecule2$pt <- as.character(atc_mapping_molecule2$pt)
    
    #  #Version 2
    # b  <- atc_mapping_molecule2 %>% distinct(pt,ATC)
    # nrow (b) #[1] 6556
    # dim (b)
    # 
    # # Difference
    # setdiff(a, b)
    
    
  
########################################################################################################################
##  Add ATC with medical history  (multiple ATC , selected from the clinical history ###################################
########################################################################################################################

multiple_atc_unique_with_medical_history <- readxl::read_excel (file.path(DATA_PATH_2,"dataComeds_multiple_ATC_unique_with_medical_history.xlsx"))

#multiple_atc_unique_with_medical_history$final_atc_5
# dim(multiple_atc_unique_with_medical_history)
head(multiple_atc_unique_with_medical_history)
# table(atc_mapping_molecule2$ATC)

atc_mapping_molecule_end  <- atc_mapping_molecule2 %>% 
  left_join(multiple_atc_unique_with_medical_history, by= c("pt", "trpt" )) 
            #by = c("pt" = "pt")) #%>% 
View (atc_mapping_molecule_end )
  
  #VERSION change one column to another one 
  ## WHICH 
 pos = which (!is.na (atc_mapping_molecule_end$final_atc_5 ))
 atc_mapping_molecule_end$ATC = atc_mapping_molecule_end$ATC.x
 atc_mapping_molecule_end$source = atc_mapping_molecule_end$source.x
 atc_mapping_molecule_end$ATC[pos] =  atc_mapping_molecule_end$final_atc_5 [pos]
 
 
 atc_mapping_molecule_end <- atc_mapping_molecule_end %>%  select (pt,trpt,ATC, source)
 
head (atc_mapping_molecule_end)
table (atc_mapping_molecule_end$source,exclude = NULL )

# ATTENTION #### Table with number, without  SPLIT
# direct_atc     first_word_medical          manual_mapped multiple_atc_discarded   multiple_atc_disease    multiple_atc_unique                   <NA> 
#   2556                    682                    539                    568                    613                    322                    526 


# direct_atc     first_word_medical          manual_mapped multiple_atc_discarded   multiple_atc_disease    multiple_atc_unique                   <NA> 
#   2930                    875                    909                    668                    680                    488                     37 


table (atc_mapping_molecule_end$ATC,exclude = NULL )
  # WHOMEO    <NA> 
  #   9      1142

na_atc  <- atc_mapping_molecule_end %>% filter (is.na(ATC) )
dim(na_atc) #[1] 1142    4
head(na_atc)
View (na_atc)


################################################################@
### NADIR  Medications discarded  ################################
##################################################################

dim(base_comedic) #[1] 5364    2


base_comedic <- distinct(base_comedic) # [1] 5210



base_comedic <- atc_mapping_molecule_end %>%
  mutate(base_cletri= paste0("20_",pt))  %>% 
  mutate(atc_cod= ATC)  %>%
  filter (! (atc_cod == "NA"))   %>% 
  filter (! (atc_cod == "WHOMEO"))   %>% 
  filter (! is.na(atc_cod)) %>% 
  select (base_cletri,atc_cod) #%>% 

# Funtion DataDict Rt2Lab

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


removeNonChronicMedications = function(data_medication_long, path_data_dictionary_folder, project){
  #' Remove non systemic medications present in the data dictionary
  #
  #library(xlsx)
  f2 = getMaximumDataDictFile(path_data_dictionary_folder)
  data_dict = as.data.frame ( readxl::read_excel (f2, sheet = 'comedic_atc_remove'))
  ATCtoRemove = as.character(data_dict$ATC_cod)
  ATCtoRemove = ATCtoRemove[which(trim(as.character(data_dict[,project])) == 'Yes')]
  removedNonSystemic = c()
  
  for(ATC_remove in ATCtoRemove){
    removedNonSystemic = c(removedNonSystemic, grep(paste0('^',ATC_remove), data_medication_long$atc_cod))
  }

  data_medication_long_chronic = data_medication_long[-removedNonSystemic,]
  data_medication_long_removed = data_medication_long[which(!data_medication_long$atc_cod %in% data_medication_long_chronic$atc_cod),]
  
  return(list(data_medication_long_chronic,data_medication_long_removed))
}

l = removeNonChronicMedications(data_medication_long = base_comedic, path_data_dictionary_folder = '/Users/beatrizgrandal/GitHub/databases/core/00_common/docs/', project = 'class_to_discard_NEOALTTO')

ttconc_chronic = l[[1]]
View(ttconc_chronic)
dim(ttconc_chronic) #[1] 2899    2
nrow(casa <- distinct(ttconc_chronic)) #[1] 2832

ttconc_chronic <- as.data.frame(ttconc_chronic)
ttconc_removed = l[[2]]


############################################################################
## Elise, multiple LEVELS ATC ##############################################
############################################################################

    ###################
    #  ATC To remove ##
    ###################
write.csv (ttconc_removed , "/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_generic_to_remove.csv", row.names = F, quote = F)
system('export PROJECT_PATH=/Users/beatrizgrandal/GitHub/databases;cd $PROJECT_PATH;Rscript core/00_common/src/main_comedic.R --db_name=20_neoaltto --file_name=comedic_generic_to_remove.csv --id_file_name=20_neoaltto_preprocessed_labels.RData  --long --wide')


   ####################################
   #ATC to keep: Chronic comedications##
   ###################################
write.csv (ttconc_chronic, "/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_generic_chronic.csv", row.names = F, quote = F)

 ## Haces lo mismo que con TERMINAL, pero directamente en el codigo
system('export PROJECT_PATH=/Users/beatrizgrandal/GitHub/databases;cd $PROJECT_PATH;Rscript core/00_common/src/main_comedic.R --db_name=20_neoaltto --file_name=comedic_generic_chronic.csv --id_file_name=20_neoaltto_preprocessed_labels.RData  --long --wide')

  
