###########
# Neoalto #
###########

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

datasets <- list(
  "dataset1_baseline.sas7bdat",
  "dataset2_radiotherapy.sas7bdat",
  "dataset3_hormonetherapy.sas7bdat",
  "dataset4_concomitant.sas7bdat",
  "dataset5_comorbidities.sas7bdat",
  "NeoALTTO_TILs_CD_Lymphocyte.xlsx",
  "NeoALTTO_TILs_RS_Lymfocyte_count.xlsx",
  "dataset7_pcr.sas7bdat",
  "dataset8_aes.sas7bdat",
  "dataset9_efs.sas7bdat",
  "dataset10_os.sas7bdat",
  "TILs_samples_pts_mapping.xlsx"
)

patients          <- read_sas(file.path(DATA_PATH, datasets[1]), NULL)
patients_rth      <- read_sas(file.path(DATA_PATH, datasets[2]), NULL)
patients_hth      <- read_sas(file.path(DATA_PATH, datasets[3]), NULL)
patients_concomit <- read_sas(file.path(DATA_PATH, datasets[4]), NULL)
patients_comorb   <- read_sas(file.path(DATA_PATH, datasets[5]), NULL)
patients_tils     <- read_excel(file.path(DATA_PATH, datasets[6]), NULL)
patients_tils2    <- read_excel(file.path(DATA_PATH, datasets[7]), NULL)
patients_mapping  <- read_excel(file.path(DATA_PATH, datasets[12]), NULL)
patients_pcr      <- read_sas(file.path(DATA_PATH, datasets[8]), NULL)
patients_tox      <- read_sas(file.path(DATA_PATH, datasets[9]), NULL)
patients_efs      <- read_sas(file.path(DATA_PATH, datasets[10]), NULL)
patients_os       <- read_sas(file.path(DATA_PATH, datasets[11]), NULL)


#######################
# Datatset 1(baseline)#
#######################

patients$ERCat
patients$PgRCat
dim (patients)

# Give all patients status Her2 + 
patients <- patients %>% mutate (her2_status =1) #%>% colnames()

# Check size tumor. Check where is the problem 
patients$RTmSz
summary (patients$RTmSz)
mean    (patients$RTmSz) #  10.1291 
median  (patients$RTmSz) #  4
range   (patients$RTmSz) # [1]  2.06 95.00

df <-patients [order(patients$RTmSz),]

df$RTmSz
# 
#       ## Test 1, >= 20.00 cm 
#       patients <-  patients %>% mutate ( test_size= ifelse (RTmSz >= 20.00, RTmSz/10, RTmSz))
#       head(patients)
#       patients$test_size
#       df <- patients [order(patients$test_size),]
#       df$test_size
#       mean(df$test_size) #[1] 4.343385
#       df <-df %>% mutate ( a= test_size*10)
#       df$a
#       mean (df$a)
     
      ## Test 2, >= 10.00 cm  --> NOT possible because trial >2 cm 
       patients <-  patients %>% mutate ( test_size= ifelse (RTmSz >= 9.6, NA, RTmSz))
       head(patients)
       # patients$test_size
       # df <- patients [order(patients$test_size),]
       # df$test_size
       # mean(df$test_size, na.rm=T) #[1] 3.93967
       # summary(df$test_size)
       
#  Check randomization arm.
       ## All set to 1 --> neo_antiher2, set to 1
       patients <- patients %>% mutate (neo_antiher2=1) 
       
   
###########################
# Dataset 2 (radiotherapy)#
###########################

# compute number of rth per patients.Group all radio in one 

n_rth_per_patient <- patients_rth %>% 
  dplyr::group_by(pt) %>% 
  dplyr::summarise(n_rth=n()) 
# 325 x 2

#patients_rth$sitesp %>% unique()

# Give same valor
n_rth_per_patient$n_rth <- 1

# Join two databases
neoaltto <- patients %>% 
  left_join(n_rth_per_patient, by="pt") #%>% 

# Give value to NA  --> NO
neoaltto[is.na(neoaltto$n_rth), "n_rth"] <- 0

# patients_rth <- patients_rth %>% 
#   mutate(rth_kind=case_when(
#     sitesp %in% c("BOOST","BREAST BOOST") ~ "rth_breast",
#     # TODO: add more
#     TRUE ~ "others"
#   ))

  
###############################
# Dataset 3 (hormone therapy)#
###############################

#Know your dataset
head(patients_hth)

# summary of the variable
n_hth_per_patient <- patients_hth %>% 
  dplyr::group_by(pt) %>% 
  dplyr::summarise(n_hth=n()) 
#201 x 2 --> n= 201 patients avec hormono

# Give same valor
n_hth_per_patient$n_hth <- 1

# Join two databases
neoaltto <- neoaltto %>% 
   left_join(n_hth_per_patient, by="pt") #%>% 

# Give value to NA  --> NO
neoaltto[is.na(neoaltto$n_hth), "n_hth"] <- 0


#(unique (patients_hth$trpt) , sep="\n")

# Prepare column for hormone type mapping 

patients_hth <- patients_hth %>%
  #(200) %>%
  dplyr:: group_by(trpt) %>%
  mutate(one=1) %>%
  distinct(pt, trpt, .keep_all = TRUE) %>% # TODO
  mutate(trpt = gsub(" ","_",trpt)) %>%
  pivot_wider(id_cols=pt,
              names_from=trpt,
              values_from=one,
              values_fill=0) %>%
   mutate(ht_type_5cl=case_when(
   TAMOXIFEN == 1 & GOSERELIN == 1 ~ 3,
   TAMOXIFEN == 1 & GONADORELIN == 1 ~ 3,
   TAMOXIFEN == 1 & LEUPRORELIN_ACETATE == 1 ~ 3,
   TAMOXIFEN == 1 & TRIPTORELIN == 1 ~ 3,
   TAMOXIFEN == 1 & LEUPRORELIN == 1 ~ 3 , 

   TAMOXIFEN_CITRATE == 1 & GOSERELIN == 1 ~ 3,
   TAMOXIFEN_CITRATE == 1 & GONADORELIN == 1 ~ 3,
   TAMOXIFEN_CITRATE == 1 & LEUPRORELIN_ACETATE == 1 ~ 3,
   TAMOXIFEN_CITRATE == 1 & TRIPTORELIN == 1 ~ 3,
   TAMOXIFEN_CITRATE == 1 & LEUPRORELIN == 1 ~ 3 ,

   ANASTROZOLE == 1 & GOSERELIN == 1 ~ 4,
   ANASTROZOLE == 1 & GONADORELIN == 1 ~ 4,
   ANASTROZOLE == 1 & LEUPRORELIN_ACETATE == 1 ~ 4,
   ANASTROZOLE == 1 & TRIPTORELIN == 1 ~ 4,
   ANASTROZOLE == 1 & LEUPRORELIN == 1 ~ 4 ,

   LETROZOLE == 1 & GOSERELIN == 1 ~ 4,
   LETROZOLE == 1 & GONADORELIN == 1 ~ 4,
   LETROZOLE == 1 & LEUPRORELIN_ACETATE == 1 ~ 4,
   LETROZOLE == 1 & TRIPTORELIN == 1 ~ 4,
   LETROZOLE == 1 & LEUPRORELIN == 1 ~ 4 ,

   EXEMESTANE == 1 & GOSERELIN == 1 ~ 4,
   EXEMESTANE == 1 & GONADORELIN == 1 ~ 4,
   EXEMESTANE == 1 & LEUPRORELIN_ACETATE == 1 ~ 4,
   EXEMESTANE == 1 & TRIPTORELIN == 1 ~ 4,
   EXEMESTANE == 1 & LEUPRORELIN == 1 ~ 4 ,

   TAMOXIFEN == 1 & OOPHORECTOMY_BILATERAL == 1 ~ 5,
   TAMOXIFEN == 1 & OOPHORECTOMY == 1 ~ 5 ,

   ANASTROZOLE == 1 & OOPHORECTOMY_BILATERAL== 1 ~ 5,
   ANASTROZOLE == 1 & OOPHORECTOMY ~ 5 ,

   LETROZOLE  == 1 & OOPHORECTOMY_BILATERAL == 1 ~ 5,
   LETROZOLE  == 1 & OOPHORECTOMY == 1 ~ 5 ,

   EXEMESTANE == 1 & OOPHORECTOMY_BILATERAL == 1 ~ 5,
   EXEMESTANE == 1 & OOPHORECTOMY == 1 ~ 5,

   OOPHORECTOMY_BILATERAL== 1 ~ 5,
   OOPHORECTOMY == 1 ~ 5,
   NOT_APPLICABLE == 1 ~ 5,
   TOREMIFENE_CITRATE == 1 ~ 5 ,

  GOSERELIN == 1 ~ 5,
  GONADORELIN == 1~ 5,
  LEUPRORELIN_ACETATE == 1 ~ 5,
  TRIPTORELIN == 1 ~ 5,
  LEUPRORELIN == 1 ~ 5,

   TAMOXIFEN == 1 ~ 1,
   TAMOXIFEN_CITRATE == 1 ~ 1,

   ANASTROZOLE == 1 ~ 2,
   LETROZOLE == 1 ~ 2,
   EXEMESTANE == 1 ~ 2,
   TRUE ~ -1))

patients_hth <- patients_hth %>% select (pt,ht_type_5cl)

# Join  two databases
neoaltto <- neoaltto %>% 
  left_join(patients_hth, by="pt") #%>% 

#############################
# Dataset 4 (comedications)#
############################

n_concomit_per_patient <- patients_concomit %>% 
   dplyr::group_by(pt) %>% 
   dplyr::summarise(n_concomit=n()) 
#403 x 2
dim(patients_concomit)
dim(n_concomit_per_patient) #[1] 403   2

length(unique(patients_concomit$trpt)) #[1] 1093

a <- ddply(patients_concomit, ~trpt, summarise, number_of_distinct_matches=length(unique(trsct)))

write.csv ( patients_concomit, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/patients_concomit.csv")

# Comedication with ATC  ( mapping Nadir v2, 15/04/)
load("/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/dataComeds_mapping.RData")
#load("/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v1/dataComeds_mapping.RData")

write.csv ( ATC, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/ATC.csv")
write.csv ( dataComeds, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds.csv")
write.csv ( dataComeds_mapped_first_word, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_mapped_first_word.csv")
write.csv ( dataComeds_mapped_parsed_number, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_mapped_parsed_number.csv")
write.csv ( dataComeds_mapped_whole_name, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_mapped_whole_name.csv")
write.csv ( dataComeds_not_mapped, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_not_mapped.csv")
write.csv ( dataComeds_split_slash, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_split_slash.csv")
write.csv ( dataComeds_split_slash, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_split_slash.csv")
write.csv ( dataComeds_multiple_ATC, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_multiple_ATC.csv")
#write.csv ( dataComeds_split_slash_unique, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/raw_nadir_v2/dataComeds_split_slash_unique.csv")

      ## Data with unique molecule
      #length(unique(dataComeds$trpt))

# Check and choose ATC between multiple ATC. Merge with Comorbidities 
dim(patients_comorb) #[1] 609   6
dim(dataComeds) # 1087   11
# 
str(patients_comorb)
str(dataComeds)

comorbities_atc  <- dataComeds %>% 
   left_join(patients_comorb ,by = c("pt" = "pt")) 

tmp_comorbities_atc <- comorbities_atc %>% select ("pt","multipleATC","diagpt")
write.csv ( tmp_comorbities_atc, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/processed/tmp_comorbities_atc.csv")



dim(tmp_comorbities_atc) #[1] 2210   16
colnames(tmp_comorbities_atc)

class(dataComeds$pt) #[1] "character"
class(patients_comorb$pt)  #[1] "character"

patients_comorb$pt

#############################
# Dataset 5 (comorbidities)#
############################

n_comorb_per_patient <- patients_comorb  %>% 
  group_by(pt) %>% 
  summarise(n_comorb=n()) 
#403 x 2
dim(patients_comorb )

## Pprint different lines 
#cat(unique (patients_comorb$diagpt) , sep="\n")


###################################
# Dataset 6 (immune infiltration)#
#################################


dim(patients_tils) #[1] 424   8
dim(patients_tils2) #[1] 424   7

# Look differences between others bases and tils --> where to find patients_id and patients_sample???
setdiff(patients$pt,patients_tils$"Patient sample_id") #-->0

## Look differences between two bases by columns
setdiff(patients_tils$"Patient sample_id",patients_tils2$"Patient sample_id") #-->0

setdiff(patients_tils$"INTRA-TUMORAL LY %",patients_tils2$"INTRATUMORAL LY %") #-->70
table(patients_tils$"INTRA-TUMORAL LY %",patients_tils2$"INTRATUMORAL LY %",useNA="always") 

# 0   1   2   3   5  10  15  20  25  30  40  60 <NA>
#    0    164  21  18   2  22   2   1   0   0   0   0   0    0
# 2      2   1   0   0   0   0   0   0   0   0   0   0    0
# 5     31   3   3   2   5   7   2   0   3   0   1   0    0
# 10    29   1   7   1  10   8   2   1   0   2   0   0    0
# 15     1   0   0   0   1   0   0   0   0   0   0   0    0
# 20     6   0   1   0  12   6   1   1   0   1   0   1    0
# 25     0   0   1   0   0   0   0   0   0   0   0   0    0
# 30     1   1   0   0   5   0   2   1   0   0   0   0    0
# 40     0   0   0   0   1   0   0   0   0   0   0   0    0
# 70     0   0   0   0   0   0   1   0   0   0   0   0    0
# <NA>  24   0   0   0   0   0   0   0   0   0   0   0    5

setdiff(patients_tils$"STROMAL    LY %",patients_tils2$"STROMAL    LY %")# -->[1]  65 100
table(patients_tils$"STROMAL    LY %",patients_tils2$"STROMAL    LY %", useNA="always")


setdiff(patients_tils$"TLS/G",patients_tils2$"TLS/G")  #[1]  1 NA
table(patients_tils$"TLS/G",patients_tils2$"TLS/G",useNA="always") 
# 0  no yes <NA>
#   0      5 380   1    0
# 1      0   6   3    0
# <NA>  20   9   0    0

## Interpretation 
#patients_tils2$"TLS/G"
# O, no --> 0 --> 420 
# yes   --> 1 --> 4

setdiff(patients_tils$"TLS/ no G",patients_tils2$"TLS/no G") #[1]  1 NA
table(patients_tils$"TLS/ no G",patients_tils2$"TLS/no G", useNA = "always")
# 0  no yes <NA>
#   0      5 308   6    0
# 1      0  60  16    0
# <NA>  20   9   0    0

setdiff(patients_tils$"Comments CD",patients_tils2$"Comments") 


# Differences in the percentage of TILs among anatomopathologists
# it_tils <- tils %>% select ("INTRA-TUMORAL LY %","INTRATUMORAL LY %")
# str_tils <- tils %>% select ("STROMAL    LY %.x","STROMAL    LY %.y")

# Merge both AP
tils <- patients_tils %>% 
   left_join(patients_tils2 ,by = "Patient sample_id") #%>% 
head(tils)
dim(tils)# [1] 424   14
colnames(tils)

tils <- tils %>% rename(
    "it_1"  = "INTRA-TUMORAL LY %", 
    "str_1" = "STROMAL    LY %.x", 
    "it_2"  = "INTRATUMORAL LY %", 
    "str_2" = "STROMAL    LY %.y", 
      "id"  =  "Patient sample_id"
)

colnames(tils)

# Filter biopsies with tissue available. Following Salgado 2015, I take only data from 1° anapath
tils <- tils %>% filter (!is.na (str_1 ))
dim(tils) # [1] 395  14
colnames(tils)

## @ mail Marick Lae, 21/03/25, averaging TILs from 2 anapath
tils$str_mean = (tils$str_1 + tils$str_2) / 2
tils$it_mean = (tils$it_1 + tils$it_2) / 2
         # colnames(tils)
         # tils %>% select ("str_1","str_2", "str_mean" )
         # tils %>% select ("it_1","it_2", "it_mean" )

# it_tils <- tils %>% select ("INTRA-TUMORAL LY %","INTRATUMORAL LY %")

# Merge all 
head(patients_mapping)
dim(patients_mapping) # [1] 424   2

setdiff(patients_mapping$PT_sample_ID,tils$"id") # 29!!! 
setdiff(patients_mapping$SCREENING_NUMBER,neoaltto$pt) #[1] "100365" NA       "103510" "200433" "200502" #5 ID
                                    # Possible alternative                                      "200520"

setdiff(neoaltto$pt,patients_mapping$SCREENING_NUMBER)  #%>% order() # 43 ID

# Merge  AP and mapping 
tils_mapping <- tils %>% 
   left_join(patients_mapping ,by = c("id"="PT_sample_ID")) 

head(tils_mapping)
dim(tils_mapping) #[1] 395  15
colnames(tils_mapping)


# Merge  AP and mapping 
neoaltto <- neoaltto %>% 
   left_join(tils_mapping, by = c("pt"="SCREENING_NUMBER")) 

head(neoaltto)
dim(neoaltto) #[1] 455  53
colnames(neoaltto)
str(neoaltto)

table(neoaltto$"str_1", useNA="always" )



 ###################################
# Dataset 7 (Pathological reponse)#
###################################
dim(patients_pcr) #[1] 455   2
head (patients_pcr )

neoaltto <- neoaltto %>% 
  left_join(patients_pcr, by="pt") #%>% 

##########################
# Dataset 8 (safety - AE)#
###########################

patients_tox$AEHLGT %>% unique() #65 groups 
patients_tox$AEPT %>% unique() # 147 toxicidades

n_tox_per_patient <- patients_tox  %>% 
  dplyr::group_by(pt) %>% 
  dplyr::summarise(n_tox=n())  #281

n_tox_per_patient$n_tox <- 1 %>% as.factor()


neoaltto <- neoaltto %>% 
  left_join(n_tox_per_patient) #%>% 

###################################
# Dataset 9 - event free surivival#
###################################
dim(patients_efs) #[1] 455   7
colnames(patients_efs)
head(patients_efs)

neoaltto <- neoaltto %>% 
   left_join(patients_efs ,by = c("pt" = "PT")) #%>% 

table(patients_efs$EVNTDESC)
# Censored: Clinical follow-up ended (lost to follow-up) 20 
# Censored: Clinical follow-up ended (withdrew but consent for survival) 11 
# Censored: Clinical follow-up ended (withdrew)  51 
# Censored: Clinical follow-up ongoing 246 
# CNS recurrence  23 
# Death during clinical follow-up (no surgery) 3 
# Death during clinical follow-up (post-surgery)  4 
# Distant bone recurrence  15 
# Distant visceral recurrence  29 
# Invasive contralateral breast cancer 8 
# Invasive SPM in ipsilateral breast  1 
# Local recurrence 20 
# Other distant recurrence (assumed soft tissue) 7 
# Progression or SPM/CBC during neo. treatment 3 
# Regional recurrence 4 
# Second (non-breast) primary malignancy  10 


neoaltto <- neoaltto %>% mutate ( 
  ev_meta = ifelse (EVNTDESC == "Distant bone recurrence" | EVNTDESC =="Distant visceral recurrence" | EVNTDESC =="Other distant recurrence (assumed soft tissue)" ,1, 0))
table(neoaltto$ev_meta)

neoaltto <- neoaltto %>% mutate ( 
  ev_recloc = ifelse (EVNTDESC == "Local recurrence",1, 0))
table(neoaltto$ev_recloc)

neoaltto <- neoaltto %>% mutate ( 
  ev_contro = ifelse (EVNTDESC == "Invasive contralateral breast cancer",1, 0))
table(neoaltto$ev_contro)

neoaltto <- neoaltto %>% mutate ( 
  ev_prog_neo = ifelse (EVNTDESC == "Progression or SPM/CBC during neo. treatment",1, 0))
table(neoaltto$ev_prog_neo)

# neoaltto <- neoaltto %>% mutate ( 
#   status_vital = ifelse (EVNTDESC == "Death during clinical follow-up (no surgery)" | EVNTDESC =="Death during clinical follow-up (post-surgery)",1, 0))
# table(neoaltto$status_vital)

neoaltto <- neoaltto %>% mutate ( 
  ev_secondk = ifelse (EVNTDESC == "Invasive SPM in ipsilateral breast",1, 0))
table(neoaltto$ev_secondk)


###################################
# Dataset 10 - overall survival#
###################################
dim(patients_os ) #[1] 455   7
colnames(patients_os)

neoaltto <- neoaltto %>% 
   left_join(patients_os ,by = c("pt" = "PT")) #%>% 

neoaltto$EVNTDESC.y
colnames(neoaltto)



write.csv ( neoaltto, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/neoaltto.csv")
save ( neoaltto, file="/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/neoaltto.RData")



##################
# Multiple ATC ##
################# 

# Know how many patients have at least one comedications with multiple ATC

unique_set_multiple <- read.csv(file = "/Users/beatrizgrandal/GitHub/databases/core/20_neoaltto/data/comedic_ATC/processed/tmp_dataComeds_mapped_whole_name_first_name_multipleATC.csv",sep = ";")

df_multiple  <- patients_concomit %>% filter(trpt %in% unique_set_multiple$trpt)

nrow(df_multiple) #  2130

length(unique(df_multiple$pt))

