
# Load data and data dictionary
options(max.print = 9999)
options(stringsAsFactors=FALSE)
options(dplyr.print_max = Inf)
load("/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/data/raw/BC_full_49var_BreastReconstruction_Subset.RData")
head(dataset)
nrow(dataset)

dataset$NumberOfPositiveNode
dataset$SurvivalDelayInMonths <- as.integer(dataset$SurvivalDelayInMonths)

dataset$year_diag <- as.character(dataset$year_diag)

# rm(list = ls())
# getwd()
require(dplyr)
require(tableone)
require(ggsci)
require(stringr)
require(tidyverse)
require(ggplot2)
require(reshape2)
require(gridExtra)
require(cowplot)
require(xtable)
require(knitr)
require(gProfileR)
library(readxl)
library(survival)
library(kableExtra)
library(readxl)

extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv(),"RT2Lab","databases","core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict   <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) 
data_dict                <- read_excel(paste0("~/RT2Lab/databases/core/00_common/docs/", last_version_data_dict), 1)
datadict_comp            <- read_excel("~/RT2Lab/databases/core/21_SEER_Breast/doc/datadict_seer_breast_comp.xlsx")
new_datadict              <- bind_rows (data_dict, datadict_comp %>% filter( ! var %in% data_dict$var))# %>% tail()

# Correct levels if needed
# new_datadict[which(new_datadict$var == "antiher2"),"names_var" ] <- "Trastuzumab"

# map to RT2datadict
data_dict %>% select(family_var) %>% unique() %>% as.data.frame()
# family_var
# 1                patient_id
# 2              patient_char
# 3              comedication
# 4               comorbidity
# 5              bc_diagnosis
# 6                bc_biology
# 7                   surgery
# 8         treatments_binary
# 9             neoadj_or_not
# 10  neoadjuvant_ct_antiher2
# 11     adjuvant_ct_antiher2
# 12     settings_and_regimen
# 13               treatments
# 14          tumor_char_surg
# 15                 numhisto
# 16           tumor_char_neo
# 17             pre_post_neo
# 18          delays_pathways
# 19        events_and_censor
# 20                     evol

# Import data
head(dataset)
dataset %>% group_by(Histology) %>% count() %>% arrange(desc(n))
dataset %>% group_by(AJCC7thN) %>% count() %>% arrange(desc(n))
dataset %>% group_by(TypeSurgeryPrimitiveSite) %>% count() %>% arrange(desc(n))
dataset %>% group_by(AJCC7thM,Surgery,TypeSurgeryPrimitiveSite) %>% count() %>% arrange(desc(n))
dataset %>% group_by(Surgery) %>% count() %>% arrange(desc(n))
dataset %>% group_by(Surgery,TypeSurgeryPrimitiveSite) %>% count() %>% arrange(desc(n))
dataset %>% group_by(ChemoTherapy) %>% count() %>% arrange(desc(n))
dataset %>% group_by(DelayDiagnosisFirstTreatment) %>% count() %>% arrange(desc(n))
as.integer(dataset$DelayDiagnosisFirstTreatment) *30.45
dataset %>% group_by(TumorSize) %>% count() %>% arrange(desc(n))
dataset %>% group_by(TumorSize) %>% count() 
dataset %>% group_by(Surgery,TumorSize) %>% count() 
dataset %>% group_by(Surgery,NumberOfNodeRemoved) %>% count() 
dataset %>% group_by(Surgery,NumberOfPositiveNode) %>% count() 
dataset %>% group_by(BreastSubtype) %>% count() 
dataset %>% group_by(MetastasisAtDiagnosis,AJCC7thM) %>% count() 

dataset %>% group_by(Method2EvalTumSizeExt) %>% count() 
dataset %>% group_by(Method2EvalTumSizeExt,Method2EvalRegNode) %>% count() 
dataset %>% group_by(Method2EvalRegNode) %>% count() 
dataset %>% group_by(Ethnicity) %>% count() 

dataset %>% group_by(Method2EvalRegNode) %>% count() 
table(dataset$Method2EvalRegNode)
dataset %>% group_by(BreastReconstruction) %>% count() 

# Ethnicity
# InvasiveComponent
# Method2EvalTumSizeExt
# MetastasisAtDiagnosis
# BreastSubtype
# NumberOfPositiveNode
# NumberOfNodeRemoved NumberOfPositiveNode
# TumorSize
head(dataset)

# patient_id
dataset <- dataset %>% 
            mutate( 
                   # patient_id
                    database   = 21,
                    year_birth = YearOfBirth,
                    year_diag  = YearOfDiagnosis,
                    age        = AgeAtDiagnosis,
                    side       = case_when(Laterality == "Right - origin of primary" ~ 2,
                                           Laterality == "Left - origin of primary"  ~ 1),
                    # bc_diagnosis
                    ctuicc_5cl = case_when(AJCC7thT == "T0"         ~ 0,
                                           AJCC7thT == "T1"         ~ 1,
                                           AJCC7thT == "T2"         ~ 2,
                                           AJCC7thT == "T3"         ~ 3,
                                           AJCC7thT == "T4"         ~ 4) ,
                    cnuicc_4cl = case_when(AJCC7thN == "N0"         ~ 0,
                                           AJCC7thN == "N1"         ~ 1,
                                           AJCC7thN == "N2"         ~ 2,
                                           AJCC7thN == "N3"         ~ 3) ,
                    muicc      =  case_when(AJCC7thM == "M0" |  MetastasisAtDiagnosis == "No" ~ 0,
                                            AJCC7thM == "M1" |  MetastasisAtDiagnosis == "Yes"  |
              BrainMetastasisAtDiagnosis == "Yes" |   BoneMetastasisAtDiagnosis  == "Yes" | 
              LiverMetastasisAtDiagnosis == "Yes" |   LungMetastasisAtDiagnosis  == "Yes" |
              DistantNodeMetastasisAtDiagnosis == "Yes" |  OtherMetastasisAtDiagnosis == "Yes" ~ 1),
                    # bc_biology	
                    grade_3cl  = case_when(Grade == "Well differentiated; Grade I"         ~ 1,
                                           Grade == "Moderately differentiated; Grade II"  ~ 2,
                                           Grade == "Poorly differentiated; Grade III"     ~ 3),
                    histo_3cl  = case_when(Histology == "8500/3: Infiltrating duct carcinoma NOS" ~ 1,
                                           Histology == "8520/3: Lobular carcinoma NOS"           ~ 2,
                                           TRUE ~ 9 ),
                                           # Histology %in% c("Other",
                                           #                  "8522/3: Infiltrating duct and lobular carcinoma",
                                           #                  "8523/3: Infiltrating duct mixed with other types of carcinoma")~ 3),
                    subtype4   = case_when(BreastSubtype == "HR-/HER2- (Triple Negative)" ~  2,
                                           BreastSubtype == "HR-/HER2+ (HER2 enriched)"   ~  4,
                                           BreastSubtype == "HR+/HER2- (Luminal A)"       ~  1,
                                           BreastSubtype == "HR+/HER2+ (Luminal B)"       ~  3),
                    er_status  = case_when(ER == "Positive" ~ 1,
                                           ER == "Negative" ~ 0) ,
                    pr_status  = case_when(PR == "Positive" ~ 1,
                                           PR == "Negative" ~ 0) ,
                    her2_status  = case_when(HER2 == "Positive" ~ 1,
                                           HER2 == "Negative" ~ 0) ,
                    dcis_component = case_when(InvasiveComponent == "Entire tumor reported as invasive (No in situ component reported)" ~ 0,
                                               InvasiveComponent == "Invasive and in situ components present" ~ 1),
                    # surgery
                    # dataset %>% group_by(Surgery,TypeSurgeryPrimitiveSite) %>% count() %>% arrange(desc(n))
                    breast_surgery_3cl  = case_when(TypeSurgeryPrimitiveSite == "Lumpectomy" ~ 1,
                                                    TypeSurgeryPrimitiveSite == "Mastectomy" ~ 2),
                    breast_surgery      = case_when(Surgery == "Not Recommended" ~  0,
                                                    Surgery == "Performed"   | 
                                                      breast_surgery_3cl %in% c(1,2) |
                                                      Method2EvalTumSizeExt ==  "Resection after neoadjuvent chemo"~ 1) , # @ashp beware, cf other proxies of surgery
  axillary_surgery = case_when(Method2EvalRegNode %in% c("No regional lymph nodes removed for examination") ~ 0,
                               Method2EvalRegNode %in% c("Regional lymph nodes removed for examination AFTER neoadjuvant therapy") 
                               | !is.na(NumberOfPositiveNode) |
                                 as.integer(NumberOfNodeRemoved) >0 ~ 1),
  breast_reconstruction = case_when(BreastReconstruction == "FALSE" ~ 0,
                                    BreastReconstruction == "TRUE"  ~ 1),
                    # cancer_surgery = case_when(breast_surgery == 1 | axillary_surgery == 1 ~ "Yes",
                    #                            breast_surgery == 0 & axillary_surgery == 0 ~ "No"),
                    # treatments_binary                       
                    ct = case_when(ChemoTherapy == "Yes" ~ 1),
                    rt = case_when(Radiation == "No" ~ 0,
                                   Radiation == "Yes" ~ 1),
                    neo_ct = case_when(Method2EvalTumSizeExt == "Resection after neoadjuvent chemo" |
                                       Method2EvalRegNode == "Regional lymph nodes removed for examination AFTER neoadjuvant therapy" ~ 1,
                                       TRUE ~ 0) ,
                    # tumor_char_surg
                    histo_size = case_when ( is.na(neo_ct) &  breast_surgery == 1 ~ TumorSize) ,
                    # @ashp; recouper avec la chir !! 
                    # car si pas de chir, mapper a la taille clin

                    # NumberOfNodeRemoved
                    # @ashp : good proxy of surgery
                    nbggprel        = as.integer(NumberOfNodeRemoved),
                    nbggpos         = case_when ( neo_ct == 0 ~ as.integer(NumberOfPositiveNode)),
                    # @ashp : beware of neoadj ! map to nbggpospostneo
                    nbggpos_postneo = case_when ( neo_ct == 1 ~  as.integer(NumberOfPositiveNode)),
                    
                    # Method2EvalTumSizeExt
                    # delays_pathways 
                    delay_diag_to_first_ttt = as.numeric(DelayDiagnosisFirstTreatment), #*30.45,
                    # @ashp cf definition seer; more faithful proxy?
                    
                    #events_and_censor
                    status_vital = case_when(VitalStatus == "Alive" ~ 0,
                                             VitalStatus == "Dead"  ~ 1),
                    cause_death  = case_when(DeathSpecificOfBreastCancer == "Yes" ~ 1,
                                             DeathDueToOtherCause        == "Yes" ~ 2,
  (DeathSpecificOfBreastCancer != "Yes" | DeathDueToOtherCause != "Yes" | 
    ( is.na(DeathSpecificOfBreastCancer) & is.na(DeathDueToOtherCause))) & VitalStatus == "Dead" ~ 3),
                    delay_os    = SurvivalDelayInMonths)

dataset$status_vital
dataset$SurvivalDelayInMonths

dataset %>% mutate(delay_follow_up = ifelse(status_vital == 0 , SurvivalDelayInMonths , NA ),
                   time_to_death   = ifelse(status_vital == 1 , SurvivalDelayInMonths , NA )) -> dataset

dataset %>% group_by(breast_surgery_3cl,breast_reconstruction) %>% count()
dataset %>% group_by(Histology,histo_3cl) %>% count()
dataset %>% group_by(VitalStatus,status_vital,DeathSpecificOfBreastCancer,DeathDueToOtherCause,cause_death) %>% count()
dataset %>% group_by() %>% count()

levels(dataset$MaritalStatus) <- c("Divorced","Married - Coupled up",
                                   "Separated","Single",                                         
                                    "Widowed")
dataset$MaritalStatus <- factor(dataset$MaritalStatus, levels = c("Single","Married - Coupled up",
                                                                     "Separated","Divorced",
                                                                     "Widowed"))
dataset %>% group_by(MaritalStatus) %>% count()

# dataset$delay_diag_to_first_ttt_grouped <- ifelse(dataset$delay_diag_to_first_ttt <6, dataset$delay_diag_to_first_ttt, 6)
dataset$delay_diag_to_first_ttt_grouped     <- ifelse(dataset$delay_diag_to_first_ttt <4, dataset$delay_diag_to_first_ttt, 4)
dataset %>% mutate(delay_diag_to_first_ttt_grouped_4cl = case_when(delay_diag_to_first_ttt_grouped == 1 ~ "Within one month",
                                                                   delay_diag_to_first_ttt_grouped == 2 ~ "1-2 months",
                                                                   delay_diag_to_first_ttt_grouped == 3 ~ "2-3 months",
                                                                   delay_diag_to_first_ttt_grouped == 4 ~ "4 months or +" )) -> dataset 
dataset %>% group_by(delay_diag_to_first_ttt_grouped_4cl) %>% count()
dataset %>% group_by(delay_diag_to_first_ttt_grouped,delay_diag_to_first_ttt_grouped_4cl) %>% count()


dataset$nbggpos
dataset$nbggprel
dataset$nbggpos_postneo
dataset$nbggpos_postneo
dataset$primary 

head(dataset)

as.integer(dataset$NumberOfPositiveNode)
dataset_before_mapping <- dataset
dataset_before_mapping$breast_reconstruction
dataset_before_mapping$axillary_surgery
dataset_before_mapping$breast_surgery

save(dataset_before_mapping, file = "~/RT2Lab/databases/core/21_SEER_Breast/data/raw/dataset_before_mapping.Rdata")
dataset_before_mapping$nbggpos_postneo
dataset_before_mapping$delay_diag_to_first_ttt_grouped
dataset_before_mapping$breast_surgery

#  Then run mapping and preprocessing.
# ahamypet@macbook-pro-de-ahamypet ~ % export PROJECT_PATH="/Users/ahamypet/RT2Lab/databases"
# ahamypet@macbook-pro-de-ahamypet ~ % cd $PROJECT_PATH;
# ahamypet@macbook-pro-de-ahamypet databases % Rscript core/00_common/src/main.R --db_name=21_SEER_Breast --class_name=MappingSEER_Breast --mapping --preprocessing

# After mapping and preprocessing
load("/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/data/21_SEER_Breast_preprocessed_labels.RData")
head(database_preprocessed_labels)
database_preprocessed_labels$nbggpos
database_preprocessed_labels$MonthOfDiagnosis

database_preprocessed_labels$nbggpos_postneo
database_preprocessed_labels$pnuicc_4cl

database_preprocessed_labels %>% group_by(breast_surgery_3cl,breast_reconstruction) %>% count()
database_preprocessed_labels %>% group_by(cancer_surgery) %>% count()
database_preprocessed_labels %>% group_by(breast_reconstruction) %>% count()

database_preprocessed_labels$Method2EvalRegNode
head(dataset)
dataset$TimingOfRadiotherapy
database_preprocessed_labels %>% group_by(Ethnicity) %>% count()
database_preprocessed_labels %>% group_by(MaritalStatus) %>% count()
database_preprocessed_labels %>% group_by(RuralUrban) %>% count() # regroup; 
database_preprocessed_labels %>% group_by(primary_ttt_5cl) %>% count()
database_preprocessed_labels %>% group_by(cnuicc_4cl) %>% count()
database_preprocessed_labels %>% group_by(delay_diag_to_first_ttt_grouped) %>% count()
database_preprocessed_labels$delay_diag_to_first_ttt

database_preprocessed_labels %>% select(
  # Patient_id
  # database
  Sex,  age,age_cl_10_1,
  Ethnicity, Insurance,MaritalStatus,
  RuralUrban , CostLivingIndex,
  MedianFamIncome,MedianHouseholdIncome,
  year_diag, MonthOfDiagnosis,
  # bc_diagnosis
  Quadrant,   side,
  ctuicc_5cl,  cnuicc_4cl,
  muicc, contains("Metastasis"),
  # bc_biology
  grade_3cl,  histo_3cl,   
  subtype4, er_status, pr_status,her2_status,
  dcis_component,
  nbggprel,nbggpos,nbggpos_postneo,
  pnuicc_4cl,
  ypnuicc_4cl,
  # treatments
  primary_ttt_5cl,
  cancer_surgery,
  breast_surgery,axillary_surgery,
  breast_reconstruction,
  ChemoTherapy,
  neo_ct,
  rt,
  # tumorchar
  # delays_and_event
  delay_diag_to_first_ttt,#delay_diag_to_first_ttt_grouped,
  delay_diag_to_first_ttt_grouped_4cl,
  delay_follow_up,
  time_to_death,  
  status_vital_txt,cause_death  ) -> database_preprocessed_labels_short


save(database_preprocessed_labels_short, file = "/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/data/processed/database_preprocessed_labels_short.RData")
load("/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/data/processed/database_preprocessed_labels_short.RData")
head(database_preprocessed_labels_short)
ncol(database_preprocessed_labels_short)
database_preprocessed_labels_short$subtype4

# in our dataset, only  1st breast cancer.
# FirstMalignantPrimary => i think get rid of it.

# Quality controls
head(database_preprocessed_labels)
head(database_preprocessed_labels_short)
database_preprocessed_labels_short %>% group_by(delay_diag_to_first_ttt) %>% count()

database_preprocessed_labels %>% group_by(cancer_surgery,breast_surgery,axillary_surgery) %>% count() 
database_preprocessed_labels %>% group_by(cancer_surgery) %>% count() 

database_preprocessed_labels %>% group_by(primary_ttt_5cl) %>% count() 
database_preprocessed_labels %>% group_by(rt) %>% count() 
database_preprocessed_labels %>% group_by(cancer_surgery,primary_ttt_5cl) %>% count() 
# cancer_surgery primary_ttt_5cl                   n
# <chr>          <fct>                         <int>
#   1 No             Others neoadjuvant treatments     6
# 2 No             No surgery                     1520
# 3 Yes            Surgery                       24821
# 4 Yes            Others neoadjuvant treatments  2498
# 5 Yes            No surgery                      102
# 6 NA             Surgery                         632
# 7 NA             No surgery                      421

# load("/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/data/21_SEER_Breast_mapped.RData")
# head(database)

# Cf corresponding data dictionary
extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv(),"RT2Lab","databases","core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict   <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6)))))
data_dict                <- read_excel(paste0("~/RT2Lab/databases/core/00_common/docs/", last_version_data_dict), 1)
datadict_comp            <- read_excel("~/RT2Lab/databases/core/21_SEER_Breast/doc/datadict_seer_breast_comp.xlsx")
new_datadict             <- bind_rows (data_dict, datadict_comp %>% filter( ! var %in% data_dict$var))# %>% tail()

colnames(database_preprocessed_labels_short)[!colnames(database_preprocessed_labels_short) %in% new_datadict$var] %>% as.data.frame()

new_datadict_SEER_breast <- new_datadict %>% filter(var %in% colnames(database_preprocessed_labels_short))



new_datadict_SEER_breast$var
new_datadict_SEER_breast$family_var %>% unique()

socio_demo <- c("Sex", "Ethnicity","Insurance","MaritalStatus","RuralUrban",
                "CostLivingIndex","MedianFamIncome","MedianHouseholdIncome")
meta_at_diag <- c("BoneMetastasisAtDiagnosis","BrainMetastasisAtDiagnosis","LiverMetastasisAtDiagnosis",
                  "LungMetastasisAtDiagnosis","DistantNodeMetastasisAtDiagnosis","OtherMetastasisAtDiagnosis")

# 36 NA             MonthOfDiagnosis   NA        NA                                                          Month of diagnosis        NA                                                                               NA            NA            
# 37 NA             Quadrant           NA        NA                                                          Quadrant                  NA                                                                               NA            NA            

new_datadict_SEER_breast[which(new_datadict_SEER_breast$family_var),"family_var"]
new_datadict_SEER_breast$var == "Chemotherapy"

new_datadict_SEER_breast <- new_datadict_SEER_breast %>% 
              dplyr :: select(family_var, var, var_type,levels ,names_var,descript) %>% 
              dplyr :: mutate(family_var2 = case_when(var %in% socio_demo       ~ "patient_char",
                                                      var %in% meta_at_diag      ~ "bc_diagnosis",
                                                      var ==  "MonthOfDiagnosis" ~ "Pathway",
                                                      var ==  "Quadrant"         ~ "bc_diagnosis",
                                                      var %in% c(  "MetastasisAtDiagnosis",
                                                                   "side") ~ "bc_diagnosis",
                                                      var %in% c(  "delay_diag_to_first_ttt_grouped",
                                                                   # "delays_pathways",
                                                                   "delay_diag_to_first_ttt",
                                                                   "year_diag",
                                                                   "delay_follow_up",
                                                                   "time_to_death") ~ "Pathway",
                                                      var ==  "ChemoTherapy" ~ "treatments_binary",
                                                      TRUE                  ~ family_var),
                              groups = case_when(#family_var2 == "patient_ID" ~ "",
                                                 family_var2 == "patient_char" ~ "Socio Demographics",
                                                 family_var2 == "bc_diagnosis" ~ "Presentation at diagnosis",
                                                 family_var2 == "bc_biology"   ~ "Tumor biology",
                                                 family_var2 == "surgery"      ~ "Treatments",
                                                 family_var2 == "treatments_binary" ~ "Treatments",
                                                 family_var2 == "neoadj_or_not"     ~ "Treatments",
                                                 family_var2 == "tumor_char_surg"   ~ "Pathology",
                                                 family_var2 == "tumor_char_neo"    ~ "Pathology",
                                                 family_var2 == "events_and_censor"   ~ "Oncologic events",
                                                 TRUE ~ family_var2
                                                 )) 

new_datadict_SEER_breast$family_var2 %>% unique()

new_datadict_SEER_breast %>% select(var, family_var2,groups) %>% arrange(groups)

save(new_datadict_SEER_breast, file = "~/RT2Lab/databases/core/21_SEER_Breast/doc/new_datadict_SEER_breast.RData")
head(new_datadict_SEER_breast)
load("/Users/ahamypet/RT2Lab/databases/core/21_SEER_Breast/doc/new_datadict_SEER_breast.RData")
nrow(new_datadict_SEER_breast)



