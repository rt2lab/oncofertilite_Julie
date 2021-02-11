##########################################################################################################
##########################################################################################################
###########################################################################################################
###########################################  Tableaux au propre ############################################
##########################################################################################################




install.packages("ggplot2")
install.packages('table1')
install.packages('psych')

library(ggplot2):;
library(table1)
library(psych)
library(kableExtra)
library(forcats)
library('dplyr')
source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))


######################################################################################################
#######################################################################################################

# Création de la variable chemotherapy setting 

base_julie$ct_setting_5cl.2  <- NA

base_julie$ct_setting_5cl.2[base_julie$ct_setting_5cl == "Chemotherapy without surgery" & base_julie$ct_setting_5cl =="NAC and adjuvant" & base_julie$ct_setting_5cl == "No" ] <- NA
base_julie$ct_setting_5cl.2[base_julie$ct_setting_5cl=="NAC"] <- "NAC"
base_julie$ct_setting_5cl.2[base_julie$ct_setting_5cl== "Adjuvant"] <- "Adjuvant"
table(base_julie$ct_setting_5cl.2)


base_julie$center_curie.2 = NA
base_julie$center_curie.2[base_julie$center_curie == "Others"] <- NA
base_julie$center_curie.2[base_julie$center_curie == "Curie Paris"] <- "Curie Paris"
base_julie$center_curie.2[base_julie$center_curie == "Curie St Cloud"] <- "Curie St Cloud"
table(base_julie$center_curie.2)

base_julie$bmi_4cl_ord <- fct_relevel(base_julie$bmi_4cl,"<18.5", "18.5-24.9", "25-29.9",">=30")


data_fertil_preserv = base_julie %>% filter(fertil_preserv=="Yes")

data_fertil_preserv$fertil_miv_cos_2 <- NA
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "No"] <- "IVM"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "No"] <- NA
table(data_fertil_preserv$fertil_miv_cos_2)




var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


##############################################################################################################



##################################################### Table1 ##############################################




tab0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_julie, missing = F, perc_by_column = F)


tab0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Patients Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')




################################################# Table colonne âge##########################################

a = base_julie %>% subset(is.na(age_young_cl_40_bin))


var_selected<-c("nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


table1<-preformatTable1(stratif = "age_young_cl_40_bin", stratif_order = c("[0 -40)","40+"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_julie, missing = F, perc_by_column = F)


table1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Age")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



################################################### Table treatment center colonnnes #####################################

a = base_julie %>% subset(is.na(center_curie.2))


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab2<-preformatTable1(stratif = "center_curie.2", stratif_order =c("Curie Paris","Curie St Cloud") , stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_julie, missing = F, perc_by_column = F)


tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Treatment center")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_treatment_center_csv.xlsx')



############################################################## Table fertility discussion lignes #############################




var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting")



tab7<-preformatTable1(stratif = "pf_discussion", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_julie, missing = F, perc_by_column = TRUE)


tab7[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Fertility Procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fp_discussion_csv.xlsx')



############################################################### Table fertility procedures lignes #################################@


a = base_julie %>% subset(is.na(fertil_preserv))

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab8<-preformatTable1(stratif = "fertil_preserv", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_julie, missing = F, perc_by_column = TRUE)


tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_csv.xlsx')




################@@@######################################## Type de fertility procedure  colonne #############################################


a = data_fertil_preserv %>% subset(is.na(fertil_miv_cos_2))

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab13<-preformatTable1(stratif = "fertil_miv_cos_2", stratif_order = c("IVM","At least one COS"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab13[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab13[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_miv_cos_csv.xlsx')


####################################################### Type de centre de fertilité colonne #############################################

a = data_fertil_preserv %>% subset(is.na(center_fpp))


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab12<-preformatTable1(stratif = "center_fpp", stratif_order = c("Bondy", "Clamart", "Others", "Port Royal"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab12[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who are the patients in the different fertility centers? ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab12[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table12_center_fpp_csv.xlsx')




