################################################################################################################################################################
################################################################################################################################################################
############################################# Storytelling 4 : Analyse par colomnes. ############################################################################
#################################################################################################################################################################
##################################################################################################################################################################


install.packages("ggplot2")
install.packages('table1')
install.packages('psych')

library(ggplot2)
library(table1)
library(psych)
library(kableExtra)
library(forcats)

source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))


# vecteur de variables qu'on a sélectionné 

# ordonner la variable bmi_4cl

data_v2 = database_preprocessed_labels

data_v2$bmi_4cl_ord <- fct_relevel(data_v2$bmi_4cl,"<18.5", "18.5-24.9 ", "25-29.9",">=30")

data_v2$amh_num<-as.numeric(data_v2$amh)
cfa_num <- as.numeric(data_v2$cfa)

library(dplyr)
data_fertil_preserv = data_v2 %>% filter(fertil_preserv=="Yes")

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


# Part I : social charcateristics


# AGE/ baseline var 

var_selected<-c( "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_2cl","grade_2cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


table1<-preformatTable1(stratif = "age_young_cl_40_bin", stratif_order = c("[0 -40)","40+"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


table1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Age")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")


# Treatment center / baseline var 

var_selected<-c("age_young_cl_40_bin","age", "nb_child_3cl", "bmi_4cl_ord", "bmi","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_2cl","grade_2cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Age (mean)", "Number of children", "BMI","BMI (mean)","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab2<-preformatTable1(stratif = "center_curie", stratif_order =c("Curie Paris","Curie St Cloud") , stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Treatment center")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_treatment_center_csv.xlsx')


#######Partie 2 

###### Grade VS baseline 
var_selected<-c("age_young_cl_40_bin","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie", "brca_screen","brca_mut", "inflammatory_bc","tclin" ,"cnuicc_2cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")




tab3<-preformatTable1(stratif = "grade_2cl", stratif_order = c("Grade I-II", "Grade III"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


tab3[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Grade")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab3[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')




########## Partie 3 

#########  neo_adj




# neoadjuvant therapy 

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Chemotherapy setting", "Fertility preservation discussion")



tab5<-preformatTable1(stratif = "neo_ct", stratif_order = c("No", "Yes"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


tab5[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Neoadjuvant therapy")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab5[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table5_neoadjuvanttherapy_csv.xlsx')


####### Partie 4 

### Who had the talk 



var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)","Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting")




tab7<-preformatTable1(stratif = "pf_discussion", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


tab7[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Fertility Procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fp_discussion_csv.xlsx')




#Partie 5 : fertility procedure

## Fertil_preserv


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab8<-preformatTable1(stratif = "fertil_preserv", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v2, missing = F, perc_by_column = F)


tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_csv.xlsx')



# Fertility center 



var_selected<-c("age_young_cl_40_bin","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_2cl","grade_2cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab12<-preformatTable1(stratif = "center_fpp", stratif_order = c("Bondy", "Clamart", "Others", "Port Royal"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab12[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who are the patients in the different fertility centers? ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab12[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table12_center_fpp_csv.xlsx')


# ivm 

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab10<-preformatTable1(stratif = "ivm", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among the women who had fertility preservation procedures, who chooses the ivm procedure ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table10_ivm_csv.xlsx')



# cos 


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic Analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab10<-preformatTable1(stratif = "cos", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = FALSE)


tab11[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among the women who had fertility preservation procedures, who chooses the cos procedure ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab11[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table10_cos_csv.xlsx')



######## dernier tableau cos ivm




# variables sur les personnes qui choissisent de faire data_fertil_preserv
data_fertil_preserv$fertil_miv_cos_2 <- NA
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "No"] <- "IVM"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "No"] <- NA
table(data_fertil_preserv$fertil_miv_cos_2)

# At least one COS              IVM 
# 83                           172 


# tableau en ligne quel type de procédure est choisie ? 


#  data_fertil_preserv$bmi = as.numeric(data_fertil_preserv$bmi)
# Warning message:
#   NAs introduits lors de la conversion automatique 
# class(data_fertil_preserv$bmi)
# [1] "numeric"
# 

var_selected<-c("age_young_cl_40_bin","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie", "brca_screen","brca_mut", "inflammatory_bc","tclin" ,"cnuicc_2cl","grade_2cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center", "Genetic Analysis","Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab13<-preformatTable1(stratif = "fertil_miv_cos_2", stratif_order = c("IVM","At least one COS"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = TRUE)


tab13[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab13[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_miv_cos_csv.xlsx')










