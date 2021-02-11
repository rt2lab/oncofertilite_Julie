################################################################################################
################################################################################################
###########@@########################### Regression logistique #################################


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




var_selected<-c("age_young_cl","nb_child_3cl", "bmi_4cl_ord", "center_curie.2","brca_mut", "inflammatory_bc", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2")     




names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center","Hereditary predisposition", "Inflammatory BC", "Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting")
                      


################################################################## Regression logistique pf_discussion 

lm =logisticRegressionTable(base_julie,var_selected,names_var_selected, var_to_explain= "pf_discussion",level_to_import="Yes",variables_use_multivariate = NA, alpha_cut_multivariate = 1, all_multivariate_values = F)

lm%>% kbl("latex", align = "llr", vline = "|", caption = "Logistic regression results for fertility preservation discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



################################################################### Regression logistique fertil_preserv 



var_selected<-c("age_young_cl","nb_child_3cl", "bmi_4cl_ord", "center_curie.2","brca_mut", "inflammatory_bc", "ctuicc_3cl","cnuicc_2cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")     


names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center","Hereditary predisposition", "Inflammatory BC", "Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion") 




lm2 =logisticRegressionTable(base_julie,var_selected,names_var_selected, var_to_explain= "fertil_preserv",level_to_import="Yes",variables_use_multivariate = NA, alpha_cut_multivariate = 1, all_multivariate_values = F)


lm2%>% kbl("latex", align = "llr", vline = "|", caption = "Logistic regression results for Fertility preservation procedure")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



