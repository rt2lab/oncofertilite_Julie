####################### Mise au propre des tableaux projet oncofertilité##########################
##################################################################################################


###################### Patients Characteristics ###################################################

install.packages("ggplot2")
install.packages('table1')
install.packages('psych')

library(ggplot2)
library(table1)
library(psych)
library(kableExtra)

source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))


# vecteur de variables qu'on a sélectionné 

# ordonner la variable bmi_4cl

data_v1$bmi_4cl_ord <- fct_relevel(data_v1$bmi_4cl,"<18.5", "18.5-24.9 ", "25-29.9",">=30")

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


# baseline characteristics 



tab0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Patients Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')



# Tableau croisé Patients characteristics 

# AGE/ baseline var 

var_selected<-c( "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab1<-preformatTable1(stratif = "age_young_cl", stratif_order = c("[0 -30)","[30 -35)", "[35 -40)", "40+" ), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Age")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")

# Treatment center / baseline var 

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab2<-preformatTable1(stratif = "center_curie", stratif_order =c("Curie Paris","Curie St Cloud") , stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Treatment center")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_treatment_center_csv.xlsx')


##### A compléter : si on a le temps. 

# Hereditary var/baseline var  ou ? 


############################################################################################################
#####################  Patients Clinical characteristics ##################################################

###### Grade VS baseline 
var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")




tab3<-preformatTable1(stratif = "grade_3cl", stratif_order = c("Grade I", "Grade II", "Grade III"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab3[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Grade")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab3[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')

####### Histo VS baseline 


var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")



tab4<-preformatTable1(stratif = "histo_3cl", stratif_order = c("NST", "Lobular", "Others"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab4[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Grade")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab4[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table4_histology_baseline_csv.xlsx')



########################################## Treatment VS baseline ########################################################################
#########################################################################################################################################


# neoadjuvant therapy 

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Chemotherapy setting", "Fertility preservation discussion")



tab5<-preformatTable1(stratif = "neo_ct", stratif_order = c("No", "Yes"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab5[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Neoadjuvant therapy")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab5[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table5_neoadjuvanttherapy_csv.xlsx')



##### Baseline vs Chemotherapy setting 

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Fertility preservation discussion")



tab6<-preformatTable1(stratif = "ct_setting_5cl", stratif_order = c("Adjuvant", "NAC"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab6[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Chemotherapy setting")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab6[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_chemotherapysetting_csv.xlsx')



##################################################################################################################################################################################################
######################################################################################################################################################################################################
############################################## Fertility Procedure #####################################################################################

# Fertility Discussion 


var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting")




tab7<-preformatTable1(stratif = "pf_discussion", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab7[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Fertility Procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fp_discussion_csv.xlsx')




#Fertil_preserv : qui accepte la procédure de fertilité 


var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab8<-preformatTable1(stratif = "fertil_preserv", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_csv.xlsx')



# Frozen material : who reuse frozen material ? 


var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab9<-preformatTable1(stratif = "reuse_frozen_material", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = TRUE)


tab9[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who chooses to reuse frozen material ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab9[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table9_reuse_frozen_material_csv.xlsx')



# ivm 

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab10<-preformatTable1(stratif = "ivm", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = TRUE)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among the women who had fertility preservation procedures, who chooses the ivm procedure ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table10_ivm_csv.xlsx')



#cos 

var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab11<-preformatTable1(stratif = "cos", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = TRUE)


tab11[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who chooses the cos procedure ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab11[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table11_cos_csv.xlsx')


# cfa and amh decsription 

#cfa all population that has fertility procedure

ggplot(data_fertil_preserv, aes(x=cfa)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram with cfa", 
       subtitle="cfa") + xlab("Cfa") + ylab("Density")

# amh all population that has fertility procedure 

ggplot(data_fertil_preserv, aes(x=amh)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram with Amh", 
       subtitle="amh") + xlab("Amh") + ylab("Density")


# cfa en fonction de caractéristiques 

#age 
ggplot(data_fertil_preserv, aes(x=cfa)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram with cfa", 
       subtitle="cfa") + xlab("Cfa") + ylab("Density")+ facet_grid(. ~age_young_cl)

#age 

ggplot(data_fertil_preserv, aes(x=cfa,colour=age_young_cl)) + 
  geom_density(alpha=.2) +
  labs(title="Density of Cfa as a function of Age", 
       subtitle="cfa") + xlab("Cfa") + ylab("Density")

# brca_mut 

ggplot(data_fertil_preserv, aes(x=cfa,colour=brca_mut,fill=brca_mut)) + 
  geom_density(alpha=.2) +
  labs(title="Density of Cfa as a function of Brca mutation", 
       subtitle=" Brca mutation") + xlab("Brca mutation observed") + ylab("Density")

ggplot(data_fertil_preserv, aes(x=cfa)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Cfa as a function of Brca mutation", 
       subtitle="cfa") + xlab("Brca mutation observed") + ylab("Density")+ facet_grid(. ~brca_mut)


#amh as a function of characteristics 

#age


ggplot(data_fertil_preserv, aes(x=amh,colour=age_young_cl)) + 
  geom_density(alpha=.2) +
  labs(title="Density of Amh as a function of Age", 
       subtitle="Amh") + xlab("Amh") + ylab("Density")

ggplot(data_fertil_preserv, aes(x=amh)) + 
  geom_density(alpha=.2,fill="#FF6666") +
  labs(title="Density of Amh as a function of Age", 
       subtitle="Amh") + xlab("Amh") + ylab("Density")+ facet_grid(. ~age_young_cl)



#brca_mut 

ggplot(data_fertil_preserv, aes(x=amh,colour=brca_mut,fill = brca_mut)) + 
  geom_density(alpha=.2) +
  labs(title="Density of Amh as a function of Brca mutation", 
       subtitle=" Brca mutation") + xlab("Brca mutation observed") + ylab("Density of Amh")

ggplot(data_fertil_preserv, aes(x=amh)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram with Amh", 
       subtitle="Amh") + xlab("Brca mutation observed") + ylab("Density of Amh")+ facet_grid(. ~brca_mut)

# center, neo_ct, fp_discussion

# Boxplot amh as a function of neo_ct


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = amh, x = neo_ct ,fill= neo_ct), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = amh, x = neo_ct) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Amh as a function of Neo_ct", 
       subtitle="Amh/neo_ct") + xlab("Neoadjuvant Chemotherapy")+ ylab("amh")

# Violin plot amh as a function of center curie


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = amh, x = center_curie ,fill= center_curie), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = amh, x = center_curie) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Amh as a function of Treatment center", 
       subtitle="Amh/center_curie") + xlab("Treatment center")+ ylab("amh")


#Violin plot amh as a function of fpp center 


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = amh, x = center_fpp ,fill= center_fpp), adjust = .5, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = amh, x = center_fpp) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Amh as a function of Fpp center", 
       subtitle="Amh/center_fpp") + xlab("Treatment center")+ ylab("amh")


# violin plot as a function of fp_discussion 


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = amh, x = pf_discussion ,fill= pf_discussion), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = amh, x = pf_discussion) , col = "black", alpha = 0.2) +  
  labs(title="Amh as a function of Fertility preservation Discussion", 
       subtitle="Amh/pf_discussion") + xlab("Fertility Preservation Discussion")+ ylab("amh")



################# même chose pour amh
# Boxplot cfaas a function of neo_ct


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = cfa, x = neo_ct ,fill= neo_ct), adjust = 1, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = cfa, x = neo_ct) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Cfa as a function of Neo_ct", 
       subtitle="cfa/neo_ct") + xlab("Neoadjuvant Chemotherapy")+ ylab("cfa")

# Violin plot cfa as a function of center curie


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = cfa, x = center_curie ,fill= center_curie), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = cfa, x = center_curie) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Cfa as a function of Treatment center", 
       subtitle="Cfa/center_curie") + xlab("Treatment center")+ ylab("cfa")


#Violin plot cfa as a function of fpp center 




ggplot(data_fertil_preserv) +
  geom_violin(aes(y = cfa, x = center_fpp ,fill= center_fpp), adjust = .5, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = cfa, x = center_fpp) , col = "black", alpha = 0.2) +  
  labs(title="Violinplot of Cfa as a function of Fpp center", 
       subtitle="cfa/center_fpp") + xlab("Treatment center")+ ylab("cfa")


# violin plot as a function of fp_discussion 


ggplot(data_fertil_preserv) +
  geom_violin(aes(y = cfa, x = pf_discussion ,fill= pf_discussion), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = cfa, x = pf_discussion) , col = "black", alpha = 0.2) +  
  labs(title="Cfa as a function of Fertility preservation Discussion", 
       subtitle="cfa/pf_discussion") + xlab("Fertility Preservation Discussion")+ ylab("cfa")



ggplot(data_v1) +
  geom_violin(aes(y = cfa, x = pf_discussion ,fill= pf_discussion), adjust = .8, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_jitter(aes(y = cfa, x = pf_discussion) , col = "black", alpha = 0.2) +  
  labs(title="Cfa as a function of Fertility preservation Discussion", 
       subtitle="cfa/pf_discussion") + xlab("Fertility Preservation Discussion")+ ylab("cfa")



#### tableau 12 : Qui va dans les fertility center 

library(dplyr)

data_fertil_preserv =data_v1 %>%filter(fertil_preserv=="Yes")



var_selected<-c("age_young_cl", "nb_child_3cl", "bmi_4cl_ord", "center_curie", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age", "Number of children", "BMI", "Treatment center", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab12<-preformatTable1(stratif = "center_fpp", stratif_order = c("Bondy", "Clamart", "Others", "Port Royal"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = TRUE)


tab12[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who are the patients in the different fertility centers? ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab12[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table12_center_fpp_csv.xlsx')

####### Among the women who had fertility preservtaion, what kind of procedure is chosen ? 


var_selected<-c("ivm", "cos", "frozen_oocytes", "frozen_embryos")

names_var_selected <-c("IVM", "COS", "Frozen Ovocytes", "Frozen embryos")


tab13<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = TRUE)


tab13[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among the women who had fertility preservtaion, what kind of procedure is chosen  ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab13[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table12_type_procedure_csv.xlsx')




