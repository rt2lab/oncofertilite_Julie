# Rendu 2 : Storytelling Patiente en fertility procedure 

## téléchargement de la base de données 

data = X07_oncofertilite_consore_preprocessed_labels

summary(data)

data_v1 = read.csv2("/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/data_v1.csv")

summary(data_v1)

source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))

#library et installation de package

install.packages("ggplot2")
install.packages('table1')
install.packages('psych')

library(ggplot2)
library(table1)
library(psych)

# I : Qui vient en consultation 

# variables patient_char

# age, age_cl_young, nb_child, bmi, weight 

# tableau de répartition des effectifs 

table1(~ bmi_3cl + factor(year_diag)+ center_curie + brca_screen + nb_child_3cl| age_young_cl, data=data_v1)


# Analyse de l'âge et graphique histogramme 

summary(data_v1$age)
summary(data$age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 28.0   331.0   379.0   346.2   413.0   439.0       2 

# la colonne âge est claquée 

# g = ggplot() +geom

theme_set(theme_classic())

# Histogram on a Continuous (Numeric) Variable
g <- ggplot(data_v1, aes(age_young_cl)) 

g + geom_histogram(stat = "count") +  
  labs(title="Histogram with Age", 
       subtitle="age_young_cl") + geom_text(stat='count', aes(label=..count..), vjust=-1, size=2) + xlab("Age Categories")+ theme_classic()


# II. Quelles sont les caractéristiques des patients au diagnostique 

# variables bc_diagnosis

# inflammatory_bc, tclin, ctuicc, cnuicc 

# Table 1 

# Représentation graphique 

# inflammatory Breast cancer 

g <- ggplot(data_v1, aes(inflammatory_bc)) 

g + geom_histogram(stat = "count") +  
  labs(title="Histogram with Inflammatory BC", 
       subtitle="inflammatory_bc") + geom_text(stat='count', aes(label=..count..), vjust=-1, size=2) + xlab("Inflammatory BC ")+ theme_classic()


# tclin 

summary(data_v1$tclin)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0    15.0    25.0    30.3    40.0   150.0      11 

g <- ggplot(data_v1, aes(tclin)) 

g + geom_histogram() +  
  labs(title="Histogram with Clinical tumor size", 
       subtitle="tclin") + xlab("Size of Clinical Tumor Size")+ theme_classic()+ 
  geom_bar(stat="count",aes(tclin), width = 0.5,vjust=-1)



# Histogramme clinical tumor size et density 

ggplot(data_v1, aes(x=tclin)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram with Clinical tumor size", 
       subtitle="tclin") + xlab("Size of Clinical Tumor Size") + ylab("Density")


# cnuicc : Clinical nodes stage at diagnosis 

g <- ggplot(data_v1, aes(cnuicc_4cl)) 

g + geom_histogram(stat = "count") +  
  labs(title="Histogram with Clinical nodes stage at diagnosis", 
       subtitle="cnuicc_4cl") + geom_text(stat='count', aes(label=..count..), vjust=-1, size=2) + xlab("Clinical Nodes stage at diagnosis ")+ theme_classic()


# table de pourcentage 

table <- round(prop.table(table(data_v1$cnuicc_4cl))*100,1)
table

# N0   N1   N2   N3 
# 63.4 35.8  0.5  0.2


# on crée une variable tclin divisée équitablement pour en faire une variable catégorielle

summary(data_v1$tclin)

data_v1$tclin_2 <- cut(data_v1$tclin, c(0, 20, 40, 60, 80,100,150), include.lowest = TRUE, labels = c("<20mm", "20-40 mm", "40-60 mm", "60-80mm","80-100mm", "100-150mm"))
table(data_v1$tclin_2)


# tableau variables cliniques 

# tableau pour cnuicc 
# rajouter un titre sur les tableaux ??

table1(~ age_young_cl + inflammatory_bc + nb_child_3cl + tclin_2| cnuicc_4cl, data=data_v1)


table1(~ age_young_cl + inflammatory_bc + nb_child_3cl + tclin| cnuicc_4cl, data=data_v1, render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]"))


# tableau pour tclin_2

table1(~ age_young_cl + inflammatory_bc + nb_child_3cl + cnuicc_4cl| tclin_2, data=data_v1)



# boxplot tclin in fonction age_young_cl avec projection de points

ggplot(data_v1) +
  geom_boxplot(aes(x = age_young_cl, y = tclin)) + 
  geom_point(aes(x = age_young_cl, y = tclin), col = "red", alpha = 0.2) +  
  labs(title="Boxplot of the size of the tumor as a function of age", 
       subtitle="age_young_cl / tclin") + xlab("Age Categories")+ ylab("Clinical Tumor size (mm)")

ggplot(data_v1) +
  geom_violin(aes(x = age_young_cl, y = tclin)) + 
  geom_point(aes(x = age_young_cl, y = tclin), col = "red", alpha = 0.2) +  
  labs(title="Boxplot of the size of the tumor as a function of age", 
       subtitle="age_young_cl / tclin") + xlab("Age Categories")+ ylab("Clinical Tumor size (mm)")


# boxplot tclin in function of cnuicc_4cl

ggplot(data_v1) +
  geom_violin(aes(x = cnuicc_4cl, y = tclin)) + 
  geom_point(aes(x = cnuicc_4cl, y = tclin), col = "red", alpha = 0.2) +  
  labs(title="Violinplot of the size of the tumor as a function of clinical N stage", 
       subtitle="cnuicc_4cl / tclin") + xlab("Clinical N stage at diagnosis")+ ylab("Clinical Tumor size (mm)")


#III. Au niveau biologique 

# Category : bc_biology 
# Variables : Var : toutes les réponses aux hormones (qu’on n’a pas encore), 
# luminal, tnbc , subtype, histo_3cl, grade

# Table selon le subtype de cancer du sein 

table1(~ age_young_cl + nb_child_3cl + tclin+ cnuicc_2cl + grade_3cl| histo_3cl, data=data_v1, render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]"))


# III. bis : qui a eu la chirurgie du sein ou non ? 

table(data_v1$breast_surgery)

# No  Yes 
# 1   1356 

table <- round(prop.table(table(data_v1$breast_surgery))*100,1)
table

# No  Yes 
# 0.1 99.9 

# IV. Quels traitements ont-elles reçu ? 

# Category : neoadj_or_not

# Variables : neo_ct , primary_ttt, ct_setting_5cl

table <- round(prop.table(table(data_v1$neo_ct))*100,1)
table

# No  Yes 
# 54.9 45.1 

table <-round(prop.table(table(data_v1$primary_ttt))*100,1)
table 

# Neoadjuvant treatment               Surgery 
# 45.1                                54.9 



table <- round(prop.table(table(data_v1$ct_setting_5cl))*100,1)
table

# Adjuvant Chemotherapy    without surgery                   NAC 
# 54.9                          0.1                         45.1 

# tableau 

table1(~ age_young_cl + nb_child_3cl+ tclin+ cnuicc_2cl + grade_3cl + histo_3cl| primary_ttt, data=data_v1, render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]"))



table <- round(prop.table(table(data_v1$grade_3cl))*100,1)
table

# Grade I  Grade II Grade III 
# 4.3      39.2      56.5 


table <- round(prop.table(table(data_v1$histo_3cl))*100,1)
table
# 
# Lobular     NST  Others 
# 4.0         93.4     2.7


#création des tableaux grâce à la fonction de Nadir 

library(tableone)
library(tidyverse)

# définition des vecteurs de variables 

# attention cette fonction renvoie une liste de deux objets : le premier est le data frame et le second est la caption je pense


var_selected<-c("bmi_3cl", "center_curie", "brca_mut", "nb_child_3cl", "age_young_cl")

names_var_selected <-c("BMI", "Curie Center", "BRCA", "Number of child", "Age Categories")

tab1<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)

# 
# Variable name          level      Overall
# 1                                 n         1357
# 2                BMI          <18.5     79 (6.3)
# 3                              >=25   364 (29.1)
# 4                         18.5-24.9   807 (64.6)
# 5  Year of Diagnosis                2014.0 (1.9)
# 6       Curie Center    Curie Paris   818 (60.3)
# 7                    Curie St Cloud   538 (39.7)
# 8               BRCA             No   547 (79.6)
# 9                               Yes   140 (20.4)
# 10   Number of child              0   373 (27.5)
# 11                                1   279 (20.6)
# 12                      More than 1   705 (52.0)
# 13    Age Categories        [0 -30)     95 (7.0)
# 14                         [30 -35)   246 (18.2)
# 15                         [35 -40)   460 (33.9)
# 16                              40+   554 (40.9)
# 
# [[2]]
# [1] "Missing data: BMI, n=107; Year of Diagnosis, n=2; Curie Center, n=1; BRCA, n=670; Age Categories, n=2"
# 

library(kableExtra)
library(xlsx)
tab1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Table Baseline Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab1[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_csv.xlsx')
  
# baseline characteristics table1  
table1(~ age_young_cl + bmi_3cl + center_curie + brca_screen + nb_child_3cl, data=data_v1)


# baseline diagnosis characteristics 

var_selected<-c("tclin", "tclin_2", "cnuicc_4cl","muicc")

names_var_selected <-c("Clinical Tumor Size ", "Clinical Tumor size categories", "Clinical N stage at diagnosis", "Distant Metastases at diagnosis")

tab3<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab3[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Table Baseline Diagnosis Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab3[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_diag_csv.xlsx')


# baseline at Biology stage 

var_selected<-c("primary_ttt", "neo_ct", "histo_3cl","grade_3cl")

names_var_selected <-c("Primary treatment ", "Neoadjuvant chemotherapy", "Histology", "Grade")

tab4<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab4[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Table Baseline Biological Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab4[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_bio_csv.xlsx')

# Caractéristiques des patientes neo_ct

var_selected<-c("age_young_cl","nb_child_3cl","tclin_2", "cnuicc_4cl","inflammatory_bc","grade_3cl", "histo_3cl", "center_curie")

names_var_selected <-c("Age", "Number of children", "T (mm)", "Number of Nodes", "Inflammatory BC", "Grade", "Histology", "Curie Center")


tab5<-preformatTable1(stratif="neo_ct" , stratif_order =c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


library(kableExtra)

tab5[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Table Fertility procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab5[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_neoct_csv.xlsx')



# caractéristiques des patientes grade 



var_selected<-c("age_young_cl","nb_child_3cl","tclin_2", "cnuicc_4cl","inflammatory_bc","neo_ct", "histo_3cl", "center_curie")

names_var_selected <-c("Age", "Already has a child at diag", "T (mm)", "Number of Nodes", "Inflammatory BC", "Neo-adjuvant chemotherapy", "Histology", "Curie Center")



tab6<-preformatTable1(stratif="grade_3cl" , stratif_order =c("Grade I","Grade II", "Grade III"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


library(kableExtra)

tab6[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Grade")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab6[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_grade_csv.xlsx')






# pf discussion and characteristics 

var_selected<-c("age_young_cl","prev_child", "tclin","cnuicc_4cl","inflammatory_bc","grade_3cl", "histo_3cl", "neo_ct",  "center_curie")

names_var_selected <-c("Age", "Already has a child at diag", "T (mm)", "Number of Nodes", "Inflammatory BC", "Grade", "Histology", "Chemotherapy", "Curie Center")



tab2<-preformatTable1(stratif="pf_discussion" , stratif_order =c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


library(kableExtra)

tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Table Fertility procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_fp_csv.xlsx')



#Baseline fertility procedure 

var_selected<-c("pf_discussion", "fertil_preserv", "frozen_oocytes","frozen_embryos", "ivm", "cos")

names_var_selected <-c("FP Discussion", "FP accepted", "Frozen oocytes", "Froen embryos", "IVM", "COS")

tab8<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Fertility procedure")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_fertilityprocedure_csv.xlsx')



#Baseline frozen material 

var_selected<-c("return_center_pf", "reuse_frozen_material", "reuse_frozen_cortex","reuse_frozen_embryo", "reuse_frozen_oocytes")

names_var_selected <-c("Return to fp center", "Frozen Material reused", "Frozen cortex reused", "Froen embryos reused", "frozen oocytes reused")

tab9<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab9[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Frozen Fertility procedure")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_frozen_fertilityprocedure_csv.xlsx')



# Characteristics among fertility preservation 




var_selected<-c("age_young_cl","prev_child", "tclin","cnuicc_4cl","inflammatory_bc","grade_3cl", "histo_3cl", "neo_ct",  "center_curie", "pf_discussion", "amh", "cfa")

names_var_selected <-c("Age", "Already has a child at diag", "T (mm)", "Number of Nodes", "Inflammatory BC", "Grade", "Histology", "Chemotherapy", "Curie Center", "Had a FP discussion","Amh", "Cfa")



tab10<-preformatTable1(stratif="fertil_preserv" , stratif_order =c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


library(kableExtra)

tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Characteristics as a function of FP")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_fpcharacteristics_csv.xlsx')



### Baseline traitement reçu par les patients 

var_selected<-c("neo_ct", "breast_surgery", "primary_ttt","ct_setting_5cl")

names_var_selected <-c("Neoadjuvant therapy", "Breast Surgery", "Primary treatment", "Adjuvant or NAC")

tab11<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


tab11[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Treatment")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab11[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_baseline_treatment_csv.xlsx')


# Who reuse their frozen material ? 


var_selected<-c("age_young_cl","prev_child", "tclin","cnuicc_4cl","inflammatory_bc","grade_3cl", "histo_3cl", "neo_ct",  "center_curie", "pf_discussion")

names_var_selected <-c("Age", "Already has a child at diag", "T (mm)", "Number of Nodes", "Inflammatory BC", "Grade", "Histology", "Chemotherapy", "Curie Center", "Had a FP discussion")



tab12<-preformatTable1(stratif="reuse_frozen_material" , stratif_order =c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_v1, missing = F, perc_by_column = F)


library(kableExtra)

tab12[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who reused their frozen material ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab12[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_frozenmat_characteristics_csv.xlsx')






