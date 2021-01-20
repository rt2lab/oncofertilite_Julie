# Rendu 2 : Storytelling Patiente en fertility procedure 

## téléchargement de la base de données 

data = X07_oncofertilite_consore_preprocessed_labels

summary(data)

data_v1 = read.csv2("C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/data_v1.csv")

summary(data_v1)


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


