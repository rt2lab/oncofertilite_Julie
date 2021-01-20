######################## Time-to-pregnancy project###################

#### On utilise la base oncofertilite_consore... C:\Users\Julie\Desktop\Institut Curie\07_oncofertilite_consore_preprocessed_labels.csv


setwd(dir ="C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/")
data_dir = 'data/'


## téléchargement de la base de données 

data = X07_oncofertilite_consore_preprocessed_labels

summary(data)

data_v1 = read.csv2("C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/data_v1.csv")

summary(data)


#library 

install.packages("ggplot2")

library(ggplot2)




# première description de la base de donnée 

summary(data)

str(data)

# on a 1357 observations et 274 variables 
# on remarque dans un premier temps qu'il y a un nombre de Na très important
# que fait-on des NA ? 
library(psych)
describe(data)


install.packages('funModeling')
library(funModeling)
df_status(data_v1)


profiling_num(data_v1)


# ##################### Première analyse : découverte de la base############################

########################## Distribution de l'âge ############################## 

# A priori on a 1357 observations donc 1357 patientes. 

describe(data$age)
# en moyenne, les femmes de l'échantillon ont 37,8 ans (38 ans); la médiane est à 38 ans. 
# la plus jeune a un peu plus de 22 ans et la plus âgée a presque 44 ans. 

hist(data$age, main = "Distribution du nombre de patients en fonction de l'âge", xlab = "Âge", ylab = "Effectif")

#il faut que je crée une variable numérique catégorielle d'âge ...

hist(data$age_young_cl, main = "Distribution du nombre de patients en fonction de la classe d'âge", xlab = "Âge", ylab = "Effectif")

table(data$age_cl_10_1)

##### [0 -30) [30 -40) [40 -50) 
#########95      706      554 

round(prop.table(table(data$age_cl_10_1))*100,1)
round(prop.table(table(data$age_young_cl))*100,1)

############################ Fertility procedure################################

table(data_v1$pf_discussion)

# No Yes 
# 909 447 

round(prop.table(table(data_v1$pf_discussion))*100,1)
# No Yes 
# 67  33 

ggplot(data_v1) +
  aes(x = pf_discussion) +
  geom_bar(stat = "count") +
  xlab("Having the discussion") +
  ylab("Count")+ theme_classic() +
  ggtitle("Discussion about fertility procedure")+ 
  geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)



table(data$pf_discussion, data$age_cl_10_1)
round(prop.table(table(data$pf_discussion,data$age_cl_10_1))*100,1)


#[0 -30) [30 -40) [40 -50)
#No      1.7     27.4     37.9
#Yes     5.3     24.7      3.0

mosaicplot(age_cl_10_1 ~ pf_discussion, data = data, shade = TRUE, main = "Age Vs Fp discussion", xlab= "Age Categories", ylab= "Having the discussion")
# lecture de ce graphique : chaque rectangle repr?sente une case du tableau. sa largeur correspond aux 
# pourcentages en colonnes (il y a peu de femmes patientes entre 0-30 ans et beaucoup de femmes entre 30-40 ans)
# la hauteur d'une case repr?sente le pourcentage en ligne  : Plus de la  majorit? femmes patientes entre 0-30 ans ont eu une disscussion
# sur les proc?dures de fertilit? . On voit que ce n'est clairement pas le cas des femmes patientes netre 40-50 ans. 
# Interpr?tation des couleurs : cela correspond au r?sidu du test du Chi 2: les cases en rouge sont sous-repr?sent?es
# Les cases en bleues sont sur-repr?sent?e. Donc les femmes entre 40-50 ans qui ont la discussion sont sous-repr?sent?es. 
# Les femmes entre 0-30 ans qui n'ont pas la discussion sont sous-repr?sent?e aussi. 

# customisation de nos barplot 
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 



install.packages('rlang')
library(rlang)
install.packages('ggplot2')
library(ggplot2)

table0 <- round(prop.table(table(data_v1$pf_discussion,data_v1$age_young_cl))*100,1)
table0

#       [0 -30) [30 -35) [35 -40)  40+
#No      1.7      5.4     22.0   37.9
#Yes     5.3     12.8     12.0    3.0


g <- ggplot(data_v1, aes(pf_discussion))
g + geom_bar(aes(fill=age_young_cl), width = 0.5) +
  labs(title="Fertility Discussion Vs Age", 
       caption="data_v1")

g <- ggplot(data_v1, aes(age_young_cl))
g + geom_bar(aes(fill=pf_discuss), width = 0.5) +
  labs(title="Fertility Discussion Vs Age", 
       caption="data_v1")


# en fonction du nombre d'enfants 

# Transform this data in %

table <- round(prop.table(table(data_v1$pf_discussion,data_v1$nb_child_3cl))*100,1)
table

#######0    1 More than 1
#No  10.4 13.3        43.4
#Yes 17.0  7.3         8.6

# Make a stacked barplot--> it will be in %!
barplot(table, col=coul , border="white", main = "Discussion sur la fertilit? en fonction du nombre d'enfant", xlab="Nombre d'enfant avant le diagnostique de BC", 
        ylab="% patientes ayant eu la discussion",legend.text = TRUE, 
        args.legend = list(x = "topright", bty = "n"))


g <- ggplot(data_v1, aes(nb_child_3cl))
g + geom_bar(aes(fill=pf_discussion), width = 0.5) +
  labs(title="Fertility Discussion Vs Nb of children", 
       caption="data_v1")


g <- ggplot(data_v1, aes(pf_discussion))
g + geom_bar(aes(fill=nb_child_3cl), width = 0.5) +
  labs(title="Fertility Discussion Vs Nb of children", 
       caption="data_v1")




# en fonction du year of BC diagnostic

# Transform this data in %

table1 <- round(prop.table(table(data_v1$pf_discussion,data_v1$year_diag))*100,1)
table1

#####2011 2012 2013 2014 2015 2016 2017
#No  10.6 11.7 10.0 11.4  8.3  8.9  6.1
#Yes  1.7  2.3  3.8  5.1  8.0  7.1  5.0

# Make a stacked barplot--> it will be in %!
barplot(table1, col=coul , border="white", main = "Discussion sur la fertilit? en fonction du nombre d'enfant", xlab="ann?e de diagnostique de BC", 
        ylab="% patientes ayant eu la discussion", legend.text = TRUE, 
        args.legend = list(x = "topright", bty = "n"))


g <- ggplot(data_v1, aes(year_diag))
g + geom_bar(aes(fill=pf_discussion), width = 0.5) +
  labs(title="Fertility Discussion Vs Year", 
       caption="data_v1")



##########################Choice of FP procedure #############################################

tablefp <- round(prop.table(table(data_v1$fertil_preserv))*100,1)
tablefp
#tableau des pourcentages
#No  Yes 
#80.7 19.3 

#Nombre de femmes qui ont recours ? ces proc?dures de fertilit?

tablefp1 <- table(data_v1$fertil_preserv)
tablefp1

#No  Yes 
#1095  262 

#Il y a tr?s peu de femmes qui utilisent les proc?dures de fertilit?. Seulement 262 femmes sur l'?chantillon (soit environ 19%)

ggplot(data_v1) +
  aes(x = fertil_preserv) +
  geom_bar(stat = "count") +
  xlab("Choice of FP") +
  ylab("Count")+ theme_classic() +
  ggtitle("Accepting or not fertility procedure")+ 
  geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)






mosaicplot(age_young_cl ~ fertil_preserv, data = data_v1, shade = TRUE, main = "Age and FP choice ", clegend = TRUE, xlab = "Age",
           ylab = "Accept FP")

mosaicplot(nb_child_3cl ~ fertil_preserv, data = data_v1, shade = TRUE, main = "Number of children and Fp choice", clegend = TRUE, xlab = "Number of children",
           ylab = "Accept FP")


# quelles sont les raisons de ce refus de la fertility preservation ? 


tablereason <- round(prop.table(table(data_v1$reason_no_pf_2))*100,1)
tablereason
# % raisons pour lesquelles les femmes refusent ces proc?dures 
#Medical condition     Not discussed   Patient refusal 
#       7.7              76.1              16.1 



tablereason1 <- table(data_v1$reason_no_pf_2)
tablereason1

ggplot(data_v1) +
  aes(y = reason_no_pf_2) +
  geom_bar(stat = "count") +
  xlab("Count") +
  ylab("Reason of refusal")+ theme_classic() +
  ggtitle("Why do women refuse FP ?")+ 
  geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)


# On a vu qu'il y avait 1085 femmes qui refusent ces proc?dures. N?anmoins pour la majorit? des cas, cette raison n'est pas partag?e par le m?decin. 


#Medical condition     Not discussed   Patient refusal 
#        85               835               177 

library(ggplot2)

# en fonction du nombre d'enfant 

boxplot_reason_no_pf1 	<-ggplot(data_v1, aes(x= reason_no_pf_2 , y=nb_child , fill=reason_no_pf_2)) +
  geom_boxplot(aes(x= reason_no_pf_2 , y=nb_child , fill=reason_no_pf_2),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("Why do women refuse ?")+ylab("Number of children before BC diagnosis")  

boxplot_reason_no_pf1 

# en fonction de l'?ge 

boxplot_reason_no_pf2 	<-ggplot(data, aes(x= reason_no_pf_2 , y=age , fill=reason_no_pf_2)) +
  geom_boxplot(aes(x= reason_no_pf_2 , y=age , fill=reason_no_pf_2),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("Why do women refuse ?")+ylab("Patient's age at diag")  

boxplot_reason_no_pf2 


# en fonction de l'amh (niveau d'hormone anti-mulerienne) 

boxplot_reason_no_pf3 	<-ggplot(data_v1, aes(x= reason_no_pf_2 , y=amh , fill=reason_no_pf_2)) +
  geom_boxplot(aes(x= reason_no_pf_2 , y=amh , fill=reason_no_pf_2),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("raisons de refus des pf")+ylab("AMH de la patiente au diag")  

boxplot_reason_no_pf3 



# en fonction du cfa (nombre de follicules) 

boxplot_reason_no_pf4 	<-ggplot(data_v1, aes(x= reason_no_pf_2 , y=cfa , fill=reason_no_pf_2)) +
  geom_boxplot(aes(x= reason_no_pf_2 , y=cfa , fill=reason_no_pf_2),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("Why do women refuse fertility procedure ? ")+ylab("Patient cfa at BC dignosis")  

boxplot_reason_no_pf4 


####################### Analyse de base FP ###############################################

table(data_v1$neo_ct)

# No Yes 
# 744 612 
# 

# On a 744 femmes qui ont un traitement chymio ne. 

table <- round(prop.table(table(data_v1$neo_ct))*100,1)
table


# No  Yes 
# 54.9 45.1 


################################### Delay ###################################################

qplot(data = data_v1, y = growth, x = Year, geom = "line") + 
  facet_wrap(~ cna)




################################   Analyse descriptive des donn?es biologiques   ################################

# amh // cat d'?ge

boxplot_amh_age 	<-ggplot(data_v1, aes(x= age_young_cl , y=amh , fill=age_young_cl)) +
  geom_boxplot(aes(x= age_young_cl , y=amh , fill=age_young_cl),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("Tranche d'?ge")+ylab("Amh de la patiente au diag")  

boxplot_amh_age



#cfa // cat ?ge

boxplot_cfa_age 	<-ggplot(data_v1, aes(x= age_young_cl , y=cfa , fill=age_young_cl)) +
  geom_boxplot(aes(x= age_young_cl , y=cfa , fill=age_young_cl),outlier.shape=NA)	+
  theme_bw()+
  theme(axis.ticks.x = element_blank() , legend.position="none",
        plot.title = element_text(face="plain"))+
  xlab("Tranche d'?ge")+ylab("Cfa de la patiente au diag")  

boxplot_cfa_age

# Number of cfa selon l'?ge . On diff?rencie les femmes qui ont fait une proc?dure de preservation

p <- ggplot(data = data, aes(y = cfa, x = age))+ geom_point() +
  facet_grid(. ~fertil_preserv)+ geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") + aes(color = age < 30) +
  scale_color_brewer("", palette = "Set1",
                     labels = c("age > 30", "age < 30"))

p

#cfa en fonction de l'?ge et histo 2 cl : en fonction de l'histology (aka type de cancer)

m <- ggplot(data = data, aes(y = cfa, x = age))+ geom_point() +
  facet_grid(. ~histo_2cl)+ geom_smooth(method = "loess", se = FALSE, size = 1, color = "black") + aes(color = age < 30) +
  scale_color_brewer("", palette = "Set1",
                     labels = c("age > 30", "age < 30"))

m


### stats simples maladie

table(data_v1$neo_ct)

# No Yes 
# 744 612 

# No  Yes 
# 54.9 45.1 

# Quasi 55% des femmes ne font pas de traitement n?o-adjuvant ? ; v?rifier l'interpr?tation. 

table <- round(prop.table(table(data_v1$primary_ttt))*100,1)
table

# Neoadjuvant treatment Surgery 
# 45.1                  54.9 

table(data_v1$breast_surgery)

table <- round(prop.table(table(data_v1$breast_surgery))*100,1)
table

# No  Yes 
# 0.1 99.9 

# 1356 patientes ont eu recours ? des breast surgeries. 1 personne n'y a pas eu recours. 
install.packages("ggplot2")
library(ggplot2)

#cfa : En moyenne, le cfa est de 23 sur la population avec un minimum de 1 et un max de 105. 

g <- ggplot(data_v1, aes(cfa)) + scale_fill_brewer(palette = "Spectral")

g + geom_histogram(aes(fill=cfa), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with cfa") 

summary(data_v1$cfa)

hist(data_v1$cfa, col = "skyblue",
     main = "Histogramme of cfa",
     xlab = "cfa",
     ylab = "Effectif")


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.00   13.00   20.00   22.94   30.00  105.00    1134 

# amh : hormone 

summary(data_v1$amh)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.150   1.680   3.205   3.870   4.978  20.000    1149 

# Le niveau d'hormone m?dian est de 3,2 avec une moyenne de 3,8. 



# part de cancer du seinn inflammatoire 
table(data_v1$inflammatory_bc)

# No  Yes 
# 1338   18 

# Seulement 18 cas de inflammatory bc. 

pie <- ggplot(data_v1, aes(x = "", fill = factor(inflammatory_bc))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="inflammatory_bc", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of inflammatory breast cancer", 
       caption="Source: data_v1")

pie + coord_polar(theta = "y", start=0)

# type de cancer dans la base : rappel on n'a pas subtype. 

pie <- ggplot(data_v1, aes(x = "", fill = factor(histo_3cl))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="histo_3cl", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of cancer type", 
       caption="Source: data_v1")

pie + coord_polar(theta = "y", start=0)

table <- round(prop.table(table(data_v1$histo_3cl))*100,1)
table
# Lobular     NST  Others 
# 4.0    93.4     2.7 

# grade de la maladie 

table <- round(prop.table(table(data_v1$grade_3cl))*100,1)
table

# Grade I  Grade II Grade III 
# 4.3      39.2      56.5 

#On a environ 57 % des cancers qui sont de grades 3. 

pie <- ggplot(data_v1, aes(x = "", fill = factor(grade_3cl))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="grade_3cl", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of cancer grade type", 
       caption="Source: data_v1")

pie + coord_polar(theta = "y", start=0)

plot(data_v1$grade_3cl, col = "skyblue",
     main = "Histogramme of Grade",
     xlab = "Type of Grade",
     ylab = "Effectif")

#### statut des rfs patientes : rechute ou non 

table(data_v1$status_rfs_txt)
# No  Yes 
# 1194  157

table <- round(prop.table(table(data_v1$status_rfs_txt))*100,1)
table

# No  Yes 
# 88.4 11.6 

# 88 % des patientes ont fait de rechute. 

################################### statut vital des patientes : sont-elles vivantes ou non ? 

# Alive  Dead 
# 1298    58 

table(data_v1$status_vital_txt)


table <- round(prop.table(table(data_v1$status_vital_txt))*100,1)
table

# Alive  Dead 
# 95.7   4.3

# 96 % des patientes de l'?tude sont encore en vie !! 

# quel type de chimio a ?t? recue 
table(data_v1$ct_setting_5cl)

# Adjuvant Chemotherapy    without surgery                 NAC 
# 744                            1                          611 



table <- round(prop.table(table(data_v1$ct_setting_5cl))*100,1)
table

# Adjuvant Chemotherapy    without surgery                  NAC 
# 54.9                          0.1                         45.1 

pie <- ggplot(data_v1, aes(x = "", fill = factor(ct_setting_5cl))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="ct_setting_5cl", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of treatment (chemotherapy ?)", 
       caption="Source: data_v1")

pie + coord_polar(theta = "y", start=0)


dotchart(table(data_v1$ct_setting_5cl), main = "Dotchart of chemo type",
         xlab = "",
         ylab = "Count")

dot<-ggplot(data_v1) +
  aes(y = ct_setting_5cl) +
  geom_point(stat = "count") +
  xlab("Count") +
  ylab("Type of chemotherapy")

dot



# nombre de ganglions chez la population de femmes 

# PCR : le plus important. la r?ponse au traitement
ggplot(data_v1) +
  aes(x = pcr) +
  geom_point(stat = "count") +
  xlab("rechute") +
  ylab("Effectifs")

ggplot(data_v1) +
  aes(x = pcr) +
  geom_bar(stat = "count") +
  xlab("PCR") +
  ylab("Count") + theme_classic() +
  ggtitle("Absence of invasive disease in breast and in nodes after treatment")



table(data_v1$pcr)

# No Yes 
# 380 230 

# Difficile de tirer des conclusions ? partir de cette variable. 

# on a ?norm?ment de NA sur cette question. c'est assez ?trange ?tant donn? l'importance de cette variable. 

# nmbre de ganglions pnuic

ggplot(data_v1) +
  aes(x = pnuicc_4cl) +
  geom_bar(stat = "count") +
  xlab("Pnuicc") +
  ylab("Count")+ theme_classic() +
  ggtitle("Number of nodes involved")


table <- round(prop.table(table(data_v1$pnuicc_4cl))*100,1)
table

# [1-3]       [4-9]        0       10 and more 
# 35.7         8.4        53.6         2.3 

# en majorit?, on a environ 54 % des patientes qui ont 0 ganglion attaqu?.

###########################################################################################################################
##########################################################################################################################
########################################## Fertility procedure ############################################################

# ovocytes

table(data_v1$frozen_oocytes)

# 0    1 
# 1135  222 

# Sur 1357 patientes, on a 222 femmes (16,4% des patientes) qui font congeler leurs ovocytes. 

table <- round(prop.table(table(data_v1$frozen_oocytes))*100,1)
table

# 0    1 
# 83.6 16.4 

sum(is.na(data_v1$frozen_oocytes))

############ Frozen embryos

table(data_v1$frozen_embryos)

sum(is.na(data_v1$frozen_embryos))

table <- round(prop.table(table(data_v1$frozen_embryos))*100,1)
table

# 0    1 
# 1333   24 


### On a  24 femmes qui ont congel?s des embryons. 0 NA. 1333 qui n'ont pas utilis? d'embryon. 


################## Reuse frozen material
sum(is.na(data_v1$reuse_frozen_material))

# Nbre de NA important : 1109

table(data_v1$reuse_frozen_material)

# No Yes 
# 225  23 

# 23 femmes ont r?utilis? le mat?riel congel? et 225 femmes ne l'ont pas r?utilis?. Pour 11109 femmes on ne saait pas


sum(is.na(data_v1$reuse_frozen_oocytes))
# On a 1334 NA

table(data_v1$reuse_frozen_oocytes)
# 19 femmes ont r?utilis? les ovocytes et 4 ne l'ont pas r?utilis?. On avait 222 femmes qui avaient congel?s leurs ovocytes 
# ca fait 8,56 % de femmes qui r?utilisent les ovocytes (19/222). On a 199 femmes pour lesquelles on ne peut pas r?pondre


table(data_v1$reuse_frozen_cortex)
# L? je suis plus emb?t? : Une femme a r?utilis? son cortex. 21 femmes n'ont pas r?utilis? le cortex. On a 1135 na.
# je n'ai pas la variable_cortex
sum(is.na(data_v1$reuse_frozen_cortex))


table(data_v1$reuse_frozen_embryo)
# 6 femmes ont r?utilis? leurs embryos. 15 femmes ne l'ont pas fait. On avait 24 femmes qui ont congel? leurs embryons. 
# On a 3 valeurs manquantes. 

table(data_v1$pregnancy_post_reuse_frozen_cortex)
# Aucune femme n'a eu une grossesse du faait d'avoir r?utilis? son frozen cortex. On a a priori 21 femmes qui ont r?utilis? leur cortex. 
# ca fait environ 5 % de sfemmes qui r?utilisent leur cortex ont un b?b? apr?s avoir utilis? cette m?thode. 



table(data_v1$pregnancy_post_reuse_frozen_embryo_nbr)

# 1 seule femme a eu une grossesse suite ? la r?utilisation de ses embryons congel?s. On avait 6 femmes qui ont r?utilis?s leurs embryons. 
# 24 femmes ont fait le choix de congeler leur embryon. (1/6)*100 caa fait environ 17 % de femmes qui ont r?utilis? leur embryon qui ont une grosssesse par la suite. 




table(data_v1$pregnancy_post_reuse_frozen_oocytes_nbr)
# on a une grossesse qui survient apr?s l'utilisation d'ovocytes congel?s. 19 femmes ont r?utilis?s leurs ovocytes. 
# On avait 222 femmes qui ont cong?l? leurs ovocytes. )
  
  
table(data_v1$pregnancy_post_k)
# On a 88 grossesses post cancer du sein. 

table(data_v1$spontan_art_preg_1)

# ART with frozen material reuse   ART wo frozen material reuse                   egg donation                    spontaneous 
# 3                              2                              8                             76 

# On a 76 grossesses spontan?es parmi les grossesses apr?s cancer. 
table <- round(prop.table(table(data_v1$spontan_art_preg_1))*100,1)
table

# ART with frozen material reuse   ART wo frozen material reuse                   egg donation                    spontaneous 
# 3.4                            2.2                            9.0                           85.4 

# ca fait 85.4 % de grossesse spontan?es parmi les grossesses post cancers et seulement 3,4% ? partr de r?utilisation d emat?riel congel?. 


ggplot(data_v1) +
  aes(y = spontan_art_preg_1) +
  geom_bar(stat = "count") +
  xlab("Count") +
  ylab("")+ theme_classic() +
  ggtitle(" Pregnancy origin type")



table<-round(prop.table(table(data_v1$spontan_art_preg_2))*100,1)
table

# ART with frozen material reuse   ART wo frozen material reuse                   egg donation                    spontaneous 
#                    3.7                            3.7                           18.5                           74.1 
# 

# sur les grosssesses men?es ? terme apr?ss le cancer (sans miscarrage), on en a 74 % qui sont spontann?es.

ggplot(data_v1) +
  aes(y = spontan_art_preg_2) +
  geom_bar(stat = "count") +
  xlab("Count") +
  ylab("")+ theme_classic() +
  ggtitle(" Pregnancy origin type among full term pregnancy")




table<-round(prop.table(table(data_v1$preg_outcome_preg_2))*100,1)
table

# ectopic pregnancy full term pregnancy         miscarriage   ongoing pregnancy 
# 3.7                55.6                25.9                14.8 


table<-round(prop.table(table(data_v1$preg_outcome_preg_3))*100,1)
table

# full term pregnancy         miscarriage 
# 83.3                16.7 


ggplot(data_v1) +
  aes(y = is.na!(preg_outcome_preg_3)) +
  geom_bar(stat = "count") +
  xlab("Count") +
  ylab("")+ theme_classic() +
  ggtitle(" Results among spontaneous post BC pregnancies")



# parmi les grosssesses spontan?es post-cancer, les r?sultats  sont que 83% de ces grosssesses sont men?es ? terme. 

######################### juste pour s'amuser 


g = ggplot(data_v1)+ geom_histogram(mapping=aes(x=age, color = data_v1$fertil_preserv)) + facet_grid(rows=vars(data_v1$pf_discussion), cols=vars(data_v1$histo_3cl))
g


g = ggplot(data_v1)+ geom_histogram(mapping=aes(x=age, color = data_v1$fertil_preserv)) + facet_grid(rows=vars(data_v1$pf_discussion))
g


g = ggplot(data_v1)+ geom_histogram(mapping=aes(x=, color = data_v1$fertil_preserv)) + facet_grid(rows=vars(data_v1$pf_discussion))
g

