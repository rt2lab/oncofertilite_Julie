####################################################################################################
################################# Base docteurs ####################################################
####################################################################################################




library(ggplot2)
library(table1)
library(psych)
library(kableExtra)
library(forcats)
library('dplyr')
source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))


base_doctor = d2_long_2_with_names_centers

# quand on avait la mauvaise base 

# base_doctor$pf_discussion.2 = NA
# base_doctor$pf_discussion.2[base_doctor$pf_discussion=="oui"]<-"Yes"
# base_doctor$pf_discussion.2[base_doctor$pf_discussion=="non"]<-"No"
# 
# table(base_doctor$pf_discussion.2)
# base_doctor$pf_discussion.2 <-as.factor(base_doctor$pf_discussion.2)


# pf_discussion.2
# No  Yes 
# 1339  448 

# a voir si on met la nouvelle variable pf_discussion en facteur. initialmeent, elle est en character

################################################## Table 1 docteur ##############################
#################################################################################################



var_selected<-c("pf_discussion", "specialty", "junior_senior","gender_bin", "center_anonym")

names_var_selected <-c("Fertility preservation discussion", "Specialty", "Age","Gender", "Treatment center")


table0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


table0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Doctors Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_doctor_csv.xlsx')




################################################################ Table : Fertility preservation talk ligne

install.packages('openxlsx')
library(openxlsx)
install.packages('xlsx')

a = base_doctor %>% subset(is.na(pf_discussion)) #

var_selected<-c("specialty", "junior_senior","gender_bin", "center_anonym")

names_var_selected <-c("Specialty", "Age","Gender","Treatment center")

tab7<-preformatTable1(stratif = "pf_discussion", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)
tab7[[1]]%>% kbl("latex", align = "llr", vline = "|", caption = "Specialty effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write.csv(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_fertility_doctor_csv.xlsx')
write_xlsx(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_fertility_doctor_excel.xlsx')


a = read.csv('/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table_fertility_doctor_csv.xlsx')



############################################################## Table : specialty


a = base_doctor %>% subset(is.na(specialty))

var_selected<-c("pf_discussion", "junior_senior","gender_bin","center_anonym")

names_var_selected <-c("Fertility preservation discussion", "Age","Gender", "Treatment center")


tab9<-preformatTable1(stratif = "specialty", stratif_order = c("oncologist", "radiation oncologist", "surgeon"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab9[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Specialty effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab9[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_specialty_csv.xlsx')


############################################################## Table Gender 


a = base_doctor %>% subset(is.na(gender_bin))

var_selected<-c("pf_discussion", "junior_senior","specialty", "center_anonym")

names_var_selected <-c("Fertility preservation discussion", "Age","Specialty","Treatment center")


tab10<-preformatTable1(stratif = "gender_bin", stratif_order = c("female", "male"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Gender effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_gender_csv.xlsx')


############################################################# Table Age 

a = base_doctor %>% subset(is.na(center_anonym))


var_selected<-c("pf_discussion.2", "gender_bin","specialty","center_anonym")

names_var_selected <-c("Fertility preservation discussion", "Gender","Specialty","Treatment center")


tab10<-preformatTable1(stratif = "junior_senior", stratif_order = c("junior", "senior"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Age effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_age_csv.xlsx')
write_xlsx(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_age_excel.xlsx')


###############################################################@  Table Center 


var_selected<-c("pf_discussion", "gender_bin","specialty","junior_senior")

names_var_selected <-c("Fertility preservation discussion", "Gender","Specialty","Age")


tab11<-preformatTable1(stratif = "center_anonym", stratif_order = c("Center 1", "Center 2"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab11[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Treatment center effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_age_csv.xlsx')










#############################################################  ACM 
library(devtools)
library(FactoMineR)
library(ade4)
library(dplyr)
library("factoextra")

library(tidyr)
data.active <- base_doctor %>% select(pf_discussion, specialty, junior_senior, gender_bin,center_anonym) %>% drop_na()



# création ACM 

res.mca <- MCA (data.active,ncp = 3, graph = FALSE)

plot(res.mca, axes = c(1, 2),choix = "ind", invisible = "ind")

plot(res.mca, axes = c(2, 3),choix = "ind", invisible = "ind")

plotellipses(res.mca, axes = c(1, 2), means = FALSE)


plotellipses(res.mca, axes = c(2, 3), means = FALSE)



eig.val <- get_eigenvalue(res.mca)
eig.val

# eigenvalue variance.percent cumulative.variance.percent
# Dim.1  0.2649284         22.07737                    22.07737
# Dim.2  0.2392246         19.93538                    42.01275
# Dim.3  0.2023629         16.86358                    58.87632
# Dim.4  0.1900887         15.84072                    74.71705
# Dim.5  0.1707165         14.22638                    88.94342
# Dim.6  0.1326789         11.05658                   100.00000

fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

# on partirait sur 3 axes d'analyse a priori 


var <- get_mca_var(res.mca)
var

# Coordonnées
var$coord

# Dim 1      Dim 2         Dim 3
# No                    0.2177414 -0.2563612 -0.1639731619
# Yes                  -0.6452884  0.7597401  0.4859432430
# oncologist            0.2588091 -0.7660308 -0.0002043015
# radiation oncologist  3.5326599  2.0950373 -0.0475680388
# surgeon              -0.4429138  0.4476456  0.0034296959
# junior               -0.4269167  0.1695997  0.8371004514
# senior                0.2771923 -0.1101192 -0.5435201983
# female               -0.1402966  0.5529823 -0.5285093592
# male                  0.1637085 -0.6452607  0.6167039472
# Center 1             -0.4810879 -0.2365723 -0.2813123453
# Center 2              0.9187331  0.4517820  0.5372219035

# axe 1 : 



# Cos2: qualité de représentation
var$cos2

#                        Dim 1      Dim 2            Dim 3
# No                   0.14050602 0.19476787 0.07968165006765
# Yes                  0.14050602 0.19476787 0.07968165006765
# oncologist           0.04841283 0.42412512 0.00000003016787
# radiation oncologist 0.48428632 0.17032644 0.00008780697922
# surgeon              0.23318631 0.23819537 0.00001398221287
# junior               0.11833802 0.01867618 0.45498100333657
# senior               0.11833802 0.01867618 0.45498100333655
# female               0.02296773 0.35681774 0.32593380796117
# male                 0.02296773 0.35681774 0.32593380796118
# Center 1             0.44199139 0.10687911 0.15112715360566
# Center 2             0.44199139 0.10687911 0.15112715360566


# dim 3 : junior senior et center 
# dim 2 : male and female plus speciality
# dim 1 : center ? 

var$contrib

#                          Dim 1      Dim 2           Dim 3
# No                    2.6761575  4.1082497  1.986884727923
# Yes                   7.9309360 12.1750178  5.888239250634
# oncologist            2.1214562 20.5821317  0.000001730676
# radiation oncologist 35.1940484 13.7079298  0.008353984793
# surgeon               8.0430801  9.0986178  0.000631382980
# junior                5.4166276  0.9467070 27.264375601806
# senior                3.5169573  0.6146865 17.702461883636
# female                0.8001767 13.7669258 14.866022585578
# male                  0.9337056 16.0642708 17.346778533857
# Center 1             11.4674592  3.0709236  5.133263040364
# Center 2             21.8993954  5.8645396  9.802987277753


library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Dim 1 : effet centre et oncologiste 
#Dim 2 : Female, male , oncologist et oui non
#Dim 3 : junior senior

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

###########################################@ biplot pf_discussion 

fviz_mca_biplot(res.mca,col.ind = data.active$pf_discussion, ggtheme = theme_minimal(), axes=c(1,2), title="MCA with Fertility preservation discussion",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")


fviz_mca_biplot(res.mca,col.ind = data.active$pf_discussion, ggtheme = theme_minimal(), axes=c(2,3), title="MCA (axis 2 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")



#########################################@  biplot  gender


fviz_mca_biplot(res.mca,col.ind = data.active$gender_bin, ggtheme = theme_minimal(), axes=c(2,3), title="MCA (axis 2 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Gender")


###################################@##  Biplot treatment center


fviz_mca_biplot(res.mca,col.ind = data.active$center_anonym, ggtheme = theme_minimal(), axes=c(2,3), title="MCA (axis 2 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Treatment center")



fviz_mca_biplot(res.mca,col.ind = data.active$center_anonym, ggtheme = theme_minimal(), axes=c(1,2), title="MCA (axis 1 and 2)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Treatment center")



########################################@ Biplot age

fviz_mca_biplot(res.mca,col.ind = data.active$junior_senior, ggtheme = theme_minimal(), axes=c(2,3), title="MCA (axis 2 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Age")



fviz_mca_biplot(res.mca,col.ind = data.active$junior_senior, ggtheme = theme_minimal(), axes=c(1,3), title="MCA (axis 1 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Age")



########################################### Biplot specialty 


fviz_mca_biplot(res.mca,col.ind = data.active$specialty, ggtheme = theme_minimal(), axes=c(2,3), title="MCA (axis 2 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Specialty")


################################################################ il est très joli

fviz_mca_biplot(res.mca,col.ind = data.active$specialty, ggtheme = theme_minimal(), axes=c(1,2), title="MCA (axis 1 and 2)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Specialty")


fviz_mca_biplot(res.mca,col.ind = data.active$specialty, ggtheme = theme_minimal(), axes=c(1,3), title="MCA (axis 1 and 3)",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Specialty")






plot(res.mca, 
     invisible = "ind",
     cex = 0.8,
     autoLab = "yes")


install.packages('explor')
library(explor) 
explor(res.mca)


###############################################  Projection des variables sur les axes 

res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 3, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Cos2",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.46, 1.52),
                     ylim = c(-1.1, 1.88))

res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 2, yax = 3, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Cos2",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.45, 4.23),
                     ylim = c(-2.45, 3.23))


############################################################################ A mettre dans la figure 2 

res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Cos2",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.45, 4.23),
                     ylim = c(-2.45, 3.23))




#######################################################################################################################
###########################################################################################################################
###########################################################################################################################
#########################################################################@ Regression logistique ##############################

base_doctor$pf_discussion.2 <-as.factor(base_doctor$pf_discussion)

var_selected<-c("specialty", "junior_senior","gender_bin")

names_var_selected <-c("Specialty", "Age","Gender")

lm =logisticRegressionTable(base_doctor, var_selected, names_var_selected, var_to_explain= "pf_discussion.2",level_to_import="Yes",variables_use_multivariate = NA, alpha_cut_multivariate = 1, all_multivariate_values = F)

lm %>% kbl("latex", align = "llr", vline = "|", caption = "Logistic regression results for fertility preservation discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



#################################################################################################################################
#################################################################################################################################
################################################################################################################################

########################################################################################################################  Figure 2 : Factors related to patient management 



################## Figure 2 plot A : specialty 


AA <- base_doctor %>%drop_na(pf_discussion,specialty) %>% 
  group_by(specialty,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
AA$perc <- round(AA$perc,2)
AA$x <- paste0(paste0(as.character(AA$count),sep="\n"),paste(paste0(as.character(100*AA$perc),'%',sep= '')))
AA


aa=ggplot(data=AA, aes(fill=pf_discussion,y=100*perc,x=specialty),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Doctor's Specialty")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=specialty,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
aa



################## Figure 2 plot B : Age 


BB <- base_doctor %>%drop_na(pf_discussion,junior_senior) %>% 
  group_by(junior_senior,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
BB$perc <- round(BB$perc,2)
BB$x <- paste0(paste0(as.character(BB$count),sep="\n"),paste(paste0(as.character(100*BB$perc),'%',sep= '')))
BB

bb=ggplot(data=BB, aes(fill=pf_discussion,y=100*perc,x=junior_senior),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Doctor's Age")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=junior_senior,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
bb



################## Figure 2 plot C : Gender


CC <- base_doctor %>%drop_na(pf_discussion,gender_bin) %>% 
  group_by(gender_bin,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
CC$perc <- round(CC$perc,2)
CC$x <- paste0(paste0(as.character(CC$count),sep="\n"),paste(paste0(as.character(100*CC$perc),'%',sep= '')))
CC

cc=ggplot(data=CC, aes(fill=pf_discussion,y=100*perc,x=gender_bin),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Doctor's Gender")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=gender_bin,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
cc



##############################  Figure 2 plot D : Center 



DD <- base_doctor %>%drop_na(pf_discussion,center_anonym) %>% 
  group_by(center_anonym,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
DD$perc <- round(DD$perc,2)
DD$x <- paste0(paste0(as.character(DD$count),sep="\n"),paste(paste0(as.character(100*DD$perc),'%',sep= '')))
DD

dd=ggplot(data=DD, aes(fill=pf_discussion,y=100*perc,x=center_anonym),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Treatment center")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=center_anonym,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
dd





##################################@@@ Figure 2 plot E et F : les ACMs qu'on va mettre dans le plot 



ee <-fviz_mca_biplot(res.mca,col.ind = data.active$pf_discussion, ggtheme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()), axes=c(1,2), title="MCA with Fertility preservation discussion",
                     addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")


ff<-fviz_mca_biplot(res.mca,col.ind = data.active$specialty,ggtheme = theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()), axes=c(1,2), title="MCA with doctors'specialty",
                    addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Specialty")

ee
ff

########################################################### Création du grid de Figure 2 !!!!!!!

install.packages("gridExtra")
library("gridExtra")
library('cowplot')



plot_row <-plot_grid(aa, bb, cc, dd, ee, ff, labels=c("A", "B", "C","D","E","F"), ncol = 2, nrow = 3,align = "h")


# now add the title
title <- ggdraw() + 
  draw_label(
    "Figure 2 : Factors related to patient management",
    fontface = 'bold',
    x = 0,
    hjust = 0) +theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))



########################################################### Création figure 2 


library(patchwork)


patchwork <- (aa + bb +cc+dd) / (ee+ff)
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 2 : Factors associated with Fertility preservation discussion and related to patient management',
  subtitle = "These 6 plots describe the relation between patients'management characteristics and the Fertility preservation discussion",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="bottom")











