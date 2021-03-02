##########################################################################################################
##########################################################################################################
###########################################################################################################
###########################################  Tableaux au propre ############################################
##########################################################################################################




install.packages("ggplot2")
install.packages('table1')
install.packages('psych')

library(ggplot2)
library(table1)
library(psych)
library(kableExtra)
library(forcats)
library('dplyr')
source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))
library(tidyr)

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

base_complet <- merge(base_julie,database_preprocessed_labels, by = "numdos_curie")

data_fertil_preserv = base_complet %>% filter(fertil_preserv=="Yes")

data_fertil_preserv$fertil_miv_cos_2 <- NA
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "No"] <- "IVM"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "No"] <- NA
table(data_fertil_preserv$fertil_miv_cos_2)

base_complet$year_diag<-as.character(base_complet$year_diag)

################################################################################## Fusion des deux bases pour obtenir les subtypes 





var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


##############################################################################################################



##################################################### Table1 ##############################################




tab0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F)


tab0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Patients Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')




################################################# Table colonne âge##########################################

a = base_julie %>% subset(is.na(age_young_cl_40_bin))


var_selected<-c("nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


table1<-preformatTable1(stratif = "age_young_cl_40_bin", stratif_order = c("[0 -40)","40+"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F)


table1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Age")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



################################################### Table treatment center colonnnes #####################################

a = base_julie %>% subset(is.na(center_curie.2))


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade", "BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab2<-preformatTable1(stratif = "center_curie.2", stratif_order =c("Curie Paris","Curie St Cloud") , stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F)


tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients characteristics as a function of Treatment center")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_treatment_center_csv.xlsx')



############################################################## Table fertility discussion lignes #############################




var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","year_diag","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Year BC diagnosis","Year BC diagnosis","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting")



tab7<-preformatTable1(stratif = "pf_discussion", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = TRUE)


tab7[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of Fertility Procedure discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fp_discussion_csv.xlsx')



############################################################### Table fertility procedures lignes #################################@


a = base_julie %>% subset(is.na(fertil_preserv))

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab8<-preformatTable1(stratif = "fertil_preserv", stratif_order = c("Yes", "No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = TRUE)


tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_csv.xlsx')




################@@@######################################## Type de fertility procedure  colonne #############################################


a = data_fertil_preserv %>% subset(is.na(fertil_miv_cos_2))

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab13<-preformatTable1(stratif = "fertil_miv_cos_2", stratif_order = c("IVM","At least one COS"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab13[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Patients Characteristics as a function of patients'choice of Fertility preservation")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab13[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_fertil_preserv_miv_cos_csv.xlsx')


####################################################### Type de centre de fertilité colonne #############################################

a = data_fertil_preserv %>% subset(is.na(center_fpp))


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_2cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")


tab12<-preformatTable1(stratif = "center_fpp", stratif_order = c("Bondy", "Clamart", "Others", "Port Royal"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, data_fertil_preserv, missing = F, perc_by_column = F)


tab12[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who are the patients in the different fertility centers? ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab12[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table12_center_fpp_csv.xlsx')




##################################################################################################################################################################################################################
#################################################################################################################################################################################################################@
##########################################################################################        Last plots            ################################################################@#############################



############ Figure 1 





######################@ Figure 1 Plot B : BMI

B <- base_complet %>%drop_na(pf_discussion,bmi_4cl_ord) %>% 
  group_by(bmi_4cl_ord,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
B$perc <- round(B$perc,2)
B$x <- paste0(paste0(as.character(B$count),sep="\n"),paste(paste0(as.character(100*B$perc),'%',sep= '')))
B


b=ggplot(data=B, aes(fill=pf_discussion,y=100*perc,x=bmi_4cl_ord),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "BMI")+
  xlab(" ")+ ylab("  ")+theme(legend.position="none")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=bmi_4cl_ord,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
b

################# Figure 1 Plot C : Children


C<- base_complet %>%drop_na(pf_discussion,nb_child_3cl) %>% 
  group_by(nb_child_3cl,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
C$perc <- round(C$perc,2)
C$x <- paste0(paste0(as.character(C$count),sep="\n"),paste(paste0(as.character(100*C$perc),'%',sep= '')))
C


c<-ggplot(data=C, aes(fill=pf_discussion,y=100*perc,x=nb_child_3cl),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Children")+
  xlab(" ")+ ylab("  ")+theme(legend.position="none")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=nb_child_3cl,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
c


################## Figure 1 Plot E : ctuicc_3cl


E <- base_complet %>%drop_na(pf_discussion,ctuicc_3cl) %>% 
  group_by(ctuicc_3cl,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
E$perc <- round(E$perc,2)
E$x <- paste0(paste0(as.character(E$count),sep="\n"),paste(paste0(as.character(100*E$perc),'%',sep= '')))
E


e=ggplot(data=E, aes(fill=pf_discussion,y=100*perc,x=ctuicc_3cl),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Clinical T stage (TNM)")+
  xlab(" ")+ ylab("  ")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=ctuicc_3cl,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
e


################## Figure 1 Plot F : neo_ct 


FF <- base_complet %>%drop_na(pf_discussion,neo_ct) %>% 
  group_by(neo_ct,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
FF$perc <- round(FF$perc,2)
FF$x <- paste0(paste0(as.character(FF$count),sep="\n"),paste(paste0(as.character(100*FF$perc),'%',sep= '')))
FF


f=ggplot(data=FF, aes(fill=pf_discussion,y=100*perc,x=neo_ct),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Neoadjuvant chemotherapy")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=neo_ct,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
f



################ figure 1 plot A : boxplot age pf_dicussion

A <- base_complet %>%drop_na(pf_discussion,age) 


a = ggplot(A) +
  geom_violin(aes(y = age, x = pf_discussion ,fill= pf_discussion), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 45))+theme_minimal()+
  geom_boxplot(aes(y = age, x = pf_discussion),width=0.1)+labs(title="Age at BC diagnostic") + xlab("")+ ylab("")

a



######################@ Figure 1 plot D : boxplot tclin 

D <- base_complet %>%drop_na(pf_discussion,tclin) 


d = ggplot(D) +
  geom_violin(aes(y = tclin, x = pf_discussion ,fill= pf_discussion), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(0, 100))+theme_minimal()+
  geom_boxplot(aes(y = tclin, x = pf_discussion),width=0.1)+labs(title="Clinical Tumor size (mm)") + xlab("")+ ylab("")

d


########################################################### Création du grid de Figure 1 !!!!!!!

install.packages("gridExtra")
library("gridExtra")
library('cowplot')




plot_row <-plot_grid(a, b, c, e, labels=c("A", "B", "C","E"), ncol = 2, nrow = 3,align = "h")

# now add the title
title <- ggdraw() + 
  draw_label(
    "Figure 1 : Factors associated with Fertility preservation discussion",
    fontface = 'bold',
    x = 0,
    hjust = 0)

legend <- get_legend(e+theme(legend.position =c(0.7,1)))

plot_grid(
  title,plot_row,legend,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))


##############################################Tentative 2 : Figure 1 



install.packages('patchwork')
library(patchwork)


patchwork <- (a + b) / (c+g) /(e + f) / ddd
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 1 : Factors associated with Fertility preservation discussion',
  subtitle = "These 7 plots describe the relation between patients'characteristics and the Fertility preservation discussion",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="bottom")




#########################################################################################################  Figure supplémentaire !!! : year diag 



######################@ Figure 1 E  : year diag !!!   

library(dplyr)
library(tidyr)

G <- base_complet %>%drop_na(pf_discussion,year_diag) %>% 
  group_by(year_diag,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
G$perc <- round(G$perc,2)
G$x <- paste0(paste0(as.character(G$count),sep="\n"),paste(paste0(as.character(100*G$perc),'%',sep= '')))
G


g=ggplot(data=G, aes(fill=pf_discussion,y=100*perc,x=year_diag),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Year BC diagnosis")+
  xlab("")+ ylab("")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=year_diag,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
g











