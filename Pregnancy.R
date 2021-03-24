##### ########################################################Pregnancies following breast cancer  !! 


library('readxl')
library(writexl)
library(arsenal)
library(ggplot2)
library(table1)
library(psych)
library(kableExtra)
library(forcats)
library('dplyr')
source(file.path("/Users/julieborghese/Documents/GitHub/databases/core/00_common/src/R_functions_Nadir/functions_RT2_Nadir.R"))
library(tidyr)
library(forcats)
library(dplyr)
library(kableExtra)

############### Chargement des bases de données 


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



# 1364 observations pour base complet 

base_complet <- left_join(base_julie, database_preprocessed_labels, by = c("numdos_curie" = "numdos_curie")) 


# maintenant si on enlève les duplicats dans notre base de données : on a bien 1357 observations !!

base_complet<-base_complet[!duplicated(base_complet$numdos_curie), ]



data_fertil_preserv = base_complet %>% filter(fertil_preserv=="Yes")

data_fertil_preserv$fertil_miv_cos_2 <- NA
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "No"] <- "IVM"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "No"] <- NA
table(data_fertil_preserv$fertil_miv_cos_2)

base_complet$year_diag<-as.character(base_complet$year_diag)



base_complet$age_young_2cl <- cut(base_complet$age, c(22, 37,44))
table(base_complet$age_young_2cl)

base_complet$age_young_acm  <- NA
base_complet$age_young_acm [base_complet$age_young_2cl == "(22,37]"] <- "below 37 y.o"
base_complet$age_young_acm [base_complet$age_young_2cl == "(37,44]"] <- "above 37 y.o"
table(base_complet$age_young_acm )

base_complet<-base_complet %>% drop_na(age_young_acm)


base_complet$nb_child_3cl<- as.character(base_complet$nb_child_3cl)

base_complet$nb_child_2cl  <- NA
base_complet$nb_child_2cl[base_complet$nb_child_3cl == "More than 1"] <- "Has children"
base_complet$nb_child_2cl[base_complet$nb_child_3cl == "1"] <- "Has children"
base_complet$nb_child_2cl[base_complet$nb_child_3cl=="0"] <- "No children"
table(base_complet$nb_child_2cl)






# Intro : chiffre de base 


# table 0 de base sur les variables caractéristiques  de la patiente : 

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")

tab0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F)


tab0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Patients Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_csv.xlsx')
write_xlsx(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_patient_cara.xlsx')


# table 0 des delay 

var_selected<-c("delay_diag_to_surg_day","delay_rfs", "delay_os")

names_var_selected <-c("Delay diagnosis to surgery (in days)","Delay RFS since surgery (in months)", "Delay OS since surgery (in months)")


tab1<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F,n_digits = 0)


tab1[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Delays Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab1[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_delays_cara_csv.xlsx')
write_xlsx(tab1[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_delays_cara.xlsx')


# table 0 des pregnancy 


var_selected<-c("age_young_cl","age", "nb_child_3cl","preg_dg","mention_preg_desire","fertil_preserv","pregnancy_post_k")

names_var_selected <-c("Age","Age (mean)", "Number of children","Pregnancy at BC diagnosis", "Pregnancy desire", "Fertility preservation procedure","Pregnancy after BC")


tab2<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = F,n_digits = 0)


tab2[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Pregnancy Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_pregnancy_cara_csv.xlsx')
write_xlsx(tab2[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table2_pregnancy_cara.xlsx')


# nombre et effectifs des gens qui ont des grossesses post-cancer 

P <- base_complet %>%drop_na(pregnancy_post_k) %>% 
  group_by(pregnancy_post_k) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
P$perc <- round(P$perc,2)
P$x <- paste0(paste0(as.character(P$count),sep="\n"),paste(paste0(as.character(100*P$perc),'%',sep= '')))
P


g=ggplot(data=P, aes(y=100*perc,x=pregnancy_post_k),position="fill",stat='identity') +geom_col(show.legend = F,width = 0.6) + ggtitle(label = "Pregnancy after BC")+
  xlab("")+ ylab("")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Pregnancy after BC diagnosis",reverse=T))+
  geom_text(aes(x=pregnancy_post_k,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
g


# Parmi celles qui ont une pregnancy, quel est le résultat ? parmi celles qui sont enceintes quelle est l'origine de la grossesse ? 

preg <- base_complet %>% filter(pregnancy_post_k=="Yes")


var_selected<-c("age_young_cl","age", "nb_child_3cl","preg_dg","mention_preg_desire","fertil_preserv","spontan_art_preg_1", "preg_outcome_preg_1")

names_var_selected <-c("Age","Age (mean)", "Number of children","Pregnancy at BC diagnosis", "Pregnancy desire", "Fertility preservation procedure","Pregnancy occurrence","Pregnancy outcome")


tab3<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, preg, missing = F, perc_by_column = F,n_digits = 0)

tab3[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Pregnancy post BC Outcome")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab3[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table3_pregnancy_csv.xlsx')
write_xlsx(tab3[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table3_pregnancy.xlsx')


# Résultats des grossesses en fonction des méthodes de conception 

base_complet$spontan_art_preg_1.2 = NA
base_complet$spontan_art_preg_1.2[base_complet$spontan_art_preg_1 == "others"] <- NA
base_complet$spontan_art_preg_1.2[base_complet$spontan_art_preg_1 == "spontaneous"] <- "spontaneous"
base_complet$spontan_art_preg_1.2[base_complet$spontan_art_preg_1 == "ART wo frozen material reuse"] <- "ART wo frozen material reuse"
base_complet$spontan_art_preg_1.2[base_complet$spontan_art_preg_1 == "ART with frozen material reuse"] <- "ART with frozen material reuse"
base_complet$spontan_art_preg_1.2[base_complet$spontan_art_preg_1 == "egg donation"] <- "egg donation"
table(base_complet$spontan_art_preg_1.2)


var_selected<-c("age_young_cl","age", "nb_child_3cl","preg_dg","mention_preg_desire","fertil_preserv", "preg_outcome_preg_1")

names_var_selected <-c("Age","Age (mean)", "Number of children","Pregnancy at BC diagnosis", "Pregnancy desire", "Fertility preservation procedure","Pregnancy outcome")


tab4<-preformatTable1(stratif = "spontan_art_preg_1.2", stratif_order = c("spontaneous","ART wo frozen material reuse","ART with frozen material reuse","egg donation"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, preg, missing = F, perc_by_column = F,n_digits = 0)

tab4[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Pregnancy outcome as a function of Pregnancy occurence")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab4[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table4_pregnancy_csv.xlsx')
write_xlsx(tab4[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table4_pregnancy.xlsx')




# table 0 des caractéristiques des grossesses 



#I.  Number - age of patients and - delays from diagnosis 

# tableau ou boxplot : en fonction de pregnancy_after_k quel est le delay ? boxplot et density plot et le fameux tableaux des var caractéristiques 

# table principale 



var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion","delay_diag_to_surg_day","delay_rfs", "delay_os","preg_dg","mention_preg_desire","fertil_preserv")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion","Delay diagnosis to surgery (in days)","Delay RFS since surgery (in months)", "Delay OS since surgery (in months)","Pregnancy at BC diagnosis", "Pregnancy desire", "Fertility preservation procedure")



tab5<-preformatTable1(stratif = "pregnancy_post_k", stratif_order = c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = TRUE,n_digits =0 )

tab5[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who got pregnant after BC ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab5[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table5_pregnancy_allvariable_csv.xlsx')
write_xlsx(tab5[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table5_pregnancy_allvariable.xlsx')



# Boxplot sur les delays to surgery 

D <- base_complet %>%drop_na(delay_diag_to_surg_day, pregnancy_post_k) 


d = ggplot(D) +
  geom_violin(aes(y = delay_diag_to_surg_day, x = pregnancy_post_k ,fill= pregnancy_post_k), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 45))+theme_minimal()+
  geom_boxplot(aes(y = delay_diag_to_surg_day, x = pregnancy_post_k),width=0.1)+labs(title="Delay diagnosis to surgery (in days)") + xlab("Pregnancy post BC")+ ylab("")

d


d = d + facet_wrap(~age_young_acm, ncol = 2)

# Delay surgery VS age et fertil_preserv 



D <- base_complet %>%drop_na(delay_diag_to_surg_day, pregnancy_post_k,age_young_acm,fertil_preserv) 


y = ggplot(D) +
  geom_violin(aes(y = delay_diag_to_surg_day, x = fertil_preserv ,fill= fertil_preserv), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(5, 62))+theme_minimal()+
  geom_boxplot(aes(y = delay_diag_to_surg_day, x = fertil_preserv),width=0.1)+labs(title="Delay RFS since surgery (in months)") + xlab("")+ ylab("")


y = y + facet_wrap(~age_young_acm, ncol = 2)



# Boxplot delay rfs 


R <- base_complet %>%drop_na(delay_rfs, pregnancy_post_k) 


r = ggplot(R) +
  geom_violin(aes(y = delay_rfs, x = pregnancy_post_k ,fill= pregnancy_post_k), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 120))+theme_minimal()+
  geom_boxplot(aes(y = delay_rfs, x = pregnancy_post_k),width=0.1)+labs(title="Delay RFS since surgery (in months)") + xlab("Pregnancy post BC")+ ylab("")

r

r = r + facet_wrap(~age_young_acm, ncol = 2)

# Boxplot delay rfs 


R <- base_complet %>%drop_na(delay_rfs, pregnancy_post_k,age_young_acm,fertil_preserv) 


x = ggplot(O) +
  geom_violin(aes(y = delay_rfs, x = fertil_preserv ,fill= fertil_preserv), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 120))+theme_minimal()+
  geom_boxplot(aes(y = delay_rfs, x = fertil_preserv),width=0.1)+labs(title="Delay RFS since surgery (in months)") + xlab("")+ ylab("")


x = x + facet_wrap(~age_young_acm, ncol = 2)





# Boxplot delay os comparaison Age and pregancy after BC 

O <- base_complet %>%drop_na(delay_os, pregnancy_post_k,age_young_acm,fertil_preserv) 


o = ggplot(O) +
  geom_violin(aes(y = delay_os, x = pregnancy_post_k ,fill= pregnancy_post_k), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 120))+theme_minimal()+
  geom_boxplot(aes(y = delay_os, x = pregnancy_post_k),width=0.1)+labs(title="Delay OS since surgery (in months)") + xlab("Pregnancy after BC")+ ylab("")
  

o = o + facet_wrap(~age_young_acm, ncol = 2)



# Boxplot delay os as comparaison Age and fertil_preserv 


O <- base_complet %>%drop_na(delay_os, pregnancy_post_k,age_young_acm,fertil_preserv) 


w = ggplot(O) +
  geom_violin(aes(y = delay_os, x = fertil_preserv ,fill= fertil_preserv), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(22, 120))+theme_minimal()+
  geom_boxplot(aes(y = delay_os, x = fertil_preserv),width=0.1)+labs(title="Delay OS since surgery (in months)") + xlab("")+ ylab("")


w= w + facet_wrap(~age_young_acm, ncol = 2)







################################## Figure delay

library(patchwork)


patchwork <- (d+o+r)
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 1 : Delays associated with Pregnancy post BC',
  subtitle = "These 7 plots describe the relation between Pregnancy post cancer and delays",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="top")




patchwork <- (w+x+y)
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 2 : Delays associated with Fertility preservation procedure as a function of Age',
  subtitle = "These 7 plots describe the relation between fertility preservation procedure and delays",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="top")


patchwork <- (d+o+r)
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 3 : Delays associated with Pregnancy post BC as a function of Age',
  subtitle = "These 7 plots describe the relation between pregnancy post BC and delays",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="top")






# geom_density pour les trois . On peut essayer un facet grid 

install.packages('ggridges')
library(ggridges)


ggplot(O, aes(x = delay_os, y = pregnancy_post_k,fill=pregnancy_post_k)) + 
  geom_density_ridges2(aes(x=delay_os, fill=pregnancy_post_k),scale=1,alpha = .8, color = "white") +stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  labs(title="Density of Delay OS since surgery (in months)", subtitle="") + xlab("Delay OS since surgery (in months)") + ylab("Pregnancy after BC")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)

install.packages('plotly')
library(plotly)

p <- ggplot(O, aes(x = delay_os)) + 
  geom_density(aes(fill = pregnancy_post_k), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")

fig <- ggplotly(p)

fig



p <- ggplot(O, aes(x=delay_os, fill = pregnancy_post_k)) + 
  geom_density(alpha = 0.5, position = "stack") + 
  ggtitle("stacked density chart")

fig <- ggplotly(p)

fig


#### geom_density delay os 


p <- ggplot(O, aes(x=delay_os, fill = pregnancy_post_k)) + geom_density(alpha = 0.2)+labs(title="", subtitle="") + xlab("Delay OS since surgery (in months)") + ylab("Pregnancy after BC")+
  theme(legend.position='none')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),
                                        axis.text.y = element_blank(),axis.ticks = element_blank())

fig <- ggplotly(p)

fig


######################## geom_density delay_rfs



rr <- ggplot(O, aes(x=delay_rfs, fill = pregnancy_post_k)) + geom_density(alpha = 0.2)+labs(title="", subtitle="") + xlab("Delay RFS since surgery (in months)") + ylab("Pregnancy after BC")+
  theme(legend.position='none')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),axis.ticks = element_blank())

rfs <- ggplotly(rr)

rfs

######################### geom_density delay_diag_to_surg_day : wtf les courbes !!! Vérifier si on a de svaleurs négatives ? que veulent dire les valeurs négatives ? 

dd <- ggplot(O, aes(x=delay_diag_to_surg_day, fill = pregnancy_post_k)) + geom_density(alpha = 0.2)+labs(title="", subtitle="") + xlab("Delay diagnosis to surgery (in days)") + ylab("Pregnancy after BC")+
  theme(legend.position='none')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),axis.ticks = element_blank()) + scale_x_continuous(limits=c(0, 300))
dd
delayd <- ggplotly(dd)

delayd


################### Figure density 


patchwork <- (dd+rr+p)
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 1 : Delays associated with Pregnancy post BC',
  subtitle = "These 7 plots describe the relation between Pregnancy post cancer and delays",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="right")






################################# Petit point sur delay_diag_to_surg_day 

summary(base_complet$delay_diag_to_surg_day)

# déjà on a des valeurs négatives pour cette variable. qu'est ce que ca veut dire ???? 

# hist(base_complet$delay_diag_to_surg_day)

#que faire des valeurs négatives ???? 




#II. Spontaneous vers artificial : tableau des variables caractéristiques et du des pregnancy outcome 


#1) Parmi celle qui ne font pas de fertil_preserv, quels sont les raisons 

no_fertil_preserv <- base_complet %>% filter(fertil_preserv=="No")


table(no_fertil_preserv$reason_no_pf)


table(no_fertil_preserv$reason_no_pf_2)


var_selected=c("reason_no_pf")
names_var_selected=c("Reason why no PF")


tab6<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, no_fertil_preserv, missing = F, perc_by_column = F)
tab6[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among patient who did no fertility preservation procedure,why do they choose not to do fertility preservation procedure ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab6[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_no_fertil_preserv_csv.xlsx')
write_xlsx(tab6[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table6_no_fertil_preserv.xlsx')


#2° est-ce qu'on n'a le gros tableau fertil_prserv oui/non vs gros vecteur de variables ? Caractériqtiques générales des femmes qui ne font pas de fertil preserv et d ecelle qui font des fertil_preserv 

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion","delay_diag_to_surg_day","delay_rfs", "delay_os","preg_dg","mention_preg_desire","pregnancy_post_k")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion","Delay diagnosis to surgery (in days)","Delay RFS since surgery (in months)", "Delay OS since surgery (in months)","Pregnancy at BC diagnosis", "Pregnancy desire","Pregnancy after BC")



tab7<-preformatTable1(stratif = "fertil_preserv", stratif_order = c("Yes","No"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_complet, missing = F, perc_by_column = TRUE,n_digits = 0)

tab7[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Who chose to do fertility preservation procedure ? ")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table7_pregnancy_allvariable_csv.xlsx')
write_xlsx(tab7[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table7_pregnancy_allvariable.xlsx')

################ gros tableau Spontaneous Vs art : vecteur et résultat des grossesse 


##Parmi celles qui ont eu une grossesse 


base_complet$spontan_art_preg_1.3 = NA
base_complet$spontan_art_preg_1.3[base_complet$spontan_art_preg_1 == "others"] <- NA
base_complet$spontan_art_preg_1.3[base_complet$spontan_art_preg_1 == "spontaneous"] <- "spontaneous"
base_complet$spontan_art_preg_1.3[base_complet$spontan_art_preg_1== "ART wo frozen material reuse"] <- "ART"
base_complet$spontan_art_preg_1.3[base_complet$spontan_art_preg_1 == "ART with frozen material reuse"] <- "ART"
base_complet$spontan_art_preg_1.3[base_complet$spontan_art_preg_1== "egg donation"] <- "egg donation"
table(base_complet$spontan_art_preg_1.3)


preg<-base_complet %>% filter(pregnancy_post_k=="Yes")


var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie.2","brca_screen", "brca_mut", "inflammatory_bc","tclin", "ctuicc_3cl","cnuicc_4cl","grade_3cl","subtype4.y", "histo_3cl", "neo_ct", "ct_setting_5cl.2", "pf_discussion","preg_dg","mention_preg_desire","preg_outcome_preg_1")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)","Clinical T stage (TNM)", "Clinical N stage (TNM)", "SBR grade","BC subtype", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion","Pregnancy at BC diagnosis", "Pregnancy desire","Pregnancy outcome")



tab8<-preformatTable1(stratif = "spontan_art_preg_1.3",  stratif_order = c("spontaneous","ART","egg donation"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, preg, missing = F, perc_by_column = F,n_digits = 0)

tab8[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among patient who had a pregnancy after BC, what were the pregnancy occurencies ? ")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table8_pregnancy_allvariable_csv.xlsx')
write_xlsx(tab8[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table8_pregnancy_allvariable.xlsx')







#III. return rate to fertility center 

# table 0 

# Parmi les femmes qui ont eu un egrosses post cancer diag


var_selected<-c("pregnancy_post_art_after_cancer","pregnancy_post_egg_donation","pregnancy_post_reuse_frozen_cortex","pregnancy_post_reuse_frozen_embryo","pregnancy_post_reuse_frozen_oocytes","return_center_pf")

names_var_selected <-c("Pregnancy after ART","Pregnancy after ART (egg donation)","Pregnancy after ART (cortex)","Pregnancy after ART (embryo)","Pregnancy after ART (oocytes)","Return rate to fertility center")


tab9<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, preg, missing = F, perc_by_column = F,n_digits = 0)


# There are no pregnancy post reuse frozen cortex

tab9[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among women who got pregnant after BC, what is the return rate to fertility center ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab9[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table9_pregnancy_csv.xlsx')
write_xlsx(tab9[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table9_pregnancy.xlsx')



#Parmi les femmes qui ont fait une procédure de fertilité : 
  
fertil<- base_complet %>% filter(fertil_preserv=="Yes")  



var_selected<-c("pregnancy_post_art_after_cancer","pregnancy_post_egg_donation","pregnancy_post_reuse_frozen_cortex","pregnancy_post_reuse_frozen_embryo","pregnancy_post_reuse_frozen_oocytes","return_center_pf")

names_var_selected <-c("Pregnancy after ART","Pregnancy after ART (egg donation)","Pregnancy after ART (cortex)","Pregnancy after ART (embryo)","Pregnancy after ART (oocytes)","Return rate to fertility center")


tab10<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, fertil, missing = F, perc_by_column = F,n_digits = 0)


# There are no pregnancy post reuse frozen cortex

tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Among women who had fertility preservation, what is the return rate to fertility center ?")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table10_pregnancy_csv.xlsx')
write_xlsx(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table10_pregnancy.xlsx')





##################################################################################################################################
##################################################################################################################################
##################################### ACM Pregnancy Post BC #####################################################################



library(devtools)
library(FactoMineR)
library(ade4)
library(dplyr)
library("factoextra")


library(tidyr)
data.active <- base_complet %>% select(pregnancy_post_k,age_young_acm,nb_child_2cl,neo_ct,grade_2cl) %>% drop_na()


#on crée l'ACM 
res.mca <- MCA (data.active,ncp = 3, graph = FALSE)
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))


# 2 dims

var <- get_mca_var(res.mca)
var


library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# dim 1 : preg, age , children 
# dim 2 : grade et neo_adj 

fviz_mca_biplot(res.mca,col.ind = data.active$pregnancy_post_k, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for pregnancy post BC",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Pregnancy post BC")




######################################## Box plot pregnancy neo_adj et grade



library(dplyr)
library(tidyr)

G <- base_complet %>%drop_na(pregnancy_post_k,neo_ct) %>% 
  group_by(neo_ct,pregnancy_post_k) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
G$perc <- round(G$perc,2)
G$x <- paste0(paste0(as.character(G$count),sep="\n"),paste(paste0(as.character(100*G$perc),'%',sep= '')))
G


g=ggplot(data=G, aes(fill=pregnancy_post_k,y=100*perc,x=neo_ct),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Pregnancy post BC as a function of Neoadjuvant chemotherapy")+
  xlab("Neoadjuvant chemotherapy")+ ylab("")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ guides(fill=guide_legend(title="Pregnancy post BC",reverse=T))+
  geom_text(aes(x=neo_ct,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
g


# grade 

G <- base_complet %>%drop_na(pregnancy_post_k,grade_2cl) %>% 
  group_by(grade_2cl,pregnancy_post_k) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
G$perc <- round(G$perc,2)
G$x <- paste0(paste0(as.character(G$count),sep="\n"),paste(paste0(as.character(100*G$perc),'%',sep= '')))
G


g=ggplot(data=G, aes(fill=pregnancy_post_k,y=100*perc,x=grade_2cl),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Pregnancy post BC as a function of Grade")+
  xlab("")+ ylab("")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),legend.position="top")+ guides(fill=guide_legend(title="Pregnancy post BC",reverse=T))+
  geom_text(aes(x=grade_2cl,label=x,size=3), position=position_stack(vjust=0.5), hjust=0.4,size=2.5)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
g


#########################################################   Amh, cfa et autres pour les patientes qui ont des procédures de fertilité
########################################################   On fait une figure patchwork 


#cfa, amh  : density et boxplot et corrélogramme 



D <- data_fertil_preserv %>%drop_na(amh,cfa, pregnancy_post_k,age_young_acm) 



a = ggplot(D) +
  geom_violin(aes(y = cfa, x = age_young_acm ,fill= age_young_acm), adjust = .8, show.legend=F)+scale_y_continuous(limits=c(5, 70))+theme_minimal()+
  geom_boxplot(aes(y = cfa, x = age_young_acm),width=0.1)+labs(title="Cfa as a function of age among the women who did fertility preservation procedure") + xlab("")+ ylab("")

a


data_fertil_preserv$amh<-as.numeric(data_fertil_preserv$amh)


b = ggplot(D) +
  geom_violin(aes(y = amh, x = age_young_acm ,fill= age_young_acm), adjust = .8, show.legend=F)+theme_minimal()+scale_y_continuous(limits=c(0, 20))+
  geom_boxplot(aes(y = amh, x = age_young_acm),width=0.1)+labs(title="Amh as a function of age among the women who did fertility preservation procedure") + xlab("")+ ylab("")

b




# Density cfa et amh 

# cfa 


c <- ggplot(D, aes(x=cfa, fill = age_young_acm)) + geom_density(alpha = 0.2)+labs(title="Cfa as a function of age among the women who did fertility preservation procedure", subtitle="") + xlab("") + ylab("")+
  theme(legend.position='none')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),axis.ticks = element_blank())

fig <- ggplotly(c)

fig



# amh 


d <- ggplot(D, aes(x=amh, fill = age_young_acm)) + geom_density(alpha = 0.2)+labs(title="Amh as a function of age among the women who did fertility preservation procedure", subtitle="") + xlab("") + ylab("")+
  theme(legend.position='none')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),axis.ticks = element_blank())

fig2 <- ggplotly(d)

fig2


############################# Corrplot entre cfa et amh et on colore en fonction de l'âge 

e = ggplot(D) + 
  geom_point(aes(x = amh, y = cfa, shape=age_young_acm, color=age_young_acm), 
             size = 3, alpha = 0.3)+theme_minimal()+geom_smooth(aes(x = amh, y = cfa, shape=age_young_acm, color=age_young_acm),method=lm, se=FALSE, fullrange=TRUE)




################################################## Figure 4 : cfa et amh pour le spatientes en fertil_preserv 

patchwork <- (a+b)/e
patchwork + plot_annotation(
  tag_levels = 'A',
  title = 'Figure 4: Biological parameters among the patients who did fertility preservation procedure as a function of Age',
  subtitle = "These 3 plots describe the relation between age and biological parameters",
  caption = '')+ plot_layout(guides="collect")&theme(legend.position ="right")












