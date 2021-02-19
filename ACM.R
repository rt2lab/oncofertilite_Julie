##############################################################################################################
##############################################################################################################
#############################################################################################################
################################     ACM : variables qualitatives   #################################################
#################################################################################################################



################ Fertility discussion 


########### Installation package et library : à télécharger en dehors du proxy

install.packages('devtools')

install.packages('FactoMineR')

install.packages("ade4", dep = TRUE)
install.packages('factoextra')

library(devtools)
library(FactoMineR)
library(ade4)
library(dplyr)
library("factoextra")

################################################## base de données pour l'acm



data_fertil_preserv = base_julie %>% filter(fertil_preserv=="Yes")

base_julie$nb_child_3cl<- as.character(base_julie$nb_child_3cl)

base_julie$nb_child_2cl  <- NA
base_julie$nb_child_2cl[base_julie$nb_child_3cl == "More than 1"] <- "Has children"
base_julie$nb_child_2cl[base_julie$nb_child_3cl == "1"] <- "Has children"
base_julie$nb_child_2cl[base_julie$nb_child_3cl=="0"] <- "No children"
table(base_julie$nb_child_2cl)



base_julie$age_young_2cl <- cut(base_julie$age, c(22, 37,44))
table(base_julie$age_young_2cl)







################################## ACM FactomineR
library(tidyr)
data.active <- base_julie %>% select(pf_discussion,age_young_2cl,nb_child_2cl,neo_ct,grade_2cl) %>% drop_na()
                        

#on crée l'ACM 
res.mca <- MCA (data.active,ncp = 3, graph = FALSE)


############################################################## à mettre dans le rendu 
#axe 1 et 2 
plot(res.mca, axes = c(1, 2),choix = "ind", invisible = "ind")



#distribution des individus sur les axes chosiis en fonction de variables 

################# à mettre sur le mail et rendu 
plotellipses(res.mca, axes = c(1, 2), means = FALSE)


#valeurs propres pour aider au choix des axes qu'on va utiliser 

eig.val <- get_eigenvalue(res.mca)
eig.val



# eigenvalue variance.percent cumulative.variance.percent
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1 0.37518467        37.518467                    37.51847
# Dim.2 0.21573299        21.573299                    59.09177
# Dim.3 0.16414088        16.414088                    75.50585
# Dim.4 0.15653874        15.653874                    91.15973
# Dim.5 0.08840272         8.840272                   100.00000

# #################### A mettre dans le rendu : plot des valeurs propres pour visualiser la règle de décision dite du coude 

fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

# oon ne garde que deux dimensions

####################### Coordonnées, qualité de réprésentation et controbutiona aux axes des variables 


var <- get_mca_var(res.mca)
var

# Multiple Correspondence Analysis Results for variables
# ===================================================
#   Name       Description                  
# 1 "$coord"   "Coordinates for categories" 
# 2 "$cos2"    "Cos2 for categories"        
# 3 "$contrib" "contributions of categories"

# Coordonnées
var$coord
# Dim 1      Dim 2       Dim 3
# pf_discussion_No  -0.5747186 -0.1711726  0.05795223
# pf_discussion_Yes  1.1662645  0.3473570 -0.11760127
# (22,37]            0.9805459  0.1738657 -0.07942678
# (37,44]           -0.5876280 -0.1041954  0.04759940
# Has children      -0.3823415 -0.1550088 -0.01057949
# No children        1.0150752  0.4115314  0.02808740
# neo_ct_No         -0.3642917  0.5593574 -0.61925375
# neo_ct_Yes         0.4389416 -0.6739798  0.74615001
# Grade I-II        -0.3247913  0.8518911  0.67113342
# Grade III          0.2507612 -0.6577183 -0.51816098




# Cos2: qualité de représentation
var$cos2

# Dim 1      Dim 2        Dim 3
# pf_discussion_No  0.67027392 0.05945800 0.0068152560
# pf_discussion_Yes 0.67027392 0.05945800 0.0068152560
# (22,37]           0.57619620 0.01811600 0.0037806668
# (37,44]           0.57619620 0.01811600 0.0037806668
# Has children      0.38810541 0.06379098 0.0002971503
# No children       0.38810541 0.06379098 0.0002971503
# neo_ct_No         0.15990278 0.37699558 0.4620561925
# neo_ct_Yes        0.15990278 0.37699558 0.4620561925
# Grade I-II        0.08144504 0.56030439 0.3477551518
# Grade III         0.08144504 0.56030439 0.3477551518


# Interprétation de la qualité de la représentation 



#################################################################
# #####################Contributions aux axes######################
var$contrib

# Dim 1      Dim 2       Dim 3
# pf_discussion_No  11.794999  1.8196355  0.27412970
# pf_discussion_Yes 23.935347  3.6925485  0.55628572
# (22,37]           19.205650  1.0501457  0.28804169
# (37,44]           11.509688  0.6293382  0.17261951
# Has children       5.660570  1.6180734  0.00990638
# No children       15.028197  4.2958090  0.02630036
# neo_ct_No          3.865881 15.8510209 25.53380720
# neo_ct_Yes         4.658070 19.0991809 30.76614474
# Grade I-II         2.450017 29.3127763 23.91147094
# Grade III          1.891581 22.6314716 18.46129377


######################## Interprétation ############################
####################################################################

###################### Plot corrélation axe et variables : même info mais plus visuel 
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

##################### Barplot qualité de représentation 

# Cos2 des variable sur Dim.1 
fviz_cos2(res.mca, choice = "var", axes = 1)

# pf discussion  et âge

# Cos2 des variable sur Dim.2
fviz_cos2(res.mca, choice = "var", axes = 2:2)

# Grade, Center, 

# Cos2 des variable sur Dim.3
fviz_cos2(res.mca, choice = "var", axes = 3:3)

#brca_mut, neo_ct

############################@  A mettre dans le rendu : graph joli : synthèse qualité de représentation et positionnement sur les axes 

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())


fviz_mca_biplot(res.mca,col.ind = data.active$pf_discussion, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for fertility preservation discussion",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")





########### Application Rshiny 
install.packages('explor')
library(explor) 
explor(res.mca)



#####################################################  A mettre dans le rapport : Plot variable 
res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 12, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.29, 1.87),
                     ylim = c(-1.42, 1.74))

####################@



###################################################@#########################################################
#################################################################################################################
##################################################################################################################
#############################################  ACM sur fertil_preserv




# les variables qu'on garde 


data.active.2 <- base_julie %>% select(fertil_preserv, pf_discussion,age_young_2cl,nb_child_2cl,neo_ct,grade_2cl) %>% drop_na()





res.mca.2 <- MCA (data.active.2,ncp = 3, graph = FALSE)

plot(res.mca.2)



#axe 1 et 2 
plot(res.mca.2, axes = c(1, 2),choix = "ind", invisible = "ind")



plotellipses(res.mca.2, axes = c(1, 2), means = FALSE)


# Valeurs propres 
eig.val.2 <- get_eigenvalue(res.mca.2)
head(eig.val.2)

# eigenvalue variance.percent cumulative.variance.percent
# Dim.1 0.41203448        41.203448                    41.20345
# Dim.2 0.18875438        18.875438                    60.07889
# Dim.3 0.13701944        13.701944                    73.78083
# Dim.4 0.13050676        13.050676                    86.83151
# Dim.5 0.08075943         8.075943                    94.90745
# Dim.6 0.05092551         5.092551                   100.00000


#graphique valeurs propres et règle de décision 

fviz_screeplot (res.mca.2, addlabels = TRUE, ylim = c (0, 45))

# 2 voire trois axes 

# concernant la qualité de la représentation 
var2 <- get_mca_var(res.mca.2)
var2


# Coordonnées des variables sur l'axe 
var2$coord



# Dim 1       Dim 2       Dim 3
# fertil_preserv_No  -0.4174151 -0.10276422  0.01801340
# fertil_preserv_Yes  1.7336319  0.42680618 -0.07481426
# pf_discussion_No   -0.5994780 -0.07530757  0.04467655
# pf_discussion_Yes   1.2165082  0.15282009 -0.09066120
# (22,37]             0.9425505 -0.02958208 -0.05409500
# (37,44]            -0.5648578  0.01772814  0.03241840
# Has children       -0.3787981 -0.06830711 -0.03428352
# No children         1.0056677  0.18134794  0.09101901
# neo_ct_No          -0.2528984  0.62773953 -0.60653231
# neo_ct_Yes          0.3047219 -0.75637468  0.73082172
# Grade I-II         -0.1933974  0.87375891  0.69093380
# Grade III           0.1493160 -0.67460174 -0.53344823





# Cos2: qualité de représentation des variables sur les axes 
var2$cos2

# Dim 1        Dim 2       Dim 3
# fertil_preserv_No  0.72364403 0.0438604039 0.001347659
# fertil_preserv_Yes 0.72364403 0.0438604039 0.001347659
# pf_discussion_No   0.72926986 0.0115085101 0.004050430
# pf_discussion_Yes  0.72926986 0.0115085101 0.004050430
# (22,37]            0.53240701 0.0005244352 0.001753673
# (37,44]            0.53240701 0.0005244352 0.001753673
# Has children       0.38094497 0.0123873532 0.003120452
# No children        0.38094497 0.0123873532 0.003120452
# neo_ct_No          0.07706367 0.4748062903 0.443266989
# neo_ct_Yes         0.07706367 0.4748062903 0.443266989
# Grade I-II         0.02887734 0.5894392811 0.368577408
# Grade III          0.02887734 0.5894392811 0.368577408



# Contributions des variables aux axes
var2$contrib

# Dim 1       Dim 2       Dim 3
# fertil_preserv_No   5.6801316  0.75152350  0.03181008
# fertil_preserv_Yes 23.5910446  3.12127000  0.13211544
# pf_discussion_No    9.7378737  0.33545286  0.16264038
# pf_discussion_Yes  19.7608654  0.68072754  0.33004275
# (22,37]            13.4658156  0.02895458  0.13337948
# (37,44]             8.0698823  0.01735209  0.07993253
# Has children        4.2160230  0.29926489  0.10385073
# No children        11.1930828  0.79451575  0.27571240
# neo_ct_No           1.4137494 19.01409802 24.45341492
# neo_ct_Yes          1.7034522 22.91042959 29.46436060
# Grade I-II          0.6591615 29.37042831 25.29966556
# Grade III           0.5089178 22.67598286 19.53307512

#plot de corrélation 

library("corrplot")
corrplot(var2$cos2, is.corr=FALSE)



# Graphique récapitulatif de projection des variables ur l'axe 1 et 2 

plot(res.mca.2, 
     invisible = "ind",
     cex = 0.8,
     autoLab = "yes")


######################################## API sortie de graph 

library(explor)
explor(res.mca.2)


res <- explor::prepare_results(res.mca.2)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 14, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.63, 1.66),
                     ylim = c(-1.62, 1.67))



fviz_mca_biplot(res.mca.2,col.ind = data.active.2$pf_discussion, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for Fertility preservation discussion",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")


fviz_mca_biplot(res.mca.2,col.ind = data.active.2$fertil_preserv, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for Fertility preservation procedure",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation procedure")




#############################################################################################################################################################
##############################################################################################################################################################


##########################################################################################  Figure 3 sur les ACMS !!!! 



######################################################################################### Figure 3 plot A Age 2 cat 

AAA <- base_julie %>%drop_na(pf_discussion,age_young_2cl) %>% 
  group_by(age_young_2cl,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
AAA$perc <- round(AAA$perc,2)
AAA$x <- paste0(paste0(as.character(AAA$count),'(',sep = ''),paste(paste0(as.character(100*AAA$perc),'%)',sep= '')))
AAA


aaa=ggplot(data=AAA, aes(fill=pf_discussion,y=100*perc,x=age_young_2cl),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Age")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme_minimal()+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=age_young_2cl,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=3)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
aaa


################################################################################### Figure 3 children 2 categories 


BBB <- base_julie %>%drop_na(pf_discussion,nb_child_2cl) %>% 
  group_by(nb_child_2cl,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
BBB$perc <- round(BBB$perc,2)
BBB$x <- paste0(paste0(as.character(BBB$count),'(',sep = ''),paste(paste0(as.character(100*BBB$perc),'%)',sep= '')))
BBB


bbb=ggplot(data=BBB, aes(fill=pf_discussion,y=100*perc,x=nb_child_2cl),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Children")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme_minimal()+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=nb_child_2cl,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=3)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
bbb


################################################################################## Figure 3 neo_adj


CCC <- base_julie %>%drop_na(pf_discussion,neo_ct) %>% 
  group_by(neo_ct,pf_discussion) %>% 
  summarise(count = n()) %>%
  mutate(perc = (count/sum(count)))
CCC$perc <- round(CCC$perc,2)
CCC$x <- paste0(paste0(as.character(CCC$count),'(',sep = ''),paste(paste0(as.character(100*CCC$perc),'%)',sep= '')))
CCC


ccc=ggplot(data=CCC, aes(fill=pf_discussion,y=100*perc,x=neo_ct),position="fill",stat='identity') +geom_col(show.legend = T,width = 0.6) + ggtitle(label = "Neoadjuvant chemotherapy")+
  xlab(" ")+ ylab("  ")+theme(legend.position="bottom")+theme_minimal()+ guides(fill=guide_legend(title="Fertility preservation discussion",reverse=T))+
  geom_text(aes(x=neo_ct,label=x,size=4), position=position_stack(vjust=0.5), hjust=0.4,size=3)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
ccc





#################################################################### Figure 3 plot D ACM pf_discussion


ddd<-fviz_mca_biplot(res.mca,col.ind = data.active$pf_discussion, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for fertility preservation discussion",
                addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation discussion")



#################################################################### Figure 3 plot E ACM fertil_preserv



eee<-fviz_mca_biplot(res.mca.2,col.ind = data.active.2$fertil_preserv, ggtheme = theme_minimal(), axes=c(1,2), title="MCA for Fertility preservation procedure",
                     addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE, legend.title = "Fertility preservation procedure")






###############################################################@ Grid figure 3 



library("gridExtra")
library('cowplot')



plot_row <-plot_grid(aaa, bbb, ccc, ddd, eee, labels=c("A", "B", "C","D","E"), ncol = 2, nrow = 3,align = "h")


# now add the title
title <- ggdraw() + 
  draw_label(
    "Figure 3 ",
    fontface = 'bold',
    x = 0,
    hjust = 0) +theme(plot.margin = margin(0, 0, 0, 7))

plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))







