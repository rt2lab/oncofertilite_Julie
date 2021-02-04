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


data_v2 = database_preprocessed_labels

data_v2$bmi_4cl_ord <- fct_relevel(data_v2$bmi_4cl,"<18.5", "18.5-24.9 ", "25-29.9",">=30")

data_v2$amh_num<-as.numeric(data_v2$amh)
cfa_num <- as.numeric(data_v2$cfa)
data_fertil_preserv = data_v2 %>% filter(fertil_preserv=="Yes")

var_selected<-c("age_young_cl","age", "nb_child_3cl", "bmi_4cl_ord","bmi", "center_curie","brca_screen", "brca_mut", "inflammatory_bc","tclin" ,"cnuicc_4cl","grade_3cl", "histo_3cl", "neo_ct", "ct_setting_5cl", "pf_discussion")

names_var_selected <-c("Age","Age (mean)", "Number of children", "BMI","BMI (mean)", "Treatment center","Genetic analysis", "Hereditary predisposition", "Inflammatory BC", "Clinical Tumor size (mm)", "Clinical N stage (TNM)", "SBR grade", "Histological type", "Neoajuvant chemotherapy", "Chemotherapy setting", "Fertility preservation discussion")




################################## ACM FactomineR
library(tidyr)
data.active <- data_v2 %>% select(pf_discussion, age_young_cl, bmi_2cl,nb_child_3cl, center_curie, neo_ct,grade_2cl) %>% drop_na()
                        


#On enlève les valeurs manquantes NA pour toutes les variables 
# sinon ca perturbe trop les axes 

#on crée l'ACM 
res.mca <- MCA (data.active,ncp = 3, graph = FALSE)

# projection des variables selon les axes de l'acm 

#axe 2 et 3 
plot(res.mca, axes = c(2, 3),choix = "ind", invisible = "ind")


############### à mettre dans le rendu 
#axe 1 et 2 
plot(res.mca, axes = c(1, 2),choix = "ind", invisible = "ind")

# Axe 1 et 3
plot(res.mca, axes = c(1, 3),choix = "ind", invisible = "ind")


#distribution des individus sur les axes chosiis en fonction de variables 

################# à mettre sur le mail et rendu 
plotellipses(res.mca, axes = c(1, 2), means = FALSE)


plotellipses(res.mca, axes = c(2, 3), means = FALSE)

plotellipses(res.mca, axes = c(1, 3), means = FALSE)


#valeurs propres pour aider au choix des axes qu'on va utiliser 

eig.val <- get_eigenvalue(res.mca)
eig.val
# Il ya un vrai décrochage entre 1 et 2 donc je ne garderai que l'axe 1 et 2. 
# a discuter pour le troisième. 
# je pense qu'il est drivé par le brca_mut

# eigenvalue variance.percent cumulative.variance.percent
# Dim.1 0.21136748        24.659539                    24.65954
# Dim.2 0.16985691        19.816640                    44.47618
# Dim.3 0.14877973        17.357635                    61.83381
# Dim.4 0.13393374        15.625604                    77.45942
# Dim.5 0.10981873        12.812185                    90.27160
# Dim.6 0.08338627         9.728398                   100.00000

# #################### A mettre dans le rendu : plot des valeurs propres pour visualiser la règle de décision dite du coude 

fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

# on garderait 2-3 dimensions a priori

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

#                      Dim 1      Dim 2        Dim 3
# pf_discussion_No   0.6935321 -0.3031002 -0.056601862
# pf_discussion_Yes -0.8398944  0.3670662  0.068547060
# [0 -40)           -0.4321631  0.1562855  0.001692577
# 40+                1.4187995 -0.5130883 -0.005556763
# Curie Paris       -0.1255357 -0.3642521  0.201843926
# Curie St Cloud     0.2398053  0.6958149 -0.385573654
# brca_screen_Yes    0.0000000  0.0000000  0.000000000
# brca_mut_No        0.1102971  0.1573850 -0.392983950
# brca_mut_Yes      -0.4262195 -0.6081807  1.518602264
# neo_ct_No          0.4246005  0.4614313  0.621482787
# neo_ct_Yes        -0.3719641 -0.4042291 -0.544439466
# Grade I-II         0.3006457  0.9242007  0.213281568
# Grade III         -0.1625664 -0.4997375 -0.115326459




# Cos2: qualité de représentation
var$cos2

# Dim 1      Dim 2          Dim 3
# pf_discussion_No  0.58249367 0.11125783 0.003879891217
# pf_discussion_Yes 0.58249367 0.11125783 0.003879891217
# [0 -40)           0.61315276 0.08018826 0.000009405252
# 40+               0.61315276 0.08018826 0.000009405252
# Curie Paris       0.03010412 0.25345206 0.077825700164
# Curie St Cloud    0.03010412 0.25345206 0.077825700164
# brca_screen_Yes          NaN        NaN            NaN
# brca_mut_No       0.04701078 0.09571854 0.596786315868
# brca_mut_Yes      0.04701078 0.09571854 0.596786315868
# neo_ct_No         0.15793614 0.18652395 0.338359757108
# neo_ct_Yes        0.15793614 0.18652395 0.338359757108
# Grade I-II        0.04887488 0.46185773 0.024597007962
# Grade III         0.04887488 0.46185773 0.024597007962


# Interprétation de la qualité de la représentation 

# sur l'axe 1, les mieux représentés sont pf_discussion, l'âge
#sur l'axe2 les mmieux représentés sont le Grade et le centre de traitement 
# axe 3 : le mieux représentée : brca_mut et neo_ct 

## Donc corrélation entre âge et pf_discussion 
### Donc corrélation entre Grade et centre d etraitement 
#### Corrélation entre neo_ct et brca_mut


#################################################################
# #####################Contributions aux axes######################
var$contrib

# 
# Dim 1     Dim 2         Dim 3
# pf_discussion_No  17.8056820  4.232071  0.1684928053
# pf_discussion_Yes 21.5633746  5.125203  0.2040513519
# [0 -40)            9.6757022  1.574634  0.0002108525
# 40+               31.7655129  5.169552  0.0006922326
# Curie Paris        0.6991308  7.324590  2.5677337491
# Curie St Cloud     1.3355190 13.991844  4.9050298541
# brca_screen_Yes    0.0000000  0.000000  0.0000000000
# brca_mut_No        0.6531939  1.654991 11.7803444022
# brca_mut_Yes       2.5241280  6.395359 45.5226165830
# neo_ct_No          5.6899025  8.362052 17.3179477472
# neo_ct_Yes         4.9845427  7.325434 15.1710947207
# Grade I-II         2.1439994 25.211699  1.5329064317
# Grade III          1.1593119 13.632570  0.8288792696


######################## Interprétation ############################
####################################################################

# ax1 : max contribution ets pf_discussion, age 
#axe 2 : max contribution  : Grade et le centre 
# axe 3 : max contribution brca et neo_ct 

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

###################################### Plot qualité de représentation des individus
ind <- get_mca_ind (res.mca)
ind

fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())


# ####################################  A mettre dans le rendu :   graph récapitulatif de la projection des variables sur axe 1 et 2 

plot(res.mca, 
     invisible = "ind",
     cex = 0.8,
     autoLab = "yes")



########### Application Rshiny 
install.packages('explor')
library(explor) 
explor(res.mca)



##################@ Plot ellispe 

res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "pf_discussion", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-1.04, 1.35), ylim = c(-1.15, 1.25))



##################### Plot variable 
res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 12, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.29, 1.87),
                     ylim = c(-1.42, 1.74))

####################@



############################# ACM3 



res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
                     size_range = c(10, 300), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE)



res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.04, 1.68),
                     ylim = c(-1.25, 1.47))


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "pf_discussion", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.44, 1.82), ylim = c(-1.47, 1.79))


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "age_young_cl", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.44, 1.82), ylim = c(-1.47, 1.79))



res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "nb_child_3cl", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.44, 1.82), ylim = c(-1.47, 1.79))



res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 2, yax = 3, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "center_curie", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.44, 1.82), ylim = c(-1.47, 1.79))









###################################################@#########################################################
#################################################################################################################
##################################################################################################################
#############################################  ACM sur fertil_preserv




# les variables qu'on garde 


data.active.2 <- data_v2 %>% select(fertil_preserv, pf_discussion, age_young_cl_40_bin, center_curie, brca_screen, brca_mut, neo_ct,grade_2cl) %>% drop_na()





res.mca.2 <- MCA (data.active.2,ncp = 3, graph = FALSE)

plot(res.mca.2)



#axe 1 et 2 
plot(res.mca.2, axes = c(1, 2),choix = "ind", invisible = "ind")

#axe 2 et 3 
plot(res.mca.2, axes = c(2, 3),choix = "ind", invisible = "ind")

# Axe 1 et 3
plot(res.mca.2, axes = c(1, 3),choix = "ind", invisible = "ind")


plotellipses(res.mca.2, axes = c(1, 2), means = FALSE)


plotellipses(res.mca.2, axes = c(2, 3), means = FALSE)

plotellipses(res.mca.2, axes = c(1, 3), means = FALSE)


# Valeurs propres 
eig.val.2 <- get_eigenvalue(res.mca.2)
head(eig.val.2)

# 
# eigenvalue variance.percent cumulative.variance.percent
# Dim.1 0.25175061         28.77150                    28.77150
# Dim.2 0.15440814         17.64664                    46.41814
# Dim.3 0.13049247         14.91343                    61.33157
# Dim.4 0.11787295         13.47119                    74.80276
# Dim.5 0.09734135         11.12473                    85.92749
# Dim.6 0.08636294          9.87005                    95.79754
# 

#graphique valeurs propres et règle de décision 

fviz_screeplot (res.mca.2, addlabels = TRUE, ylim = c (0, 45))

# 2 voire trois axes 

# concernant la qualité de la représentation 
var2 <- get_mca_var(res.mca.2)
var2


# Coordonnées des variables sur l'axe 
var2$coord



# Dim 1       Dim 2       Dim 3
# fertil_preserv_No  -0.5580156808 -0.10140829 -0.02678426
# fertil_preserv_Yes  1.3047719595  0.23711644  0.06262790
# pf_discussion_No   -0.8036404537 -0.08076833 -0.01315574
# pf_discussion_Yes   0.9732399001  0.09781360  0.01593212
# [0 -40)             0.3722781774 -0.02960312 -0.04182839
# 40+                -1.2221962807  0.09718760  0.13732338
# Curie Paris         0.0502202709 -0.34951781  0.23936316
# Curie St Cloud     -0.0959335944  0.66766863 -0.45724501
# brca_screen_Yes     0.0000000000  0.00000000  0.00000000
# brca_mut_No        -0.0547983756  0.17654643 -0.39046270
# brca_mut_Yes        0.2117565800 -0.68222587  1.50885942
# neo_ct_No          -0.1922193243  0.61567486  0.60721317
# neo_ct_Yes          0.1683904825 -0.53935153 -0.53193881
# Grade I-II         -0.0002875463  0.97551650  0.16492345
# Grade III           0.0001554832 -0.52748517 -0.08917806





# Cos2: qualité de représentation des variables sur les axes 
var2$cos2

#                           Dim 1       Dim 2        Dim 3
# fertil_preserv_No  0.72808321323888 0.024045573 0.0016774420
# fertil_preserv_Yes 0.72808321323888 0.024045573 0.0016774420
# pf_discussion_No   0.78213495481292 0.007900241 0.0002095988
# pf_discussion_Yes  0.78213495481293 0.007900241 0.0002095988
# [0 -40)            0.45499700385754 0.002877056 0.0057440153
# 40+                0.45499700385755 0.002877056 0.0057440153
# Curie Paris        0.00481781110195 0.233362074 0.1094476103
# Curie St Cloud     0.00481781110195 0.233362074 0.1094476103
# brca_screen_Yes                 NaN         NaN          NaN
# brca_mut_No        0.01160391660882 0.120444544 0.5891533139
# brca_mut_Yes       0.01160391660882 0.120444544 0.5891533139
# neo_ct_No          0.03236790476993 0.332065177 0.3230002468
# neo_ct_Yes         0.03236790476993 0.332065177 0.3230002468
# Grade I-II         0.00000004470862 0.514570483 0.0147075542
# Grade III          0.00000004470862 0.514570483 0.0147075542



#### Concernant la qualité de la représentation : 

#dim 1 : fertil_preserv, pf_discussion, et âge bien représentée 
#dim2 : Grade les plu sreprésentée avec neo-ct mais c'est une moins bonne qualité de représentation 

# dim3 : brca_mut le plus représentée mais ca ne convient pas spécialement 
#car on a peu d'observation de brca mut et de plus la qualité d ereprésentation est faible 



# Contributions des variables aux axes
var2$contrib

# Dim 1       Dim 2        Dim 3
# fertil_preserv_No  10.829379212550  0.58312007  0.048134352
# fertil_preserv_Yes 25.321636688168  1.36347193  0.112549440
# pf_discussion_No   17.564054997966  0.28925691  0.009080651
# pf_discussion_Yes  21.270754916368  0.35030139  0.010997022
# [0 -40)             5.274703319800  0.05437987  0.128466577
# 40+                17.316950521608  0.17853014  0.421758196
# Curie Paris         0.082197385082  6.49140690  3.602463435
# Curie St Cloud      0.157018081758 12.40025164  6.881628870
# brca_screen_Yes     0.000000000000  0.00000000  0.000000000
# brca_mut_No         0.118447257875  2.00450835 11.602025112
# brca_mut_Yes        0.457714046504  7.74599297 44.833539896
# neo_ct_No           0.856670060109 14.32922359 16.492516216
# neo_ct_Yes          0.750471292327 12.55287356 14.447989412
# Grade I-II          0.000001440807 27.03708332  0.914408315
# Grade III           0.000000779079 14.61959935  0.494442505


#plot de corrélation 

library("corrplot")
corrplot(var2$cos2, is.corr=FALSE)

#axe 1 : fertil_preserv, pf_discussion et âge 
#axe 2 : grade et neo_ct 
#axe 3 : brca_mut 


########### BarPlot qualité de la représentation des variables selon les axes 


# Cos2 des variable sur Dim.1 
fviz_cos2(res.mca.2, choice = "var", axes = 1)

#axe 1 : pf_discussion, fertil_preserv, age

# Cos2 des variable sur Dim.2
fviz_cos2(res.mca.2, choice = "var", axes = 2)

#grade, neo_ct, center

# Cos2 des variable sur Dim.3
fviz_cos2(res.mca.2, choice = "var", axes = 3)

#brca_mut et neo_ct 

# je pense qu'on va rester sur deux axes d'analyse 



######################### Qualité de représentation des variables 


fviz_mca_var(res.mca.2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())



######################@ Qualité de représentation des individus 
ind.2 <- get_mca_ind (res.mca.2)
ind.2

fviz_mca_ind(res.mca.2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())


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


res <- explor::prepare_results(res.mca.2)
explor::MCA_biplot(res, xax = 1, yax = 2, col_var = "Variable", ind_point_size = 32,
                   ind_opacity = 0.5, ind_opacity_var = NULL, ind_labels = FALSE, var_point_size = 96,
                   var_sup = FALSE, ind_sup = FALSE, labels_size = 12, bi_lab_min_contrib = 0,
                   symbol_var = "Nature", transitions = TRUE, labels_positions = NULL, xlim = c(-1.51,1.65), ylim = c(-1.17, 1.98))


res <- explor::prepare_results(res.mca.2)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "fertil_preserv", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-1.11, 1.14), ylim = c(-1.06, 1.19))


res <- explor::prepare_results(res.mca.2)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "pf_discussion", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-1.11, 1.14), ylim = c(-1.06, 1.19))

