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

base_doctor$pf_discussion.2 = NA
base_doctor$pf_discussion.2[base_doctor$pf_discussion=="oui"]<-"Yes"
base_doctor$pf_discussion.2[base_doctor$pf_discussion=="non"]<-"No"

table(base_doctor$pf_discussion.2)
base_doctor$pf_discussion.2 <-as.factor(base_doctor$pf_discussion.2)


# pf_discussion.2
# No  Yes 
# 1339  448 

################################################## Table 1 docteur ##############################
#################################################################################################


var_selected<-c("pf_discussion.2", "specialty", "junior_senior","gender_bin")

names_var_selected <-c("Fertility preservation discussion", "Specialty", "Age","Gender")


table0<-preformatTable1(stratif = NA, stratif_order = NA, stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


table0[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Baseline Doctors Characteristics")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab0[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table0_baseline_doctor_csv.xlsx')




################################################################ Table : Fertility preservation talk ligne



var_selected<-c("pf_discussion.2")

names_var_selected <-c("Fertility preservation discussion")


tab7<-preformatTable1(stratif = c("junior_senior"), stratif_order = c("junior", "senior"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = TRUE)


tab7[[1]]


############################################################## Table : specialty


var_selected<-c("pf_discussion.2", "junior_senior","gender_bin")

names_var_selected <-c("Fertility preservation discussion", "Age","Gender")


tab9<-preformatTable1(stratif = "specialty", stratif_order = c("oncologist", "radiation oncologist", "surgeon"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab9[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Specialty effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab9[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_specialty_csv.xlsx')


############################################################## Table Gender 

var_selected<-c("pf_discussion.2", "junior_senior","specialty")

names_var_selected <-c("Fertility preservation discussion", "Age","Specialty")


tab10<-preformatTable1(stratif = "gender_bin", stratif_order = c("female", "male"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Gender effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_gender_csv.xlsx')


############################################################# Table Age 

var_selected<-c("pf_discussion.2", "gender_bin","specialty")

names_var_selected <-c("Fertility preservation discussion", "Gender","Specialty")


tab10<-preformatTable1(stratif = "junior_senior", stratif_order = c("junior", "senior"), stratif2=NA, stratif2_order=NA, var_selected, names_var_selected, base_doctor, missing = F, perc_by_column = F)


tab10[[1]] %>% kbl("latex", align = "llr", vline = "|", caption = "Age effect")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")
write_csv2(tab10[[1]] , '/Users/julieborghese/Documents/GitHub/oncofertilite_Julie/Institut Curie/table1_doctor_age_csv.xlsx')



#############################################################  ACM 
library(devtools)
library(FactoMineR)
library(ade4)
library(dplyr)
library("factoextra")

library(tidyr)
data.active <- base_doctor %>% select(pf_discussion.2, specialty, junior_senior, gender_bin) %>% drop_na()



# création ACM 

res.mca <- MCA (data.active,ncp = 3, graph = FALSE)

plot(res.mca, axes = c(1, 2),choix = "ind", invisible = "ind")

plot(res.mca, axes = c(2, 3),choix = "ind", invisible = "ind")

plotellipses(res.mca, axes = c(1, 2), means = FALSE)


plotellipses(res.mca, axes = c(2, 3), means = FALSE)



eig.val <- get_eigenvalue(res.mca)
eig.val

# eigenvalue variance.percent cumulative.variance.percent
# Dim.1  0.3183440         25.46752                    25.46752
# Dim.2  0.2858605         22.86884                    48.33636
# Dim.3  0.2351040         18.80832                    67.14468
# Dim.4  0.2131723         17.05378                    84.19846
# Dim.5  0.1975192         15.80154                   100.00000
# 

fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

# on partirait sur 3 axes d'analyse a priori 


var <- get_mca_var(res.mca)
var

# Coordonnées
var$coord

#                         Dim 1       Dim 2       Dim 3
# No                   -0.4196591  0.10047503  0.23417603
# Yes                   1.0936150 -0.26183395 -0.61025343
# oncologist           -0.7119780 -0.42927753 -0.07973921
# radiation oncologist -1.0370712  3.42218907  1.43461967
# surgeon               0.6162078  0.03198069 -0.06108475
# junior                0.5469048 -0.37235213  1.05716110
# senior               -0.3380570  0.23016120 -0.65346065
# female                0.4132587  0.65565123 -0.08821781
# male                 -0.4020289 -0.63783463  0.08582058
# 


# axe 1 : 



# Cos2: qualité de représentation
var$cos2

#                         Dim 1       Dim 2       Dim 3
# No                   0.4589455 0.026307775 0.142906726
# Yes                  0.4589455 0.026307775 0.142906726
# oncologist           0.3476197 0.126371028 0.004360287
# radiation oncologist 0.0517573 0.563588933 0.099043966
# surgeon              0.4590433 0.001236446 0.004510918
# junior               0.1848850 0.085701011 0.690813183
# senior               0.1848850 0.085701011 0.690813183
# female               0.1661419 0.418197059 0.007570904
# male                 0.1661419 0.418197059 0.007570904



#Axe 1 : pf_discussion et type de médecin 
# Axe 2 : genre des médecin 
# Axe 3 : Senior / Junior

var$contrib

#                          Dim 1       Dim 2      Dim 3
# No                    9.995016  0.63804168  4.2141669
# Yes                  26.046615  1.66271126 10.9819515
# oncologist           16.193933  6.55598610  0.2750425
# radiation oncologist  3.877954 47.02577812 10.0483716
# surgeon              16.319845  0.04895307  0.2171521
# junior                8.972887  4.63189810 45.3970675
# senior                5.546391  2.86310492 28.0611889
# female                6.613537 18.53864717  0.4080740
# male                  6.433822 18.03487959  0.3969850


library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Dim 1 : yes/no et métier 
#Dim 2 : Female, male 
#Dim 3 : Age 

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())


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


res <- explor::prepare_results(res.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = "Cos2",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.45, 4.23),
                     ylim = c(-2.45, 3.23))




################################################ Projection individu 


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 2, yax = 3, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "junior_senior", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63)) 

########################@ Ce graph ets un peu mieux pour age 
res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 3, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "junior_senior", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))




######################################## graph individu discussion 


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 3, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "pf_discussion.2", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "pf_discussion.2", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))


##################################################  Gender 

res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "gender_bin", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "specialty", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))


res <- explor::prepare_results(res.mca)
explor::MCA_ind_plot(res, xax = 2, yax = 3, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "specialty", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = FALSE,
                     labels_positions = NULL, xlim = c(-1.89, 2.1), ylim = c(-1.37, 2.63))


#######################################################################################################################
###########################################################################################################################
###########################################################################################################################
#########################################################################@ Regression logistique ##############################

base_doctor$pf_discussion.2 <-as.factor(base_doctor$pf_discussion.2)

var_selected<-c("specialty", "junior_senior","gender_bin")

names_var_selected <-c("Specialty", "Age","Gender")

lm =logisticRegressionTable(base_doctor, var_selected, names_var_selected, var_to_explain= "pf_discussion.2",level_to_import="Yes",variables_use_multivariate = NA, alpha_cut_multivariate = 1, all_multivariate_values = F)

lm %>% kbl("latex", align = "llr", vline = "|", caption = "Logistic regression results for fertility preservation discussion")%>%kable_styling() %>% column_spec(1, bold = F, color = "red")



#################################################################################################################################
#################################################################################################################################
################################################################################################################################

############################################################################  Graphiques ######################################
###############################################################################################################################



g <- ggplot(base_doctor, aes(pf_discussion.2)) 

g + geom_histogram(stat = "count") +  
  labs(title="Histogram with fertility preservation discussion", 
       subtitle="fp_discussion") + geom_text(stat='count', aes(label=..count..), vjust=-1, size=2) + xlab("Has a discussion ")+ theme_classic()





######################################################################### Pour faire la grille des graphiques : code Amyn

# Nouvelle page
grid.newpage()
# Créer la mise en page : nrow = 3, ncol = 1
pushViewport(viewport(layout = grid.layout(3, 1)))

# Arranger les graphiques
print(a, vp = define_region(1, 1))
print(c, vp = define_region(2, 1))
print(d, vp = define_region(3, 1))






