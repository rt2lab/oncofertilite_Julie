#####################################################################################################################################################@@@@
#######################################################################################################################################################

#############################################################################  R2MLwiN #################################################################


library(devtools)
# Build and install rbugs
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/rbugs/rbugs_0.5-9.1.tar.gz")
# Build and install R2MLwiN
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/R2MLwiN/R2MLwiN_0.8-5.tar.gz")

install.packages('R2MLwiN')

library('R2MLwiN')

install.packages('lme4')
library(lme4)
library(forcats)



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





######################################################################  Base de données ############################################################

base_julie$bmi_4cl_ord <- fct_relevel(base_julie$bmi_4cl,"<18.5", "18.5-24.9", "25-29.9",">=30")



base_doctor = d2_long_2_with_names_centers



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


data_fertil_preserv = base_complet %>% filter(fertil_preserv=="Yes")

data_fertil_preserv$fertil_miv_cos_2 <- NA
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "No"] <- "IVM"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "Yes" & data_fertil_preserv$cos == "Yes"] <- "At least one COS"
data_fertil_preserv$fertil_miv_cos_2[data_fertil_preserv$ivm == "No" & data_fertil_preserv$cos == "No"] <- NA
table(data_fertil_preserv$fertil_miv_cos_2)

base_complet$year_diag<-as.character(base_complet$year_diag)




# 1364 observations pour base complet 

base_complet <- left_join(base_julie, database_preprocessed_labels, by = c("numdos_curie" = "numdos_curie")) 


# maintenant si on enlève les duplicats dans notre base de données : on a bien 1357 observations !!

base_complet<-base_complet[!duplicated(base_complet$numdos_curie), ]



####################################################################### Modification de la base docteur 

library(dplyr)
doc <-base_doctor %>% arrange(numdos_curie)
doc <- doc %>% tibble::rownames_to_column() 
doc<- doc %>% rename("consultation" = rowname)


################################################################### notre base doc est prête 

################################################################## on va raccourcir la base patientes déjà 

patient <- base_complet %>% select("numdos_curie","age_young_cl","nb_child_3cl", "bmi_4cl_ord","brca_mut", "inflammatory_bc","grade_3cl","subtype4.y", "histo_3cl", "neo_ct")


# vérifier la base mixte !!! que fait left_join ? : 

mixte <-left_join(doc, patient, by = c("numdos_curie" = "numdos_curie")) 

#joining by numdos_curie


################################################################ la table me semble bonne 

# A. Specifying and estimating basic models

# A.a) Null model for numdos : on teste s'il y a bien un effet patiente 

###############@ il faut transformer pf_discussion en factor 

mixte$pf_discussion <- as.factor(mixte$pf_discussion)

M0 <- glmer(pf_discussion ~ (1 | numdos_curie),family = binomial("logit"), data = mixte)
summary(M0)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | numdos_curie)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1977.5   1988.5   -986.8   1973.5     1785 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7585 -0.3564 -0.3564  0.5971  1.5875 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 3.053    1.747   
# Number of obs: 1787, groups:  numdos_curie, 1035
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3756     0.1197   -11.5   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


singlelevel <- glm(pf_discussion ~ 1, data = mixte, family = binomial("logit"))
summary(singlelevel)

# Call:
#   glm(formula = pf_discussion ~ 1, family = binomial("logit"), 
#       data = mixte)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.7598  -0.7598  -0.7598   1.6634   1.6634  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.09489    0.05458  -20.06   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2012.5  on 1786  degrees of freedom
# Residual deviance: 2012.5  on 1786  degrees of freedom
# AIC: 2014.5
# 
# Number of Fisher Scoring iterations: 4

logLik(M0)-logLik(singlelevel) 
# 'log Lik.' 19.50487 (df=2)


################################################### Test pour anova 


anova(M0,singlelevel)

# Data: mixte
# Models:
#   singlelevel: pf_discussion ~ 1
# M0: pf_discussion ~ (1 | numdos_curie)
# npar    AIC    BIC   logLik deviance Chisq Df Pr(>Chisq)    
# singlelevel    1 2014.5 2020.0 -1006.26   2012.5                        
# M0             2 1977.5 1988.5  -986.76   1973.5 39.01  1  4.217e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### On plote les résidus 


u0 <- ranef(M0, postVar = TRUE) 
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 
commid <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
colnames(u0tab)[2] <- "u0"
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$commid), ]
colnames(u0tab)[4] <- "u0rank"




############################################## On fait le modèle M0 pour le level des docteurs. 


# on doit enlever les na pour dr_anonym sinon on ne peut pas runner le proc anova 
library(dplyr)
mixte$dr_anonym<-as.factor(mixte$dr_anonym)
mixte <- mixte %>%drop_na(pf_discussion,dr_anonym)


M0doc <- glmer(pf_discussion ~ (1 | dr_anonym),family = binomial("logit"), data = mixte)
summary(M0doc)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | dr_anonym)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1952.1   1963.1   -974.1   1948.1     1780 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.1385 -0.6353 -0.4756  0.8783  3.2699 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# dr_anonym (Intercept) 0.6492   0.8057  
# Number of obs: 1782, groups:  dr_anonym, 96
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3531     0.1274  -10.62   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(M0doc,singlelevel)


# Data: mixte
# Models:
#   singlelevel: pf_discussion ~ 1
# M0doc: pf_discussion ~ (1 | dr_anonym)
#             npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# singlelevel    1 1971.2 1976.7 -984.61   1969.2                         
# M0doc          2 1911.8 1922.7 -953.89   1907.8 61.443  1  4.559e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 


############################################### On fait le modèle M0 en combinant les deux levels pour voir s'il y a bien un effet conjugué

mixte$pf_discussion <- as.factor(mixte$pf_discussion)

singlelevel <- glm(pf_discussion ~ 1, data = mixte, family = binomial("logit"))
summary(singlelevel)

# Call:
#   glm(formula = pf_discussion ~ 1, family = binomial("logit"), 
#       data = mixte)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.7656  -0.7656  -0.7656   1.6555   1.6555  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.07727    0.05507  -19.56   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1972.2  on 1739  degrees of freedom
# Residual deviance: 1972.2  on 1739  degrees of freedom
# AIC: 1974.2
# 
# Number of Fisher Scoring iterations: 4

library(forcats)
library(tidyr)# pour avoir la fonction drop_na 
mixte$dr_anonym<-as.factor(mixte$dr_anonym)
mixte <- mixte %>%drop_na(pf_discussion,dr_anonym)


M0 <- glmer(pf_discussion ~  (1|dr_anonym) + (1|numdos_curie),family = binomial("logit"), data = mixte)
summary(M0)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | dr_anonym) + (1 | numdos_curie)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1946.9   1963.3   -970.4   1940.9     1779 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9766 -0.5495 -0.4151  0.8031  2.3611 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 0.7609   0.8723  
# dr_anonym    (Intercept) 0.6710   0.8191  
# Number of obs: 1782, groups:  numdos_curie, 1035; dr_anonym, 96
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.4201     0.1407  -10.09   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#################### On fait maintenant la différence entre les deux modèles et on réalise le test statistqiue



anova(M0,singlelevel)

###???



################################################################### On teste s'il y a un effet docteur 




two_level <-glmer(pf_discussion ~  (1|numdos_curie),family = binomial("logit"), data = mixte)
summary(two_level)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | numdos_curie)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1975.4   1986.3   -985.7   1971.4     1780 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.7563 -0.3600 -0.3600  0.6040  1.5883 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 2.959    1.72    
# Number of obs: 1782, groups:  numdos_curie, 1035
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3644     0.1176   -11.6   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(M0,two_level)



# Data: mixte
# Models:
#   two_level: pf_discussion ~ (1 | numdos_curie)
# M0: pf_discussion ~ (1 | dr_anonym) + (1 | numdos_curie)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# two_level    2 1975.4 1986.3 -985.68   1971.4                         
# M0           3 1946.9 1963.3 -970.44   1940.9 30.476  1  3.381e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

################################################################################ on teste s'il y a un effet patient 


two_level_s <-glmer(pf_discussion ~  (1|dr_anonym),family = binomial("logit"), data = mixte)
summary(two_level_s)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | dr_anonym)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1952.1   1963.1   -974.1   1948.1     1780 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.1385 -0.6353 -0.4756  0.8783  3.2699 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# dr_anonym (Intercept) 0.6492   0.8057  
# Number of obs: 1782, groups:  dr_anonym, 96
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3531     0.1274  -10.62   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



anova(M0,two_level_s)


# Data: mixte
# Models:
#   two_level_s: pf_discussion ~ (1 | dr_anonym)
# M0: pf_discussion ~ (1 | dr_anonym) + (1 | numdos_curie)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# two_level_s    2 1952.1 1963.1 -974.05   1948.1                        
# M0             3 1946.9 1963.3 -970.44   1940.9 7.2171  1   0.007221 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 


################################################################################## Analyse des résidus 


library(lme4)

install.packages('insight')
library(insight)
model_info(M0)
get_parameters(M0)
get_variance(M0)

# $var.fixed
# [1] 0
# 
# $var.random
# [1] 1.431914
# 
# $var.residual
# [1] 3.289868
# 
# $var.distribution
# [1] 3.289868
# 
# $var.dispersion
# [1] 0
# 
# $var.intercept
# numdos_curie    dr_anonym 
# 0.7609254    0.6709886 
# 
# 
# 


################################################################ Introduction des coefficients et des niveaux dans le code 

# modèle : 

M <- glmer(pf_discussion ~ age_young_cl + nb_child_3cl + inflammatory_bc + neo_ct + grade_3cl + center_anonym + gender_bin + junior_senior + specialty +  (1|dr_anonym) + (1|numdos_curie), family = binomial("logit"), data = mixte)


# the model seems to complex for the data. We do not have enough observations it seems. With lme4, it is fairly common to have convergence warnings (too many fixed or rendom parameters). 


# Warning messages:
#   1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :failure to converge in 10000 evaluations
#  2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :  convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
#  3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :   Model failed to converge with max|grad| = 0.217762 (tol = 0.002, component 1)                                                                                 
#                                                                                    

# On essaie de résoudre le problème en testant le shypothèses suivantes : 

install.packages('numDeriv')
install.packages('RCurl')
install.packages('reshape2')
install.packages('plyr')

# on essaie de s'intéresser à un problème de singularité 


library("numDeriv")
library("RCurl")
library("reshape2")
library("plyr")
library("RColorBrewer")

tt <- getME(M,"theta")
ll <- getME(M,"lower")
min(tt[ll==0])

# 0.854093

# On essaie d'augmenter les itérations maximales du modèle 

ss <- getME(M,c("theta","fixef"))
M1 <- update(M,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


# Warning messages:
#   1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :failure to converge in 20000 evaluations
#                                                                         


# toujours le même problème de convergence 


# on essaie de changer l'optimisation du modèle 

M2 <- update(M,start=ss,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
                                                 
# If we set the number of evaluations large enough so the optimization actually finishes, we do avoid the warning. 

summary(M2)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ age_young_cl + nb_child_3cl + inflammatory_bc +      neo_ct + grade_3cl + center_anonym + gender_bin + junior_senior +      specialty + (1 | dr_anonym) + (1 | numdos_curie)
# Data: mixte
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
# 
# AIC      BIC   logLik deviance df.resid 
# 1395.5   1488.2   -680.8   1361.5     1712 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4044 -0.3627 -0.1511  0.2082  7.6393 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 1.2613   1.1231  
# dr_anonym    (Intercept) 0.7664   0.8754  
# Number of obs: 1729, groups:  numdos_curie, 1005; dr_anonym, 95
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    2.16999    0.79120   2.743 0.006095 ** 
#   age_young_cl[30 -35)           0.11051    0.33567   0.329 0.741977    
# age_young_cl[35 -40)          -1.29325    0.39678  -3.259 0.001117 ** 
#   age_young_cl40+               -3.70778    0.62687  -5.915 3.32e-09 ***
#   nb_child_3cl1                 -1.11953    0.27765  -4.032 5.53e-05 ***
#   nb_child_3clMore than 1       -2.19187    0.37593  -5.830 5.53e-09 ***
#   inflammatory_bcYes            -0.70983    0.73335  -0.968 0.333084    
# neo_ctYes                     -0.69673    0.20174  -3.454 0.000553 ***
#   grade_3clGrade II             -0.05341    0.51058  -0.105 0.916693    
# grade_3clGrade III            -0.08702    0.50388  -0.173 0.862883    
# center_anonymCenter 2          0.72926    0.27268   2.674 0.007485 ** 
#   gender_binmale                -0.63770    0.30092  -2.119 0.034074 *  
#   junior_seniorsenior           -0.48498    0.30156  -1.608 0.107787    
# specialtyradiation oncologist -3.37047    1.03726  -3.249 0.001156 ** 
#   specialtysurgeon               0.03481    0.31350   0.111 0.911598    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


##################  Predicted probabilities : à voir comment ca avance !!! 

##################################################  Two level random slopes model 

# 
# The models fitted in previous exercises have allowed the probability of having fertility preservation discussion from a doctor
# to depend on the doctors (as well as individual characteristics). 
# This was achieved by allowing the model intercept to vary randomly across doctors and individual in a random intercept model. 
# We have assumed, however, that the effects of individual characteristics such as age and number of children are the saùe for each doctor,
# i.e. the coefficients of all explanatory variables are fixed across doctors. 
# We will now extend the random intercept model from which we calculated predicted probabilities
# in P7.4 to allow both the intercept and the coefficient of one of the explanatory variables to vary randomly across doctors


########### Allowing the effects of age and number of children to vary across doctors 


M3 <-  glmer(pf_discussion ~ age_young_cl + nb_child_3cl + inflammatory_bc + neo_ct + grade_3cl + center_anonym + gender_bin + junior_senior + specialty +  (1+age_young_cl + nb_child_3cl |dr_anonym) + (1|numdos_curie), family = binomial("logit"), data = mixte)

# on essaie de changer l'optimisation du modèle 

# redéfinir start ss 

tt <- getME(M3,"theta")
ll <- getME(M3,"lower")
min(tt[ll==0])

#[1] 0.03160009

ss <- getME(M3,c("theta","fixef"))


M4<- update(M3,start=ss,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# If we set the number of evaluations large enough so the optimization actually finishes, we do avoid the warning. 

summary(M4)

# les variables docteurs varient au niveau des docteurs ? et les variables patientes varient au niveau de la patiente ? 
# mais est ce que l'âge ne joue pas finalement aussi au niveau des docteurs ? 

# pour le savoir, on va d'abord tester pour un random slope ! 

# On a un problème avec ce modèle : 

isSingular(M4)
# The model is singular 

# Complex mixed-effect models (i.e., those with a large number of variance-covariance parameters) frequently result in singular fits, i.e. estimated variance-covariance matrices with less than full rank. 
# Less technically, this means that some "dimensions" of the variance-covariance matrix have been estimated as exactly zero. For scalar random effects such as intercept-only models,
# or 2-dimensional random effects such as intercept+slope models, singularity is relatively easy to detect because it leads to random-effect variance estimates of (nearly) zero,
# or estimates of correlations that are (almost) exactly -1 or 1. However, for more complex models (variance-covariance matrices of dimension >=3) singularity can be hard to detect; 
# models can often be singular without any of their individual variances being close to zero or correlations being close to +/-1.
# # 
# This function performs a simple test to determine whether any of the random effects covariance matrices of a fitted model are singular. 
# The model is singular 
# 

# While singular models are statistically well defined (it is theoretically sensible for the true maximum likelihood estimate to correspond to a singular fit), there are real concerns 
# that (1) singular fits correspond to overfitted models that may have poor power; 
# (2) chances of numerical problems and mis-convergence are higher for singular models (e.g. it may be computationally difficult to compute profile confidence intervals for such models);
# (3) standard inferential procedures such as Wald statistics and likelihood ratio tests may be inappropriate.
# # 
# There is not yet consensus about how to deal with singularity, or more generally to choose which random-effects specification (from a range of choices of varying complexity) to use. Some proposals include:
#   
#   avoid fitting overly complex models in the first place, i.e. design experiments/restrict models a priori such that the variance-covariance matrices can be estimated precisely enough to avoid singularity (Matuschek et al 2017)
# 
# use some form of model selection to choose a model that balances predictive accuracy and overfitting/type I error (Bates et al 2015, Matuschek et al 2017)
# 
# “keep it maximal”, i.e. fit the most complex model consistent with the experimental design, removing only terms required to allow a non-singular fit (Barr et al. 2013), or removing further terms based on p-values or AIC
# 
# use a partially Bayesian method that produces maximum a posteriori (MAP) estimates using regularizing priors to force the estimated random-effects variance-covariance matrices away from singularity (Chung et al 2013, blme package)
# 
# use a fully Bayesian method that both regularizes the model via informative priors and gives estimates and credible intervals for all parameters that average over the uncertainty in the random effects parameters (Gelman and Hill 2006, McElreath 2015; MCMCglmm, rstanarm and brms packages)


# donc ne gros le modèle est overfitted. 

# 2 posssibilités : soit on fait un modèle mixte simple. Soit on a plus de données. soit on construit un modèle bayésien 

# 1) je change le modèle. et j'essaie de voir s'il y a toujours ces problèmes de singularité 
# A) simplification 
# B) essayer de chnager avec les dummy : on intégre être plus vielle que 35 ans qui varie selon les docteurs. Mais pas le non. 
# pareail avec le fait d'avoir des enfants ou non 


# so far in this model we have fitted linear effect for age or number of children 
# we can create dummy variable 
# 
# In the random slopes analysis of this lesson, we have assumed that wealth has a linear relationship with the log- odds of antenatal care in all communities, but we have allowed the slope of this relationship to vary across communities. This implies that the between-community variance is a quadratic function of wealth, although our plot of the variance function shows that the variance depends linearly on wealth.
# Correlation of (Intr) magec 0.035
# Fixed Effects:
# In this exercise, we will reassess the assumption that wealth has a linear relationship by once again fitting dummy variables. 
# This allows for a more flexible community variance function,
# but comes at the cost of adding many more parameters to the model. 
# We will therefore investigate whether we can simplify the model by having random coefficients on a subset of the wealth dummies



# Adding level 2 explanatory variable 
# c'est équivalent à introduire un contextual effect 


# le modèle ne donne rien . on avait mis très peu de feature et c'est déjà le bordel 
















