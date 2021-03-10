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

######################################################################  Base de données ############################################################

base_julie$bmi_4cl_ord <- fct_relevel(base_julie$bmi_4cl,"<18.5", "18.5-24.9", "25-29.9",">=30")



base_doctor = d2_long_2_with_names_centers


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



mixte <-doc%>% inner_join(patient)

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
# AIC      BIC     logLik    deviance    df.resid 
# 1932.8   1943.7   -964.4   1928.8       1738 
# 
# Scaled residuals: 
#   Min      1Q     Median      3Q     Max 
# -0.8549 -0.3477 -0.3477  0.6782  1.5739 
# 
# Random effects:
#   Groups       Name        Variance   Std.Dev.
# numdos_curie (Intercept)   3.417        1.848   
# 

###Number of obs: 1740, groups:  numdos_curie, 1003
# 
# Fixed effects:
#              Estimate Std. Error  zvalue  Pr(>|z|)    
# (Intercept)  -1.3760     0.1256  -10.96   <2e-16 ***
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
# -0.7656  -0.7656  -0.7656   1.6555   1.6555  
# 
# Coefficients:
#              Estimate  Std. Error z value   Pr(>|z|)    
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


logLik(M0)-logLik(singlelevel) 
# 'log Lik.' 21.68403 (df=2)


################################################### Test pour anova 


anova(M0,singlelevel)

#     Data: mixte
#     Models:
#     singlelevel: pf_discussion ~ 1

# M0: pf_discussion ~ (1 | numdos_curie)

#              npar AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# singlelevel    1 1974.2 1979.6 -986.08   1972.2                         
# M0             2 1932.8 1943.7 -964.39   1928.8 43.368  1  4.535e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 


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
# 1911.8   1922.7   -953.9   1907.8     1733 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.1392 -0.6216 -0.4606  0.9569  3.2564 
# 
# Random effects:
#  Groups    Name        Variance Std.Dev.
# dr_anonym (Intercept) 0.6447   0.803   
# Number of obs: 1735, groups:  dr_anonym, 96
# 
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3392     0.1274  -10.51   <2e-16 ***


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
# 1904.3   1920.7   -949.2   1898.3     1732 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.0134 -0.5332 -0.4019  0.8243  2.2757 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 0.9248   0.9617  
# dr_anonym    (Intercept) 0.6713   0.8193  
# Number of obs: 1735, groups:  numdos_curie, 1003; dr_anonym, 96
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.4186     0.1434  -9.894   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 


#################### On fait maintenant la différence entre les deux modèles et on réalise le test statistqiue



anova(M0,singlelevel)

# Data: mixte
# Models:
#   singlelevel: pf_discussion ~ 1
# M0: pf_discussion ~ (1 | dr_anonym) + (1 | numdos_curie)
#             npar  AIC    BIC  logLik    deviance  Chisq Df Pr(>Chisq)    
# singlelevel    1 1971.2 1976.7 -984.61   1969.2                         
# M0             3 1904.3 1920.7 -949.17   1898.3 70.876  2  4.069e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 




################################################################### On teste s'il y a un effet docteur 




two_level <-glmer(pf_discussion ~  (1|numdos_curie),family = binomial("logit"), data = mixte)
summary(two_level)


# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: pf_discussion ~ (1 | numdos_curie)
# Data: mixte
# 
# AIC      BIC   logLik deviance df.resid 
# 1930.7   1941.6   -963.3   1926.7     1733 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.8527 -0.3515 -0.3515  0.6862  1.5747 
# 
# Random effects:
#   Groups       Name        Variance Std.Dev.
# numdos_curie (Intercept) 3.307    1.818   
# Number of obs: 1735, groups:  numdos_curie, 1003
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.3639     0.1234  -11.05   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(M0,two_level)

# Data: mixte
# Models:
#   two_level: pf_discussion ~ (1 | numdos_curie)
# M0: pf_discussion ~ (1 | dr_anonym) + (1 | numdos_curie)
#             npar AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# two_level    2 1930.7 1941.6 -963.33   1926.7                         
# M0           3 1904.3 1920.7 -949.17   1898.3 28.317  1   1.03e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


################################################################################ on teste s'il y a un effet patient 


two_level_s <-glmer(pf_discussion ~  (1|dr_anonym),family = binomial("logit"), data = mixte)
summary(two_level_s)

################################################################################## Analyse des résidus 


library(lme4)

install.packages('insight')
library(insight)
model_info(M0)
get_parameters(M0)
get_variance(M0)

# var distribution : 3.289868 ????
# var residual : donc notre e 3.289868
# var random (doctor+patientes) : 1.596091

# $var.intercept
# numdos_curie    dr_anonym 
# 0.9248041    0.6712864 

#var distribution dépend du type de modèle qu'on utilise . en gros dans notre cas spécifique : variance de dispersion est égale variance de résiduelle








