library(tidyverse)
library(forestplot)

###############################################
## #####  Test d'interaction univarié glm variable quantitative  ###############
#############################################
#cette fontion fait à present les mutli aussi

test_interaction_glm <- function(y, var, pred, pretty_var, pretty_y, data, multi) {
  
  if(missing(multi)) {
    f<-as.formula(paste(pred,"~",y,"*",var))
    
  } else {
    f<-as.formula(paste(pred,"~",y,"*",var, "+", multi))
  }
  
  #creation du glm
 
  mod <- glm(f, family = "binomial", data = data)
  #recuperation du petit p
  petit_p <- car::Anova(mod, test.statistic="Wald")
  petit_p <- round(petit_p$"Pr(>Chisq)"[3],2)
  petit_p
  
  # predictions
  mydf <- ggpredict(mod, terms = c(var,y))
  # plot
  plot <- ggplot(mydf, aes(x, predicted, colour = group)) + 
    geom_line()+
    labs(title=glue("{pretty_var}/{pretty_y}"),
         x=pretty_var,
         y="")+
    scale_color_brewer(palette = "Set1")+
    theme(legend.title=element_blank(), legend.position = "top", plot.title = element_text(size=10),axis.title.x = element_text(size=10),
          axis.title.y = element_text( size=.8), legend.text = element_text(size = 10),legend.key.size = unit(0.3, "cm"),
          legend.key.width = unit(0.3,"cm"))+
    annotate(geom = "text", label =glue("p={petit_p}"), x = 4, y = 0.2)
  
  return(list(plot,petit_p))
}

poss_inter_glm = possibly(.f = test_interaction_glm, otherwise = NULL)
###############################################
## #####  Test d'interaction multivarié glm ariable quantitative  ###############
#############################################

test_interaction_glm_multi <- function(y, var, pred, pretty_var, pretty_y, multi, data) {
  #creation du glm
  f<-as.formula(paste(pred,"~",y,"*",var,"+",multi))
  mod <- glm(f, family = "binomial", data = data)
  #recuperation du petit p
  petit_p <- car::Anova(mod, test.statistic="Wald")
  petit_p <- round(petit_p$"Pr(>Chisq)"[3],2)
  petit_p
  
  # predictions
  mydf <- ggpredict(mod, terms = c(var,y))
  # plot
  plot <- ggplot(mydf, aes(x, predicted, colour = group)) + 
    geom_line()+
    labs(title=glue("Interaction between {pretty_var} and {pretty_y}"),
         x=pretty_var,
         y="")+
    scale_color_brewer(palette = "Set1")+
    theme(legend.title=element_blank(), legend.position = "top", plot.title = element_text(size=5),axis.title.x = element_text(size=5),
          axis.title.y = element_text( size=.5), legend.text = element_text(size = 5),legend.key.size = unit(0.1, "cm"),
          legend.key.width = unit(0.1,"cm"))+
    annotate(geom = "text", label =glue("p={petit_p}"), x = 4, y = 0.2)
  
  return(list(plot,petit_p))
}

poss_inter_glm_multi = possibly(.f = test_interaction_glm_multi, otherwise = NULL)
###############################################
## #####  Test d'interaction univarié cox variable quantitative  ###############
#############################################

test_interaction_cox <- function(delay, status, y, var, pretty_var, pretty_y, pretty_survival,data, multi) {
  
  if(missing(multi)) {
    f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var))
    
  } else {
    f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var, "+", multi))
  }
  #creation du glm
  
  mod <- coxph(f, data = data)
  #recuperation du petit p
  petit_p <- car::Anova(mod, test.statistic="Wald")
  petit_p <- round(petit_p$"Pr(>Chisq)"[3],2)
  
  
  # predictions
  mydf <- ggpredict(mod, terms = c(var,y), back.transform = F)
  # plot
  plot<-ggplot(mydf, aes(x, predicted, colour = group)) + 
    geom_line()+
    labs(title=glue("{pretty_var} /{pretty_y} "),
         x=pretty_var,
         y="")+
    scale_color_brewer(palette = "Set1")+
    theme(legend.title=element_blank(), legend.position = "top", plot.title = element_text(size=10),axis.title.x = element_text(size=10),
          axis.title.y = element_text( size=.8), legend.text = element_text(size = 10),legend.key.size = unit(0.3, "cm"),
          legend.key.width = unit(0.3,"cm"))+
    annotate(geom = "text", label =glue("p={petit_p}"), x = 4, y = 0.2)
  
  return(list(plot,petit_p))
}

poss_inter_cox = possibly(.f = test_interaction_cox, otherwise = NULL)
###############################################
## #####  Test d'interaction multivarié cox  variable quantitative ###############
#############################################

test_interaction_cox_multi <- function(delay, status, y, var, multi,pretty_var, pretty_y, pretty_survival,data) {
  #creation du glm
  f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var,"+", multi))
  mod <- coxph(f, data = data)
  #recuperation du petit p
  petit_p <- car::Anova(mod, test.statistic="Wald")
  petit_p <- round(petit_p$"Pr(>Chisq)"[3],2)
  
  
  # predictions
  mydf <- ggpredict(mod, terms = c(var,y))
  # plot
  plot<-ggplot(mydf, aes(x, predicted, colour = group)) + 
    geom_line()+
    labs(title=glue("Impact of {pretty_var} on {pretty_survival} according to {pretty_y} "),
         x=pretty_var,
         y="")+
    scale_color_brewer(palette = "Set1")+
    theme(legend.title=element_blank(), legend.position = "top", plot.title = element_text(size=5),axis.title.x = element_text(size=5),
          axis.title.y = element_text( size=.5), legend.text = element_text(size = 5),legend.key.size = unit(0.1, "cm"),
          legend.key.width = unit(0.1,"cm"))+
    annotate(geom = "text", label =glue("p={petit_p}"), x = 4, y = 0.2)
  
  return(list(plot,petit_p))
}

poss_inter_cox_multi = possibly(.f = test_interaction_cox_multi, otherwise = NULL)
###############################################
## #####  Test d'interaction cox  variables quantitatives uniquement = forest plot ###############
#############################################
forest_plot_cox <- function(y, var, pretty_y,delay, status, data,multi){
  
  if(missing(multi)) {
    f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,":",var,"+", y))

  } else {
    f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,":",var,"+", y, "+", multi))
  }
  
  #f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,":",var,"+", y))
  mod <- coxph(f, data = data)
  uni <- cox_summary(mod)
  
  fp<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var))
  mod2 <- coxph(fp, data = data)
  pval <- car::Anova(mod2, test.statistic="Wald")$`Pr(>Chisq)`
  
  nmax <- length(levels(data[,y]))
  #nmin <- length(levels(data[,var]))
  nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  #nch <- (nmax*nmin)-nmax
  nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
  
  HR_plot  = round(tail(uni$exp.coef.,nch),2)
  lower_plot = round(tail(uni$IC_inf.,nch),2)
  upper_plot = round(tail(uni$IC_sup.,nch),2)
  p_plot = round(tail(uni$P_value,nch),2)
  
  HR_lab <- as.character(HR_plot)
  lower_lab <- as.character(lower_plot)
  upper_lab <- as.character(upper_plot)
  p_lab <- as.character(p_plot)
  
  inter_plot_quali<-
    data.frame(
      HR  = c(NA,NA,HR_plot),
      lower = c( NA,NA,lower_plot),
      upper = c(NA, NA,upper_plot), 
      p = c( NA,NA,p_plot))
  
  # label <- cbind(c(y, levels(d[y])),
  #                c("HR", HR_lab),
  #                c("IC-Low", lower_lab),
  #                c("IC-High",upper_lab),
  #                c("p-value" ,p_lab))
  
  # dim_var <- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  #   dim_y <- length(levels(data[,y]))
  nb <- ifelse (nmin ==1, 2, nmin)
  N <- list()
  for (i in 2:nb) {
    N[[i]] <- c(levels(data[,var])[i], rep(NA, nmax-1))
    
  }
  
  nmin_mod <- ifelse(nmin == 1, 1, nmin-1)
  deter <- ifelse(nmin == 1, 1, 2)
  
  label <- cbind(c(rep(NA, deter),unlist(N)),
                 c(NA,pretty_y, rep(levels(data[,y]),nmin_mod)),
                 c(NA,NA, HR_lab),
                 c(NA,NA, lower_lab),
                 c(NA,NA,upper_lab),
                 c( NA,tail(round(pval, 2),1),rep(NA, nch)))
  
  tibble(number = inter_plot_quali, label = label)
  
}

  
poss_inter_cox_fp = possibly(.f = forest_plot_cox, otherwise = NULL)

####################################################################################
############### TAblo des HR ##############################
############################################################

# table_plot_cox <- function(y, var, pretty_y,delay, status, data){
#   f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,":",var,"+", y))
#   mod <- coxph(f, data = data)
#   uni <- cox_summary(mod)
#   
#   fp<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var))
#   mod2 <- coxph(fp, data = data)
#   pval <- car::Anova(mod2)$`Pr(>Chisq)`
#   
#   nmax <- length(levels(data[,y]))
#   #nmin <- length(levels(data[,var]))
#   nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
#   #nch = (nmax*nmin)-nmax
#   
#   nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
#   #   if (category == "quanti") {
#   # nch = (nmax*nmin)-nmax
#   # } else {
#   # nch = 2
#   #  }
#   
#   
#   chiffres <-  tail(uni, nch)
#   deter <- ifelse(nmin == 1, 1, 2)
#  
#   
#   intitule <-    data.frame(
#     c(rep(NA, deter),unlist(N)),
#     c(NA,pretty_y, rep(levels(data[,y]),nmin_mod))
#     Labels = c(rep("", deter),pretty_y, levels(data[,y])),
#     HR = c("","",chiffres$exp.coef.),
#     IC_inf. = c( "","",chiffres$IC_inf.),
#     IC_sup. = c( "","",chiffres$IC_sup.), 
#     P_value = c( "",tail(round(car::Anova(mod2)$`Pr(>Chisq)`,2),1),rep("",nch)))
#   
#   return(intitule)
# }


###############################################
## #####  Test d'interaction coxvariables quantitatives uniqement multi###############
############################################# 
######################################################################################################

forest_plot_cox_multi <- function(y, var, pretty_y,delay, status, multi, data){
  f<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,":",var,"+", y, "+", multi))
  mod <- coxph(f, data = data)
  uni <- cox_summary(mod)
  
  fp<-as.formula(paste("Surv(",delay,",",status, ")", "~",y,"*",var, "+", multi))
  mod2 <- coxph(fp, data = data)
  pval <- car::Anova(mod2, test.statistic="Wald")$`Pr(>Chisq)`
  
  nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  nmax <- length(levels(data[,y]))
  #nmin <- length(levels(data[,var]))
  #nch <- (nmax*nmin)-nmax
  nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
  
  HR_plot  = round(tail(uni$exp.coef.,nch),2)
  lower_plot = round(tail(uni$IC_inf.,nch),2)
  upper_plot = round(tail(uni$IC_sup.,nch),2)
  p_plot = round(tail(uni$P_value,nch),2)
  
  HR_lab <- as.character(HR_plot)
  lower_lab <- as.character(lower_plot)
  upper_lab <- as.character(upper_plot)
  p_lab <- as.character(p_plot)
  
  inter_plot_quali<-
    data.frame(
      HR  = c(NA,NA,HR_plot),
      lower = c( NA,NA,lower_plot),
      upper = c(NA, NA,upper_plot), 
      p = c( NA,NA,p_plot))
  
  # label <- cbind(c(y, levels(d[y])),
  #                c("HR", HR_lab),
  #                c("IC-Low", lower_lab),
  #                c("IC-High",upper_lab),
  #                c("p-value" ,p_lab))

  # dim_var <- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  #   dim_y <- length(levels(data[,y]))
  nb <- ifelse (nmin ==1, 2, nmin)
    N <- list()
 for (i in 2:nb) {
    N[[i]] <- c(levels(data[,var])[i], rep(NA, nmax-1))
   
  }
  
    nmin_mod <- ifelse(nmin == 1, 1, nmin-1)
    deter <- ifelse(nmin == 1, 1, 2)
    
    label <- cbind(c(rep(NA, deter),unlist(N)),
    c(NA,pretty_y, rep(levels(data[,y]),nmin_mod)),
                 c(NA,NA, HR_lab),
                 c(NA,NA, lower_lab),
                 c(NA,NA,upper_lab),
                 c( NA,tail(round(pval, 2),1),rep(NA, nch)))
  
  tibble(number = inter_plot_quali, label = label)
  
}


poss_inter_cox_fp_multi = possibly(.f = forest_plot_cox_multi, otherwise = NULL)

###############################################
## #####  Test d'interaction glm variables qualitatives uniqement ###############
############################################# 
# mod <- glm(RCH5.f~menop:side + side, family = "binomial", data = d)
# 
# uni_mod<- glm_summary(mod)
# kable(uni_mod)
# 
# library(forestplot)
# nmax <- length(levels(d[,"side"]))
# nch <- nmax+(nmax-1)
# 
# 
# OR_plot  = round(uni_mod$Odds.Ratio[c(nmax:nch)],2)
# lower_plot = round(uni_mod$IC_inf.[c(nmax:nch)],2)
# upper_plot = round(uni_mod$IC_sup.[c(nmax:nch)],2) 
# p_plot = round(uni_mod$P_value[c(nmax:nch)],2)
# 
# OR_lab <- as.character(OR_plot)
# lower_lab <- as.character(lower_plot)
# upper_lab <- as.character(upper_plot)
# p_lab <- as.character(p_plot)
# 
# inter_plot_quali<-
#   data.frame(
#     OR  = c(NA,NA,OR_plot),
#     lower = c( NA,NA,lower_plot),
#     upper = c(NA, NA,upper_plot), 
#     p = c( NA,NA,p_plot))
# 
# # label <- cbind(c(y, levels(d[y])),
# #                c("HR", HR_lab),
# #                c("IC-Low", lower_lab),
# #                c("IC-High",upper_lab),
# #                c("p-value" ,p_lab))
# 
# label <- cbind(c(NA,"side", levels(d[,"side"])),
#                c(NA,NA, OR_lab),
#                c(NA,NA, lower_lab),
#                c(NA,NA,upper_lab),
#                c(NA,NA ,p_lab))
# 
# tibble(number = inter_plot_quali, label = label)


forest_plot_glm <- function(y, var, pred,  pretty_y, data, multi) {
  #creation du glm
  if(missing(multi)) {
    f<-as.formula(paste(pred,"~",y,":",var,"+", y))
    
  } else {
    f<-as.formula(paste(pred,"~",y,":",var,"+", y, "+", multi))
  }
  
  #f<-as.formula(paste(pred,"~",y,":",var,"+", y))
  mod <- glm(f, family = "binomial", data = data)
  uni_mod <- glm_summary(mod)
  
  fp<-as.formula(paste(pred,"~",y,"*",var))
  mod2 <- glm(fp, family = "binomial", data = data)
  pval <- car::Anova(mod2, test.statistic="Wald")$`Pr(>Chisq)`
  
  nmax <- length(levels(data[,y]))
  #nmin <- length(levels(data[,var]))
  nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
  
  

  
  OR_plot  = round(tail(uni_mod$Odds.Ratio,nch),2)
  lower_plot = round(tail(uni_mod$IC_inf.,nch),2)
  upper_plot = round(tail(uni_mod$IC_sup.,nch),2) 
  p_plot = round(tail(uni_mod$P_value,nch),2)
  
  OR_lab <- as.character(OR_plot)
  lower_lab <- as.character(lower_plot)
  upper_lab <- as.character(upper_plot)
  p_lab <- as.character(p_plot)
  
  inter_plot_quali<-
    data.frame(
      OR  = c(NA,NA,OR_plot),
      lower = c( NA,NA,lower_plot),
      upper = c(NA, NA,upper_plot), 
      p = c( NA,NA,p_plot))
  
  nb <- ifelse (nmin ==1, 2, nmin)
  N <- list()
  for (i in 2:nb) {
    N[[i]] <- c(levels(data[,var])[i], rep(NA, nmax-1))
    
  }
  
  nmin_mod <- ifelse(nmin == 1, 1, nmin-1)
  deter <- ifelse(nmin == 1, 1, 2)
  
  label <- cbind(c(rep(NA, deter),unlist(N)),
                 c(NA,pretty_y, rep(levels(data[,y]),nmin_mod)),
                 c(NA,NA, OR_lab),
                 c(NA,NA, lower_lab),
                 c(NA,NA,upper_lab),
                 c( NA,tail(round(pval, 2),1),rep(NA, nch)))
  
  # label <- cbind(c(NA,pretty_y, levels(data[,y])),
  #                c(NA,NA, OR_lab),
  #                c(NA,NA, lower_lab),
  #                c(NA,NA,upper_lab),
  #                c( NA,tail(round(pval,2),1), rep(NA, nch)))
  
  tibble(number = inter_plot_quali, label = label)
  
}

poss_inter_glm_fp = possibly(.f = forest_plot_glm, otherwise = NULL)


####################################################################################
############### TAblo des OR ##############################
############################################################

# table_plot_glm <- function(y, var, pred, pretty_y, data) {
#   #creation du glm
#   f<-as.formula(paste(pred,"~",y,":",var,"+", y))
#   mod <- glm(f, family = "binomial", data = data)
#   uni_mod <- glm_summary(mod)
#   
#   fp<-as.formula(paste(pred,"~",y,"*",var))
#   mod2 <- glm(fp, family = "binomial", data = data)
#   pval <- car::Anova(mod2)$`Pr(>Chisq)`
#   
#   
#   nmax <- length(levels(data[,y]))
#  # nmin <- length(levels(data[,var]))
#   nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
#   #nch = (nmax*nmin)-nmax
#   
#   nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
#   #   if (category == "quanti") {
#   # nch = (nmax*nmin)-nmax
#   # } else {
#   # nch = 2
#   #  }
#   
#   
#   chiffres <-  tail(uni_mod, nch)
#   intitule <-    data.frame(
#     Labels = c(pretty_y, levels(data[,y])),
#     Odds.Ratio  = c("",chiffres$Odds.Ratio),
#     IC_inf. = c( "",chiffres$IC_inf.),
#     IC_sup. = c( "",chiffres$IC_sup.), 
#     P_value = c( tail(round(car::Anova(mod2)$`Pr(>Chisq)`,2),1),round(chiffres$P_value,2)))
#   
#   return(intitule)
# }
#####################################################################################
############ FOrest plot glm multi #############################################
###########################################################################

forest_plot_glm_multi <- function(y, var, pred,  pretty_y, multi,data) {
  #creation du glm
  f<-as.formula(paste(pred,"~",y,":",var,"+", y,"+",multi))
  mod <- glm(f, family = "binomial", data = data)
  uni_mod <- glm_summary(mod)
  
  fp<-as.formula(paste(pred,"~",y,"*",var, "+", multi))
  mod2 <- glm(fp, family = "binomial", data = data)
  pval <- car::Anova(mod2, test.statistic="Wald")$`Pr(>Chisq)`
  
  nmax <- length(levels(data[,y]))
  #nmin <- length(levels(data[,var]))
  nmin<- ifelse(length(levels(data[,var]))==0, 1,length(levels(data[,var])))
  nch <- ifelse(is.numeric(data[,var]),nmax,(nmax*nmin)-nmax )
  


  
  OR_plot  = round(tail(uni_mod$Odds.Ratio,nch),2)
  lower_plot = round(tail(uni_mod$IC_inf.,nch),2)
  upper_plot = round(tail(uni_mod$IC_sup.,nch),2) 
  p_plot = round(tail(uni_mod$P_value,nch),2)
  
  OR_lab <- as.character(OR_plot)
  lower_lab <- as.character(lower_plot)
  upper_lab <- as.character(upper_plot)
  p_lab <- as.character(p_plot)
  
  inter_plot_quali<-
    data.frame(
      OR  = c(NA,NA,OR_plot),
      lower = c( NA,NA,lower_plot),
      upper = c(NA, NA,upper_plot), 
      p = c( NA,NA,p_plot))
  
  nb <- ifelse (nmin ==1, 2, nmin)
  N <- list()
  for (i in 2:nb) {
    N[[i]] <- c(levels(data[,var])[i], rep(NA, nmax-1))
    
  }
  
  nmin_mod <- ifelse(nmin == 1, 1, nmin-1)
  deter <- ifelse(nmin == 1, 1, 2)
  
  label <- cbind(c(rep(NA, deter),unlist(N)),
                 c(NA,pretty_y, rep(levels(data[,y]),nmin_mod)),
                 c(NA,NA, OR_lab),
                 c(NA,NA, lower_lab),
                 c(NA,NA,upper_lab),
                 c( NA,tail(round(pval, 2),1),rep(NA, nch)))
  
  # label <- cbind(c(NA,pretty_y, levels(data[,y])),
  #                c(NA,NA, OR_lab),
  #                c(NA,NA, lower_lab),
  #                c(NA,NA,upper_lab),
  #                c( NA,tail(round(pval,2),1), rep(NA, nch)))
  
  tibble(number = inter_plot_quali, label = label)
  
}
poss_inter_glm_fp_multi = possibly(.f = forest_plot_glm_multi, otherwise = NULL)



###############################################
## ##### Forest plot glm triople terme d'interaction###############
#############################################

forest_plot_glm_double <- function(y, var1,var2, pred,  pretty_var1, pretty_var2, multi, data) {
  #creation du glm
  if(missing(multi)) {
    f<-as.formula(paste(pred,"~",y,":",var1,":", var2,"+", var1, "+", var2, "+", var1, ":", var2))
    fp<-as.formula(paste(pred,"~",y,"*",var1, "*", var2))
    
  } else {
    f<-as.formula(paste(pred,"~",y,":",var1,":", var2,"+", var1, "+", var2, "+", var1, ":", var2,"+",multi))
    fp<-as.formula(paste(pred,"~",y,"*",var1, "*", var2,"+",multi))
  }
  
  
  
  mod <- glm(f, family = "binomial", data = data)
  uni_mod <- glm_summary(mod)
  
  mod2 <- glm(fp, family = "binomial", data = data)
  pval <- car::Anova(mod2, test.statistic="Wald")$`Pr(>Chisq)`
  
  levels_var1 <- length(levels(data[,var1]))
  levels_var2 <- length(levels(data[,var2]))
  levels_y <- length(levels(data[,y]))
  
  nch <- (levels_var1*levels_var2* levels_y)-(levels_var1*levels_var2)
  
  
  
  OR_plot  = round(tail(uni_mod$Odds.Ratio,nch),2)
  lower_plot = round(tail(uni_mod$IC_inf.,nch),2)
  upper_plot = round(tail(uni_mod$IC_sup.,nch),2) 
  #p_plot = round(tail(uni_mod$P_value,nch),2)
  
  OR_lab <- as.character(OR_plot)
  lower_lab <- as.character(lower_plot)
  upper_lab <- as.character(upper_plot)
  #p_lab <- as.character(p_plot)
  
  inter_plot_quali<-
    data.frame(
      OR  = c(NA,NA,OR_plot),
      lower = c( NA,NA,lower_plot),
      upper = c(NA, NA,upper_plot))
  
  
  
  deter <- nch+2
  label_inter <- glue(pretty_var1,"/",pretty_var2)
  label_level <- levels(interaction(data[,var1], data[,var2]))
  
  
  label <- cbind(c(rep(NA, deter)),
                 c(NA,label_inter, label_level),
                 c(NA,NA, OR_lab),
                 c(NA,NA, lower_lab),
                 c(NA,NA,upper_lab),
                 c( NA,tail(round(pval, 2),1),rep(NA, nch)))
  
  # label <- cbind(c(NA,pretty_y, levels(data[,y])),
  #                c(NA,NA, OR_lab),
  #                c(NA,NA, lower_lab),
  #                c(NA,NA,upper_lab),
  #                c( NA,tail(round(pval,2),1), rep(NA, nch)))
  
  tibble(number = inter_plot_quali, label = label)
  
}




#########################################################################################
###################### FOrest plot glm pour 2 terme d'interamction ##############
#########################################################################################
forest_plot_glm_2ti <- function (y, var1, var2, pred, data, multi){
  if(missing(multi)) {
    f<-as.formula(paste(pred,"~",y,"*",var1,"+", y,"*", var2))
    
  } else {
    f<-as.formula(paste(pred,"~",y,"*",var1,"+", y,"*", var2, "+", multi))
  }
  
  mod <- glm(f, family = "binomial", data = data)
  pval <- car::Anova(mod, test.statistic="Wald")$`Pr(>Chisq)`
  co <- coef(mod)
  vc <- vcov(mod)
  
  principal_terms <- as.list(c(y, var1, var2))
  principal_terms2 <- map(principal_terms, 	~ mod$xlevels[[.]][-1])
  principal_terms3 <- map2(principal_terms, principal_terms2, paste0)
  principal_terms4 <- map(principal_terms, 	~ mod$xlevels[[.]][-2])
  #inter_terms <- paste(unlist(principal_terms3), collapse = ":")
  inter_terms1 <- paste(principal_terms3[[1]], principal_terms3[[2]], sep = ":")
  inter_terms2 <- paste(principal_terms3[[1]], principal_terms3[[3]], sep = ":")
  
  tab <- cross_df(list("terms" = unlist(principal_terms3), "inter1" = inter_terms1,"inter2" = inter_terms2 )) 
  
  
  jojo_le_genie <- tab %>%  slice(1) %>% mutate(
    Odds.Ratio =   sum(co[c(.$terms, .$inter1,.$inter2)]),
    se =   sqrt(sum(vc[c(.$terms, .$inter1,.$inter2), c(.$terms, .$inter1,.$inter2)])),
    IC_inf. = Odds.Ratio - 1.96 * se,
    IC_sup. = Odds.Ratio + 1.96 * se,
    se = NULL) %>% mutate_if(is.numeric, exp) %>% mutate(variable = do.call(glue,principal_terms2))
  
  vava_le_genie <- tab %>% slice(1) %>% 
    mutate(
      Odds.Ratio =   sum(co[c(tab$terms, tab$inter1)]),
      se =   sqrt(sum(vc[c(tab$terms, .$inter1), c(tab$terms, tab$inter1)])),
      IC_inf. = Odds.Ratio - 1.96 * se,
      IC_sup. = Odds.Ratio + 1.96 * se,
      se = NULL
    ) %>% mutate_if(is.numeric, exp)%>% mutate(variable = glue(principal_terms2[[1]],principal_terms2[[2]],principal_terms4[[3]] ))
  
  le_genie <- tab %>% slice(1) %>% 
    mutate(
      Odds.Ratio =   sum(co[c(.$terms, .$inter2)]),
      se =   sqrt(sum(vc[c(.$terms, .$inter2), c(.$terms, .$inter2)])),
      IC_inf. = Odds.Ratio - 1.96 * se,
      IC_sup. = Odds.Ratio + 1.96 * se,
      se = NULL
    ) %>% mutate_if(is.numeric, exp) %>% mutate(variable = glue(principal_terms2[[1]],principal_terms4[[2]],principal_terms2[[3]] ))
  
  var_debut <- glm_summary (mod)  [1,1:3]   %>% mutate(variable = glue(principal_terms2[[1]],principal_terms4[[2]],principal_terms4[[3]]))                                       
  
  tab_OR <- rbind(jojo_le_genie[,c(4,  6:8)], vava_le_genie[,c(4,  6:8)], le_genie[,c(4,  6:8)], var_debut)
  
  
  return(tab_OR)
}


