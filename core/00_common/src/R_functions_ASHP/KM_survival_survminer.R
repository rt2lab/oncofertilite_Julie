library("survminer")
library("survival")
library(ggsci)
library("scales")
palette_jco  <- c(pal_jco("default")(10))
#  [1] "#0073C2FF" "#EFC000FF" "#868686FF" "#CD534CFF" "#7AA6DCFF" "#003C67FF" "#8F7700FF" "#3B3B3BFF" "#A73030FF" "#4A6990FF"



# A définir : copier coller et remplacer
 
# 			del       <- c("delDFS2.m")
# 			ev        <- c("DFS")
#    		dataf     <- neo_embols
# 			quali     <- c("RCH3.f")
# 			nomquali  <- c("pCR")
# 			title_curve <- "whole population"
# # 		soustitre <- "(No CT population)"
#  			titre_var_surv      <- "DFS"
#  			color_plot_ASHP     <- "BW"          			# Par défaut couleur;  
#  			color_plot_ASHP     <- "couleur"          # sinon noir et blanc
 			
 			i=1
 			categ             <- levels(as.factor(dataf[,quali[i]]))
      fit               <- survfit(Surv( dataf[, del] , dataf[, ev] )	~ dataf[ ,quali[i]   ], na.action=na.omit, data = dataf)
      # fit               <- survfit(Surv( dataf[, del] , dataf[, ev] )	~ dataf[ ,quali[i]   ], na.action=na.omit)
      
  if (color_plot_ASHP=="BW") { 
                        p  <- ggsurvplot(fit = fit, data=dataf,
                                          title = titre_var_surv2 ,                        
                                          pval = TRUE,
                                          linetype=c(1:length(categ)  ),  size = 0.5, censor=FALSE,
                                          palette = rep("black", length(categ) ),
                                          xlim=x_lim,
                                           break.time.by = breaks_ASHP,
                                           pval.coord = pval_coord,
                                           pval.size = pval_size, 
                                         # Legend
                                          # legend = loc_legend ,
                                          legend.title = nomquali[i], 
                                          legend.labs = categ, font.legend = 10 ,
                                         # risk table
                                           risk.table = TRUE ,
                                           risk.table.fontsize = 4,
                                            surv.median.line = plot_median_dfs ) # add the median survival pointer.
                        objects(p)
                        
                                          # To uncomment for single curve
                                          p$plot <- p$plot + ylab("")
                                          # To uncomment for single curve
                                          p$plot <- p$plot + theme(plot.title = element_text(hjust = 0.5, size=12, face='bold') ) 
                                          p$table[["theme"]][["axis.line"]][["colour"]]     <- "white"
                                          p$table[["theme"]][["axis.ticks"]][["colour"]]    <- "white"
                                          p$table[["theme"]][["axis.text.x"]][["colour"]]   <- "white"
                                          p$table[["labels"]][["y"]]                        <- " "
                                          p$table[["labels"]][["x"]]                        <- " "
                                          p$table[["labels"]][["title"]]                    <- " "
                                          # p$table[["labels"]][["title"]]                    <- "number at risk"
                                  } 

  if  (color_plot_ASHP=="couleur" ) {
              p  <- ggsurvplot(fit,data=dataf,
                               title = titre_var_surv2 ,      
                               palette = palette_jco[1:length(categ)],
                              pval = TRUE,
                              # legend = c(0.6, 0.25),  
                              legend.title = nomquali[i], 
                              legend.labs = categ ,
                              surv.median.line = plot_median_dfs ,
                              risk.table = TRUE , 
                              risk.table.fontsize = 4,
                              font.legend = 10 , size = 0.8, censor=TRUE  )
    
                              p$plot <- p$plot + theme(plot.title = element_text(hjust = 0.5))
                              
                              p$table[["theme"]][["axis.line"]][["colour"]]     <- "white"
                              p$table[["theme"]][["axis.ticks"]][["colour"]]    <- "white"
                              p$table[["theme"]][["axis.text.x"]][["colour"]]   <- "white"
                              p$table[["labels"]][["y"]]                        <- " "
                              p$table[["labels"]][["x"]]                        <- " "
                              p$table[["labels"]][["title"]]                    <- "number at risk"
                                    }

      print(p)
      return(p)
      
#           lr      <- survdiff(Surv(dataf[, del], dataf[, ev])	~ dataf[,quali[i]],  na.action=na.omit)
#           cox     <- coxph( Surv(dataf[, del], dataf[, ev]) ~dataf[,quali[i]], na.action=na.omit)
#           ICinf   <- exp(cox$coefficient-1.96*sqrt(diag(cox$var)))
#           ICsup   <- exp(cox$coefficient+1.96*sqrt(diag(cox$var)))
#           IC      <- format(round(cbind(ICinf, ICsup), 2))
#           ic      <- paste("[", IC[, 1], " ; ", IC[, 2], "]", sep="")
#           scox    <- as.data.frame(summary(cox)$coef)
#           scox$IC <- ic
#           rr      <- round(scox[,2],3)
# # Calcul du p du log rank qui figurera sur la figure 
#           pv        <- 1-pchisq(lr$chisq,df=length(lr$n)-1)                                          
#           pval      <- ifelse(pv<0.001,"<0.001",round(pv,2))
#           pval      <- ifelse(pv<0.01 & pv>=0.001,round(pv,3),pval)
#           n_par_categ <- fit$n
# 
# # Création de la légende, 
#         legend_categ_1 <- paste0(categ[1], "      HR=1")
#         legend_categ_2 <- paste0(categ[2],"       HR=", round(rr[1],2)," ", ic[1]     )
#         legend_categ_3 <- ifelse(!is.na(categ[3]),(paste0(categ[3]," HR=", round(rr[2],2)," ", ic[2]  )),NA)
#         legend_categ_3 <- legend_categ_3[which(!is.na(legend_categ_3))]
#         legend_categ_4 <- ifelse(!is.na(categ[4]),(paste0(categ[4]," HR=", round(rr[3],2) , " ", ic[3]      )),NA)
#         legend_categ_4 <- legend_categ_4[which(!is.na(legend_categ_4))]
#         legend_fig     <- c(legend_categ_1,legend_categ_2,legend_categ_3,legend_categ_4)										
# 
#         legend_fig_tab  <- as.data.frame(rbind(legend_categ_1,legend_categ_2,legend_categ_3,legend_categ_4))										
#         
#     #     Reste :
#     #     Mettre le titre
#     #     Incorporer la legende
#     #     Réussir à mettre les risques relatifs
#         
        
        
        
        
        
