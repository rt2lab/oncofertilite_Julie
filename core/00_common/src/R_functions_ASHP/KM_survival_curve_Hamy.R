library("survminer")
library("survival")
library(ggsci)
library("scales")
palette_jco  <- c(pal_jco("default")(10))
#  [1] "#0073C2FF" "#EFC000FF" "#868686FF" "#CD534CFF" "#7AA6DCFF" "#003C67FF" "#8F7700FF" "#3B3B3BFF" "#A73030FF" "#4A6990FF"

 			i=1
 			categ             <- levels(as.factor(dataf[,quali[i]]))
      # fit               <- survfit(Surv( dataf[, del] , dataf[, ev] )	~ dataf[ ,quali[i]   ], na.action=na.omit, data = dataf)
 			fit               <- survfit(Surv( dataf[, del] , dataf[, ev] )	~ dataf[ ,quali[i]   ], na.action=na.omit)
 			
      lr                <- survdiff(Surv(dataf[, del], dataf[, ev])	~ dataf[,quali[i]],  na.action=na.omit)
      pval_log_rank     <- 1 - pchisq(lr$chisq, length(lr$n) - 1)
      
      if(pval_log_rank <0.001) {
                                pval_log_rank_JAMA  <- "p<0.001"
                                } 
      
      if( 0.001 <= pval_log_rank &  pval_log_rank < 0.01 ) {
                               pval_log_rank_JAMA  <- paste0("p=",round(pval_log_rank,3)    )
                               } 

      if( pval_log_rank >=0.01) {
                                  pval_log_rank_JAMA  <- paste0("p=",round(pval_log_rank,2)   )
                                } 
      
      pval_log_rank_JAMA  <-    gsub("0\\.","\\ .",pval_log_rank_JAMA)
      pval_log_rank_JAMA  <-    gsub("p<","P <",pval_log_rank_JAMA)
      pval_log_rank_JAMA  <-    gsub("p=","P =",pval_log_rank_JAMA)
      
  if  (color_plot_ASHP=="couleur" ) {
    
              p  <- ggsurvplot(fit,data=dataf,
                               title = titre_var_surv2 ,      
                              palette = palette_jco[1:length(categ)],
                              xlim=x_lim,
                              legend.title = nomquali[i], 
                              legend.labs = categ ,
                              legend = loc_legend ,
                              pval.coord = pval_coord,
                              pval.size = pval_size, 
                              surv.median.line = plot_median_dfs ,
                              risk.table = TRUE , 
                              risk.table.fontsize = 4,
                              tables.y.text=FALSE,
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
      

