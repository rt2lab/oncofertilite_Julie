# On source la fonction de Frédérique


#data<- 
#del <- c("delai_dfs")
#ev <-c("etat_dfs")
#quali ...
pvalues <- NULL
RR <- NULL
IClow <- NULL
ICup <- NULL
IC <- NULL
tab <- NULL
tabf <- NULL
k <- 0
for(d in del){
      k <- k+1
      evt <- ev[k]
      j <- 0
  for (i in quali) {
      j <- j+1
      cat(j, i, "\n")
      tmp <- coxph(Surv(data[, d], data[, evt]) ~ data[,i])
  		p_global <- round( summary(tmp)$sctest[3],3)				# relabellisé p_global par  ASHP
  		p <- round( summary(tmp)$coeff[,'Pr(>|z|)'],3)				# rajouté par ASHP
      RR <-  round(summary(tmp)$conf.int[,1],2)
      IClow <- round(summary(tmp)$conf.int[,3],2)
      ICup <- round(summary(tmp)$conf.int[,4],2)
      IC <- paste("[",round(IClow,2),"-",round(ICup,2),"]")
      tab <- cbind( rownames(summary(tmp)$coeff), RR, IC, p, p_global)
      #tab <- c(evt, quali[j], p, RR, IC)
      tab2 <-rbind (c("","1","-","-","-"),tab)
      tabf <- rbind(tabf, tab2)
  }
}
tabf


### fonction originale
   quali[i]
      j <- j+1
      cat(j, i, "\n")
      tmp <- coxph(Surv(data[, d], data[, evt]) ~ data[,i])
      p <-  summary(tmp)$sctest[3]
      RR <-  round(summary(tmp)$conf.int[,1],2)
      IClow <- round(summary(tmp)$conf.int[,3],2)
      ICup <- round(summary(tmp)$conf.int[,4],2)
      IC <- paste("[",round(IClow,2),"-",round(ICup,2),"]")
      tab <- c(evt, quali[j], p, RR, IC)
      tabf <- rbind(tabf, tab)