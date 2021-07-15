# On source la fonction de Frédérique

# 	dataf<- 
#	del <- c("delai_dfs")
#	ev <-c("etat_dfs")
#	quali 

HR <- NULL
IClow <- NULL
ICup <- NULL
IC <- NULL
tab <- NULL
tabf <- NULL
						
  for (i in 1:length(quali)) {
  # i=2
		print(quali[i])
			tmp_name_variable <- quali[i]
	    	tmp <- coxph(Surv(dataf[, del], dataf[, ev]) ~ dataf[,quali[i]])
  			cox_p_model 			<- round( summary(tmp)$sctest[3],3)							# relabellisé p_global par  ASHP ; C'est celui selon le test de Wald
  			cox_p_coef				<- round( summary(tmp)$coeff[,'Pr(>|z|)'],3)				# rajouté par ASHP ; C'est le p  de chaque variable
      HR <-  round(summary(tmp)$conf.int[,1],2)
      IClow <- round(summary(tmp)$conf.int[,3],2)
      ICup <- round(summary(tmp)$conf.int[,4],2)
      IC <- paste("[",round(IClow,2),"-",round(ICup,2),"]")
      tab <- cbind(tmp_name_variable, rownames(summary(tmp)$coeff), HR, IC, cox_p_coef, cox_p_model)
     			 #tab <- c(evt, quali[j], p, RR, IC)
      tab2 <-rbind (c("","","1","-","-","-"),tab)
      tabf <- rbind(tabf, tab2)
  }

tabf

