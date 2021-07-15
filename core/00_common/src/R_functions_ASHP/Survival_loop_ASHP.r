
							
	# del     <-c("time_months")
# 	ev      <-c("status")
# 	dataf    <-base_def_her 
# 	quali <-c  ( "menopausal_status_inferred","metagene_1_ASHP_LH","age_at_diagnosis.c.f")
# 	

	tab <- NULL
	tmp_tab <- NULL

			for (i in 1:length(quali)){
										print(i)
										km	<-survfit(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], data=dataf, na.action=na.omit)
										lr 	<-survdiff(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], data=dataf, na.action=na.omit)
										cox	<-coxph(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], data=dataf, na.action=na.omit)
									
										tmp_levels		<-unlist(cox$xlevels)
										tmp_levels		<-unname(tmp_levels)
										tmp_var			<-rep(quali[i],length(tmp_levels))
										tmp_effectifs	<-unlist(lr$n)
										tmp_effectifs	<-unname(tmp_effectifs)
										tmp_events		<-lr$obs
										tmp_summary  	<-summary(cox)
										
										tmp_coef_et_IC <-tmp_summary$conf.int
										HR				<- round(tmp_coef_et_IC[,1],3)
										HR				<- c("1", HR)
										IC 				<- paste("[", round(tmp_coef_et_IC[,3],3),"-",round(tmp_coef_et_IC[,4],3),"]")
										IC				<-c("-", IC)										
										tmp_pvals 		<-round(coef(tmp_summary)[,5],3)
										pval			<-c("-",tmp_pvals)
										tmp_tab			<-cbind(tmp_var,tmp_levels,tmp_effectifs,tmp_events,HR,IC,pval)
										tab				<-rbind(tab,tmp_tab)
										}
							
	