
							
del     <-c("time_months")
ev      <-c("status")
dataf    <-base_def_her 
 	quali <-c  ( "metagene_1_ASHP_LH","metagene_2_ASHP_LH","metagene_3_ASHP_LH") # ,"lymph_nodes_positive_cl.f")
# 	
head(dataf)
dataf[,eval(parse(text=formule))]
 c1 <- coxph(Surv(base_def_her[,"time_months"],base_def_her[,"status"])~ base_def_her[,"metagene_1_ASHP_LH"]+ base_def_her[,"metagene_2_ASHP_LH"],  method="breslow")
class(base_def_her[,"metagene_1_ASHP_LH"])

formule  			<-NULL
tmp_formule			<-NULL
tmp_formule_finale	<-NULL

			for (i in 1:length(quali)) {										
										print(i)
										tmp_formule 		<- paste0("dataf","[,","'",quali[i],"'","]")
										tmp_formule_finale	<- paste(c(tmp_formule_finale,tmp_formule),collapse=" + ") 
										}


		
class( dataf[,'metagene_2_ASHP_LH'])

cox <- coxph(Surv(time_months,status)~ dataf[,quali[1]]+ dataf[,quali[2]] , data=dataf, method="breslow")
class(dataf[,quali[2]])

cox <- coxph(Surv(time_months,status)~ dataf[,quali[1]]+ dataf[,quali[2]] , data=dataf, method="breslow")



head(dataf)
cox <- coxph(Surv(dataf[,del],dataf[,ev])~ eval(parse(text=tmp_formule_finale)), method="breslow")

str(tmp_formule_finale)
cox <- coxph(Surv(dataf[,del],dataf[,ev])~ dataf[,tmp_formule_finale], method="breslow")


summary(c1)

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
							
	
	
	