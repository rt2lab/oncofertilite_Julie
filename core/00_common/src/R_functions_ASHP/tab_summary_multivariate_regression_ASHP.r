# on définit le modèle multivarié final
# 							mod_def 			<- glm(rch.f ~ ER_status + metagene_LH_cl1_genes +	Prechemo_T_status.f, data=Igna_def, family="binomial")
								
															sum_mod_def 		<- summary(mod_def)
															IC					<- round(exp(confint(mod_def)),2)															
															OR					<- round(exp(coef(mod_def)),2)
															IC_en_forme 		<- paste("[",IC[,1],"-", IC[,2],"]")
															pval 				<- round(coef(sum_mod_def)[,4],3)

								for (k in 2:length(pval))		{
																pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3))
																}					

															tmp_tab				<-rbind(OR,IC_en_forme,pval)
															tab 				<- t(tmp_tab)
		