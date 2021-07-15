# Définir son modèle de cox, le rentrer, ainsi que les variables
# et le chemin d'accès pour enregistrer

# cox <- coxph(Surv(time_months,status)~ metagene_1_ASHP_LH+lymph_nodes_positive_cl.f, data=base_def_her2[which(base_def_her2$ER_cl=="0"),], method="breslow")
# tmp_var			<-c("metagene_1_ASHP_LH","metagene_1_ASHP_LH","lymph_nodes_positive_cl.f","lymph_nodes_positive_cl.f")
									 	tmp_tab <- NULL
										tmp_levels		<-unlist(cox$xlevels)
										tmp_levels		<-unname(tmp_levels)
										tmp_summary  	<-summary(cox)
										
										tmp_coef_et_IC <-tmp_summary$conf.int
										HR				<- round(tmp_coef_et_IC[,1],2)
										IC 				<- paste("[", round(tmp_coef_et_IC[,3],2),"-",round(tmp_coef_et_IC[,4],2),"]")
										tmp_pvals 		<-round(coef(tmp_summary)[,5],3)
										tmp_tab			<-cbind(tmp_var,HR,IC,tmp_pvals)
#									write.table(tmp_tab, file="Results/All/Multivariate analysis on DFS, Whole population",row.names=FALSE)

	