# On a mis as.factor : as.factor(dataf[ ,quali[i]])
							
	# del     <-c("time_months")
# 	ev      <-c("status")
# 	dataf    <-base_def_her 
# 	quali <-c  ( "menopausal_status_inferred","metagene_1_ASHP_LH","age_at_diagnosis.c.f")
# 	

								

# 	tab <- NULL
# 	tmp_tab <- NULL

										# cox <- coxph(Surv(time_months,status)~ metagene_1_ASHP_LH+lymph_nodes_positive_cl.f, data=base_def_her2[which(base_def_her2$ER_cl=="0"),], method="breslow")
										cox <- coxph(Surv(time_months,status)~ metagene_1_ASHP_LH+lymph_nodes_positive_cl.f + size_cl.f, data=base_def_her2[which(base_def_her2$chimio_2cl=="NON_CT"),], method="breslow")

										cox <- coxph(Surv(time_months,status)~ metagene_1_ASHP_LH+lymph_nodes_positive_cl.f, data=base_def_her, method="breslow")
									
										tmp_levels		<-unlist(cox$xlevels)
										tmp_levels		<-unname(tmp_levels)
										tmp_var			<-c("metagene_1_ASHP_LH","metagene_1_ASHP_LH","lymph_nodes_positive_cl.f","lymph_nodes_positive_cl.f")
										tmp_summary  	<-summary(cox)
										
										tmp_coef_et_IC <-tmp_summary$conf.int
										HR				<- round(tmp_coef_et_IC[,1],3)
										IC 				<- paste("[", round(tmp_coef_et_IC[,3],3),"-",round(tmp_coef_et_IC[,4],3),"]")
										tmp_pvals 		<-round(coef(tmp_summary)[,5],3)
										tmp_tab			<-cbind(tmp_var,tmp_levels,HR,IC,tmp_pvals)
											# write.table(tmp_tab, file="Results/All/Multivariate analysis on DFS, Whole population",row.names=FALSE)
											write.table(tmp_tab, file="Results/All/Multivariate analysis on DFS, no CT population",row.names=FALSE)

	