# Définir son modèle de cox, le rentrer, ainsi que les variables
# et le chemin d'accès pour enregistrer

# cox <- coxph(Surv(time_months,status)~ metagene_1_ASHP_LH+lymph_nodes_positive_cl.f, data=base_def_her2[which(base_def_her2$ER_cl=="0"),], method="breslow")
# tmp_var			<-c("metagene_1_ASHP_LH","metagene_1_ASHP_LH","lymph_nodes_positive_cl.f","lymph_nodes_positive_cl.f")
									 	tmp_tab 		<- NULL
									 	tmp_tab_1 		<- NULL
									 	tmp_tab_2 		<- NULL
										tmp_vect_1		<- c("1","-","-")
										tmp_summary		<- NULL
										tmp_levels		<-unlist(cox$xlevels)								
										
		for (k in 1:length(tmp_var)) {						# Here, 2
								# k = 1
									print(tmp_var[k])				# combined_prolif_Immunity
									tmp_levels_var_k					<- cox$xlevels[[tmp_var[k]]]
										# [1] "Prolif high /Immunity high" "Prolif high /Immunity low"  "Prolif low /Immunity high"  "Prolif low /Immunity low"  
									tmp_summary 						<- summary(cox)
									tmp_position_tmp_var_in_cox 	 	<- which(names(cox$xlevels)==tmp_var[k])			# Attention, k # position
									tmp_pos_levels						<- grep(tmp_var[k],names(tmp_levels))
									tmp_names_levels					<- tmp_levels[tmp_pos_levels]

									tmp_coef_et_IC_all 					<- tmp_summary$conf.int
									tmp_coef_et_IC_all_var_k			<- tmp_coef_et_IC_all[grep(tmp_var[k],names(cox$coefficients)),]
                  
									class(tmp_coef_et_IC_all_var_k)
                    if(length(tmp_levels_var_k)==2) {
                                                    tmp_coef_et_IC_all_var_k <- t(as.matrix(tmp_coef_et_IC_all_var_k))                      
                                                    }
									HR				<- round(tmp_coef_et_IC_all_var_k[,1],2)
									IC 				<- paste("[", round(tmp_coef_et_IC_all_var_k[,3],2),"-",round(tmp_coef_et_IC_all_var_k[,4],2),"]")
								
						
									tmp_pvals_all 		<- round(coef(tmp_summary)[,5],3)
								    tmp_pvals_all 		<- ifelse(tmp_pvals_all < 0.001, "<0.001", round(tmp_pvals_all, 3))	
									tmp_pvals_var_k	<- tmp_pvals_all[grep(tmp_var[k],names(cox$coefficients))]
												
									tmp_tab_1				<- cbind(HR,IC,tmp_pvals_var_k)
									tmp_tab_2				<- rbind( tmp_vect_1,tmp_tab_1)
# 									rownames(tmp_tab_2)[1] 	<- c(unname(tmp_names_levels[1]))
									rownames(tmp_tab_2) 	<- c(unname(tmp_names_levels))
																			
									tmp_tab					<- rbind(tmp_tab,tmp_tab_2)
										}	
																			
#									write.table(tmp_tab, file="Results/All/Multivariate analysis on DFS, Whole population",row.names=FALSE)

	