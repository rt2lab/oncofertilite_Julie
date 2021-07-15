# 								variable_to_compare			<- c( "ESR1","PR","ERBB2","prolif","immunity")
# 								variable_to_compare_name	<- variable_to_compare 
# 								dataf						<- clinical_database
# 								variables_to_test			<- c("LST_status")
# 								names_variables_to_test		<- variables_to_test

		tab_reg_linear		<- NULL

		for (k in 1:length(variable_to_compare)	) { # debut boucle variable_to_compare
							# k=1			
		  print(k)
							tmp_var_to_compare			<- variable_to_compare[k]
							tmp_name_var_to_compare		<- variable_to_compare_name[k]
							print(k)				# ESR1												
							print(tmp_name_var_to_compare)				# ESR1												
	
								for (i in 1:length(variables_to_test)	) { # debut boucle variables_to_test
														# i=35
								            print(i)
														tmp_var 	<- variables_to_test[i]
														print(tmp_var)
														tmp_name 	<- names_variables_to_test[i]
														print(tmp_name)		# LST_status 
														tmp_mat		<- dataf[which(!is.na(dataf[,tmp_var_to_compare]) & !is.na(dataf[,tmp_var])    ),c(tmp_var,tmp_var_to_compare)]
														tmp_mat[,tmp_var_to_compare]   <- as.numeric(tmp_mat[,tmp_var_to_compare]) 
														
														tab <- table(tmp_mat[,tmp_var])	
														
							if( 	!any(tab==0)	&  length(rownames(tab))>1 )	{ # debut boucle si tab pas null
														tmp_mod	 	<- lm(tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var])   )
														droplevels(as.factor(tmp_mat[,tmp_var]))
														class(tmp_mat[,tmp_var_to_compare])
														tmp_summary					<- summary(tmp_mod)		#
														tmp_anova					  <- anova(tmp_mod) 
														tmp_p_global				<- tmp_anova["Pr(>F)"]
														tmp_p_global				<- tmp_p_global[which(!is.na(tmp_p_global)	),	]	
														tmp_level					  <- rownames(tmp_summary$coefficients)	
														tmp_number					<- unlist(unname(tab))
		
														class(tmp_mat[,tmp_var_to_compare])
														tmp_summary					<- summary(tmp_mod)		#
												#		tmp_anova					  <- anova(tmp_mod) 
					if (length(tmp_p_global)>0) {   # debut boucle si tmp_p_global existe
					  
											if (  tmp_p_global <= 0.05 & length(tmp_level) >2 ) {

 														tmp_mod2	 				  <- aov(tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var]))
														tmp_anova2					<- TukeyHSD(tmp_mod2)
														# Make post hoc analysis with Tukey
														tmp_val_tukey				<- round(tmp_anova2[[1]][,"p adj"],3)
														str(tmp_val_tukey) 				
														tmp_tab_tukey_2				<- data.frame(	comp_classes	= names(tmp_val_tukey[])	,
																									pval			= tmp_val_tukey				)														
														comp_class					<- as.character(tmp_tab_tukey_2[,"comp_classes"])
														pval_tukey					<- as.character(tmp_tab_tukey_2[,"pval"])
														
														  # Beware, run tukey manually if 4 levels or more !
														if(length(tmp_level) >3  ){
                          														comp_class					<- rep("run Tukey manually",length(tmp_level)	)
                          														pval_tukey					<- rep("run Tukey manually",length(tmp_level)	)														
                          														} 
														
																								} else 		{
																											comp_class					<- rep("",length(tmp_level)	)
																											pval_tukey					<- rep("",length(tmp_level)	)														
																											}
																								
														tmp_variable	      			<- rep(tmp_name,length(tmp_level)	)
														tmp_variable_unique	      <- c(tmp_name,rep("", (length(tmp_level)-1)	)	)	
														tmp_global_p_val		    	<- rep(round(tmp_p_global,3),length(tmp_level)	)
														tmp_global_p_val_unique		<- c(round(tmp_p_global,3),rep("", (length(tmp_level)-1)	)	)	
														
														tmp_value					<- tmp_summary$coefficients[,1]
														tmp_cum_value				<- tmp_value + tmp_value[1]
														tmp_cum_value[1]			<- tmp_value[1]
														names(tmp_cum_value)		<- tmp_mod$xlevels[[1]]
														tmp_level					<- tmp_mod$xlevels[[1]]																																
														tmp_pval					<- tmp_summary$coefficients[,4]
                            
														
														# Here if we want to change the round for the mean !!
														titi 		<- rbind(tmp_variable,tmp_variable_unique,tmp_level,tmp_number,round(tmp_cum_value,1),round(tmp_pval,3) ,tmp_global_p_val,tmp_global_p_val_unique	, comp_class, pval_tukey)
														tata		<- t(titi)
														tata[1,6]	<-	"reference class"		
# 														tata[2:length(tmp_level),c("tmp_variable","tmp_global_p_val")]		<- ""

														
														if( 	any(tab<30)	 )	{
														  tmp_kruskal          <- kruskal.test(  tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var])   )
														  tmp_kruskal_pval     <- round(tmp_kruskal$p.value,3)
														                         }
														if( 	!any(tab<30)	& length(rownames(tab))>1   )	{
														  
														  tmp_kruskal_pval    <- ""
			                        											}
# Je viens de le rajouter la    														
					                            }    # fin boucle si tmp_p_global existe 

														tutu						<- cbind(   matrix(tmp_var,nrow=length(tmp_variable), ncol=1 )	, matrix(tmp_name_var_to_compare,nrow=length(tmp_variable), ncol=1 )) 	
														toto						<- cbind(tutu,tata)
														toto            <- as.data.frame(toto)
														toto$p_kruskall   <- rep(tmp_kruskal_pval,nrow(toto))
														# colnames(toto)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels","number_per_level","mean_value","pvalue vs ref class (t test)","pvalue ANOVA","pvalue ANOVA2","post-hoc comp class","p val Tukey","p_Kruskal")
														colnames(toto)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels","number_per_level","mean_value","pvalue_vs_ref_class_t_test","pvalue_ANOVA","pvalue_ANOVA2","post_hoc_comp_class","p_val_Tukey","p_Kruskal")
														
														tab_reg_linear				<- rbind(tab_reg_linear, toto)

														# Rajouté ici
								colnames(tab_reg_linear)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels","number_per_level","mean_value","pvalue_vs_ref_class_t_test","pvalue_ANOVA","pvalue_ANOVA2","post_hoc_comp_class","p_val_Tukey","p_Kruskal")
														
																												
										n 			<- table(tmp_mat[,tmp_var]) 					
										ylim		<- range(dataf[,tmp_var_to_compare])
										colnames(tmp_mat)[which(colnames(tmp_mat)==tmp_var)]				<- "tmp_var"
										colnames(tmp_mat)[which(colnames(tmp_mat)==tmp_var_to_compare)]	<- "tmp_var_to_compare"

															}     # fin boucle si tab pas null
															
														}				# fin boucle variables_to_test
												}				 # fin  boucle variable_to_compare
														# dev.off()
														
					# colnames(tab_reg_linear)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels","number_per_level","mean_value","pvalue vs ref class (t test)","pvalue ANOVA","pvalue ANOVA2","post-hoc comp class","p val Tukey","p_Kruskal")
		
		
		if( !is.null(tab_reg_linear)) {
		  tab_reg_linear[which(tab_reg_linear[,"pvalue_vs_ref_class_t_test"]=="0"),"pvalue_vs_ref_class_t_test"]  <- "<0.01"
		  tab_reg_linear[which(tab_reg_linear[,"pvalue_ANOVA"]=="0"),"pvalue_ANOVA"]  <- "<0.001"
		  tab_reg_linear[which(tab_reg_linear[,"pvalue_ANOVA2"]=="0"),"pvalue_ANOVA2"]  <- "<0.001"
		  tab_reg_linear[which(tab_reg_linear[,"p_val_Tukey"]=="0"),"p_val_Tukey"]  <- "<0.001"
		}
		
		# Pimp table
		head(tab_reg_linear)
		tab_reg_linear <- tab_reg_linear %>% 
		            mutate(test = ifelse(p_Kruskal != "", "Kruskal","ANOVA" ),
                      pval_global_final = ifelse(p_Kruskal != "", p_Kruskal,pvalue_ANOVA) ,
                      pval_global_clean = case_when(var_qual_unique != "" ~ pval_global_final,
                                                    var_qual_unique == "" ~ "") , 
                      test_clean = case_when(var_qual_unique != "" ~ test,
                                             var_qual_unique == "" ~ ""),
                      pvalue_vs_ref_class_clean = case_when(test == "ANOVA" ~ pvalue_vs_ref_class_t_test,
                                                            test != "ANOVA"  ~ ""),
		                   p_signif = ifelse(str_detect("0.0",pval_global_clean),"signif","") )   
		tab_reg_linear[which(is.na(tab_reg_linear$p_signif)),"p_signif"] <- ""
		head(tab_reg_linear)

		tab_reg_linear_long <- tab_reg_linear
		tab_reg_linear_short <- tab_reg_linear %>% select(var_qual_unique,levels,number_per_level,mean_value,
		                                                  test_clean,pval_global_clean,p_signif,pvalue_vs_ref_class_clean,
		                                                  post_hoc_comp_class,p_val_Tukey)
		tab_reg_linear_short_clean <- tab_reg_linear_short %>% 
		                        dplyr :: rename( variable = var_qual_unique ,
		                               levels = levels,
		                               n = number_per_level,
		                               mean = mean_value,
		                               test = test_clean,
		                               pval = pval_global_clean,
		                                p_vs_ref = pvalue_vs_ref_class_clean  )
		head(tab_reg_linear_short_clean)
