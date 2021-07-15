# 								variable_to_compare			<- c( "ESR1","PR","ERBB2","prolif","immunity")
# 								variable_to_compare_name	<- variable_to_compare 
# 								dataf						<- clinical_database
# 								variables_to_test			<- c("LST_status")
# 								names_variables_to_test		<- variables_to_test


		tab_reg_linear		<- NULL

		for (k in 1:length(variable_to_compare)	) {
							# k=1							
							tmp_var_to_compare			<- variable_to_compare[k]
							tmp_name_var_to_compare		<- variable_to_compare_name[k]
							print(k)				# ESR1												
							print(tmp_name_var_to_compare)				# ESR1												
	
								for (i in 1:length(variables_to_test)	) {
														# i=27
														tmp_var 	<- variables_to_test[i]
														print(tmp_var)
														tmp_name 	<- names_variables_to_test[i]
														print(tmp_name)		# LST_status 
														tmp_mat		<- dataf[which(!is.na(dataf[,tmp_var_to_compare]) & !is.na(dataf[,tmp_var])    ),c(tmp_var,tmp_var_to_compare)]
														tmp_mat[,tmp_var_to_compare]   <- as.numeric(tmp_mat[,tmp_var_to_compare]) 
														
														tab <- table(tmp_mat[,tmp_var])	
														
							if( 	!any(tab==0)	 )	{
														tmp_mod	 	<- lm(tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var]))
														class(tmp_mat[,tmp_var_to_compare])
														tmp_summary					<- summary(tmp_mod)		#
														tmp_anova					<- anova(tmp_mod) 
														tmp_p_global				<- tmp_anova["Pr(>F)"]
														tmp_p_global				<- tmp_p_global[which(!is.na(tmp_p_global)	),	]	
														tmp_level					<- rownames(tmp_summary$coefficients)	
														tmp_number					<- unlist(unname(tab))
		
														class(tmp_mat[,tmp_var_to_compare])
														tmp_summary					<- summary(tmp_mod)		#
												#		tmp_anova					<- anova(tmp_mod) 

											if (tmp_p_global<=0.05 & length(tmp_level) >2 ) {

 														tmp_mod2	 				  <- aov(tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var]))
														tmp_anova2					<- TukeyHSD(tmp_mod2)
														# Make post hoc analysis with Tukey
														tmp_val_tukey				<- round(tmp_anova2[[1]][,"p adj"],2)
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
														tmp_global_p_val		    	<- rep(round(tmp_p_global,2),length(tmp_level)	)
														tmp_global_p_val_unique		<- c(round(tmp_p_global,2),rep("", (length(tmp_level)-1)	)	)	
														
														tmp_value					<- tmp_summary$coefficients[,1]
														tmp_cum_value				<- tmp_value + tmp_value[1]
														tmp_cum_value[1]			<- tmp_value[1]
														names(tmp_cum_value)		<- tmp_mod$xlevels[[1]]
														tmp_level					<- tmp_mod$xlevels[[1]]																																
														tmp_pval					<- tmp_summary$coefficients[,4]
                            
														
														# Here if we want to change the round dor the mean !!
														titi 		<- rbind(tmp_variable,tmp_variable_unique,tmp_level,tmp_number,round(tmp_cum_value,1),round(tmp_pval,2) ,tmp_global_p_val,tmp_global_p_val_unique	, comp_class, pval_tukey)
														tata		<- t(titi)
														tata[1,6]	<-	"reference class"		
# 														tata[2:length(tmp_level),c("tmp_variable","tmp_global_p_val")]		<- ""

														tutu						<- cbind(   matrix(tmp_var,nrow=length(tmp_variable), ncol=1 )	, matrix(tmp_name_var_to_compare,nrow=length(tmp_variable), ncol=1 )) 	
														toto						<- cbind(tutu,tata)
														tab_reg_linear				<- rbind(tab_reg_linear, toto)
														
										# And the corresponding plot							
										n 			<- table(tmp_mat[,tmp_var]) 					
										ylim		<- range(dataf[,tmp_var_to_compare])
										colnames(tmp_mat)[which(colnames(tmp_mat)==tmp_var)]				<- "tmp_var"
										colnames(tmp_mat)[which(colnames(tmp_mat)==tmp_var_to_compare)]	<- "tmp_var_to_compare"

										p_tmp_mod 	<- 	ggplot(tmp_mat) + geom_boxplot(aes(y=tmp_var_to_compare,x=factor(tmp_var),fill=factor(tmp_var))) + # colour=factor(variable),
														theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.title = element_blank(),axis.text.y = element_text(colour="black",face="bold")  , plot.title = element_text(face="bold", size=11)) +
														ggtitle(	paste0(tmp_name_var_to_compare," by ",tmp_name)	) +
														scale_fill_discrete(name = tmp_name,labels=paste0(names(n),' (n=',n,')'))		

																		if(tmp_p_global < 0.001)							{		p_tmp_mod <- p_tmp_mod + geom_text(data = NULL, x = (length(tmp_level)) /2 + 0.5 , y = ylim [2] * 0.9,  label = "***", size=12, colour="darkred")}
																		if(tmp_p_global > 0.001 & tmp_p_global < 0.01)		{		p_tmp_mod <- p_tmp_mod +geom_text(data = NULL, x = (length(tmp_level)) /2 + 0.5 , y = ylim[2]*0.9 ,  label = "**", size=12, colour="darkred")}
																		if(tmp_p_global > 0.01 & tmp_p_global < 0.05)		{ 		p_tmp_mod <- p_tmp_mod + geom_text(data = NULL , x= (length(tmp_level)) /2 + 0.5  ,y = ylim[2]*0.9,  label = "*", size=12, colour="darkred")}

											print(p_tmp_mod)	
															}
															
														}				
													}				
														dev.off()
														
					colnames(tab_reg_linear)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels","number_per_level","mean_value","pvalue vs ref class (t test)","pvalue ANOVA","pvalue ANOVA2","post-hoc comp class","p val Tukey")
					
					tab_reg_linear[which(tab_reg_linear[,"pvalue vs ref class (t test)"]=="0"),"pvalue vs ref class (t test)"]  <- "<0.01"
					tab_reg_linear[which(tab_reg_linear[,"pvalue ANOVA"]=="0"),"pvalue ANOVA"]  <- "<0.01"
					tab_reg_linear[which(tab_reg_linear[,"pvalue ANOVA2"]=="0"),"pvalue ANOVA2"]  <- "<0.01"
					tab_reg_linear[which(tab_reg_linear[,"p val Tukey"]=="0"),"p val Tukey"]  <- "<0.01"
