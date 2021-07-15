# dataf						        <- TILs_WP[which(TILs_WP$subtype.f=="luminal"  &TILs_WP$RCH3==0),] ; dim(dataf)
# variables_to_test			<- c("embols_sein")
# names_variables_to_test		<- variables_to_test
# variable_to_compare			<- c("perc_TIL","perc_stromal_lymphocytes","perc_cellules_tumorales","cis_infiltrant","Index_mitotique", "perc_TIL2","perc_stromal_lymphocyte2",var_quanti_class_post)
# variable_to_compare_name	<-c("pre-NAC IT TIL","pre-NAC str TIL","pre-NAC cellularity","perc in situ","pre-NAC mitotic index","perc_TIL","perc_stromal_lymphocytes",names_var_quanti_class_post)


		tab_reg_linear		<- NULL

		for (k in 1:length(variable_to_compare)	) {
							# k=2							
							tmp_var_to_compare			                <- variable_to_compare[k]
							tmp_name_var_to_compare		              <- variable_to_compare_name[k]
							print(k)				                      # ESR1												
							print(tmp_name_var_to_compare)				# ESR1												
	
														tmp_var 	<- variables_to_test
														print(tmp_var)
														tmp_name 	<- names_variables_to_test[i]
														print(tmp_name)		# LST_status 
														tmp_mat		<- dataf[which(!is.na(dataf[,tmp_var_to_compare]) & !is.na(dataf[,tmp_var])    ),c(tmp_var,tmp_var_to_compare)]
                            tmp_levels_factor  <- levels(as.factor(dataf[,tmp_var]) )
                            tmp_mat[,tmp_var_to_compare]   <- as.numeric(tmp_mat[,tmp_var_to_compare]) 
                            
														tab <- table(tmp_mat[,tmp_var])	
														
							if( 	!any(tab==0)	&length(tmp_levels_factor)>1  )	{
														tmp_mod	 	<- lm(as.numeric(tmp_mat[,tmp_var_to_compare])	~ as.factor(tmp_mat[,tmp_var]))
														class(as.numeric(tmp_mat[,tmp_var_to_compare]))
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

 														tmp_mod2	 				<- aov(tmp_mat[,tmp_var_to_compare]	~ as.factor(tmp_mat[,tmp_var]))
														tmp_anova2					<- TukeyHSD(tmp_mod2)
														# Make post hoc analysis with Tukey
														tmp_val_tukey				<- round(tmp_anova2[[1]][,"p adj"],2)
														str(tmp_val_tukey) 				
														tmp_tab_tukey_2				<- data.frame(	comp_classes	= names(tmp_val_tukey[])	,
																									pval			= tmp_val_tukey				)														
														comp_class					<- as.character(tmp_tab_tukey_2[,"comp_classes"])
														pval_tukey					<- as.character(tmp_tab_tukey_2[,"pval"])
																								} else 		{
																											comp_class					<- rep("",length(tmp_level)	)
																											pval_tukey					<- rep("",length(tmp_level)	)														
																											}
																								
														tmp_variable				<- rep(tmp_name,length(tmp_level)	)
														tmp_global_p_val			<- rep(round(tmp_p_global,2),length(tmp_level)	)
														tmp_global_p_val_unique		<- c(round(tmp_p_global,2),rep("", (length(tmp_level)-1)	)	)	
														
														tmp_value					<- tmp_summary$coefficients[,1]
														tmp_cum_value				<- tmp_value + tmp_value[1]
														tmp_cum_value[1]			<- tmp_value[1]
														names(tmp_cum_value)		<- tmp_mod$xlevels[[1]]
														tmp_level					<- tmp_mod$xlevels[[1]]																																
														tmp_pval					<- tmp_summary$coefficients[,4]

														titi 		      <- rbind(tmp_variable,tmp_level,tmp_number,round(tmp_cum_value,2),round(tmp_pval,2) ,tmp_global_p_val,tmp_global_p_val_unique	, comp_class, pval_tukey)
														titi[c("tmp_variable","tmp_level","tmp_number"),]
														names_var     <- rep(tmp_name_var_to_compare,2)
														n_and_mean    <- c ("n","mean")
														titi_horiz    <- cbind(names_var,n_and_mean,titi[3:4,],titi[6,],titi[6,], titi[8:9,])
														tata		<- t(titi)
														tata[1,5]	<-	"reference class"		
# 														tata[2:length(tmp_level),c("tmp_variable","tmp_global_p_val")]		<- ""

														tutu						<- matrix(tmp_name_var_to_compare,nrow=length(tmp_variable), ncol=1 )		
														toto						<- cbind(tutu,tata)
														tab_reg_linear				<- rbind(tab_reg_linear, titi_horiz)
														
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
															# }
															
														}				
													}				
														dev.off()

					colnames(tab_reg_linear)			<- c("variable","n_and_mean",tmp_level,"p","p ","post-hoc comp class","p val Tukey")
