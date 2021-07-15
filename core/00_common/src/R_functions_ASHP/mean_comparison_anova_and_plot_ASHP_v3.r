# We want to compare the level of 


# variable_to_compare			<- "perc_TIL"
# variable_to_compare_name		<- "Baseline IT TILs"

# To define
# 				variables_to_test			<- c(var_clin_path_quali.f	,"trastu.f")
# 				names_variables_to_test		<- c(names_var_clin_path_quali,"trastu neoadj or not")
# 							pdf("results/description/TILs/mean_IT_TILs_value_quali_variables.pdf")



							tab_reg_linear		<- NULL
				
								for (i in 1:length(variables_to_test)	) {
														# i=1
														tmp_var 	<- variables_to_test[i]
														tmp_name 	<- names_variables_to_test[i]
														print(tmp_name)
														print(i)
														tmp_mat		<- dataf[which(!is.na(dataf[,variable_to_compare]) & !is.na(dataf[,tmp_var])    ),c(tmp_var,variable_to_compare)]
												
														tab <- table(tmp_mat[,tmp_var])	
							if( 	!any(tab==0)	 )	{
														tmp_mod	 	<- lm(tmp_mat[,variable_to_compare]	~ as.factor(tmp_mat[,tmp_var]))
														tmp_summary					<- summary(tmp_mod)		#
														tmp_anova					<- anova(tmp_mod) 
														tmp_p_global				<- tmp_anova["Pr(>F)"]
														tmp_p_global				<- tmp_p_global[which(!is.na(tmp_p_global)	),	]	
														tmp_level					<- rownames(tmp_summary$coefficients)	
														tmp_number					<- unlist(unname(tab))
																	
														tmp_variable				<- rep(tmp_name,length(tmp_level)	)
														tmp_global_p_val			<- rep(round(tmp_p_global,4),length(tmp_level)	)
														tmp_value					<- tmp_summary$coefficients[,1]
														tmp_cum_value				<- tmp_value + tmp_value[1]
														tmp_cum_value[1]			<- tmp_value[1]
														names(tmp_cum_value)		<- tmp_mod$xlevels[[1]]
														tmp_level					<- tmp_mod$xlevels[[1]]																																
														tmp_pval					<- tmp_summary$coefficients[,4]
														titi 		<- rbind(tmp_variable,tmp_level,tmp_number,round(tmp_cum_value,2),round(tmp_pval,2) ,tmp_global_p_val	)
														tata		<- t(titi)
														tata[1,5]	<-	"reference class"		
														tata[2:length(tmp_level),c("tmp_variable","tmp_global_p_val")]		<- ""
		
														tab_reg_linear		<- rbind(tab_reg_linear, tata)
										
										# And the corresponding plot							
										n 			<- table(tmp_mat[,tmp_var]) 					
										ylim		<- range(dataf[,variable_to_compare])
										colnames(tmp_mat)[which(colnames(tmp_mat)==tmp_var)]				<- "tmp_var"
										colnames(tmp_mat)[which(colnames(tmp_mat)==variable_to_compare)]	<- "variable_to_compare"
										
										print(class(tmp_mat[,"tmp_var"]))

										p_tmp_mod 	<- 	ggplot(tmp_mat) + geom_boxplot(aes(y=variable_to_compare,x=factor(tmp_var),fill=factor(tmp_var))) + # colour=factor(variable),
														theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.title = element_blank(),axis.text.y = element_text(colour="black",face="bold")  , plot.title = element_text(face="bold", size=11)) +
														ggtitle(	paste0(variable_to_compare_name," by ",tmp_name)	) +
														scale_fill_discrete(name = tmp_name,labels=paste0(names(n),' (n=',n,')'))		

# 										p_tmp_mod 	<- 	ggplot(tmp_mat)+geom_jitter(aes(y=variable_to_compare,x=factor(tmp_var))) + geom_boxplot(aes(y=variable_to_compare,x=factor(tmp_var),fill=factor(tmp_var))) + # colour=factor(variable),
# 														theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(),axis.title = element_blank(),axis.text.y = element_text(colour="black",face="bold")  , plot.title = element_text(face="bold", size=11)) +
# 														ggtitle(	paste0(variable_to_compare_name," by ",tmp_name)	) +
# 														scale_fill_discrete(name = tmp_name,labels=paste0(names(n),' (n=',n,')'))		



# dev.off()

 if ( !(i %in% c(15,22,29,30) )	) {
							for (j in 1:length(tmp_pval[-1])	) {# j=1
																		tmp2_pval		<- tmp_pval[-1][j] 
																		if(tmp2_pval < 0.001)					{p_tmp_mod <- p_tmp_mod + geom_text(data = NULL, x = j+0.5, y = ylim [2] * 0.9,  label = "***", size=12, colour="darkred")}
																		if(tmp2_pval > 0.001 & tmp2_pval < 0.01){p_tmp_mod <- p_tmp_mod +geom_text(data = NULL, x = j+0.5, y = ylim[2]*0.9 ,  label = "**", size=12, colour="darkred")}
																		if(tmp2_pval > 0.01 & tmp2_pval < 0.05){ p_tmp_mod <- p_tmp_mod + geom_text(data = NULL, x = j+0.5 , y = ylim[2]*0.9,  label = "*", size=12, colour="darkred")}
																	}
 			}
											print(p_tmp_mod)	
															}
													}				
														dev.off()
# 					colnames(tab_reg_linear)			<- c("variable","levels","mean_TILs_value","pvalue vs ref class","pvalue ANOVA")
					colnames(tab_reg_linear)			<- c("variable","levels","number_per_level","mean_TILs_value","pvalue vs ref class","pvalue ANOVA")
