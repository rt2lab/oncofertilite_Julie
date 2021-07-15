variable_to_compare			  <- "strLy"
variable_to_compare_name	<- "Pre-NAC TILs"
head(GBG_with_codes_comedic)

load(file="data/processed/matrix_codes.RData")
load("/Users/ahamypet/RT2Lab/NEOREP/medic_NEOREP/data/processed/ATC_english.RData")

dataf						        <- GBG_with_codes_comedic
variables_to_test       <- unique(colnames(matrix_codes)   )
names_variables_to_test <- tolower(ATC_english[match(variables_to_test,ATC_english$code),"name"])
  
		tab_reg_linear		<- as.data.frame(matrix(NA, length(variables_to_test)*2 , 21 ))
		head(tab_reg_linear)
		tab_reg_linear[,1] <- rep(variables_to_test,each=2)
		tab_reg_linear[,2] <- rep(names_variables_to_test,each=2)
		# tab_reg_linear[,3] <- rep(names_variables_to_test,each=2)
		tab_reg_linear[,5] <- rep(c(0,1),length(variables_to_test))		
		rownames(tab_reg_linear) <- paste0(tab_reg_linear[,1],"_",tab_reg_linear[,5]) 
		
		head(tab_reg_linear)
		
		colnames(tab_reg_linear)			<- c("variable_coded","variable","variable_quali","var_qual_unique","levels",
		                                "number_per_level_WP","mean_value_WP","pvalue_ANOVA_WP","p_Kruskal_WP",
		                                "number_per_level_Luminal","mean_value_Luminal","pvalue_ANOVA_Luminal","p_Kruskal_Luminal",
		                                "number_per_level_TNBC","mean_value_TNBC","pvalue_ANOVA_TNBC","p_Kruskal_TNBC",
		                                "number_per_level_HER2","mean_value_HER2","pvalue_ANOVA_HER2","p_Kruskal_HER2")
		
								for ( i in 1:length(variables_to_test)	) {
														# i=3
								  
          								  print(i)
														tmp_var 	<- variables_to_test[i]; print(tmp_var)
														tmp_name 	<- names_variables_to_test[i] ; print(tmp_name)		
														
														for (m in 1 : length(subtype_list	)  ) {
														  # m=4
														  subtype							<- subtype_list[[m]]										# samples names_ of subtype ex: MB-0046
														  subtype_name				<- names(subtype_list[m]); print(subtype_name) ; print(subtype_name)
														  tmp_path						<- path[m]
														  tmp_file_end				<- file_end[m]			
														  dataf  			        <-  neorep_medic_final %>% filter(numdos7 %in% subtype) ; dim(dataf) 
														head(dataf)
														tmp_mat		    <- dataf[which(!is.na(dataf[,variable_to_compare]) & !is.na(dataf[,tmp_var])    ),c(tmp_var,variable_to_compare)]
														str(tmp_mat)
														tab <- table(tmp_mat[,tmp_var])	
														
														# tmp_mat[,tmp_var_to_compare]   <- as.numeric(tmp_mat[,tmp_var_to_compare]) 
														
														n_no  <- unname(tmp_mat[which(tmp_mat[,tmp_var]==0),] %>%  summarize(count=n()))
														n_yes <- unname(tmp_mat[which(tmp_mat[,tmp_var]==1),] %>%  summarize(count=n()))
														tab_reg_linear[2*i-1,paste0("number_per_level_",tmp_file_end)]   <- unlist(n_no)
														tab_reg_linear[2*i,paste0("number_per_level_",tmp_file_end)] <- unlist(n_yes)
														head(tab_reg_linear)

														
							if( 	!any(tab<30)	&  length(rownames(tab))>1 )	{
														tmp_mod	 	       <- lm(tmp_mat[,variable_to_compare]	~ as.character(tmp_mat[,tmp_var])   )
														tmp_summary			 <- summary(tmp_mod)		#
														tmp_anova				 <- anova(tmp_mod) 
														tmp_p_global		 <- tmp_anova["Pr(>F)"]
														tmp_p_global		 <- tmp_p_global[which(!is.na(tmp_p_global)	),	]	
														
														
														tmp_value					<- tmp_summary$coefficients[1,1]
														tmp_cum_value			<- tmp_value + tmp_summary$coefficients[2,1]
														tab_reg_linear[2*i-1,paste0("mean_value_",tmp_file_end)]   <- round(tmp_value,1)
														tab_reg_linear[2*i,paste0("mean_value_",tmp_file_end)] <- round(tmp_cum_value,1)
														
														tab_reg_linear[2*i-1,paste0("pvalue_ANOVA_",tmp_file_end)]   <- round(tmp_p_global,3)
														tab_reg_linear[2*i,paste0("pvalue_ANOVA_",tmp_file_end)] <- round(tmp_p_global,3)
														head(tab_reg_linear)
							                                                    }

														if( 	any(tab<30)	&  length(rownames(tab))>1 )	{
														  # Juste pour extraire les valeurs des mean, on fait un lm
														  tmp_mod	 	       <- lm(tmp_mat[,variable_to_compare]	~ as.character(tmp_mat[,tmp_var])   )
														  tmp_summary			 <- summary(tmp_mod)		#
														  tmp_value					<- tmp_summary$coefficients[1,1]
														  tmp_cum_value			<- tmp_value + tmp_summary$coefficients[2,1]
														  tab_reg_linear[2*i-1,paste0("mean_value_",tmp_file_end)]   <- round(tmp_value,1)
														  tab_reg_linear[2*i,paste0("mean_value_",tmp_file_end)] <- round(tmp_cum_value,1)
														  
														  # On prend la pvalue du kruskall
														  tmp_kruskal          <- kruskal.test(  tmp_mat[,variable_to_compare]	~ as.factor(tmp_mat[,tmp_var])   )
														  tmp_kruskal_pval     <- round(tmp_kruskal$p.value,3)
														  tab_reg_linear[2*i-1,paste0("p_Kruskal_",tmp_file_end)]   <- round(tmp_kruskal_pval,3)
														  tab_reg_linear[2*i,paste0("p_Kruskal_",tmp_file_end)] <- round(tmp_kruskal_pval,3)
														  head(tab_reg_linear)
														}
												}  
								}
		
		head(tab_reg_linear)
		
		tab_reg_linear <- tab_reg_linear %>% mutate(levels2       = ifelse(levels==0,"no","yes")  ,
		                          pval_comb_WP  = ifelse(!is.na(pvalue_ANOVA_WP),pvalue_ANOVA_WP, p_Kruskal_WP ),
		                          pval_comb_Luminal  = ifelse(!is.na(pvalue_ANOVA_Luminal),pvalue_ANOVA_Luminal, p_Kruskal_Luminal ),
		                          pval_comb_TNBC  = ifelse(!is.na(pvalue_ANOVA_TNBC),pvalue_ANOVA_TNBC, p_Kruskal_TNBC ),
		                          pval_comb_HER2  = ifelse(!is.na(pvalue_ANOVA_HER2),pvalue_ANOVA_HER2, p_Kruskal_HER2 ) ) %>%
		                          select(variable,variable_coded,levels2,
		                                 number_per_level_WP,mean_value_WP,pvalue_ANOVA_WP,p_Kruskal_WP,pval_comb_WP,
		                                 number_per_level_Luminal,mean_value_Luminal,pvalue_ANOVA_Luminal,p_Kruskal_Luminal,pval_comb_Luminal,
		                                 number_per_level_TNBC,mean_value_TNBC,pvalue_ANOVA_TNBC,p_Kruskal_TNBC,pval_comb_TNBC,
		                                 number_per_level_HER2,mean_value_HER2,pvalue_ANOVA_HER2,p_Kruskal_HER2,pval_comb_HER2)
		head(tab_reg_linear)

		write.xlsx(tab_reg_linear, file="data/processed/tab_reg_linear.xlsx")

		# Ready for publication
		tab_reg_linear_pub  <- tab_reg_linear
		
		tab_reg_linear_pub  <- tab_reg_linear_pub %>% 	select(variable,variable_coded,levels2,
                                  number_per_level_WP,mean_value_WP,pval_comb_WP,
                                  number_per_level_Luminal,mean_value_Luminal,pval_comb_Luminal,
                                  number_per_level_TNBC,mean_value_TNBC,pval_comb_TNBC,
                                  number_per_level_HER2,mean_value_HER2,pval_comb_HER2)

		head(tab_reg_linear_pub)
		
		even <- seq_len(nrow(tab_reg_linear_pub)) %% 2  
		tab_reg_linear_pub[even==0,c("variable","variable_coded","pval_comb_WP","pval_comb_Luminal","pval_comb_TNBC","pval_comb_HER2")] <- ""
		head(tab_reg_linear_pub)
		write.xlsx(tab_reg_linear_pub, file="data/processed/tab_reg_linear_pub.xlsx")
		
		