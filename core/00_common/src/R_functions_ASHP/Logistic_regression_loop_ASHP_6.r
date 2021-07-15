# quali <- c("trastuneo","ER_status","metagene_1__bin","metagene_2__bin","metagene_3__bin","metagene_4__bin","metagene_5__bin","metagene_6__bin")
# nomquali<-
# var_to_explain<-
# dataf<-
# dataf[,quali]

tab 											<- NULL
tmp_tab 										<- NULL
variables_to_keep_in_the_multivariate_analysis	<- NULL


for (i in 1:length(quali)){
# i=8
							print(paste (i,quali[i]))							
							tmp_tableau							<- table(dataf[,quali[i]],dataf[,var_to_explain])
		# No variable to explain
	if(any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1  )		{
															print("beware, at least one categ without event")
															print(tmp_tableau)		
																										
							variable				<- rep(quali[i],nrow(tmp_tableau))
							name_variable			<- variable
							modalities				<- rownames(tmp_tableau)
							n_tot					<- unname(rowSums(tmp_tableau))		
							n_in_model				<- rep("NA",nrow(tmp_tableau))		
							nb_var_to_explain		<- unname(tmp_tableau[modalities,level_to_import])		
							perc_var_to_explain		<- round(nb_var_to_explain/n_tot*100,1)
							OR						<- rep("NA",nrow(tmp_tableau))
							IC						<- rep("NA",nrow(tmp_tableau))
							pval					<- rep("NA",nrow(tmp_tableau))

							tab									<-cbind(variable,variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
							colnames(tab)						<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")    							         
							tmp_tab								<- rbind(tmp_tab,tab)							
															}


							
	if(!any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1  )		{
							modalities 							<-levels(as.factor(dataf[,quali[i]]))
							variable 							<-rep(quali[i],length(modalities))
							name_variable						<-rep(nomquali[i],length(modalities))						
						
							tmp_mod    							<- glm(dataf[,var_to_explain] ~  dataf[,quali[i]]    , family="binomial")  
 							n_tot								<-							table(dataf[,  quali[i]])
							n_in_model							<-							table(dataf[which(!is.na(dataf[,var_to_explain])),  quali[i]])

# 							nb_var_to_explain					<- 							unlist(table(dataf[, var_to_explain],dataf[, quali[i]]))[1:length(modalities)]
 							nb_var_to_explain					<- 							table(dataf[, var_to_explain],dataf[, quali[i]])
 # str(unlist(table(dataf[, var_to_explain],dataf[, quali[i]])))
# as.matrix(nb_var_to_explain)
# class(as.matrix(nb_var_to_explain))
# class(as.data.frame(nb_var_to_explain))

							tmp 								<- as.data.frame(		table(dataf[, var_to_explain],dataf[, quali[i]])		)
# 							tmp 								<- as.data.frame(nb_var_to_explain)
 							nb_var_to_explain					<-	tmp[which(tmp$Var1 == level_to_import),"Freq"]

							tmp_summary  						<-summary(tmp_mod)
							OR 									<-round(exp(coef(summary(tmp_mod))[,1]),2) # On remplace le coefficient de l'intercept par l'OR de référence=1
							OR[1]								<- "1"
							pval	 							<- coef(summary(tmp_mod))[,4]
							
							if(	any(as.numeric(pval[-1])[which(as.numeric(pval[-1]) <0.101)	])		)		{
																								tmp_variables_to_keep								<- 				unique(variable)
																								variables_to_keep_in_the_multivariate_analysis		<- c(variables_to_keep_in_the_multivariate_analysis,tmp_variables_to_keep)							         							         
																									}

							pval	 							<- round(coef(summary(tmp_mod))[,4],3)
							pval[1] 							<- ""

								for (k in 2:length(modalities))	{
								# k=2
																pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3))
																}					

								tab 						<- round(exp(confint(tmp_mod)),2)
							    IClow 						<- tab[,1]
								ICup 						<- tab[,2]
								tmp_IC						<- rbind(IClow,ICup)
								IC 							<- paste0("[",IClow," - ",ICup,"]")
								IC[1]						<- c("")
								perc_var_to_explain			<- round(nb_var_to_explain*100/n_in_model,1)

							tab									<-cbind(variable,name_variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
							colnames(tab)						<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")    							         
							tmp_tab								<- rbind(tmp_tab,tab)							
																}
																
																
																
							}
														
 vector_variables_to_keep_in_the_multivariate_analysis		<-  paste(variables_to_keep_in_the_multivariate_analysis, sep="", collapse="+") 
 
 
	 