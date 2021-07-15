# quali <- c("trastuneo","ER_status","metagene_1__bin","metagene_2__bin","metagene_3__bin","metagene_4__bin","metagene_5__bin","metagene_6__bin")
# nomquali<-
# var_to_explain<-
# level_to_import	<- "1"
# dataf<-
# dataf[,quali]

# DEFINIR PATH DE SORTIE +++++

# 1 Univariate analysis
#----------------------------------------------------------------

                      tab 											  <- NULL
                      tmp_tab 										<- NULL
                      variables_to_keep_in_the_multivariate_analysis	      <- NULL
                      vector_variables_to_keep_in_the_multivariate_analysis  <- NULL
                      
                      for (i in 1:length(quali)){
                      # i=22
                      							print(paste (i,quali[i]))
                                    # effectives : ex: pCR par regime de chimio
                      							tmp_tableau							<- table(dataf[,quali[i]],dataf[,var_to_explain])
                      
                      				# 1. Un des >=4 groupes vides
                      							# Exporter les effectifs et % sans les OR 
                                	if(any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1  )		{
                                															print("beware, at least one categ without event")
                                															print(tmp_tableau)		
                                																										
                                							variable				  <- rep(quali[i],nrow(tmp_tableau))
                                							name_variable			<- variable
                                							modalities				<- rownames(tmp_tableau)
                                							n_tot					    <- unname(rowSums(tmp_tableau))		
                                							n_in_model				<- rep("NA",nrow(tmp_tableau))		
                                							nb_var_to_explain		<- unname(tmp_tableau[modalities,level_to_import])		
                                							perc_var_to_explain		<- round(nb_var_to_explain/n_tot*100,1)
                                							OR						<- rep("NA",nrow(tmp_tableau))
                                							IC						<- rep("NA",nrow(tmp_tableau))
                                							pval					<- rep("NA",nrow(tmp_tableau))
                      
                                							# Exporter tableau final (10 colonnes) et le coller a celui davant
                                							tab									<- cbind(variable,variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
                                							colnames(tab)				<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")    							         
                                							tmp_tab							<- rbind(tmp_tab,tab)							
                                															}
                      				# 2. Aucun groupe vide
                      							
                              	if( (!any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1) |
                              	     class(dataf[,quali[i]])  == "numeric"    )		{
                              	          	modalities 							<-levels(as.factor(dataf[,quali[i]]))
                              							variable 							  <-rep(quali[i],length(modalities))
                              							name_variable						<-rep(nomquali[i],length(modalities))						
                              						
                              							# tmp_mod    							<- glm(as.numeric(dataf[,var_to_explain]) ~  dataf[,quali[i]]    , family="binomial")  
                               							tmp_mod    							<- glm(dataf[,var_to_explain] ~  dataf[,quali[i]]    , family="binomial")  
                               							n_tot								    <-  table(dataf[,  quali[i]])
                              							n_in_model							<-	table(dataf[which(!is.na(dataf[,var_to_explain])),  quali[i]])
                               							nb_var_to_explain				<-  table(dataf[, var_to_explain],dataf[, quali[i]])
                      
                              							tmp 							    	<- as.data.frame(		table(dataf[, var_to_explain],dataf[, quali[i]])		)
                               							nb_var_to_explain				<-	tmp[which(tmp$Var1 == level_to_import),"Freq"]
                              							tmp_summary  						<- summary(tmp_mod)
                              							OR 									    <- round(exp(coef(summary(tmp_mod))[,1]),2) # On remplace le coefficient de l'intercept par l'OR de référence=1
                              							OR[1]								    <- "1"
                              							pval	 							    <- coef(summary(tmp_mod))[,4]
                              							
                              							# Selection des variables a garder dans l'analyse multivariee
                              							if(	any(as.numeric(pval[-1])[which(as.numeric(pval[-1]) <0.101)	])		)		{
                              																								tmp_variables_to_keep								              <- 				unique(variable)
                              																								variables_to_keep_in_the_multivariate_analysis		<- c(variables_to_keep_in_the_multivariate_analysis,tmp_variables_to_keep)							         							         
                              																								print("jai reussi a penetrer dans la boucle de la multivariée")
                              																									}
                      
                              							# Extraction des pvalues, OR, IC  pour le tableau
                              							pval	 							<- round(coef(summary(tmp_mod))[,4],3)
                              							pval[1] 							<- ""
                                            
                                            							# Arrondis
                                              						for (k in 2:length(modalities))	{
                                                          						                  		# k=2
                                                          																pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3))
                                                          																}					
                      
                            								tab 		           	<- round(exp(confint(tmp_mod)),2)
                            							  IClow 	           	<- tab[,1]
                            								ICup 		           	<- tab[,2]
                            								tmp_IC	           	<- rbind(IClow,ICup)
                            								IC 			           	<- paste0("[",IClow," - ",ICup,"]")
                            								IC[1]		           	<- c("")
                        								    perc_var_to_explain	<- round(nb_var_to_explain*100/n_in_model,1)
                      
                        								    if(length(OR) != length(variable) ) {
                              								    OR						<- c(OR, rep("NA",  (length(variable)-length(OR) )   )    )
                              								    IC						<- c(IC, rep("NA",  (length(variable)-length(IC) )   )    )
                              								    pval						<- c(pval, rep("",  (length(variable)-length(pval) )   )    )
                              								    }

                                          # Exporter tableau final (10 colonnes) et le coller a celui davant
                              							tab									    <- cbind(variable,name_variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
                              							colnames(tab)						<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")    							         
                              							tmp_tab								  <- rbind(tmp_tab,tab)							
                      																}
                      							}
                      														
                                tmp_name           <-tmp_tab[,"name"]
                                vector_first_name  <- match(unique(tmp_name),tmp_name )
                                tmp_name2          <- rep("",length(tmp_name))
                                tmp_name2[vector_first_name] <- unique(tmp_name)          
                                tmp_tab[,"name"]   <- tmp_name2
                      
              vector_variables_to_keep_in_the_multivariate_analysis		<-  paste(variables_to_keep_in_the_multivariate_analysis, sep="", collapse="+") 
                                