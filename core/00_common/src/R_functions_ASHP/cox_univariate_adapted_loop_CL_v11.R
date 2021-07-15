# degre_signif_multivariate <- 0.101
            
pvalues   <- variables_to_keep_in_the_multivariate_analysis <- RR <- IClow <- ICup <- IC <- tab <-  tabf <- NULL

for (i in 1:length(quali)) {
  # i=1
  
  if(!class(dataf[,quali[i]]) %in% c("numeric","integer") )
  {
              dataf[,quali[i]] <- droplevels(as.factor(dataf[,quali[i]]))
              tmp_tab 		    <- table(dataf[,quali[i]])
          		tmp_tab_NA 		  <- table(dataf[,quali[i]],exclude=NULL)		 # remove if the only observation is NA for the outcome
           		table_events 		<- table(dataf[, ev],dataf[,quali[i]],exclude=NULL)
           		table_events_no_NA 		<- table(dataf[, ev],dataf[,quali[i]])
           		tmp_ncol	      <- ncol(table_events)-1
           		print("1er coucou")
           		
           		if(length(which(is.na(rownames(table_events))))>0){
           		  print("rentree dans 1ere boucle")
          		  titi	<- table_events[which(is.na(rownames(table_events))),-(tmp_ncol+1)] 
          		  tmp_tab_no_NA_for_variable		<- tmp_tab	-  titi 
           	                                                	}
           		else{
           		  tmp_tab_no_NA_for_variable		<- tmp_tab	
           		}

  if(	! any(tmp_tab_no_NA_for_variable==0)	)  			{
      
      		if(  (length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&    ! any(tmp_tab==0)	&	!any(rowSums(table_events_no_NA)==0)	)	{
          print(quali[i])
      		if(grepl("before",quali[i])){
      		  #Computes the Cox model
      		 weights_var <- propensity[,c(paste(var,"weigth",sep="_"))]
      		 tmp_cox <- coxph(Surv(dataf[,del],dataf[,ev]) ~ dataf[,var]  + cluster(dataf[,"NUMDOS"]) + dataf[,"N_clinic"] + dataf[,"agepull"] + dataf[,"BMI_cl_5"] +  dataf[,"T_clinic"] + dataf[,"gradeclasse"] + dataf[,"typhist"] + dataf[,"embols"] + dataf[,"pTpull"] + dataf[,"nbggpos_cl.f"],weights = propensity[,paste(quali[i],"weigth",sep="_")])
      		print(tmp_cox)
      		                              }
      		else{
      		  tmp_cox 			      <- coxph(Surv(dataf[, del], dataf[, ev]) ~ dataf[,quali[i]])
      		  }
}
   
if(class(dataf[,quali[i]]) %in% c("numeric","integer") )
{
tmp_cox 			      <- coxph(Surv(dataf[, del], dataf[, ev]) ~ dataf[,quali[i]])
}
            		p_lr_test 		      <- summary(tmp_cox)$sctest["pvalue"]                                  # le p obtenu par le score du logrank
            		p_Wald 		          <- summary(tmp_cox)$waldtest["pvalue"]                                 # le p obtenu par le score de Wald
            		p_RdV 		          <- summary(tmp_cox)$logtest["pvalue"]                                 # le p obtenu par le score du RDV

            		lowest_pval        <- min(p_lr_test,p_Wald, p_RdV,na.rm=TRUE)
                # p_cox_global 		<- summary(tmp_cox)$sctest[3]                                          # le p obtenu par le score du logrank
                # p_cox_global2 	<- ifelse(p_cox_global < 0.01, "<0.01", round(p_cox_global, 2))	          # le p obtenu par le score du logrank
                # p_cox_global3 	<- ifelse(p_cox_global < 0.001, "<0.001", round(p_cox_global, 3))	       # le p obtenu par le score du logrank
                
            		p_lr_test2 	 <- ifelse(p_lr_test < 0.01, "<0.01", round(p_lr_test, 2))	        
            		p_lr_test3 	 <- ifelse(p_lr_test < 0.001, "<0.001", round(p_lr_test, 3))	     

            		p_Wald2 	   <- ifelse(p_Wald < 0.01, "<0.01", round(p_Wald, 2))	         
            		p_Wald3 	   <- ifelse(p_Wald < 0.001, "<0.001", round(p_Wald, 3))	     

            		p_RdV2 	     <- ifelse(p_RdV < 0.01, "<0.01", round(p_RdV, 2))	        
            		p_RdV3 	     <- ifelse(p_RdV < 0.001, "<0.001", round(p_RdV, 3))	      
            		
                # Si parmi 3 tests ci dessus est signif, on a le droit de faire les test individuellement 
                if(lowest_pval<=degre_signif_multivariate){
                                                              p_indiv 				<- summary(tmp_cox)$coeff[,'Pr(>|z|)']																			# Wald test
                                                              p_indiv2 			<- ifelse(p_indiv < 0.01,"<0.01"   , round(p_indiv, 2))                       # 
                                                              p_indiv3 			<- ifelse(p_indiv < 0.001,"<0.001" , round(p_indiv, 3))
                                                              } 
                
              else  if(!lowest_pval<=degre_signif_multivariate ){
                                                                p_indiv 			<- ""
                                                                p_indiv2 			<- ""
                                                                p_indiv3 			<- "" 
                                                              } 
                
                # Extract variables to keep in multivariate analysis
                #----------------------------------------------------------------
                if(	lowest_pval < degre_signif_multivariate)		{
            																								tmp_variables_to_keep								              <- 				quali[i]
            																								variables_to_keep_in_the_multivariate_analysis		<- c(variables_to_keep_in_the_multivariate_analysis,tmp_variables_to_keep)							         							         
            																									}
                #----------------------------------------------------------------
                
                              # Build log rank model for extracting effectives
                            	lr 				    <- survdiff(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], na.action=na.omit) #  data=dataf,
                            	tmp_events		<- lr$obs
                            	tmp_effectifs	<- unlist(lr$n)
                            	tmp_effectifs	<- unname(tmp_effectifs)
                            	print(tmp_effectifs)
                RR 				    <-  round(summary(tmp_cox)$conf.int[,1],2)
                IClow 			  <- round(summary(tmp_cox)$conf.int[,3],2)
                ICup 			    <- round(summary(tmp_cox)$conf.int[,4],2)
                IC 				    <- paste0("[",round(IClow,2)," - ",round(ICup,2),"]")

                tab1 			    <- data.frame( variable_name=quali[i],Variable = nomquali[i], "Class"=levels(as.factor(dataf[,quali[i]]))[1], "HR"=1, "CI"=NA, "p_indiv2"=NA,"p_indiv3"=NA,  # Test indiv
                                              "p_log_rank2"=p_lr_test2, "p_log_rank3"=p_lr_test3,
                                               "p_Wald2"=p_Wald2, "p_Wald3"=p_Wald3) 

                tab2 			    <- data.frame(variable_name = quali[i],Variable = nomquali[i], 	"Class"=levels(as.factor(dataf[,quali[i]]))[-1], "HR"=RR, "CI"=IC, "p_indiv2"=p_indiv2,"p_indiv3"=p_indiv3 ,
                                           "p_log_rank2"=NA, "p_log_rank3"=NA,
                                           "p_Wald2"=NA, "p_Wald3"=NA)

                if(class(dataf[,quali[i]]) %in% c("numeric","integer") )
                {
                  tab1[,c("Class","HR")] <- c("","")
                  tab 			    <- tab1
                  tab$Var_unique <- c(nomquali[i])
                }
                else {
                  tab 			    <- rbind(tab1, tab2)
                # Add numbers from log rank model
                print(tmp_effectifs)
                tab$Number[1:length(tmp_effectifs)] 		<- as.matrix(tmp_effectifs)
                tab$Events[1:length(tmp_effectifs)] 		<- as.matrix(tmp_events)
                tab[] 			    <- lapply(tab, as.character)
                tab$Var_unique <- c(nomquali[i],  rep(NA, (length(levels(as.factor(dataf[,quali[i]])))  -1)      )     )
                }                
                
                # tab$variable_name   <- c(rep(quali[i], (length(levels(as.factor(dataf[,quali[i]])))  )      )     )
                tab				       <- tab[,c("variable_name","Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "p_indiv3", "p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2")] 			

                tabf 			<- rbind(tabf, tab)        
            		}
          
                      }   else if(	( any(tmp_tab_no_NA_for_variable==0))| (!(length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&    ! any(tmp_tab==0)	) | 	any(rowSums(table_events_no_NA)==0)   ) 			{
                                              
                                              tmp_levels  <- levels(as.factor(dataf[,quali[i]    ]))
                                              tab         <- matrix("NA",ncol=12,nrow=(length(tmp_levels))   )        
                                              colnames(tab) <-  c("variable_name","Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "p_indiv3", "p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2") 			
                                              tabf 			<- rbind(tabf, tab)        
                                                                                                          }
                          }

              # Mise en page tableau
              rownames(tabf) <- NULL
              
              tabf           <- tabf[,c("variable_name","Variable","Var_unique", "Class", "Number","Events", "HR", "CI",  "p_indiv3","p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2")]   	
              mat_tab_f      <- as.matrix(tabf)
              mat_tab_f[is.na(mat_tab_f)]               <- ""
              mat_tab_f[which(mat_tab_f[,4]=="1.00"),4] <-"1"
              tabf		                                  <- mat_tab_f

              vector_variables_to_keep_in_the_multivariate_analysis		<-  paste(variables_to_keep_in_the_multivariate_analysis, sep="", collapse="+") 
              
              # ASHP : grosso modo, il faudra toujours virer dans les tableaux sauf p et p*
              colnames(tabf)  <-  c("variable_name","Variable_trash","Variable", "Category", "n","ev", "HR", "95%CI", "p*", "p_Wald3","p","p_indiv2","p_Wald2","p_log_rank2") 			
              
              # Remove those with variable continue; 

              tabf_short      <- tabf[,c("Variable","Category","n","ev","HR","95%CI","p_log_rank2","p_indiv2") ]
              
              
