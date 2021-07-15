# quali <- c( "Menopause.f", "Size.f", "Node.f")
# dataf<- 
#	del <- c("delai_dfs")
#	ev <-c("etat_dfs")
#	quali  <- c() 
# nomquali =c("","")              
# degre_signif_multivariate <- 0.101
            
pvalues   <- variables_to_keep_in_the_multivariate_analysis <- RR <- IClow <- ICup <- IC <- tab <-  tabf <- NULL
print(quali)


for (i in 1:length(quali)) {
              # i=1
               print(quali[i])
                    
                    tmp_tab 		    <- table(dataf[,quali[i]])
                		tmp_tab_NA 		  <- table(dataf[,quali[i]],exclude=NULL)		 # remove if the only observation is NA for the outcome
                 		table_events 		      <- table(dataf[, ev],dataf[,quali[i]],exclude=NULL)
                 		table_events_no_NA 		<- table(dataf[, ev],dataf[,quali[i]])
                 		 tmp_ncol	      <- ncol(table_events)-1
                		titi		        <- table_events[which(is.na(rownames(table_events))),-(tmp_ncol+1)		] 
      tmp_tab_no_NA_for_variable		<- tmp_tab	-  titi 
         
        if(	! any(tmp_tab_no_NA_for_variable==0)	)  			{
            
            		if(  (length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&    ! any(tmp_tab==0)	&	!any(rowSums(table_events_no_NA)==0)	)	{

            		tmp_cox 			      <- coxph(Surv(dataf[, del], dataf[, ev]) ~ dataf[,quali[i]])
                p_cox_global 		    <- summary(tmp_cox)$sctest[3]     # le p obtenu par le score du logrank
                p_cox_global2 	<- ifelse(p_cox_global < 0.01, "<0.01", round(p_cox_global, 2))	
                p_cox_global3 	<- ifelse(p_cox_global < 0.001, "<0.001", round(p_cox_global, 3))	
                p_indiv 				<- summary(tmp_cox)$coeff[,'Pr(>|z|)']																			# Wald test
                p_indiv2 			<- ifelse(p_indiv < 0.01,"<0.01"   , round(p_indiv, 2))
                p_indiv3 			<- ifelse(p_indiv < 0.001,"<0.001" , round(p_indiv, 3))

                # Extract variables to keep in multivariate analysis
                #----------------------------------------------------------------
                if(	p_cox_global < degre_signif_multivariate)		{
            																								tmp_variables_to_keep								              <- 				quali[i]
            																								variables_to_keep_in_the_multivariate_analysis		<- c(variables_to_keep_in_the_multivariate_analysis,tmp_variables_to_keep)							         							         
            																									}
                #----------------------------------------------------------------
                
                              # Build log rank model for extracting effectives
                            	lr 				    <- survdiff(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], na.action=na.omit) #  data=dataf,
                            	tmp_events		<- lr$obs
                            	tmp_effectifs	<- unlist(lr$n)
                            	tmp_effectifs	<- unname(tmp_effectifs)

                RR 				    <-  round(summary(tmp_cox)$conf.int[,1],2)
                IClow 			  <- round(summary(tmp_cox)$conf.int[,3],2)
                ICup 			    <- round(summary(tmp_cox)$conf.int[,4],2)
                IC 				    <- paste0("[",round(IClow,2)," - ",round(ICup,2),"]")

                tab1 			    <- data.frame( "Variable"= nomquali[i], "Class"=levels(as.factor(dataf[,quali[i]]))[1], "HR"=1, "CI"=NA, "pval2"=NA,"pval3"=NA  , "pval_global2"=p_cox_global2, "pval_global3"=p_cox_global3) 
                
                tab2 			<- data.frame( "Variable"=nomquali[i], 	"Class"=levels(as.factor(dataf[,quali[i]]))[-1], "HR"=RR, "CI"=IC, "pval2"=p_indiv2,"pval3"=p_indiv3 ,"pval_global2"=NA, "pval_global3"=NA ) 
                tab 			<- rbind(tab1, tab2)
                
                # Add numbers from log rank model
                tab$Number 		<- as.matrix(tmp_effectifs)
                tab$Events 		<- as.matrix(tmp_events)
                tab[] 			    <- lapply(tab, as.character)
                tab$Var_unique <- c(nomquali[i],  rep(NA, (length(levels(as.factor(dataf[,quali[i]])))  -1)      )     ) 
                tab				      <- tab[,c("Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "pval3", "pval_global3","pval2","pval_global2")] 			
                #
                tabf 			<- rbind(tabf, tab)        
            		}
          
                      }   else if(	( any(tmp_tab_no_NA_for_variable==0))| (!(length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&    ! any(tmp_tab==0)	) | 	any(rowSums(table_events_no_NA)==0)   ) 			{
                                              
                                              tmp_levels  <- levels(as.factor(dataf[,quali[i]    ]))
                                              tab         <- matrix("NA",ncol=11,nrow=(length(tmp_levels))   )        
                                              colnames(tab) <-  c("Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "pval3", "pval_global3","pval2","pval_global2") 			
                                              tabf 			<- rbind(tabf, tab)        
                                                                                                          }
                          }

              # Mise en page tableau
              rownames(tabf) <- NULL
              tabf           <- tabf[,c("Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "pval3", "pval_global3","pval2","pval_global2")]   	
              mat_tab_f      <- as.matrix(tabf)
              mat_tab_f[is.na(mat_tab_f)]               <- ""
              mat_tab_f[which(mat_tab_f[,4]=="1.00"),4] <-"1"
              tabf		                                  <- mat_tab_f

              vector_variables_to_keep_in_the_multivariate_analysis		<-  paste(variables_to_keep_in_the_multivariate_analysis, sep="", collapse="+") 

