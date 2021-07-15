# quali <- c( "Menopause.f", 
#             "Size.f", 
#             "Node.f", 
#             "Grade.f", 
#             "NPI.f", 
#             "gp1_immunoLH", 
#             "gp2_angiogenesisLH", 
#             "gp3_signallingLH", 
#             "gp4_cellcycleLH")
        
# 	dataf<- 
#	del <- c("delai_dfs")
#	ev <-c("etat_dfs")
#	quali  <- c() 
#       
# nomquali =c("","")              
            
pvalues<- variables_to_keep_in_the_multivariate_analysis <- RR <- IClow <- ICup <- IC <- tab <-  tabf <- NULL
print(quali)
for (i in 1:length(quali)) 
{
# i=1
   print(quali[i])

		tmp_tab <- table(dataf[,quali[i]])
		
		if(  length(which(tmp_tab == 0)) != (length(tmp_tab)-1)			)	{

   	tmp 			<- coxph(Surv(dataf[, del], dataf[, ev]) ~ dataf[,quali[i]])
    p_global 		<- summary(tmp)$sctest[3]

							if(	p_global <0.101)		{
																								tmp_variables_to_keep								<- 				quali[i]
																								variables_to_keep_in_the_multivariate_analysis		<- c(variables_to_keep_in_the_multivariate_analysis,tmp_variables_to_keep)							         							         
																									}

#     summary(tmp)$sctest[1]
#     objects(summary(tmp))
#     summary(tmp)$waldtest[3]
	lr 				<-survdiff(Surv(dataf[, del], dataf[, ev])~dataf[ ,quali[i]], na.action=na.omit) #  data=dataf,
	tmp_events		<-lr$obs
	tmp_effectifs	<-unlist(lr$n)
	tmp_effectifs	<-unname(tmp_effectifs)
    p_global 		<- ifelse(p_global < 0.001, "<0.001", round(p_global, 3))	
#     real_p_global 	<- ifelse(p_global < 0.0001, format.pval(p_global, eps=0.0001, scientific=TRUE), round(p_global, 3))	
    p 				<- summary(tmp)$coeff[,'Pr(>|z|)']																			# Wald test
    p 				<- ifelse(p < 0.001,"<0.001", round(p, 3))
    RR 				<-  round(summary(tmp)$conf.int[,1],2)
    IClow 			<- round(summary(tmp)$conf.int[,3],2)
    ICup 			<- round(summary(tmp)$conf.int[,4],2)
    IC 				<- paste0("[",round(IClow,2)," - ",round(ICup,2),"]")
    tab1 			<- data.frame( "Variable"= nomquali[i], "Class"=levels(as.factor(dataf[,quali[i]]))[1], "HR"=1, "CI"=NA, "Pvalue"=NA, "PvalueGlobal"=NA) #,"realPvalueGlobal"=NA)
    
#     Beware, only for the TCGA project,  tab2 uncommented, because one single tab generated for cox, and want to be able to filter by variable, and see in each localisation ; and want to see all classes ...
#      tab2 			<- data.frame( "Variable"=NA, 	"Class"=levels(as.factor(dataf[,quali[i]]))[-1], "HR"=RR, "CI"=IC, "Pvalue"=p, "PvalueGlobal"=p_global,"realPvalueGlobal"=real_p_global)

    tab2 			<- data.frame( "Variable"=nomquali[i], 	"Class"=levels(as.factor(dataf[,quali[i]]))[-1], "HR"=RR, "CI"=IC, "Pvalue"=p, "PvalueGlobal"=p_global   ) # ,"realPvalueGlobal")  #=real_p_global)
    tab 			<- rbind(tab1, tab2)
    tab$Number 		<- as.matrix(tmp_effectifs)
    tab$Events 		<- as.matrix(tmp_events)
    tab[] 			<- lapply(tab, as.character)
    tab				<- tab[,c("Variable", "Class", "Number","Events", "HR", "CI", "Pvalue", "PvalueGlobal")] 				#,"realPvalueGlobal")]
    tabf 			<- rbind(tabf, tab)    
    
    																			}
}

rownames(tabf) <- NULL
tabf <- tabf[,c("Variable", "Class", "Number","Events", "HR", "CI", "Pvalue", "PvalueGlobal")]   				#,"realPvalueGlobal")]
mat_tab_f<-as.matrix(tabf)
mat_tab_f[is.na(mat_tab_f)] <- ""
mat_tab_f[which(mat_tab_f[,4]=="1.00"),4]<-"1"
tabf		<-mat_tab_f


 vector_variables_to_keep_in_the_multivariate_analysis		<-  paste(variables_to_keep_in_the_multivariate_analysis, sep="", collapse="+") 

