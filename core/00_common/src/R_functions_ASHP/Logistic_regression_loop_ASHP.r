# quali <- c("trastuneo","ER_status","metagene_1__bin","metagene_2__bin","metagene_3__bin","metagene_4__bin","metagene_5__bin","metagene_6__bin")
# var_to_explain<-
# dataf<-
# dataf[,quali]


tab <- NULL
tmp_tab <- NULL

for (i in 1:length(quali)){
#i=5
							print(i)
							#  quali[i]
							modalities 							<-levels(as.factor(dataf[,quali[i]]))
							variable 							<-rep(quali[i],length(modalities))
							tmp_mod    							<- glm(as.factor(dataf[,var_to_explain]) ~  dataf[,quali[i]]    , family="binomial")  
							number_per_category					<-							table(dataf[,  quali[i]])
							nb_var_to_explain					<- 							table(dataf[, var_to_explain],dataf[, quali[i]])[2,]
							tmp_summary  						<-summary(tmp_mod)
							OR 									<-round(exp(coef(summary(tmp_mod))[,1]),2) # On remplace le coefficient de l'intercept par l'OR de référence=1
							OR[1]								<- "1"
							pval	 							<-round(coef(summary(tmp_mod))[,4],3)
							pval[1] 							<- "-"							
							IC									<- round(exp(confint(tmp_mod)),2)
							IC[1,]								<- c("-","-")
							IC									<- paste("[",IC[,1],"-",IC[,2],"]")
							tab									<-cbind(variable,modalities,number_per_category,nb_var_to_explain,OR,IC,pval)
							tmp_tab								<-rbind(tmp_tab,tab)
							}