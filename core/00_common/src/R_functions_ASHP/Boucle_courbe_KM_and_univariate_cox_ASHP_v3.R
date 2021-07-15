# A définir : copier coller et remplacer
# 
# 			del      <-c("delDFS.m")
# 			ev      <-c("etatDFS")
# 			dataf      <-tnadj_noCT
# 			quali <-c  (  "ageclasse.f"    , "menop.f", "bmi_cl.f", "pT.f","gradeclasse.f"  ,"embols.f"     , "nbggpos_cl.f", "NPI_cl.f" ,
#                       "chirsein.f"     ,"chirgg.f"     ,   "chimio_2cl.f" , "RT.f"      )
# 
# 			nomquali <-c( "age"    , "menopausal status", "BMI", "tumoral size","SBR grade"  ,"LVI"     , "Number of positive lymph nodes", "NPI" ,
#                       "Breast surgery"     ,"axillary surgery"     ,   "Chemotherapy" , "Radiotherapy"    )
# 			soustitre<-" (No CT population)"
#  
# 			titre_var_surv  <- "DFS"

# 			pdf("Results/Figure 4 Univariate analysis on DFS (no chemotherapy population).pdf")           

library(survival)
source("/Users/ahamypet/RT2Lab/TILs/TILs_C_Laurent/src/graph.surv_test_debug.r")

for(i in 1:length(quali)){
				#i=1
				print(quali[i])
			tmp_tab <- table(dataf[,quali[i]])
		
		if(  length(which(tmp_tab == 0)) != (length(tmp_tab)-1)			)	{

						# On crée km pour pouvoir dessiner la courbe de KM
						colnames(dataf)
						 
						km <-survfit(Surv(dataf[, del], dataf[, ev])	~ dataf[ ,quali[i]], na.action=na.omit)
						lr <-survdiff(Surv(dataf[, del], dataf[, ev])	~ dataf[,quali[i]],  na.action=na.omit)
						# On crée un modèle de cox univarié
						
					
						
						cox<-coxph( Surv(dataf[, del], dataf[, ev]) ~dataf[,quali[i]], na.action=na.omit)
                          		ICinf<-exp(cox$coefficient-1.96*sqrt(diag(cox$var)))
                          		ICsup<-exp(cox$coefficient+1.96*sqrt(diag(cox$var)))
                          		IC<-format(round(cbind(ICinf, ICsup), 2))
                          		ic<-paste("[", IC[, 1], " ; ", IC[, 2], "]", sep="")
                          		scox<-as.data.frame(summary(cox)$coef)
                          		scox$IC<-ic
                          		rr <- round(scox[,2],3)
				
								# Calcul du p du log rank qui figurera sur la figure 
								pv<-1-pchisq(lr$chisq,df=length(lr$n)-1)                                          
								pval<-ifelse(pv<0.001,"<0.001",round(pv,2))
								pval<-ifelse(pv<0.01 & pv>=0.001,round(pv,3),pval)
								
								# Extraction du nombre de malades par catégories à partir du km
								n_par_categ<-km$n
# 								categ <-levels(dataf[,quali[i]])
								categ <-levels(as.factor(dataf[,quali[i]]))
								
								# Création de la légende, avec le nombre de patients dans chaque catégorie; et les HR et leur IC qui est celui de chaque coef du cox
								legend_categ_1 <-paste0(categ[1],"(","n=",n_par_categ[1],")", "      HR=1")
								legend_categ_2 <-paste0(categ[2],"(","n=",n_par_categ[2],")",  "       HR=", round(rr[1],2)," ", ic[1]     )
								legend_categ_3 <- ifelse(!is.na(categ[3]),(paste0(categ[3],"(","n=",n_par_categ[3],")" ," HR=", round(rr[2],2)," ", ic[2]  )),NA)
								legend_categ_3 <-legend_categ_3[which(!is.na(legend_categ_3))]
								legend_categ_4 <- ifelse(!is.na(categ[4]),(paste0(categ[4],"(","n=",n_par_categ[4],")" , " HR=", round(rr[3],2) , " ", ic[3]      )),NA)
								legend_categ_4 <-legend_categ_4[which(!is.na(legend_categ_4))]
								legend_fig  <-c(legend_categ_1,legend_categ_2,legend_categ_3,legend_categ_4)										
								# Titre
								
								print("Jusquici tout va bien")
								
								titre <- paste(titre_var_surv , " as a function of ", nomquali[i],sep=""," ", soustitre, collapse=NULL)
									  cat(i, "\n")
									  #i=3
# 										graph.surv(dataf[, del],dataf[, ev],gp=as.factor(dataf[ ,quali[i]]),main=titre,col="black",lty=1:9,lwd=1,xlab="months",ylab="%")
										graph.surv(dataf[, del],dataf[, ev],gp=dataf[ ,quali[i]],main=titre,col="black",lty=1:9,lwd=1,xlab="months",ylab="%")
										legend(5,0.3,legend_fig,col="black",lty=1:9,bty="o",cex=0.8)
  										text.default(60,0.4,labels=paste("log rank test, ","p=",pval,sep="",collapse=NULL))
#   										text.default(12,0.4,labels=paste("log rank test, ","p=",pval,sep="",collapse=NULL))

# 														table(as.factor(dataf[ ,quali[i]]))

																				}

														}
										   


# NB : avant le 6 juin, j'utilisais la formule ci dessous pour le calcul des RR
# 	ose <-lr$obs/lr$exp
# 	rr<-ose/ose[1] ; rr   # 
# Mais je collais l'intervalle de confiance du cox; donc non homogène.

