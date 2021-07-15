# A définir : copier coller et remplacer
library(survival)
source("/Users/ahamypet/RT2Lab/R/R functions/graph.surv.r")


# 
# del      <-c("delDFS.m")
# ev      <-c("etatDFS")
# dataf      <-tnadj_noCT
# quali <-c  (  "ageclasse.f"    , "menop.f", "bmi_cl.f", "pT.f","gradeclasse.f"  ,"embols.f"     , "nbggpos_cl.f", "NPI_cl.f" ,
#                       "chirsein.f"     ,"chirgg.f"     ,   "chimio_2cl.f" , "RT.f"      )
# 
# nomquali <-c( "age"    , "menopausal status", "BMI", "tumoral size","SBR grade"  ,"LVI"     , "Number of positive lymph nodes", "NPI" ,
#                       "Breast surgery"     ,"axillary surgery"     ,   "Chemotherapy" , "Radiotherapy"    )
# soustitre<-" (No CT population)"
#  
# pdf("Results/Figure 4 Univariate analysis on DFS (no chemotherapy population).pdf")           

k <- 0
 for(d in del){
  k <- k+1
  evt <- ev[k]
  j <- 0
for(i in 1:length(quali)){
#i=3
    	 j <- j+1
    cat(j, i, "\n")
    	km<-survfit(Surv(dataf[, d], dataf[, evt])~dataf[ ,quali[i]], data=dataf, na.action=na.omit)
		lr<-survdiff(Surv(dataf[, d], dataf[, evt])~dataf[,quali[i]], data=dataf, na.action=na.omit)
		cox<-coxph( Surv(dataf[, d], dataf[, evt]) ~dataf[,quali[i]], data=dataf, na.action=na.omit)
                          		ICinf<-exp(cox$coefficient-1.96*sqrt(diag(cox$var)))
                          		ICsup<-exp(cox$coefficient+1.96*sqrt(diag(cox$var)))
                          		IC<-format(round(cbind(ICinf, ICsup), 2))
                          		ic<-paste("[", IC[, 1], " ; ", IC[, 2], "]", sep="")
                          		scox<-as.data.frame(summary(cox)$coef)
                          		scox$IC<-ic
		RdV<-2*diff(cox$loglik)
		ddl<-length(ic)
		pv<-1-pchisq(lr$chisq,df=length(lr$n)-1)                                          
		pval<-ifelse(pv<0.001,"<0.001",round(pv,2))
		pval<-ifelse(pv<0.01 & pv>=0.001,round(pv,3),pval)
		n_par_categ<-km$n
		categ <-levels(dataf[,quali[i]])
		ose<-lr$obs/lr$exp
		rr<-ose/ose[1] ; rr   # 
legend_categ_1 <-paste0(categ[1],"(","n=",n_par_categ[1],")", "      HR=1")
legend_categ_2 <-paste0(categ[2],"(","n=",n_par_categ[2],")",  "       HR=", round(rr[2],2)," ", ic[1]     )
legend_categ_3 <- ifelse(!is.na(categ[3]),(paste0(categ[3],"(","n=",n_par_categ[3],")" ," HR=", round(rr[3],2)," ", ic[2]  )),NA)
legend_categ_3 <-legend_categ_3[which(!is.na(legend_categ_3))]
legend_categ_4 <- ifelse(!is.na(categ[4]),(paste0(categ[4],"(","n=",n_par_categ[4],")" , " HR=", round(rr[4],2) , " ", ic[3]      )),NA)
legend_categ_4 <-legend_categ_4[which(!is.na(legend_categ_4))]
legend_fig  <-c(legend_categ_1,legend_categ_2,legend_categ_3,legend_categ_4)		
  titre <- paste("DFS as a function of ", nomquali[i],sep="", soustitre, collapse=NULL)
      cat(i, "\n")
      #i=3
		graph.surv(dataf[, d],dataf[, evt],gp=dataf[ ,quali[i]],main=titre,col="black",lty=1:9,lwd=1,xlab="months",ylab="%")
		legend(5,0.3,legend_fig,col="black",lty=1:9,bty="o",cex=0.8)
		text.default(60,0.4,labels=paste("log rank test, ","p=",pval,sep="",collapse=NULL))
   						}
		   }
