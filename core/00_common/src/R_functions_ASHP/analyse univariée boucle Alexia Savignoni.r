####################################################################################
#                 fonction pour l analyse descriptive
###################################################################################

############# variables qualitatives
#quali:vctr de variables qualitatives
#resQL:on crée une liste
descriptionqualitative<-function(quali,resQL){
        for (i in 1:length(quali)){
                resQL[[i]]<-Freq(base,quali[i],na.rm=T)
                names(resQL)[[i]]<-quali[i]
        }

        for(i in 1:length(resQL)) {
        #i<-1
                cat("\n\n============        ===============",names(resQL)[i], "===========================\n\n")
                print(resQL[[i]])
        }
}

##########variables quantitatives
#quanti:vctr de variables quantitatives
#resQT:on crée une liste et on cree un vecteur var

#descriptionquantitative<-function(quanti,resQT,var){
descriptionquantitative<-function(quanti,resQT){

        for (i in 1:length(quanti)){
                resQT[[i]]<-summary(base[,quanti[i]])
                #var[i]<-sd(base[,quanti[i]], na.rm=T)
                names(resQT)[[i]]<-quanti[i]
        }

        for(i in 1:length(resQT)) {
        #i<-1
                cat("\n\n============        ===============",names(resQT)[i], "===========================\n\n")
                print(resQT[[i]])
                #print(var[i])
        }
}

#######ANALYSE DE SURVIE UNIVARIEE

#########variables qualitatives
univarieequalitative<-function(vbleQL,base,delai,etat,titre,xlim,univariateQL){
        for (i in 1:length(vbleQL))        {
        #i<-1
                km<-survfit(Surv(delai, etat) ~base[,vbleQL[i]], data=base, na.action=na.omit)
                skm<-summary(km, time=c(24,60,84,120))
                lr<-survdiff(Surv(delai, etat) ~base[,vbleQL[i]], data=base, na.action=na.omit)
                cox<-coxph( Surv(delai, etat) ~base[,vbleQL[i]], data=base, na.action=na.omit)
                #names(cox$coefficient)<-substr(names(cox$coefficient),16,60)
                ICinf<-exp(cox$coefficient-1.96*sqrt(diag(cox$var)))
                ICsup<-exp(cox$coefficient+1.96*sqrt(diag(cox$var)))
                IC<-format(round(cbind(ICinf, ICsup), 2))
                ic<-paste("[", IC[, 1], " ; ", IC[, 2], "]", sep="")
                scox<-as.data.frame(summary(cox)$coef)
                scox$IC<-ic
                RdV<-2*diff(cox$loglik)
                ddl<-length(ic)
                p<-1-pchisq(RdV, ddl)
                RdV<-paste("RdV=", round(RdV,3), " avec ", ddl, "=ddl => p=", round(p, 5), sep="")
                univariateQL[[i]]<-list(km=km,skm=skm, lr=lr, cox=scox, RdV=RdV)

                plot(km,main=titre,xlab="Temps en mois",ylab="Probabilité",xaxt="n",col=c(1:5),lwd=3,lty=1,xlim=xlim)
                axis(side=1,at=seq(0,max(base$delai)+1,by=12))
                legend(5,0.2,c(names(table(base[,vbleQL[i]]))),col=c(1:5),lwd=3,lty=1)
                pv<-1-pchisq(lr$chisq,df=length(lr$n)-1)
                pval<-ifelse(pv<0.001,"<0.001",round(pv,2))
                pval<-ifelse(pv<0.01 & pv>=0.001,round(pv,3),pval)
                text.default(60,0.9,labels=paste("p",pval,sep="=",collapse=NULL))
                savePlot(filename=paste(repert,"\\",vbleQL[i],sep=""),type=c("wmf"))
                dev.off()
        }

        names(univariateQL)<-vbleQL
        for(i in 1:length(univariateQL)) {
        #i<-1
                cat("\n\n***********        ****************",names(univariateQL)[i], "**********************************\n\n")
                u<-univariateQL[[i]]
                cat("\n============        ===============","Kaplan Meier", "===========================\n\n")
                print(u$km)
                cat("\n============        ===============","Probabilité de survie estimée", "===========================\n\n")
                print(u$skm)
                cat("\n\n============        ===============","Log-Rank", "===========================\n\n")
                print(u$lr)
                cat("\n\n============        ===============","Modèle de COX", "===========================\n\n")
                print(u$cox)
                cat("\n====                ==========","Rapport de vraisemblance", "===========================\n\n")
                print(u$RdV)
#                cat(paste(u$RR, collapse="\n"), "\n")
        }
}

#variables quantitatives

univarieequantitative<-function(vble,base,delai,etat,univariate){
        for (i in 1:length(vble))        {
        #i<-1
                cox<-coxph( Surv(delai, etat) ~base[,vble[i]], data=base, na.action=na.omit)
                #names(cox$coefficient)<-substr(names(cox$coefficient),16,60)
                ICinf<-exp(cox$coefficient-1.96*sqrt(diag(cox$var)))
                ICsup<-exp(cox$coefficient+1.96*sqrt(diag(cox$var)))

                IC<-format(round(cbind(ICinf, ICsup), 2))
                ic<-paste("[", IC[, 1], " ; ", IC[, 2], "]", sep="")

                scox<-as.data.frame(summary(cox)$coef)
                scox$IC<-ic

                RdV<-2*diff(cox$loglik)
                ddl<-length(ic)
                p<-1-pchisq(RdV, ddl)
                RdV<-paste("RdV=", round(RdV,3), " avec ", ddl, "=ddl => p=", round(p, 5), sep="")
                univariate[[i]]<-list( cox=scox, RdV=RdV)
                }

        names(univariate)<-vble
        for(i in 1:length(univariate)) {
        #i<-1
                cat("\n\n***********        ****************",names(univariate)[i], "**********************************\n\n")
                u<-univariate[[i]]

                cat("\n\n============        ===============","Modèle de COX", "===========================\n\n")
                print(u$cox)
                cat("\n====                ==========","Rapport de vraisemblance", "===========================\n\n")
                print(u$RdV)
#                cat(paste(u$RR, collapse="\n"), "\n")
        }
}


