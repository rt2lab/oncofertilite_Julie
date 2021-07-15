auc.fct<-function(score, obs, graph=F) {
  vauc<-AUC(score, obs, graph=graph)
    vauctxt<-format(round(vauc$auc, 3))
    vauctxt<-paste(vauctxt[1], " [",vauctxt[2],";",vauctxt[3],"]", sep="")
    res<-list(auc=vauc, auctxt=vauctxt)
    return(res)
}

Wilcoxon.auc<-function(data.sim) {
#data.sim<-data[1:20,]
#n<-c(33,6,6,11,2);n<-c(n, c(3,2,2,11,33));g<-rep(c(1, 2), c(5,5))
#data.sim<-data.frame(g=rep(g, n), x=rep(c(1:5, 1:5), n))

	n<-table(data.sim$g)

	data.sim$r<-rank(data.sim$x)
	W<-sum(data.sim$r[data.sim$g==2])
	U<-W-(n*(n+1)/2)[2]
	u<-as.numeric(U/prod(n))

	N<-table(data.sim$g, data.sim$x)
	nAi<-c(N[2,], 0)
	nNi<-c(N[1,], 0)
	nA<-sum(nAi)
	nN<-sum(nNi)
	K<-length(nAi)-1
	p1<-rep(0, K)
	p2<-p1
	p4<-p1
	for (k in 1:K) {
#		k<-1
		nNk<-nNi[k]
		nAk<-nAi[k]
		x<-(sum(nAi[(k+1):(K+1)]))
		p4[k]<-(x^2)*nNk
		p2[k]<-(nAk*x)*nNk
		p1[k]<-((nAk^2)/3)*nNk
	}
	Q1<-sum(p1+p2+p4)/(nN*nA*nA)
	Q1

	nAi<-c(0,N[2,])
	nNi<-c(0,N[1,])
	nA<-sum(nAi)
	nN<-sum(nNi)
	K<-length(nAi)-1
	p1<-rep(0, K)
	p2<-p1
	p4<-p1
	for (k in 1:K) {
#		k<-1
		nNk<-nNi[k+1]
		nAk<-nAi[k+1]
		x<-(sum(nNi[1:k]))
		p4[k]<-(x^2)*nAk
		p2[k]<-(nNk*x)*nAk
		p1[k]<-((nNk^2)/3)*nAk
	}
	Q2<-sum(p1+p2+p4)/(nN*nN*nA)
	Q2


	v<-u*(1-u)+(n[2]-1)*(Q1-u^2)+(n[1]-1)*(Q2-u^2)
	v<-as.numeric(v/prod(n))
	auc.w<-qnorm(c(0.50, 0.025, 0.975), u, sqrt(v))
	res<-list(auc=auc.w, u=u, v=v)
	return(res)
}
AUC<-function(score, obs, graph=F) {
     data<-data.frame(g=obs+1, x=score)
     auc<-Wilcoxon.auc(data)

     txt<-format(round(auc$auc, 2))
     txt<-paste("AUC=", txt[1], " ([",txt[2],";",txt[3], "])", sep="")
     txt

    auc$roc<-NULL
          u<-sort(unique(score))
          nu<-length(u)
          Se<-u
          Sp<-u
          for (i in 1:nu) {
#                 i<-1
               s<-ifelse(score<=u[i], 0, 1)
               tab<-addmargins(table(c(0,0,1,1, s), c(0,1,0,1,obs))-1)
               Se[i]<-tab[2,2]/tab[3,2]
               Sp[i]<-tab[1,1]/tab[3,1]
          }

          roc<-data.frame(b=u, Se=Se, Sp=Sp)
          auc$roc<-roc

     if (graph) {
          par(mai=c(1.5, 1.5,1.5,0.5))
               plot(c(0,1), c(0,1), type="n", xlab="", ylab="")
               mtext(side=1, line=3, "1-Sp", cex=1.5)
               mtext(side=2, line=3, "Se", cex=1.5)
               title("ROC", cex.main=1.5)
               lines(1-Sp, Se, col="blue", lwd=2)
               abline(a=0, b=1, col="lightblue")
               text(0.6, 0.4, cex=1.5, txt)
     }
     return(auc)
}
