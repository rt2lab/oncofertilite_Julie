# J'ai pas compris grand chose dans cette boucle, mais on en a besoin pour dÈfinir le modËle logistique, et le faire tourner.

petit.p.fct<-function(p, dec=3) {
#dec<-2
	dix<-10^dec
	lim<-(1/dix)/2
	plim<-paste(c("<0.", substring("000000000000000", 1, dec-1), "1"), collapse="")

	x<-round(p*dix)
	nx<-nchar(x)
	nz<-dec-nx;nz
	zero<-substring("00000000000000000000000000000", 1, nz);zero
	xx<-paste("0.", zero, x, sep="")
	return(ifelse(p<=lim, plim,xx))
}
                #On Ècrit notre modËle logistique
                                                                    # ModeleLogistique  est une FONCTION
ModeleLogistique<-function(reg, alpha=0.05, dec=4) {
	sreg<-summary(reg)$coef
	                                                                    #unlist simplifies une structure x
                                                                      #to produce a vector
                                                                      #which contains all the atomic components which occur in x.

	v<-unlist(strsplit(as.character(reg$formula)[-1:-2], " \\+ "))          #J'ai pas trop compris cette formule..
	nbv<-length(v)

	d<-drop1(reg)                                                               # A quoi sert le drop1 ??
	nsuj<-paste(reg$df.null, " sujets", sep="")                                 # on crÈe le nb de sujet
	dev.0<-paste("DÈviance du model vide = ", round(reg$null.dev, 2)," avec ", reg$df.null, " ddl", sep="")
	dev.model<-paste("DÈviance du model = ", round(reg$dev, 2)," avec ", reg$df.res, " ddl", sep="")
	rdv<-abs(reg$null.dev-reg$dev)                                               # J'ai pas trop compris abs, mais Áa doit mettre la () en integer
	p<-1-pchisq(rdv, reg$df.null-reg$df.res)
	ptxt<-petit.p.fct(p, dec)
	ptxt<-ifelse(substring(ptxt, 1, 1)!="<", paste("=", ptxt, sep=""),ptxt)

	rdv<-paste("AIC = ", round(reg$aic, 2), " - RdV = ", round(rdv, 2), " p ", ptxt, sep="")
	d<-data.frame(v=v, dev.resid=d$Dev[-1], df.resid=d$Df[-1], AIC=d$AIC[-1])
	d$RdV<-d$dev.resid-reg$dev
	d$p<-petit.p.fct(1-pchisq(d$RdV, d$df.resid), dec)

	b<-list(classe=dimnames(sreg)[[1]], beta=sreg[, 1], sd.beta=sreg[, 2])

	v2<-c(as.character(b$classe[1]), v)
	indv<-rep(0, nbv+1);i<-match(b$classe, v2);i<-i[!is.na(i)];indv[i]<-1;indv

	vl<-list()
	vl[[1]]<-v2[1]
	b$v<-v2[1]
	for (i in 2:(nbv+1)) {
#		i<-2
#		i<-i+1
		if (indv[i]==0) {
			vl[[i]]<-paste(v2[i], levels(reg$data[, v2[i]]), sep="")
		} else {
			vl[[i]]<-v2[i]
		}
		j<-match(vl[[i]], b$classe)
		j
		if (any(is.na(j)) && any(!is.na(j))) {		#factor
			mj<-min(j, na.rm=T);mj
			b$classe<-c(b$classe[1:(mj-1)], vl[[i]][is.na(j)], b$classe[-1:-(mj-1)])
			b$beta<-c(b$beta[1:(mj-1)], 0, b$beta[-1:-(mj-1)])
			b$sd.beta<-c(b$sd.beta[1:(mj-1)], NA, b$sd.beta[-1:-(mj-1)])
		}
		b$v<-c(b$v, rep(v[i-1], length(vl[[i]])))
	}
	b<-as.data.frame(b)

	n<-nchar(as.character(b$v))
	N<-nchar(as.character(b$classe))
	b$classe<-substring(as.character(b$classe), n+1,N)
	b<-b[, c("v", "classe", "beta", "sd.beta")]

	b$z<-b$beta/b$sd.beta
	b$p<-petit.p.fct(2*(1-pnorm(abs(b$z))), dec)

	b$OR<-exp(b$beta)
	ici<-exp(qnorm(alpha/2, b$beta, b$sd.beta))
	ics<-exp(qnorm(1-alpha/2, b$beta, b$sd.beta))
	b$ic<-paste("[", format(round(ici, 2)), ";",format(round(ics, 2)), "]", sep="")
	i<-match("ic", names(b))
	names(b)[i]<-nomic<-paste("IC", 100-100*alpha, "(OR)", sep="")
	b$beta<-round(b$beta, 3)
	b$sd.beta<-round(b$sd.beta, 3)
	b$OR<-round(b$OR, 2)
	b$z<-round(b$z, 3)

	b$sd.beta[b$beta==0]<-"-"
	b$z[b$beta==0]<-"-"
	b$p[b$beta==0]<-"-"
	b[, nomic][b$beta==0]<-"-"


	cat(nsuj, "\n",
		dev.0, "\n",
		dev.model, "\n",
		rdv, "\n",
		sep="")
	cat("\nCoefficients\n")
	print(b)
	cat("\nTest sur chacune des variables\n")
	print(d)
}