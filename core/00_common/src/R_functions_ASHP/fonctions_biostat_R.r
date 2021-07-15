#setwd("c:/yann/pgm/biostat");source("fonctions_biostat_R.r")
Met.NA<-function(x) {
	if (is.numeric(x)) {
		x[is.na(x)]<-NA
		x[is.nan(x)]<-NA
	}
	if (is.factor(x)) {
		nom<-levels(x)
		nom<-nom[nom!="NA" & nom!="NaN"]
		x<-factor(x, levels=nom)
	}
	x
}
Encadre<-function(a, espace=F, ligne=F, n=0, side=0) {
	if (espace) {a<-paste(" ", a, " ", sep="")}
	if (ligne) {a<-c("", a, "")}
		lga<-max(c(n,nchar(a)))
		haut<- paste(rep("-", lga), collapse="")
		if (side<(-34) | side>34) {
			a2<-format(c(haut, a), just="left")[-1]
		}
		if (side==0) {
			a2<-format(c(haut, a, haut), just="left")
			a2<-paste(c("+", rep("|", length(a2)-2),"+"), a2, sep="")
			a2<-paste(a2,c("+", rep("|", length(a2)-2),"+"), sep="")
		}
		if (side==1) {
			a2<-format(c(a,haut), just="left")
		}
		if (side==2) {
			a2<-format(c(haut, a), just="left")[-1]
			a2<-paste(rep("|", length(a2)), a2, sep="")
		}
		if (side==3) {
			a2<-format(c(haut, a), just="left")
		}
		if (side==4) {
			a2<-format(c(haut, a), just="left")[-1]
			a2<-paste(a2, rep("|", length(a2)), sep="")
		}
		if (side==-1) {
			a2<-format(c(haut, a), just="left")
			a2<-paste(c("+", rep("|", length(a2)-1)), a2, sep="")
			a2<-paste(a2,c("+", rep("|", length(a2)-1)), sep="")
		}
		if (side==-2) {
			a2<-format(c(haut, a, haut), just="left")
			a2<-paste(a2,c("+", rep("|", length(a2)-2),"+"), sep="")
		}
		if (side==-3){
			a2<-format(c(a, haut), just="left")
			a2<-paste(c(rep("|", length(a2)-1),"+"), a2, sep="")
			a2<-paste(a2,c(rep("|", length(a2)-1),"+"), sep="")
		}
		if (side==-4){
			a2<-format(c(haut, a, haut), just="left")
			a2<-paste(c("+", rep("|", length(a2)-2),"+"), a2, sep="")
		}
		if (side==23 | side==-14) {
			a2<-format(c(haut, a), just="left")
			a2<-paste(c("+", rep("|", length(a2)-1)), a2, sep="")
		}
		if (side==12 | side==-34) {
			a2<-format(c(a, haut), just="left")
			a2<-paste(c(rep("|", length(a2)-1),"+"), a2, sep="")
		}
		if (side==-23 | side==14){
			a2<-format(c(a, haut), just="left")
			a2<-paste(a2,c(rep("|", length(a2)-1),"+"), sep="")
		}
		if (side==-13 | side==24) {
			a2<-format(c(haut, a), just="left")[-1]
			a2<-paste(rep("|", length(a2)), a2, sep="")
			a2<-paste(a2, rep("|", length(a2)), sep="")
		}
		if (side==-24 | side==13) {
			a2<-format(c(haut, a, haut), just="left")
		}
	return(a2)
}
#
# ============================================================
# ============================================================
# Cox stepwise
# ============================================================
# ============================================================
#
cox.step.logit<-function(p) {log(p/(1-p))}
cox.step.RdV<-function(data, delai="delai", etat="etat", cov, force=NULL, dans=NULL, pentre=0.05, psort=0.20, trace=T) {
#
#data<-essai$data;delai="delai";etat="etat";force<-essai$nomf;pentre=0.05; psort=0.10;cov<-essai$nomc;dans<-essai$nomd
#
# ----- nom connu ?
#
	nom<-c(delai, etat, cov, force, dans)
	i<-match(nom, names(data))
	if (any(is.na(i))) {
		cat("\n\tUne ou plusieurs variables n'existent pas\n")
		print(nom[is.na(i)])
		cat("\n")
		stop("")
	}
#
# ----- nom doublon ?
#
	i<-duplicated(nom)
	if (any(i)) {
		cat("\n\tUne ou plusieurs variables en double\n")
		print(nom[i])
		cat("\n")
		stop("")
	}
#
# ----- il y a au 1 moins une cov
#
	nbf<-length(force)
	nbc<-length(cov)
	nbd<-length(dans)
	if (nbc+nbd==0) {
		cat("\n\tIl faut mettre une covariable\n")
		stop("")
	}
#
# ----- pentre<psort
#
	if (pentre>=psort) {
		cat("\n\tJe pr?f?re pentre < psort\n")
		stop("")
	}
	if ((pentre<0 & psort<0) | (pentre>1 & psort>1) | (pentre<0 & psort>1) | (pentre>1 & psort<0)) {
		cat("\n\tIl faut pentre ou psort dans ]0 ; 1[\n")
		stop("")
	}
#
# ----- donnees du modele
#
	covt<-cov
	danst<-dans
	N<-dim(data)[1]
	datacox<-na.omit(data[, nom])
	n<-dim(datacox)[1]
	nevt<-sum(datacox[, etat])
	if (nevt==0) {
		cat("\n\tJe n'ai pas d'?v?nement\n")
		stop("")
	}
#
# ----- modele de base
#
	resultat<-list()
	if (nbf+nbd==0) {	
		f0<-paste("Surv(", delai,", ", etat, ")~1", sep="")
		ff<-f0
	} else {
		v<-c(force, dans)
		f0<-paste(v, collapse="+")
		f0<-paste("Surv(", delai,", ", etat, ")~",f0, sep="")
		if (nbf>0) {
			v<-c(force)
			ff<-paste(v, collapse="+")
			ff<-paste("Surv(", delai,", ", etat, ")~",ff, sep="")
		} else {
			ff<-paste("Surv(", delai,", ", etat, ")~1", sep="")
		}
	}
	options(warn=2)
		cox<-try(coxph(formula(f0), data=datacox), silent=T)
		if (all(class(cox)!="try-error")) {
			if (nbf+nbd==0) {scox<-cox$loglik} else {scox<-summary(cox)$coef}
		} else {
			scox<-NA
		}
	options(warn=0)

	etape0<-list(nbv=nbf+nbd, formule=f0, cox=scox)

	scoxfin<-scox
	ffin<-f0
#
# ----- on tourne
#
	rentrees<-rep(c(0,1), c(nbc, nbd))
	nbc<-nbc+nbd
	cov<-c(cov, dans)
	entre<-rep(1, nbc)
	sort<-rep(1, nbc)
	lv0<-1:nbc;ddl0<-1:nbc
	lv1<-1:nbc;ddl1<-1:nbc

	modelefait<-NULL
	lvfait<-NULL;ddlfait<-NULL;

	nbmc<-1

	covES<-NULL;ES<-NULL
	iter<-0
	while ((sum(entre, na.rm=T)>0 | sum(sort, na.rm=T)>0)) {
		iter<-iter+1
		txt<-paste("Etape", iter)

		f1<-rep("", nbc)
		f0<-f1
		if (nbc>1) {
			for (i in 1:nbc) {
#				i<-2
				if (rentrees[i]==0) {
					f1[i]<-paste(c(cov[rentrees==1], cov[i]), collapse="+")
					f0[i]<-paste(c(cov[rentrees==1]), collapse="+")
				} else {
					f0[i]<-paste(cov[-i][rentrees[-i]==1], collapse="+")
					f1[i]<-paste(c(cov[rentrees==1]), collapse="+")
				}
			}
			f1<-ifelse(f1!="", paste(ff, f1, sep="+"), ff)
			f0<-ifelse(f0!="", paste(ff, f0, sep="+"), ff)
		} else {
			f1<-paste(ff, cov, sep="+")
			f0<-ff
		}

		options(warn=2)
		for (i in 1:nbc) {
			i0<-match(f0[i], modelefait)
			if (is.na(i0)) {
				nbmc<-nbmc+1

				cox<-try(coxph(formula(f0[i]), data=datacox), silent=T)
				if (all(class(cox)!="try-error")) {
					lv0[i]<-max(cox$loglik)
					ddl0[i]<-length(cox$coef)
				} else {
					lv0[i]<-NA
					ddl0[i]<-NA
				}
				modelefait<-c(modelefait, f0[i])
				lvfait<-c(lvfait, lv0[i])
				ddlfait<-c(ddlfait, ddl0[i])
			} else {
				lv0[i]<-lvfait[i0]
				ddl0[i]<-ddlfait[i0]
			}

			i1<-match(f1[i], modelefait)
			if (is.na(i1)) {
				nbmc<-nbmc+1
				cox<-try(coxph(formula(f1[i]), data=datacox), silent=T)
				if (all(class(cox)!="try-error")) {
					lv1[i]<-max(cox$loglik)
					ddl1[i]<-length(cox$coef)
				} else {
					lv1[i]<-NA
					ddl1[i]<-NA
				}
				modelefait<-c(modelefait, f1[i])
				lvfait<-c(lvfait, lv1[i])
				ddlfait<-c(ddlfait, ddl1[i])
			} else {
				lv1[i]<-lvfait[i1]
				ddl1[i]<-ddlfait[i1]
			}
		}
		options(warn=0)
		res<-data.frame(cov=cov, test=ifelse(rentrees==1, "sortir", "rentrer"),lv0=lv0, ddl0=ddl0, lv1=lv1, ddl1=ddl1, RdV=abs(2*(lv1-lv0)), ddl=ddl1-ddl0)
		res$p<-1-pchisq(res$RdV, res$ddl)

		entre<-ifelse(res$p<=pentre & !is.na(res$p) & res$test=="rentrer", 1, 0)
		sort<-ifelse(res$p>=psort & !is.na(res$p) & res$test=="sortir", 1, 0)

		p<-ifelse(!is.na(res$p), res$p, 1.1);p[res$test=="sortir"]<-1.1
		ien<-which.min(p);if (p[ien]>pentre | entre[ien]==0) {ien<-0}
		p<-ifelse(!is.na(res$p), res$p, -0.1);p[res$test=="rentrer"]<-(-0.1)
		iso<-which.max(p);if (p[iso]<psort | sort[iso]==0) {iso<-0}


		i<-0
		if (ien==0 & iso==0) {
			i<-0
		} else {
			if (ien>0 & iso>0) {
				if(cox.step.logit(psort)-cox.step.logit(res$p[iso])<cox.step.logit(res$p[ien])-cox.step.logit(pentre)) {
					i<-(-iso)
				} else {
					i<-ien
				}
			} else {
				if (ien>0) {i<-ien} else {i<-(-iso)}
			}
		}
		res$ES<-rep("", nbc)
		if (i>0) {res$ES[i]<-"(E)"}
		if (i<0) {res$ES[-i]<-"(S)"}

		res$cov<-as.character(res$cov)

		resultat[[iter]]<-list(formule=NA, covar=res, H0=NA, H1=NA)

		if (i!=0) {
			if (i>0) {
				if (trace) {cat(txt, ": la variable", res$cov[i], "entre dans le modèle\n")}
				rentrees[i]<-1

				resultat[[iter]]$H0<-scox
				cox<-coxph(formula(f1[i]), data=datacox)
				scox<-summary(cox)$coef
				resultat[[iter]]$H1<-scox
				resultat[[iter]]$formule<-f1[i]

				covES<-c(covES, res$cov[i])
				ES<-c(ES, "E")
				ffin<-f1[i]
			} else {	
				if (trace) {cat(txt, ": la variable", res$cov[-i], "sort du mod?le\n")}
				rentrees[-i]<-0

				resultat[[iter]]$H1<-scox
				cox<-coxph(formula(f0[-i]), data=datacox)
				scox<-summary(cox)$coef
				resultat[[iter]]$H0<-scox
				resultat[[iter]]$formule<-f0[-i]

				covES<-c(covES, res$cov[-i])
				ES<-c(ES, "S")
				ffin<-f0[-i]
			}
			scoxfin<-scox
		} else {
			if (trace) {cat(txt, ": aucune variable n'entre dans le mod?le ni ne sort du mod?le\n")}
		}
	}

	mod<-list(mod=modelefait, lv=lvfait, ddl=ddlfait)
	res<-list(methode="RdV", pentre=pentre, psort=psort, data=datacox, Ntotal=N, Ncox=n, Nevt=nevt, delai=delai, etat= etat, cov.force=force, cov.test=covt, cov.dans=danst, Nmod=nbmc, etape=resultat, modele.fin=list(formule=ffin, cox=scoxfin), covES=covES, ES=ES, etape0=etape0, mod=mod)
	class(res)<-"cox.step"
	invisible(res)
}
print.cox.step<-function(x, ...) {
#x<-r;dec<-3
	tiret<-"-----------------------------------------------------------------------"
	a<-match.call()
	nba<-length(a)

	final<-T
	i<-pmatch("fin", names(a))
	if (!is.na(i)) {final<-eval(a[[i]])}

	depart<-F
	i<-pmatch("dep", names(a))
	if (!is.na(i)) {depart<-eval(a[[i]])}

	methode<-F
	i<-pmatch("met", names(a))
	if (!is.na(i)) {methode<-eval(a[[i]])}

	step<-F
	i<-pmatch("eta", names(a))
	if (!is.na(i)) {step<-eval(a[[i]])}

	dec<-3
	i<-pmatch("dec", names(a))
	if (!is.na(i)) {dec<-eval(a[[i]])}

	hist<-F
	i<-pmatch("his", names(a))
	if (!is.na(i)) {hist<-eval(a[[i]])}

	tout<-F
	i<-pmatch("all", names(a))
	if (!is.na(i)) {tout<-eval(a[[i]])}
	if (tout) {
		final<-T
		depart<-T
		methode<-T
		step<-T
		hist<-T
	}
#
# ----- impression
#
	n<-x$Ncox
	N<-x$Ntotal

	force<-x$cov.force
	nbf<-length(force)

	covt<-x$cov.test
	nbc<-length(covt)

	danst<-x$cov.dans
	nbd<-length(danst)

	etape<-x$etape
	nbe<-length(etape)

	cat(n, " sujets utilis?s avec ", x$Nevt," ?v?nements\n", sep="")
	if (n<N) {cat(N-n, " sujets retir?s\n")}

	cat("\n")
	if (nbf==0) {
		cat("Aucune cov forc?e\n")
	} else {
		if (nbf==1) {
			cat("Cov forc?e :", force,"\n")
		} else {
			cat("Cov forc?es :", paste(force, collapse=", "),"\n")
		}
	}

	if (nbd==0) {
		cat("Aucune cov d?j? dans le mod?le\n")
	} else {
		if (nbd==1) {
			cat("Cov test?e d?j? dans le mod?le :", danst,"\n")
		} else {
			if (nbd<=5) {
				cat("Cov test?es d?j? dans le mod?le :", paste(danst, collapse=", "),"\n")
			} else {
				cat("Cov test?es d?j? dans le mod?le :\n")
				deb<-1;fin<-5
				while (deb<=nbd) {
					cat("\t",paste(danst[deb:fin], collapse=", "), "\n", sep="")
					deb<-fin+1
					fin<-min(c(nbd, fin+5))
				}
			}
		}
	}

	if (nbc==0) {
		cat("Aucune cov ? rentrer dans le mod?le\n")
	} else {
		if (nbc==1) {
			cat("Cov test?e :", covt,"\n")
		} else {
			if (nbc<=5) {
				cat("Cov test?es :", paste(covt, collapse=", "),"\n")
			} else {
				cat("Cov test?es :\n")
				deb<-1;fin<-5
				while (deb<=nbc) {
					cat("\t",paste(covt[deb:fin], collapse=", "), "\n", sep="")
					deb<-fin+1
					fin<-min(c(nbc, fin+5))
				}
			}
		}
	}

	if (depart) {
		cat("\n", tiret, "\nMod?le de d?part :\n", sep="")
		if (x$etape0$nbv==0) {
			cat("Mod?le vide LV=", x$etape0$cox, "\n")
		} else {
			print(x$etape0$cox)
		}
	}

	if (methode) {
		p<-x$pentre;pen<-max(c(0,min(c(1,p))))
		p<-x$psort;pso<-max(c(0,min(c(1,p))))
		c(x$pentre, x$psort,pen, pso)
		methtxt<-switch(pmatch(x$methode, c("RdV", "Score")), "Test du Rapport des Vraisemblances", "Test du Score")
		cat("\n", tiret,"\nM?thode ", methtxt, "\n", sep="")
		if (pen>0 & pen<1) {cat("p d'entr?e  :", pen, "\n")}
		if (pso>0 & pso<1) {cat("p de sortie :", pso, "\n")}
		
	}

	if (step) {
		for (i in 1:nbe) {
			if (i<10) {
				cat("\n", tiret,"\nEtape : ", i, "\n---------\n", sep="")
			} else {
				cat("\n", tiret,"\nEtape : ", i, "\n----------\n", sep="")
			}
			print(etape[[i]]$covar)
			if (i<nbe) {
				cat("\nMod?le retenu\n")
				print(etape[[i]]$H1)
			}
		}
	}

	if (hist) {
		covES<-c(force, danst, x$covES)
		ES<-c(rep(c("F", "D"), c(nbf, nbd)),c(x$ES))
		if (length(x$ES)>0) {
			et<-c(rep(c(0,0), c(nbf, nbd)), 1:length(x$ES)) 
		} else {
			et<-c(rep(c(0,0), c(nbf, nbd))) 
		}
		h<-data.frame(cov=covES, ES=ES, ordre=et)
		ho<-h[order(h$cov, h$ordre),]
		hh<-as.matrix(h)
		hh<-cbind(hh, rep("     ", dim(h)[1]),as.matrix(ho))

		cat("\n", tiret, "\nHistoire\n", sep="")
		print(hh, quote=F)
	}

	if (final) {
		cat("\n", tiret, "\nMod?le final\n\tIl y a eu ", x$Nmod," mod?le(s) test?(s)\n\n", sep="")
		f0<-strsplit(x$modele.fin$formule, "~")[[1]][2]
		f0<-strsplit(f0, "\\+")[[1]]
		i<-match("1", f0);if (!is.na(i)) {f0<-f0[-i]}
		nbf0<-length(f0)
		if (nbf0==0) {
			cat("Mod?le vide LV=", x$modele.fin$cox, "\n")
		} else {
			H0<-x$modele.fin$cox
			if (all(is.na(H0))) {
				cat("\t", x$modele.fin$formule,"\n")
				cat("\tpas de convergence\n")
			} else {
				H0<-as.data.frame(H0)
        names(H0) <- c(names(H0)[-5], "p")
				eps<-1/(10^dec)
				H0$p2<-format(round(c(eps, H0$p), dec))[-1]
				H0$p2[H0$p<=eps/2]<-paste("<0.", paste(rep("0", dec-1), collapse=""), "1", sep="")
				H0$p2[H0$p<=eps/20]<-paste("<0.", paste(rep("0", dec), collapse=""), "1", sep="")

				H0$type<-rep("", dim(H0)[1])
				i<-match(f0, force)
				H0$type[!is.na(i)]<-"(F)"
				i<-match(f0, covt)
				H0$type[!is.na(i)]<-"(T)"
				i<-match(f0, danst)
				H0$type[!is.na(i)]<-"(D)"
				print(H0)
			}
		}
	}

	invisible()
}
#
# ============================================================
# ============================================================
# croisement de var qual : Tableau
# ============================================================
# ============================================================
#
Tableau<-function(tab, x, y, na.rm=F,...) {
#tab<-X;x<-"x5"; y<-"g";na.rm=T
#tab<-data;x<-"x";y<-"g";na.rm=T
	appel<-match.call()
	
	if (length(x)>1 | length(y)>1) {
          stop("Utilisez plut?t Tableaux")
     }
#
# calculs
#
     tab0<-tab[!is.na(tab[, x]) & !is.na(tab[, y]),]
     n<-dim(tab0)[1]
     if (n==0 | is.null(n)) {
          cat("Tableau vide\n")
     	tab<-list(obs=NA, pligne=NA, pcol=NA, pcase=NA, att=NA, var=c(x,y), test=NA, vide=T, nomcol=NA, nomligne=NA)
     	tab$Call<-match.call()
     	class(tab)<-"Tableau"
     	
     	return(tab)
	}
	obs<-tableau(tab, x, y, na.rm, T)
	nomcol<-dimnames(obs)[[2]]
	nomligne<-dimnames(obs)[[1]]
	pligne<- tableau(tab, x, y, na.rm, T, px=T)
	pcol<- tableau(tab, x, y, na.rm, T, py=T)
	pcase<- tableau(tab, x, y, na.rm, T, pc=T)
	att<-tableau(tab, x, y, na.rm, T, attendu=T)

	ddl<-prod(dim(obs)-2)
	if (ddl==1) {
		if (all(att>=5)) {
			CA<-T
			methode<-"Khi2"
		} else {
			if (all(att>=3)) {
				CA<-T
				methode<-"Yates"
			} else {
				if (obs[3,3]<1000) {
					CA<-T
					methode<-"Fisher"
				} else {
					CA<-F
					methode<-"Aucune"
				}
			}
		}
	} else {
		methode<-"Khi2"
		if (any(att<5)) {
			CA<-F
		} else {
			CA<-T
		}
	}
#
#test
#
	ind<-match(methode,c("Khi2","Yates","Fisher","Aucune"))
	test<-switch(ind, 
		sum(((obs-att)^2)/att), 
		sum((((abs(obs-att)-0.5)^2)/att)[-3,-3]),
		TestFisher(obs[-3,-3]),
		NA)

	p<-switch(ind, 
		1-pchisq(test, ddl),
		1-pchisq(test, ddl),
		test,
		NA)

	test<-list(test=test, ddl=ddl, p=p, methode=methode, CA=CA)
	tab<-list(obs=obs, pligne=pligne, pcol=pcol, pcase=pcase, att=att, var=c(x,y), test=test, vide=F, nomcol=nomcol, nomligne=nomligne)
	tab$Call<-match.call()
	class(tab)<-"Tableau"

	tab
}
Charmat<-function(a, nomcol, nomligne) {
#nomcol<-dimnames(tt2$obs)[[2]];nomligne<-dimnames(tt2$obs)[[1]]
	lga<-max(nchar(c(a, nomcol, nomligne)))

	nbc<-dim(a)[2]
	nbl<-dim(a)[1]
	nblpc<-dim(a)[3]

	side<-array(-23, dim=dim(a)[-3])
	side[1,1]<-0
	side[1,-1]<-(-2)
	side[-1, 1]<-(-3)
	side[1,nbc]<-(-24)
	side[-1,nbc]<-1
	side[nbl,]<-2

	sh<-rep(2, nbc)
	sb<-rep(1, nbl);sb[1]<-13;sb[nbl]<-99
	blanc<-paste(rep(" ", lga), collapse="")

	titre<-list()
	for (i in 1:nbl) {titre[[i]]<-c(nomligne[i], rep(blanc, nblpc-1))}

	b<-list()
	b[[1]]<-blanc
	for (i in 1:dim(a)[1]) {
#		i<-i+1		
		for (j in 1:nbc) {
#			j<-j+1
			aa<-a[i,j,]
			if (i==1) {b[[1]]<-c(b[[1]], Encadre(nomcol[j], n=lga, side=sh[j]))}
			if (j==1) {
				b[[i+1]]<-Encadre(titre[[i]], n=lga, side=sb[i])
			}
			b[[i+1]]<-paste(b[[i+1]], Encadre(aa, n=lga, side=side[i,j]), sep="")
			b
		}
		b
	}
	b[[1]]<-paste(b[[1]], collapse="")
	bu<-unlist(b)
	vide<-paste(c("|",rep(" ", lga)), collapse="")
	vide<-paste(c(blanc, rep(vide, nbc)), collapse="")
	bu<-bu[bu!=vide]

	x<-list(bu=bu, vide=vide, b=b)
	return(x$bu)
}

print.Tableau<-function(x,...) {
#tab<-tab2i
	appel<-match.call()
#
# dec       : pour la pr?cision
# test      : pour afficher le test
# enligne   : pour n'avoir que les % en ligne
# encolonne : pour n'avoir que les % en colonne
# encase    : pour n'avoir que les % en case
# attendu   : pour mettre les attendus dans le tableau
#
	ind<-pmatch("dec", names(appel))
	if (!is.na(ind)) {
		dec<- as.vector(eval(appel[[ind]]))
	} else {
		dec<-2
	}
	dec<-as.integer(dec[1])
	if (dec<0 || dec >10) {dec<-2}

	ind<-pmatch("test", names(appel))
	if (!is.na(ind)) {
		itest<- as.logical(eval(appel[[ind]]))
	} else {
		itest<-F
	}

	ind<-pmatch("att", names(appel))
	if (!is.na(ind)) {iatt<-as.logical(eval(appel[[ind]]))} else {iatt<-F}

	ind<-pmatch("enli", names(appel))
	if (!is.na(ind)) {enligne<-as.logical(eval(appel[[ind]]))} else {enligne<-F}
	ind<-pmatch("enco", names(appel))
	if (!is.na(ind)) {encolonne<-as.logical(eval(appel[[ind]]))} else {encolonne<-F}
	ind<-pmatch("enca", names(appel))
	if (!is.na(ind)) {encase<-as.logical(eval(appel[[ind]]))} else {encase<-F}
	entout<-((enligne+encase+encolonne+!iatt)==0)

     if (x$vide) {
     	print(x$Call)
          cat("Tableau vide", x$var, "\n")
          return()
     }
	nbl<-dim(x$obs)[1]
	nbc<-dim(x$obs)[2]

	yobs<-format(c(x$obs))

	att<-format(round(x$att, dec))
	yatt<- matrix(paste("(",att,")", sep=""), ncol=nbc)
	yatt[nbl, ]<-""
	yatt[, nbc ]<-""

	pligne<-format(round(x$pligne*100, dec))
	pcol<-format(round(x$pcol*100, dec))

	yp<-paste(format(paste(pligne,"%", sep="")), format(paste(pcol,"%", sep="")),sep="/")
	if (enligne) {yp<-format(paste(pligne,"%", sep=""))}
	if (encolonne) {yp<-format(paste(pcol,"%", sep=""))}

	pcase<-format(round(x$pcase*100, dec))
	yc<- matrix(paste(pcase,"%", sep=""), ncol=nbc)
	yc[nbl, ]<-""
	yc[, nbc ]<-""

	if (!entout) {
		if (!iatt) {yatt[]<-""}
		if (enligne | encolonne) {yc[]<-""}
	}
	z<-array("", dim=c(nbl, nbc, 4))
		z[,,1]<-yobs;z[,,2]<-yatt;z[,,3]<-yp;z[,,4]<-yc
		z<-Charmat(z, dimnames(x$obs)[[2]], dimnames(x$obs)[[1]])

	if (entout) {
		a<-c("obs.", "(attendus)", "pourc. ligne / pourc. colonne", "pourc. case")
	} else {
 	  if (iatt) {
	    if (enligne) {a<-c("obs.", "(attendus)", "pourc. ligne")}
	    if (encolonne) {a<-c("obs.", "(attendus)", "pourc. colonne")}
	    if (encase) {a<-c("obs.", "(attendus)", "pourc. case")}
	  } else {
		a<-c("obs.", "pourc. ligne / pourc. colonne", "pourc. case")
	    if (enligne) {a<-c("obs.", "pourc. ligne")}
	    if (encolonne) {a<-c("obs.", "pourc. colonne")}
	    if (encase) {a<-c("obs.", "pourc. case")}
	  }
	}
	a2<-Encadre(a)

	print(x$Call)
	cat(paste(a2, collapse="\n"), "\n", sep="")
	cat(paste(z, collapse="\n"), "\n")
#
# test
#
	if (itest) {
		test<-x$test
		cat("\n")
		a<-matrix("Test")
#		class(a) <- "char.matrix"
		cat(paste(Encadre(a), collapse="\n"), "\n")

		if (!iatt) {
			cat("l'attendu minimum :", att[order(x$att)][1], "\n")
		}

		if (test$methode=="Khi2") {
			cat("\t", test$methode,"=",round(test$test,3),"avec",test$ddl,"ddl - p =", round(test$p,5))
			if (!test$CA) {cat(" (Pb d'effectifs theoriques)")}
			cat("\n")
		}
		if (test$methode=="Yates") {
			cat("\t", test$methode,"=",round(test$test,3),"p =", round(test$p,5),"\n")
		}
		if (test$methode=="Fisher") {
			cat("\tFisher : p (uni)=", round(test$p[1],5) ,"\n")
			cat("\t       : p (bil)=", round(test$p[2],5),"\n")
		}
		if (test$methode=="Aucune") {
			cat("\tTest non possible : petits effectifs et N>=200\n")
		}
	}

	invisible()
}
tableau<-function(tab, x, y, na.rm=F, nomvar=F, px=F, py=F, pc=F, attendu=F) {
#tab<-data; x<-"x"; y<-"g"; na.rm=T; nomvar=F; px=F; py=F; pc=F; attendu=F

	indx<-match(x, names(tab))
	indy<-match(y, names(tab))
	if (is.na(indx+indy)) {
		stop("Pb de nom de variable")
	}
	if (nomvar) {
		nvar<-c(x,y)
	}
	x<-Met.NA(tab[,indx])
	y<-Met.NA(tab[,indy])
#
# facteurs
#
	indnomx<-0
	if (is.factor(x)) {
		indnomx<-1
		nomx <- levels(x)
		x <- c(x)
	}
	indnomy<-0
	if (is.factor(y)) {
		indnomy<-1
		nomy <- levels(y)
		y <- c(y)
	}
#
# on mets NA ou pas
#
	if (!na.rm) {
		tabx<- table(x, exclude=NULL)
		taby<- table(y, exclude=NULL)
		tab<-table(x,y, exclude=NULL)
	} else {
		tabx<- table(x)
		taby<- table(y)
		tab<-table(x,y)
	}
#
# nom des lignes et colonnes
#
	if (indnomx==0) {
		nomx<-names(tabx)
	} else {
		nomx<-nomx[as.numeric(names(tabx))]
	}
	if (indnomy==0) {
		nomy<-names(taby)
	} else {
		nomy<-nomy[as.numeric(names(taby))]
	}
	if (nomvar) {
		nomx<-paste(nvar[1], nomx)
		nomy<-paste(nvar[2], nomy)
	}

	tligne<-apply(tab,1,sum)
	tab<-cbind(tab, tligne)
	tcol<-apply(tab,2,sum)
	tab<-rbind(tab, tcol)
	nomx<-c(nomx, "total")
	nomy<-c(nomy, "total")
	nx<-length(nomx)
	ny<-length(nomy)

	dimnames(tab)<-list(nomx, nomy)

	if (px) {
		tab<-tab/tab[,ny]
		attendu<-F
	}
	if (py) {
		y<-t(tab)
		tab<-t(y/y[,nx])
		attendu<-F
	}
	if (pc) {
		tab<-tab/tab[nx,ny]
		attendu<-F
	}
	if (attendu) {
		tab2<- outer(tab[,ny], tab[nx,], "*")/tab[nx,ny]
		dimnames(tab2)<-dimnames(tab)
		tab<-tab2
	}

	return(tab)
}
plot.Tableau<-function(x,...) {
#
# recupere : col pour les couleurs
# recupere : pourcentage pour N ou Frequence (par case)
# recupere : parligne pour pourcentage par ligne
# recupere : multi pour plusieurs graphiques
# recupere : etiquette pour mettre les etiquettes
# recupere : titre pour mettre un titre
#
	nb<-dim(x$obs)
	nbx<-nb[1]
	nby<-nb[2]

	appel<-match.call()
#
# couleurs
#
	ind<-pmatch("coul", names(appel))
	if (!is.na(ind)) {
		coul<- as.vector(eval(appel[[ind]]))
	} else {
		coul<-c(1:(nby-1))
	}
	while (length(coul)< (nby-1)) {
		coul<-c(coul, coul)
	}
#
# N ou p
#
	ind<-pmatch("pour", names(appel))
	if (!is.na(ind)) {
		indp<- as.vector(eval(appel[[ind]]))
	} else {
		indp<-F
	}
	if (indp) {
		nombre<-x$pcase
		ylab<-"Fr?quence (%)"
	} else {
		nombre<-x$obs
		ylab<-"Effectif"
	}
#
# case ou ligne
#
	ind<-pmatch("parl", names(appel))
	if (!is.na(ind)) {
		indpl<-as.vector(eval(appel[[ind]]))

	} else {
		indpl<-F
	}
	if (indp && indpl) {
		nombre<-x$pligne
		ylab<-"Fr?quence par ligne"
	}
#
# multi graph
#
	ind<-pmatch("mul", names(appel))
	if (!is.na(ind)) {
		indm<-as.vector(eval(appel[[ind]]))
	} else {
		indm<-F
	}
	nv<-1
	nh<-1
	if (indm) {
		nv<-1
		nh<-(nbx-1)%/%nv
		if (nh*nv<(nbx-1)) {nh<-nh+1}
		while ((nh-nv)>2) {
			nv<-nv+1
			nh<-(nbx-1)%/%nv
			if (nh*nv<(nbx-1)) {nh<-nh+1}
		}
	}
#
# Etiquette
#
	ind<-pmatch("eti", names(appel))
	if (!is.na(ind)) {
		inde<- as.vector(eval(appel[[ind]]))
	} else {
		inde<-F
	}
#
# titre
#
	ind<-pmatch("tit", names(appel))
	if (!is.na(ind)) {
		titre<-as.character(eval(appel[[ind]]))
	} else {
		titre<-""
	}
#
# limites
#
	if (!indm) {
		L<-0.80
		xlim<-c(1-L/2-0.1,(nbx-1)+L/2+0.1)
		xlh<-(xlim[2]-xlim[1])/50
	} else {
		xlim<-c(0.4,(nby-1)+0.6)
	}
	if (!indp) {
		ylim<-c(0, max(nombre[-nbx,-nby]))
		yh<-0.05*ylim[2]
		ylim<-ylim*1.1
	} else {
		ylim<-c(0,100)
		yh<-0.05*ylim[2]
	}
	ylh<-(ylim[2]-ylim[1])/25
#
# nom des classes de x et de y
# et val interm pour le tracage des barres
#
	nbx<-nbx-1
	nomx<- dimnames(nombre)[[1]][1:nbx]
	nomx<-substring(nomx, nchar(x$var[1])+1, nchar(nomx))

	nby<-nby-1
	nomy<- dimnames(nombre)[[2]][1:nby]
	nomy<-substring(nomy, nchar(x$var[2])+1, nchar(nomy))
	if (!indm) {
		if (nby>1) {
			xx<-seq(0-L/2, 0+L/2, l=(nby+1))
		} else {
			xx<-0
		}
	}
#
# un r?seau de barres ou un graph par valeur de x
#
	par(mfrow=c(nv, nh))
	for (i in 1:nbx) {
		if (!indm) {
			if (i==1) {
				plot(xlim, ylim, type="n", xlab=x$var[1], ylab=ylab, xaxt="n", yaxs="i", xaxs="i", main=titre)
				axis(side=1, at=c(1:nbx), label= nomx, adj=0.5)
			}
			y<-nombre[i, 1:nby]
			xxx<-i+xx
			for (j in 1:(length(xxx)-1)) {
				xp<-c(xxx[j], xxx[j], xxx[(j+1)], xxx[(j+1)])
				yp<-c(0, y[j], y[j], 0)
				if (indp) {yp<-yp*100}
				polygon(xp, yp, col=coul[j], density=10, angle=45, lwd=3)
				if (inde) {
					if (indp) {
						text((xp[2]+xp[3])/2, 100*y[j]+yh, adj=0.5, paste(round(100*y[j]),"%", sep=""))
					} else {
						text((xp[2]+xp[3])/2, y[j]+yh, adj=0.5, y[j])
					}
				}
			}
		} else {
			plot(xlim, ylim, type="n", xlab=x$var[2], ylab=ylab, yaxs="i", xaxt="n", xaxs="i", main=paste(x$var[1],nomx[i]))
			mtext(outer=T, side=3, adj=0.5, titre, line=-1, cex=1.5)
			axis(side=1, at=c(1:nby), label= nomy, adj=0.5)

			y<-nombre[i, 1:nby]
			xxx<-1:nby
			for (j in 1:length(y)) {
				xp<-c(-0.5,-0.5,0.5,0.5)+j
				yp<-c(0, y[j], y[j], 0)
				if (indp) {yp<-yp*100}
				polygon(xp, yp, col=coul[j], density=10, angle=45, lwd=3)
				if (inde) {
					if (indp) {
						text(j, 100*y[j]+yh, adj=0.5, paste(round(100*y[j]),"%", sep=""))
					} else {
						text(j, y[j]+yh, adj=0.5, y[j])
					}
				}
			}
		}

	}

	if (!indm) {
		for (i in 1:nby) {
			xp<- xlim[1]+ c(1, 1, 2, 2)*xlh
			yp<- ylim[2] - c(2*i+1, 2*i, 2*i, 2*i+1)*ylh
			polygon(xp, yp, col=coul[i], density=10, angle=45, lwd=3)
			text(xp[4]+xlh, (yp[3]+yp[4])/2, adj=0., nomy[i])
		}
		text(xlim[1]+xlh, ylim[2]-ylh, adj=0., x$var[2])
	}

}
TestFisher<- function(x){
#x<-obs[-3,-3]
	x <- cbind(x, apply(x, 1, sum))
	x <- rbind(x, apply(x, 2, sum))
	ia <- x[1, 1] + 1
	n<-x[1,3]
	pp <- dhyper(0:n, x[3, 1], x[3, 2], n)
	pia <- pp[ia]+1e-010
	pbil <- sum(pp[pp <= pia])

	p<-x[1,]/x[3,]
	if (p[1]<=p[2] & all(!is.na(p[1:2]))) {
		puni<-sum(pp[1:ia])
	} else {
		puni<-sum(pp[ia:n])
	}

	return(c(puni, pbil))
}
Tableaux<-function(tab, x, y, na.rm=T,...) {
#tab<-spm2;x<-c("foie", "kar", "os");y<-"chim";na.rm<-T
#tab<-X;x<-vx;y<-"g";na.rm<-T
	appel<-match.call()

     nx<-length(x)
     ny<-length(y)
     if (nx<=1 & ny<=1) {
          cat("\nune seule variable x et une seule y : utilisez plut?t Tableau()\n")
          return()
     }
     if (nx==1) {
          z<-y
          y<-x
          x<-z
     }

     n<-length(x)
     tabs<-list()
     k<-0
     nomk<-NULL
	for (i in 1:n) {
		cat(x[i],"\n")
		ts<-Tableau(tab, x[i], y, na.rm)
		if (!ts$vide) {
               k<-k+1
     		tabs[[k]]<-ts
     		nomk<-c(nomk, x[i])
          }
	}
	names(tabs)<-nomk
	
	tabs$Call<-match.call()

	tabs[[k+2]]<-Freq(tab, y, na.rm)
	class(tabs)<-"Tableaux"

	tabs
}
print.Tableaux<-function(x,...) {
#tabs<-t
	appel<-match.call()

	ind<-pmatch("tes", names(appel))
	indp<-F
	if (!is.na(ind)) {indp<-as.logical(eval(appel[[ind]]))}

	ind<-pmatch("tot", names(appel))
	indt<-T
	if (!is.na(ind)) {indt<-as.logical(eval(appel[[ind]]))}

	ind<-pmatch("tit", names(appel))
	titre<-NULL;indtit<-F
	if (!is.na(ind)) {indtit<-T;titre<-as.character(eval(appel[[ind]]))}

	ind<-pmatch("dec", names(appel))
	dec<-1
	if (!is.na(ind)) {dec<-round(as.numeric(eval(appel[[ind]])))[1]}
	if (is.na(dec) | dec<0 | dec>4) {dec<-1}

	ind<-pmatch("pcol", names(appel))
	indpcol<-T
	if (!is.na(ind)) {indpcol<-as.logical(eval(appel[[ind]]))}

#tabs<-tab2;indp<-T;indt<-F;indpcol<-T;dec<-1
	blanc<-"                            "
	nbtabs<-length(x)
	if (nbtabs==2) {
          cat("Tous les tableaux sont vides\n")
          return()
     }
#
# v?rifions que les tables ont toutes les m?mes colonnes
#
     nomcols<-lapply(x, function(t) {t$nomcol})
     nomcolsu<-unique(unlist(nomcols))
     nomcolsu
     j<-list()
     for (i in 1:(nbtabs-2)) {
          j[[i]]<-match(nomcolsu, nomcols[[i]])
     }
     if (any(is.na(unlist(j)))) {
          jj<-sapply(j, paste, collapse=".")
          nomvar<-names(x)[1:(nbtabs-2)]
          cat("\n",
               "la variable \"", x[[1]]$var[2], "\" n'a pas les m?mes colonnes pour toutes les variables\n",
               "recommencez Tableaux en plusieurs morceaux\n", sep="")
               print(tapply(nomvar, jj, c))
          return()
     }


	tabf<-x[[nbtabs]]
     N<-max(tabf$tab$"n cum")
	x[[nbtabs]]<-NULL
	nbtabs<-length(x)-1

	y<-list()
	ytit<-list()
	yt<-list()
#
# ----- r?cup des obs et des p + les titres + les tests
# les % en ligne pour les cases
# les % en colonnes pour les totaux des lignes
# les % en ligne pour les totaux des colonnes  optionnels
#
	for (i in 1:nbtabs) {
#		i<-1
#		i<-i+1
#          cat(i, "\n")
		tab<-x[[i]]
		yobs<-tab$obs
		ycol<-tab$pcol
		ylig<-tab$pligne

          n<-max(yobs)
		nomlig<-dimnames(yobs)[[1]]
		nomcol<-dimnames(yobs)[[2]]
		nbl<-dim(yobs);nbc<-nbl[2];nbl<-nbl[1]
		if (!indpcol) {
               yp<-ylig
          } else {
               yp<-ycol
          }

		yobs<-format(yobs)
		yp<-format(round(100*yp, dec))
		yp<-array(paste("(",yp, "%)", sep=""), dim=dim(yobs))
          if (!indpcol) {
     		yp[,nbc]<-rep("", nbl)
          } else {
     		lg<-nchar(yp)[1]
     		yp[nbl,]<-substring(blanc, 1, lg)               #j'enl?ve les % lignes des totaux
          }

		y[[i]]<-array(paste(yobs, yp), dim=c(nbl, nbc))
		ytit[[i]]<-nomlig

		m<-tab$test$methode
		p<-round(tab$test$p, 4)
		if (m=="Fisher") {p<-p[2]}
		if (!is.na(p)) {
     		if (p<0.0001) {p<-"p<0.0001"} else {paste("p=", p, sep="")}
          } else {
               p<-"p=NA"
          }
		CA<-tab$test$CA;CA<-if(!CA) {CA<-"(*)"} else {CA<-""}
		yt[[i]]<-cbind(tab$test$methode, p, CA)
	}
#
# ----- on enl?ve le total ?ventuellement
#
	if (!indt) {
		nomcol<-nomcol[-nbc]
		for (i in 1:nbtabs) {y[[i]]<-y[[i]][, -nbc]}
		nbc<-nbc-1
		if (nbc==1) {
     		for (i in 1:nbtabs) {y[[i]]<-cbind(y[[i]])}
		}
		for (i in 1:nbtabs) {
			nbl<-dim(y[[i]])[1]
			if (!is.null(nbl)) {
                    y[[i]]<-y[[i]][-nbl,]
               } else {
     			nbl<-length(y[[i]])
                    y[[i]]<-y[[i]][-nbl]
               }
               ytit[[i]]<-ytit[[i]][-nbl]
          }
		if (nbc==1) {
     		for (i in 1:nbtabs) {y[[i]]<-cbind(y[[i]])}
		}
	}
#
# ----- on ajoute le test en bout de table
#
	if (indp) {
		nomcol<-c(nomcol, "test/p/CA")
		for (i in 1:nbtabs) {
#              i<-1
#              i<-i+1
#               cat(i, "\n")
			nbl<-dim(y[[i]])[1];nbl
			if (is.null(nbl)) {
                    nbl<-1
                    y[[i]]<-c(y[[i]], "")
               } else {
                    y[[i]]<-cbind(y[[i]], "")
               }
			if (nbl>=3) {
     			y[[i]][1:3, nbc+1]<-c(yt[[i]])
			}
			if (nbl==2) {
     			y[[i]][1:2, nbc+1]<-c(paste(yt[[i]][1], yt[[i]][3]),yt[[i]][2])
			}
			if (nbl==1) {
     			y[[i]][nbc+1]<-paste(yt[[i]][1], yt[[i]][3],yt[[i]][2])
			}
		}
		nbc<-nbc+1
	}
#
# ----- mise en forme pour l'affichage
#
     if (nbtabs>1) {
     	lgt<-max(unlist(lapply(ytit, nchar)))
     	lgy<-lapply(y, nchar)
     	algy<-array(0, dim=c(length(lgy), nbc))
     	for (i in 1:length(lgy)) {
#               cat(i, "\n")
               if (!is.null(dim(lgy[[i]])[1])) {
                    algy[i,]<-apply(lgy[[i]], 2, max)
               } else {
                    algy[i,]<-lgy[[i]]
               }
     	}
          algy<-apply(algy, 2, max)
     } else {
     	lgt<-max(nchar(ytit[[1]]))
     	lgy<-nchar(y[[1]])
          if (!is.null(dim(lgy)[1])) {
               algy<-apply(lgy, 2, max)
          } else {
               algy<-lgy
          }
     }
     lgc<-nchar(nomcol)
     algy<-pmax(algy, lgc)

	titretab<-c(substring(blanc, 1, lgt), nomcol)
	st<-rep(2, nbc+1);st[1]<-99
	sb<-rep(3, nbtabs)
	side<-array(23, dim=c(nbtabs, nbc))
	lg<-c(lgt, algy)
	bt<-list()
	for (i in 1:(nbc+1)) {bt[[i]]<-Encadre(titretab[i], side=st[i], n=lg[i])}

	b<-list()
	b[[1]]<-paste(unlist(bt), collapse="")
	for (i in 1:nbtabs) {
		txt<-Encadre(ytit[[i]], side=sb[i], n=lgt)
		for (j in 1:nbc) {
#               cat(i, j, "\n")
               if (!is.null(dim(y[[i]])[1])) {
     			txt<-paste(txt, Encadre(y[[i]][,j], side=side[i,j], n=lg[j+1]), sep="")
               } else {
     			txt<-paste(txt, Encadre(y[[i]][j], side=side[i,j], n=lg[j+1]), sep="")
               }
		}
		b[[i+1]]<-txt
	}
	b<-lapply(b, sub, pattern=" +$", replace="")          #enl?ve les espaces ? la fin

#	lgt<-max(unlist(lapply(ytit, nchar)))
#	lgy<-max(unlist(lapply(y, nchar)))
#	lgy<-max(c(lgy, nchar(nomcol)))
#	sb<-rep(3, nbtabs)
#	side<-array(23, dim=c(nbtabs, nbc))

#	titretab<-c(substring(blanc, 1, lgt), nomcol)
#	st<-rep(2, nbc+1);st[1]<-99
#	lg<-rep(lgy, nbc+1);lg[1]<-lgt
#	bt<-list()
#	for (i in 1:(nbc+1)) {bt[[i]]<-Encadre(titretab[i], side=st[i], n=lg[i])}
#
#	b<-list()
#	b[[1]]<-paste(unlist(bt), collapse="")
#	for (i in 1:nbtabs) {
#		txt<-Encadre(ytit[[i]], side=sb[i], n=lgt)
#		for (j in 1:nbc) {
#			txt<-paste(txt, Encadre(y[[i]][,j], side=side[i,j], n=lgy), sep="")
#		}
#		b[[i+1]]<-txt
#	}
#	b<-lapply(b, sub, pattern=" +$", replace="")          #enl?ve les espaces ? la fin
#
# ----- affichage
#
	if (indtit) {cat(paste(Encadre(titre, side=0), collapse="\n"), "\n")}
	print(tabf, court=T)
	cat("\n",paste(unlist(b), collapse="\n"),"\n", sep="")

	invisible()
}
#
# ============================================================
# ============================================================
# description de var qual : Freq
# ============================================================
# ============================================================
#
Freqs<-function(data, x, na.rm=F,...) {
#data<-mm;x<-v;na.rm<-T
     appel<-match.call()

	ind<-match(x, names(data))
	if (all(is.na(ind))) {stop("\n\tVariables inexistantes\n\n")}

     x<-x[!is.na(ind)]
     ind<-ind[!is.na(ind)]
     if (length(ind)==1) {
     	stop("\n\tPour une seule variable utilisez Freq()\n\n")
     }

	ind<-match(x, names(data))
	tabs<-list()
	for (i in 1:length(x)) {
		tab<-Freq(data, x[i], na.rm)
		tabs[[i]]<-tab
	}
	names(tabs)<-x

     res<-list(tabs=tabs, Call=appel)
	class(res)<-"Freqs"
	res
}
Freq<-function(tab, x, na.rm=F,...) {
	ind<-match(x, names(tab))
	if (all(is.na(ind))) {stop("\n\tVariable inexistante\n\n")}

     x<-x[!is.na(ind)]
     ind<-ind[!is.na(ind)]
     if (length(ind)>1) {
     	stop("\n\tPour plusieurs variables utilisez Freqs()\n\n")
     }

	nomx<-x
	x<-Met.NA(tab[,nomx])
	if (!is.factor(x)) {
		x<-as.factor(x)
	}

	if (!na.rm) {
		if (any(is.na(x))) {
			xx<-c(x)
			nom<-levels(x)
			xx[is.na(xx)]<- nlevels(x)+1
			x<-factor(xx, 1:(nlevels(x)+1))
			levels(x)<-c(nom, "NAs")
		}
		yna<-NULL
		y<-table(x)
	} else {
		yna<-length(x[is.na(x)])
		y<-table(x)
	}

	ycum<-cumsum(y)
	yp<-y/sum(y)
	ypcum<-cumsum(y/sum(y))
	n<-length(y)
	nom<-names(y)
	tab<-as.data.frame(cbind(y, ycum, yp, ypcum))
	row.names(tab)<-nom
	names(tab)<-c("n","n cum", "p","p cum")

	tab<-list(tab=tab, nom=nomx, nNA=yna)
	tab$Call<-match.call()
	class(tab)<-"Freq"

	tab
}
print.Freqs<-function(x, ...) {

     print(x$Call)
     cat("\n")
     
     tabs<-x$tabs
     for (i in 1:length(tabs)) {
          print(tabs[[i]], ..., court=T)
          cat("\n")
     }
}
print.Freq<-function(x,...) {
	appel<-match.call()

	ind<-pmatch("cou", names(appel))
	court<-F
	if (!is.na(ind)) {court<-as.logical(eval(appel[[ind]]))[1]}
     if (is.na(court)) {court<-F}

	ind<-pmatch("cum", names(appel))
	cumul<-T
	if (!is.na(ind)) {
          cumul<-as.logical(eval(appel[[ind]]))[1]
     }
     if (is.na(cumul)) {cumul<-T}

	ind<-pmatch("dec", names(appel))
	dec<-2
	if (!is.na(ind)) {dec<-as.numeric(eval(appel[[ind]]))[1]}
     if (is.na(dec)) {dec<-2}

  ############ prise en compte des labels (DH)
  ## ind<-pmatch("lab", names(appel))
	## label <- F
	## if (!is.na(ind)) {label<-as.logical(eval(appel[[ind]]))[1]}
  ## if (is.na(label)) {label<-F}
  ############ (DH)

	if (!court) {print(x$Call)} else {cat("variable :", x$nom, "\n")}

	if (!is.null(x$nNA)) {
		if (!court) {cat("\n")} else {cat("\t")}
		if (x$nNA!=0) {
			if (x$nNA==1) {
				cat("Il y a",x$nNA,"valeur manquante")
			} else {
				cat("Il y a",x$nNA,"valeurs manquantes")
			}
		} else {
			cat("Il n'y a pas de valeur manquante")
		}
		cat("\n\n")
	}
	x<-x$tab
	x[,c(-1,-2)]<-	matrix(paste(format(round(as.matrix(x[, c(-1,-2)]) *100, dec)),"%", sep=""), ncol=2)
	
	if (!cumul) {
          x<-x[,c(-2, -4)]
     }
#	x[,c(-1,-2)]<-matrix(paste(format(round(x[,c(-1,-2)]*100,2)),"%", sep=""), ncol=2)

	print(x)
}

plot.Freq<-function(x,...) {
	appel<-match.call()

	n<-x$tab$n
	nb<-length(n)

#titre
	ind<-pmatch("tit", names(appel))
	if (!is.na(ind)) {
		titre<-as.character(eval(appel[[ind]]))
	} else {
		titre<-x$nom
	}
	titre<-titre[1]
#pourcentage
	ind<-pmatch("pour", names(appel))
	if (!is.na(ind)) {
		pourc<-as.logical(eval(appel[[ind]]))
	} else {
		pourc <-F
	}
	pourc <- pourc[1]
#etique
	ind<-pmatch("eti", names(appel))
	if (!is.na(ind)) {
		etique<-as.logical(eval(appel[[ind]]))
	} else {
		etique<-F
	}
	etique<-etique[1]
#histo
	ind<-pmatch("his", names(appel))
	if (!is.na(ind)) {
		histo<-as.logical(eval(appel[[ind]]))
	} else {
		histo<-F
	}
	histo<-histo[1]
#coul
	ind<-pmatch("coul", names(appel))
	if (!is.na(ind)) {
		coul<-as.vector(eval(appel[[ind]]))
	} else {
		coul<-1:nb
	}
	while(length(coul)<nb) {
		coul<-c(coul, coul)
	}
	coul<-coul[1:nb]
	
	p<- round(x$tab$p*100, 2)
	ptxt<-paste(format(p), "%",sep="")
	nom<- row.names(x$tab)

	letique<-rep("", nb)
	leg<-nom
	if (pourc) {
		ylab<-"%"
		letique<-ptxt
	} else {
		letique<-n
		ylab<-"effectif"
	}
	letique<-paste("(", letique,")", sep="")
	if (etique) {leg<-paste(leg, letique)}

	if (!histo) {
		pie(n, leg, density=5*(1:nb), angle=10*(1:nb), lwd=2, col=coul)
	} else {
		y<-n
		if (pourc) {y<-p}
		ylim<-c(0, max(y))
		yh<-ylim[2]/20
		ylim[2]<-ylim[2]+yh
		if (etique) {ylim[2]<-ylim[2]+yh}

		plot(1:nb, y, ylim=ylim, xlim=c(0, nb)+0.5,type="n", xlab="", ylab=ylab, xaxs="i", yaxs="i", axes=F, lwd=4)
		abline(h=0, v=0.5, lwd=4)
		axis(side=1, tick=T, at=1:nb, label=nom, adj=0.5, lwd=2)
		axis(side=2, adj=1., lwd=2)
		for (i in 1:nb) {
			xx<-c(-0.4,-0.4,0.4,0.4)+i
			yy<-c(0,y[i], y[i], 0)
			polygon(xx, yy, density=10, col=coul[i], lwd=2)
			if (etique) {text(i, y[i]+yh, adj=0.5, letique[i])}
		}
	}
	title(titre)

	invisible()
}
#
# ============================================================
# ============================================================
# description de var quant : Moyenne
# ============================================================
# ============================================================
#
Moys<-function(tab, v=NULL) {

	if(is.null(v[1])) {
		v<-names(tab)
	}

	ind<-match(v, names(tab))
	ind<-ind[!is.na(ind)]
	if (sum(ind)==0 || is.na(ind[1])) {stop("Pb de nom de var")}

     if (length(ind)==1) {
          stop("\n\tUtilisez Moy() pour une seule variable\n\n")
     }

	v<-v[!is.na(ind)]
	ind<-match(v, names(tab))
     tabs<-list()
     for (i in 1:length(v)) {
          tabs[[i]]<-Moy(tab, v[i])
     }
     names(tabs)<-v
	tabs$Call<-match.call()
     class(tabs)<-"Moys"
	tabs
}


Moy<-function(tab, v=NULL) {

	if(is.null(v[1])) {
		v<-names(tab)
	}

	ind<-match(v, names(tab))
	ind<-ind[!is.na(ind)]
	if (sum(ind)==0 || is.na(ind[1])) {stop("Pb de nom de var")}

     if (length(ind)>1) {
          stop("\n\tUtilisez Moys() pour plusieurs variables\n\n")
     }

	v<-names(tab)[ind[1]]
	nbv <- length(v)
	if (nbv==1) {
		tab<-list(tab[,ind])
	} else {
		tab<-as.list(tab[,ind])
	}
	names(tab)<-v

	tab2<-tab
	for (i in 1:nbv) {
		x<-tab[[i]]
		if (is.factor(x)) {
			x<-as.numeric(x)
			tab[[i]]<-x
		}
		tab2[[i]]<-x[!is.na(x)]
	}

	nom<-c("n","NA","min","Q1","Med","Q3","max","m","v", "e-t", "e-t de m")
	mat<-as.data.frame(matrix(0, ncol=nbv, nrow=length(nom)))

	i<-1
	mat[i,]<-unlist(lapply(tab, length))
	i<-i+1
	mat[i,]<- mat[(i-1),]-unlist(lapply(tab2, length))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, min, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, quantile, probs=0.25, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, quantile, probs=0.5, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, quantile, probs=0.75, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, max, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab, mean, na.rm=T))
	i<-i+1
	mat[i,]<-unlist(lapply(tab2, var))
	i<-i+1
	mat[i,]<-sqrt(mat[9, ])
	i<-i+1
	mat[i,]<-mat[10,]/sqrt(mat[1,]-mat[2,])	#e-t de m

	row.names(mat)<-nom
	names(mat)<-v

	tab<-list(Description=t(mat), var=v, Donnees=tab)
	tab$Call<-match.call()
	class(tab)<-"Moy"

	tab
}
print.Moy<-function(x, call=T, ...) {
	if (call) {
          print(x$Call)
     	cat("\n")
     }
	print(x$Description)
	
	invisible()
}
print.Moys<-function(x, call=T, concat=T, ...) {

	if (call) {
          print(x$Call)
     	cat("\n")
     }
     
     if (concat) {
          tabs<-x[[1]]$Description
          for (i in 2:(length(x)-1)) {
               tabs<-rbind(tabs, x[[i]]$Description)
          }
          print(tabs)
     } else {
          for (i in 1:(length(x)-1)) {
          	print(x[[i]], F)
          	cat("\n")
          }
     }
	invisible()
}
plot.Moy<-function(x, ...) {
	appel<-match.call()
	nbv<- length(x$var)

#titre
	ind<-pmatch("tit", names(appel))
	if (!is.na(ind)) {
		titre<-as.character(eval(appel[[ind]]))
		appel[[ind]]<-NULL
	} else {
		titre<-row.names(x$Description)
	}
	titre<-titre[1]

#verticale
	ind<-pmatch("ver", names(appel))
	if (!is.na(ind)) {
		vert<-as.logical(eval(appel[[ind]]))
	} else {
		vert<-T
	}
     if (vert) {mfrow<-c(2, 1)} else {mfrow<-c(1,2)}

	par(mfrow=mfrow)
	for (i in 1:nbv) {
		boxplot(x$Donnees[[i]])
	}
	for (i in 1:nbv) {
		z<-x$Description[i,]
		xx<-seq(z["min"], z["max"], l=100)
		m<-z["m"]
		ecart<-sqrt(z["v"])
		yh<-hist(x$Donnees[[i]], plot=F)
		yd<- dnorm(xx, m, ecart)
		ylim<-range(c(0, yh$density, yd))
		hist(x$Donnees[[i]], ylim=ylim, prob=T, xlab="", main="", ylab="Densit?")
		lines(xx, yd)
	}
     mtext(outer=T, side=3, adj=0.5, titre, line=-2, cex=1.5)

	invisible()
}
plot.Moys<-function(x, ...) {
#x<-m
	appel<-match.call()

     nbv<-length(x)-1
#titre
	ind<-pmatch("tit", names(appel))
	if (!is.na(ind)) {
		titre<-as.character(eval(appel[[ind]]))
		appel[[ind]]<-NULL
	} else {
		titre<-names(x)[1:nbv]
	}
	titre<-rep(titre, nbv)[1:nbv]

	mfrow<-c(2,nbv)

	par(mfrow=mfrow)
	for (i in 1:nbv) {
		boxplot(x[[i]]$Donnees[[1]])
		title(titre[i])
	}
	for (i in 1:nbv) {
#         i<-1
		z<-x[[i]]$Description
		nomz<-dimnames(z)[[2]]
		mz<-z[nomz=="min"];Mz<-z[nomz=="max"]
		xx<-seq(mz, Mz, l=100)
		m<-z[nomz=="m"]
		ecart<-sqrt(z[nomz=="v"])
		yh<-hist(x[[i]]$Donnees[[1]], plot=F)
		yd<- dnorm(xx, m, ecart)
		ylim<-range(c(0, yh$density, yd))
		hist(x[[i]]$Donnees[[1]], ylim=ylim, prob=T, xlab="", main="", ylab="Densit?")
		lines(xx, yd)
	}

	invisible()
}

plot.Freqs<-function(x,...) {
	appel<-match.call()

  n<-length(x$tabs)
#titre
	ind<-pmatch("tit", names(appel))
	if (!is.na(ind)) {
		titre<-as.character(eval(appel[[ind]]))
		appel[[ind]]<-NULL
	} else {
		titre<-names(x$tabs)
	}
	titre<-rep(titre, n)[1:n]

#mfrow
	ind<-pmatch("mfr", names(appel))
	if (!is.na(ind)) {
		mfrow<-as.numeric(eval(appel[[ind]]))
	} else {
		mfrow<-c(1,1)
	}

  a<-as.list(appel[-1])

  par(mfrow=mfrow)
  for (i in 1:length(x$tabs)) {
    a$x<-quote(x$tabs[[i]])
    a$titre<-titre[i]
    do.call("plot.Freq", a)
  }
  invisible(appel)
}
#
# ============================================================
# ============================================================
# Comparaison de Moyenne
# ============================================================
# ============================================================
#
CompMoy<-function(tab, x, g, app=F) {
#tab<-d;x<-"x";g<-"g";app<-F
#tab<-sleep;x<-"extra";g<-"group";app<-F

	indx<-match(x, names(tab))
	indg<-match(g, names(tab))
	ind<-indx+indg
	if (all(is.na(ind))) {stop("Pb de nom de var")}

	x<-x[!is.na(ind)]
	g<-g[!is.na(ind)]
	ind<-ind[!is.na(ind)]
	if (length(ind)>1) {
		stop("\n\tUtilisez CompMoys() pour plusieurs variables\n\n")
	}

	indx<-match(x, names(tab))
	indg<-match(g, names(tab))
	nom<-c(x, g)

	xx<-tab[,indx]
	gg<-tab[,indg]
#	xx<-Met.NA(tab[,indx])
#	gg<-Met.NA(tab[,indg])

	x<-xx[!is.na(gg) & !is.na(xx)]
	g<-gg[!is.na(gg) & !is.na(xx)]
	nn<-length(xx)
	n<-length(x)
	nbna<-nn-n
#
#non appari?
#
	if (!app) {
          tab<-tapply(x, g, c)
          nbvg<-length(tab)
		if (nbvg<=1) {stop("Un seul groupe !")}

          ntab<-sapply(tab, length)
          if (any(ntab==0)) {
               tab2<-list()
               k<-0
               for (i in 1:nbvg) {
                    if (ntab[i]>0) {
                         k<-k+1
                         tab2[[k]]<-tab[[i]]
                    }
               }
               tab<-tab2
               valg<-names(tab)
               nbvg<-length(tab)
          }
		if (nbvg<=1) {stop("Un seul groupe !")}

          tab[[nbvg+1]]<-x
          names(tab)[nbvg+1]<-"Tous"
	} else {
		tab<-list(x1=x, x2=g, d=x-g)
		names(tab)<- c(nom, paste(nom[1],"-", nom[2],sep=""))
	}
#
# les descriptions
#
	n<-sapply(tab, length)
	xmin<-sapply(tab, range)
	xmax<-xmin[2,]
	xmin<-xmin[1,]
	xmed<-sapply(tab, median)
	m<-sapply(tab, mean)
	s<-sapply(tab, sd)
	v<-s^2

	des<-as.data.frame(cbind(n, xmin, xmed, xmax, m, v, s, s/sqrt(n)))
	names(des)<-c("n","min","mediane", "max","m","v","e-t","e-t de m")
	row.names(des)<-names(tab)
	
#	n<-n[-length(n)]   #j'enl?ve le n total
#	v<-v[-length(v)]
#
#Bartlett
#
	testb<-NULL
#	if (!app) {testb<-Bartlett(v, n)}
	if (!app) {testb<-bartlett.test(x, g)}
#
# anova
#
	testa<-NULL
	if (!app) {	testa<-summary(aov(x~factor(g)))}
#
#Kruskal-Wallis
#
	kw<-NULL
	if (!app) {
#		kw<-kruskal.test(list(x=x, g=g))
		kw<-kruskal.test(x, g)
          kw<-list(test=kw$stat, ddl=kw$param, p=kw$p.v)
	}
#
# t-test et t'
#
	testt<-NULL
	testtp<-NULL
	if (!app) {
		if (nbvg==2) {
#t.test(extra ~ group, data = sleep, var.equal=T)
			ddlres<-sum(n[-3]-1)
			res<-sum(v[-3]*(n[-3]-1))/ddlres
			vt<-sum(res/n[-3])
			test<-as.numeric(abs((m[1]-m[2])/sqrt(vt)))
			p<-2*(1-pt(test, ddlres))
			testt<-list(t=test, res=res, vt=vt, ddl=ddlres, p=p)

			vsurn<-(v/n)[-3]
			res<-NA
			vt<-sum(vsurn)
			ddlres<-ceiling((vt^2)/sum((vsurn^2)/(n[-3]-1)))
			test<-as.numeric(abs((m[1]-m[2])/sqrt(vt)))
			p<-2*(1-pt(test, ddlres))
			testtp<- list(t=test, res=res, vt=vt, ddl=ddlres, p=p)
		}
	} else {
		ddlres<-(n[3]-1)
		res<-v[3]
		test<-abs((m[3])/sqrt(sum(res/n[3])))
		p<-2*(1-pt(test, ddlres))
		testt<-list(t=test, res=res, ddl=ddlres, p=p)
	}

	tab<-list(Donnees=tab, Description=des, nom=nom, nNA=nbna, anova=testa, student=testt, tprime=testtp, kruskal=kw, bartlett=testb, apparie=app)
	tab$Call<-match.call()
	class(tab)<-"CompMoy"

	tab
}
CompMoy.old<-function(tab, x, g, app=F) {
#tab<-a;x<-"psa";g<-"ca20";app<-F

	indx<-match(x, names(tab))
	indg<-match(g, names(tab))
	ind<-indx+indg
	if (all(is.na(ind))) {stop("Pb de nom de var")}

	x<-x[!is.na(ind)]
	g<-g[!is.na(ind)]
	ind<-ind[!is.na(ind)]
	if (length(ind)>1) {
		stop("\n\tUtilisez CompMoys() pour plusieurs variables\n\n")
	}

	indx<-match(x, names(tab))
	indg<-match(g, names(tab))
	nom<-c(x, g)

	xx<-tab[,indx]
	gg<-tab[,indg]
#	xx<-Met.NA(tab[,indx])
#	gg<-Met.NA(tab[,indg])

	x<-xx[!is.na(gg) & !is.na(xx)]
	g<-gg[!is.na(gg) & !is.na(xx)]
	nn<-length(xx)
	n<-length(x)
	nbna<-nn-n
#
#non appari?
#
	if (!app) {
		if (!is.factor(g)) {g<-factor(g)}
		tabg<-table(g)
		valg<-names(tabg[tabg>0])
#		tabg<-table(g)
#		valg<-names(tabg[tabg>0])
#		if (is.numeric(g)) {
#			valg<-as.numeric(valg)
#		}
		nbvg<-length(valg)
		if (nbvg<=1) {stop("Un seul groupe !")}
#
# les donn?es par groupe
#
		tab<-list(deb=1)
		for (i in 1:nbvg) {
			tab[[i]]<-x[g==valg[i]]
		}
		names(tab)<-valg
	} else {
		tab<-list(x1=x, x2=g, d=x-g)
		names(tab)<- c(nom, paste(nom[1],"-", nom[2],sep=""))
	}
#
# les description
#
	n<-unlist(lapply(tab, length))
	xmin<-unlist(lapply(tab, min))
	xmax<-unlist(lapply(tab, max))
	xmed<-unlist(lapply(tab, median))
	m<-unlist(lapply(tab, mean))
	v<-unlist(lapply(tab, var))

	des<-as.data.frame(cbind(n, xmin, xmed, xmax, m, v, sqrt(v), sqrt(v/n)))
	names(des)<-c("n","min","mediane", "max","m","v","e-t","e-t de m")
	row.names(des)<-names(tab)
#
#Bartlett
#
	testb<-NULL
#	if (!app) {testb<-Bartlett(v, n)}
	if (!app) {testb<-bartlett.test(x, g)}
#
# anova
#
	testa<-NULL
	if (!app) {	testa<-summary(aov(x~factor(g)))}
#
#Kruskal-Wallis
#
	kw<-NULL
	if (!app) {
#		kw<-kruskal.test(list(x=x, g=g))
		kw<-kruskal.test(x, g)
          kw<-list(test=kw$stat, ddl=kw$param, p=kw$p.v)
	}
#
# t-test et t'
#
	testt<-NULL
	testtp<-NULL
	if (!app) {
		if (nbvg==2) {
			ddlres<- sum(n-1)
			res<-sum(v*(n-1))/sum(n-1)
			test<-abs((m[1]-m[2])/sqrt(sum(res/n)))
			p<-2*(1-pt(test, ddlres))
			testt<-list(t=test, res=res, ddl=ddlres, p=p)

			vsurn<-v/n
			res<-sum(vsurn)
			ddlres<-ceiling((res^2)/sum((vsurn^2)/(n-1)))
			test<-abs((m[1]-m[2])/sqrt(res))
			p<-2*(1-pt(test, ddlres))
			testtp<- list(t=test, res=res, ddl=ddlres, p=p)
		}
	} else {
		ddlres<-(n[3]-1)
		res<-v[3]
		test<-abs((m[3])/sqrt(sum(res/n[3])))
		p<-2*(1-pt(test, ddlres))
		testt<-list(t=test, res=res, ddl=ddlres, p=p)
	}

	tab<-list(Donnees=tab, Description=des, nom=nom, nNA=nbna, anova=testa, student=testt, tprime=testtp, kruskal=kw, bartlett=testb, apparie=app)
	tab$Call<-match.call()
	class(tab)<-"CompMoy"

	tab
}
CompMoys<-function(tab, x, g, app=F) {

	appel<-match.call()

	indx<-match(x, names(tab))
	if (all(is.na(indx))) {stop("Pb de nom de var x")}
	indg<-match(g, names(tab))
	if (all(is.na(indg))) {stop("Pb de nom de var g")}

	x<-x[!is.na(indx)]
	g<-g[!is.na(indg)]
     nx<-length(x)
     ng<-length(g)
     if (nx==1 & ng==1) {
          stop("\n\tUtilisez CompMoys() pour une seule variable\n\n")
     }

     if (nx!=1 & ng!=1 & !app) {
          stop("\n\tSoit 1 variable x, soit une variable g\n\n")
     }
     if (nx!=ng & app) {
          stop("\n\tIl faut autant de variable x que de variable g pour app=T\n\n")
     }

     if (app) {
          tabs<-list()
          for (i in 1:nx) {
               tabs[[i]]<-CompMoy(tab, x[i], g[i], app=T)
          }
          names(tabs)<-paste(x, g, sep="*")
          tabs$comparaison<-list(type="appari?e", x=x, g=g)
     } else {
          if (nx==1) {
               tabs<-list()
               for (j in 1:ng) {
                    tabs[[j]]<-CompMoy(tab, x, g[j], app=F)
               }
               names(tabs)<-paste(x, g, sep="*")
               tabs$comparaison<-list(type="unevarquant", x=x, g=g)
          } else {
               tabs<-list()
               for (i in 1:nx) {
                    tabs[[i]]<-CompMoy(tab, x[i], g, app=F)
               }
               names(tabs)<-paste(x, g, sep="*")
               tabs$comparaison<-list(type="unevarqual", x=x, g=g)
          }
     }
     tabs$Call<-appel
     
     class(tabs)<-"CompMoys"
     tabs
}
print.CompMoy<-function(x, ...) {
	arg<-match.call()

	a<-T
	i<-pmatch("ano", names(arg))
	if (!is.na(i)) {a<-eval(arg[[i]])}

	k<-T
	i<-pmatch("krus", names(arg))
	if (!is.na(i)) {k<-eval(arg[[i]])}

	s<-T
	i<-pmatch("stu", names(arg))
	if (!is.na(i)) {s<-eval(arg[[i]])}

	b<-T
	i<-pmatch("bar", names(arg))
	if (!is.na(i)) {b<-eval(arg[[i]])}

	de<-T
	i<-pmatch("desc", names(arg))
	if (!is.na(i)) {de<-eval(arg[[i]])}

	tot<-T
	i<-pmatch("tot", names(arg))
	if (!is.na(i)) {tot<-eval(arg[[i]])}

     if(!a & !k & !s & !b & !de) {de<-T}
     
	print(x$Call)
	cat("\n")
	if (x$nNA==0) {
		cat("Pas de valeur manquante\n")
	} else {
		cat("Il y a", x$nNA,"valeur(s) manquante(s)\n")
	}
     if (de) {
     	cat("\nDescription\n")
     	if (!tot) {
          	print(x$Description[-dim(x$Description)[1],])
     	} else {
          	print(x$Description)
          }
     }
	if (!x$apparie) {
          if (a) {
	         cat("\nTableau d'analyse de variance\n")
	    	    print(x$anova)
          }
	    	
          if (k) {
     		cat("\nKruskal-Wallis\n")
     		test<-unlist(x$kruskal)
     		names(test)<-c("test","ddl","p")
     		print(test)
          }
          
          if (s) {
     		if (!is.null(x$student)) {
     			cat("\nStudent\n")
     			test<-unlist(x$student)
     			names(test)<-c("t","res","vt","ddl","p")
     			print(test)

     			cat("\nStudent prime\n")
     			test<-unlist(x$tprime)
     			names(test)<-c("t","res","vt","ddl","p")
     			print(test)
     		}
          }

          if (b) {
     		cat("\nBartlett : egalite des variances\n")
#		test<-unlist(x$bartlett)[1:4]
#		names(test)<-c("test","res","ddl","p")
     		test<-x$bartlett
     		print(test)
          }
     		
	} else {
          if (s) {
     		cat("\nTest apparie\n")
     		cat("\nStudent\n")
     		test<-unlist(x$student)
     		names(test)<-c("t","res","ddl","p")
     		print(test)
          }
	}

	invisible()
}
print.CompMoys<-function(x, dec=3,...) {
  #x<-x2
	arg<-match.call()
  
	inda<-T
	i<-pmatch("ano", names(arg))
	if (!is.na(i)) {inda<-eval(arg[[i]])}
  
	indk<-T
	i<-pmatch("krus", names(arg))
	if (!is.na(i)) {indk<-eval(arg[[i]])}
  
	inds<-T
	i<-pmatch("stu", names(arg))
	if (!is.na(i)) {inds<-eval(arg[[i]])}
  
	indb<-T
	i<-pmatch("bar", names(arg))
	if (!is.na(i)) {indb<-eval(arg[[i]])}

	indde<-T
	i<-pmatch("desc", names(arg))
	if (!is.na(i)) {indde<-eval(arg[[i]])}

	indtot<-T
	i<-pmatch("tot", names(arg))
	if (!is.na(i)) {indtot<-eval(arg[[i]])}

  if(!inda & !indk & !inds & !indb & !indde) {indde<-T}
  
  n<-length(x)-2
  
  app<-F
  c<-x$comparaison
  
  if (x$comparaison$type=="unevarqual") {
    cat("Variable qualitative : ", c$g, "\n\n", sep="")
    boucle<-c$x
  }
  if (x$comparaison$type=="unevarquant") {
    cat("Variable quantitative : ", c$x, "\n\n", sep="")
    boucle<-c$g
  }
  if (x$comparaison$type=="appari?e") {
    cat("Appariement 2 ? 2\n\n", sep="")
    boucle<-c$x
    aboucle<-c$g
    app<-T
  }
  
  nNA<-unlist(lapply(x, function(x) {x$nNA}))
  if (any(nNA>0)) {
    Mq<-rep("", length(nNA));Mq[1]<-"Manquants : "
    #txt<-array(0, dim=c(length(x), 2))
    txt<-array(0, dim=c(length(nNA), 2))
    txt[, 1]<-Mq;txt[,2]<-paste(names(nNA), nNA, sep="=")
    txt<-apply(txt, 2, format)
    txt<-paste(apply(txt, 1, paste, collapse=""), collapse="\n")
    cat(txt, "\n\n", sep="")
  } else {
    cat("Il n'y a pas de valeurs NA\n")
  }
  
  if (indde) {
    num<-NULL
    for (i in 1:n) {
      d<-x[[i]]$Description
      if (!indtot) {
        d<-d[-dim(d)[1],]
      }
      tab<-d
      tab$groupe<-row.names(tab)
      if (app) {
        tab$var<-c(boucle[i],aboucle[i], "diff")
        tab<-tab[, c("var", names(d))]
      } else {
        tab$var<-rep("", dim(tab)[1])
        tab$var[1]<-boucle[i]
        tab<-tab[, c("var", "groupe", names(d))]
      }
      num<-c(num, rep(i, dim(tab)[1]))
      if (i==1) {
        TAB<-tab
      } else {
        TAB<-rbind(TAB, tab)
      }
    }
    row.names(TAB)<-1:dim(TAB)[1]
    TAB[, -1:-2]<-apply(TAB[, -1:-2], 2, function(x, dec=1) {format(round(x, dec))}, dec=dec)
    TAB<-rbind(names(TAB),as.matrix(TAB))
    TAB<-apply(TAB, 2, format)
    a<-apply(TAB, 1, paste, collapse=" | ")
    
    # a<-capture.output(print(TAB))
    na<-max(nchar(a))
    tiret<-paste(rep("-", na), collapse="")
    a<-tapply(a, c(0,num), c)
    for (i in 1:length(a)) {
      cat(" ", paste(a[[i]], collapse="\n "), "\n", " ", tiret,"\n", sep="")
    }
  }
  
  if (!app) {
    if (indb) {
      for (i in 1:n) {
        b<-x[[i]]$bartlett
        tab<-data.frame(B=b$stat, ddl=b$par, p=b$p.value)
        if (i==1) {
          B<-tab
        } else {
          B<-rbind(B, tab)
        }
      }
      row.names(B)<-boucle
      cat("\nTest de Bartlett\n --------------\n")
      print(B)
    }
  }
  
  
  if (!app) {
    if (inda) {
      cat("\nAnova\n ---\n")
      for (i in 1:n) {
        v<-paste(x[[i]]$nom, collapse=" x ")
        nv<-nchar(v)
        a<-capture.output(x[[i]]$anova)
        
        b<-rep(substring("                                               ", 1, nv), length(a))
        b[1]<-v
        a<-paste(b, a)
        cat(paste(a, collapse="\n"), "\n\n")
      }
    }
  }
  
  if (inds) {
    it<-0
    for (i in 1:n) {
      t<-x[[i]]$student
      if (!is.null(t)) {
        it<-it+1
        tab<-as.data.frame(t)
        if (it==1) {
          B<-tab
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        } else {
          B<-rbind(B, tab)
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        }
      }
    }
    if (it>0) {
      cat("\nStudent\n -----\n")
      print(B)
    }
  }
  
  if (inds) {
    it<-0
    for (i in 1:n) {
      t<-x[[i]]$tprime
      if (!is.null(t)) {
        it<-it+1
        tab<-as.data.frame(t)
        if (it==1) {
          B<-tab
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        } else {
          B<-rbind(B, tab)
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        }
      }
    }
    if (it>0) {
      cat("\nt prime\n -----\n")
      print(B)
    }
  }
  
  if (indk) {
    it<-0
    for (i in 1:n) {
      t<-x[[i]]$kruskal
      if (!is.null(t)) {
        it<-it+1
        tab<-as.data.frame(t)
        if (it==1) {
          B<-tab
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        } else {
          B<-rbind(B, tab)
          row.names(B)[it]<-paste(x[[i]]$nom, collapse=" x ")
        }
      }
    }
    if (it>0) {
      cat("\nKruskal-Wallis\n ------------\n")
      print(B)
    }
  }
  
  invisible()
}
plot.CompMoys<-function(x, nuage=F, titre=NULL, titres=NULL, ...) {
     n<-length(x)-2
     
     if (is.null(titre) | length(titre)!=n) {
          titre<-rep(" ", n)
          for (i in 1:n) {
               titre[i]<-paste(x[[i]]$nom, collapse=" x ")
          }
     }
     if (is.null(titres) | length(titres)!=n) {
          titres<-list()
          for (i in 1:n) {
               titres[[i]]<-names(x[[i]]$Donnees)
          }
     }
     for (i in 1:n) {
          plot(x[[i]], titre=titre[i], titres=titres[[i]])
     }
}
plot.CompMoy<-function(x, nuage=F, titre=NULL, titres=NULL, ...) {
#tab<-m
	nbvg<- length(x$Donnees)
	xmin<-min(x$Description$min)
	xmax<-max(x$Description$max)
	xlim<-c(xmin, xmax)
	
     if (is.null(titre)) {
          titre<-paste(x$nom, collapse=" x ")
     }
     if (is.null(titres)) {
          titres<-names(x$Donnees)
     }
#
#boxplot et histo
#
	par(mfrow=c(2,nbvg))
	for (i in 1:nbvg) {
		boxplot(x$Donnees[[i]], ylim=xlim, col="lightblue")
	}
	xx<-seq(xmin, xmax, l=100)
	z<- x$Description
	for (i in 1:nbvg) {
		m<-z[i,"m"]
		ecart<- z[i,"e-t"]
		yh<-hist(x$Donnees[[i]],  plot=F)
		yd<- dnorm(xx, m, ecart)
		y<-yh$density
		ylim<-range(c(0, y, yd))
		hist(x$Donnees[[i]], ylim=ylim, prob=T, xlab="", xlim=xlim,
main="", col="lightblue")
		lines(xx, yd, col="blue")
		title(titres[i])
	}
#
#"nuage"
#
	if (nuage) {
		par(mfrow=c(1,1))
		z<- x$Description
		ylim<-xlim
		xlim<-c(0, nbvg)+0.5
		xh<-0.10
		plot(xlim, ylim, type="n", xlab=x$nom[2], ylab= x$nom[1],xaxt="n", lwd=4)
		axis(side=1, at=1:nbvg, paste(titres,"\n(",z[,"n"],")", sep=""), adj=0.5,lwd=2)
		for (i in 1:nbvg) {
			y<-x$Donnees[[i]]
			xx<-rep(i, z[i,"n"])
			points(xx,y, cex=0.75)

			zlim<-qt(c(0.025,0.5, 0.975), z[i,"n"]-1)
			ic<- z[i, "m"]+zlim*z[i, "e-t"]
			lines(rep(i, 3), ic, col="blue", lwd=2)
			for (j in 1:3) {lines(i+c(-1,1)*xh, c(ic[j], ic[j]), col="blue", lwd=2)}
		}
	}

     mtext(outer=T, side=3, adj=0.5, line=-2, titre, cex=1.5)

	invisible()
}
#
# =====================================================================================
# Trucs sur les Dates
# =====================================================================================
#
# ------------------------------------------------------------------------------------- 
# ----- fait un joli summary des variables de class Date
#
summary.Date<-function(object, ...) {
	x<-na.omit(object)
	nx<-length(x)
	nd<-length(object)
	xn<-as.numeric(x)

	q<-quantile(xn)
	q<-c(q[1:3], mean(xn), q[-1:-3])
	class(q)<-"Date"
	q<-as.character(q)
	if (nx!=nd) {
		q<-c(q, nd-nx)
		names(q)<-c("Min.", "1st Qu.", "Median", "Mean",  "3rd Qu.", "Max.", "NA's")
	} else {
		names(q)<-c("Min.", "1st Qu.", "Median", "Mean",  "3rd Qu.", "Max.")
	}
	print(q, quote=F)
}
#
# ------------------------------------------------------------------------------------- 
# ----- Min & Max des plusieurs dates
#
MinDate<-function(data, vdate, m=72000) {
#ow<-options(warn=(-1))
     d<-as.list(data[, vdate])
     d<-cbind(sapply(d, as.numeric), m)
     d<-apply(d, 1, min, na.rm=T)
     d[d==m]<-NA
     class(d)<-"Date"
#options(ow)
     return(d)
}
MaxDate<-function(data, vdate, m=-72000) {
#ow<-options(warn=(-1))
     d<-as.list(data[, vdate])
     d<-cbind(sapply(d, as.numeric), m)
     d<-apply(d, 1, max, na.rm=T)
     d[d==m]<-NA
     class(d)<-"Date"
#options(ow)
     return(d)
}
#
# ===========================================================================
# ===========================================================================
# ===== Les fonctions de seuillage
# ===========================================================================
# ===========================================================================
#
# les coupures classes sont <= vs >
#
Seuil<-function(tab, ...) {
	appel<-match.call()
	indseuil<-0
#
# groupe
#
	ind<-pmatch("gr", names(appel))
	if (!is.na(ind)) {
		groupe<- as.character(eval(appel[[ind]]))
		groupe<-groupe[1]
	} else {
		stop("Il manque la variable groupe")
	}
#
# survie ou cox
#
	indsurvie<-0
#delai
	ind<-pmatch("del", names(appel))
	if (!is.na(ind)) {
		delai<- as.character(eval(appel[[ind]]))
		delai<-delai[1]
		indsurvie<- indsurvie+1
	}
#etat
	ind<-pmatch("eta", names(appel))
	if (!is.na(ind)) {
		etat<- as.character(eval(appel[[ind]]))
		etat<-etat[1]
		indsurvie<- indsurvie+1
	}
	if (indsurvie==2) {indseuil<-1}
#cox
	ind<-pmatch("cox", names(appel))
	cox<-F
	if (!is.na(ind)) {
		cox<-as.logical(eval(appel[[ind]]))
		cox<-cox[1]
	}
	if (indsurvie==2 && cox) {indseuil<-4}
#
# student
#
	indstudent<-0
#x
	ind<-match("x", names(appel))
	if (!is.na(ind)) {
		x<- as.character(eval(appel[[ind]]))
		x<-x[1]
		indstudent<- 1
	}
	if (indstudent==1 && indseuil>0) {stop("Quel est le Seuil choisi ?")}
	if (indstudent==1) {indseuil<- 2}
#
# khi2
#
	indkhi2<-0
#obs
	ind<-match("obs", names(appel))
	if (!is.na(ind)) {
		obs<- as.character(eval(appel[[ind]]))
		obs<-obs[1]
		indkhi2<- 1
	}
	if (indkhi2==1 && indseuil>0) {stop("Quel est le Seuil choisi ?")}
	if (indkhi2==1) {indseuil<-3}
#
# un seul seuil
#
	if (indseuil==0) {stop("Quel est le Seuil choisi ?")}
#
# les noms des variables
#
	nomvar<-switch(indseuil, c(delai, etat, groupe), c(x, groupe), c(obs, groupe), c(delai, etat, groupe))

	ind<-match(nomvar, names(tab))
	if (is.na(sum(ind))) {stop("Pb dans le nom des variables")}
	tab<-switch(indseuil,
		{	delai<-tab[,ind[1]]
			etat<-tab[,ind[2]]
			groupe<-tab[,ind[3]]
			somme<-delai+etat+groupe
			data.frame(delai, etat, groupe, somme)
		},
		{	x<-tab[,ind[1]]
			groupe<-tab[,ind[2]]
			somme<-x+groupe
			tab<-data.frame(x, groupe, somme)
		},
		{	obs<-tab[,ind[1]]
			groupe<-tab[,ind[2]]
			somme<-obs+groupe
			tab<-data.frame(obs, groupe, somme)
		},
		{	delai<-tab[,ind[1]]
			etat<-tab[,ind[2]]
			groupe<-tab[,ind[3]]
			somme<-delai+etat+groupe
			data.frame(delai, etat, groupe, somme)
		})
	tab<-tab[!is.na(tab$somme),]
	tab$somme<-NULL
#
# les bornes
#
	ind<-pmatch("bor", names(appel))
	if (!is.na(ind)) {
		borne<-as.vector(eval(appel[[ind]]))
	} else {
		borne<-quantile(tab$groupe, seq(0.01, 0.99, by=0.01))
	}
	minmax<-range(tab$groupe)
	if (min(borne)>minmax[1]) {borne<-c(minmax[1], borne)}
	if (max(borne)<minmax[2]) {borne<-c(borne, minmax[2])}
	borne<-borne[borne>=minmax[1] & borne<=minmax[2]]
	if (length(borne)==0) {stop("Pb dans les bornes")}
	borne<-sort(unique(borne))
#
#seuil
#
	seuil<-switch(indseuil,
		seuil.lr(tab, borne, "delai", "etat", "groupe"),
		seuil.student(tab, borne, "groupe", "x"),
		seuil.khi2(tab, borne, "obs", "groupe"),
		seuil.cox(tab, borne, "delai", "etat", "groupe"))

	if (indseuil==3) {
		nom1<-dimnames(seuil$z)[[1]]
		nom1<-paste(nomvar[1],substring(nom1,4,nchar(nom1)), sep="")
		nom2<-dimnames(seuil$z)[[2]]
		nom2<-paste(nomvar[2],substring(nom2,7,nchar(nom2)), sep="")
		dimnames(seuil$z)<-list(nom1, nom2)
	}


	x<-list(seuil=seuil, don=tab, bi=borne, var=nomvar, type=indseuil)
	x$Call<-match.call()
	class(x)<-"Seuil"

	invisible(x)
}
plot.Seuil<-function(x,...) {
#tab<-xs
	appel<-match.call()

#couleur
     coul<-c("blue", "lightblue")
	ind<-pmatch("col", names(appel))
	if (!is.na(ind)) {
		coul<-as.character(eval(appel[[ind]]))
		coul<-rep(coul, 2)
		coul<-coul[1:2]
	}

#beta
	ind<-pmatch("bet", names(appel))
	beta<-F
	if (!is.na(ind)) {
		beta<-as.logical(eval(appel[[ind]]))
		beta<-beta[1]
	}
#rr
	ind<-pmatch("rr", names(appel))
	rr<-F
	if (!is.na(ind)) {
		rr<-as.logical(eval(appel[[ind]]))
		rr<-rr[1]
	}
#or
	ind<-pmatch("or", names(appel))
	or<-F
	if (!is.na(ind)) {
		or<-as.logical(eval(appel[[ind]]))
		or<-or[1]
	}
#roc
	ind<-pmatch("roc", names(appel))
	roc<-F
	if (!is.na(ind)) {
		roc<-as.logical(eval(appel[[ind]]))
		roc<-roc[1]
	}

	par(mfrow=c(2,2))
	xlim<-range(x$don$groupe)
	nomvar<-x$var
	type<-x$type

     titres<-rep("", 3)
     yab0<-NA

	xx<-x$bi
	switch(type,
		{	p<-x$seuil$p
			ylim.p<-range(c(0.05,p), na.rm=T)
			xlab<-nomvar[3]
			if (rr) {
				y<-x$seuil$RR
				ylim.y<-range(c(0, 1, y), na.rm=T)
				ylab<-c("RR","p")
				yab<-c(0.05, NA)
				titres<-c("RR", "p", "population")
				yab0<-1
			} else {
				y<-x$seuil$khi2
				ylim.y<-range(c(0, 3.841, y), na.rm=T)
				ylab<-c("Logrank","p")
				yab<-c(0.05, 3.841)
				yab0<-NA
				titres<-c("Logrank", "p", "population")
			}
			titre<-paste("Recherche de Seuil\nLogrank\nSurv(",nomvar[1], ",", nomvar[2],")~", nomvar[3], sep="")
		},
		{ 	y<-x$seuil$tab$t
			p<-x$seuil$tab$p
			xlab<-nomvar[2]
			tlim<- qt(c(0.025, 0.975), x$seuil$ddl)
			ylim.y<-range(c(0, tlim, y), na.rm=T)
			ylim.p<-range(c(0.05,p), na.rm=T)
			ylab<-c("Student", "p")
			yab<-c(0.05,tlim)
			titre<-paste("Recherche de Seuil\nStudent\n",nomvar[1], "~", nomvar[2], sep="")
			yab0<-0
			titres<-c("Student", "p", "population")
		},
		{
			p<- x$seuil$tab.khi2$p
			ylim.p<-range(c(0.05,p), na.rm=T)
			xlab<-nomvar[2]
			if (roc) {
				xxx<- 1-x$seuil$tab.SeSp$Sp
				yy<- x$seuil$tab.SeSp$Se
				xxxlab<-"1 - Sp"
				yylab<-"Se"
     			ylab<-c("", "p")
     			titres<-c("ROC", "p", "population")
     			yab0<-NA
     			yab<-c(0.05, NA)
			} else {
     			x$seuil <-x$seuil$tab.khi2
     			if (or) {
     				y<-x$seuil$OR
     				ylim.y<-range(c(0, 1, y), na.rm=T)
     				ylab<-c("OR", "p")
     				yab<-c(0.05, NA)
     				yab0<-1
          			titres<-c("OR", "p", "population")
     			} else {
     				y<-x$seuil$khi2
     				ylim.y<-range(c(0, 3.841, y), na.rm=T)
     				ylab<-c("Khi2", "p")
     				yab<-c(0.05, 3.841)
     				yab0<-NA
          			titres<-c("Khi2", "p", "population")
                    }
			}
			titre<-paste("Recherche de Seuil\nKhi2\n",nomvar[1], "~", nomvar[2], sep="")
		},
		{	p<-x$seuil$p
			ylim.p<-range(c(0.05,p), na.rm=T)
			xlab<-nomvar[3]
			y<-x$seuil$z
			ylim.y<-range(c(0, 3.841,y), na.rm=T)
			ylab<-c("Wald","p")
			yab<-c(0.05, -1.96, 1.96)
			yab0<-0
			titres<-c("Wald", "p", "population")
			if (rr) {
				y<-x$seuil$RR
				ylim.y<-range(c(0, 1, y), na.rm=T)
				ylab<-c("RR","p")
				yab<-c(0.05, 1)
     			titres<-c("RR", "p", "population")
			}
			if (beta) {
				y<-x$seuil$beta
				ylim.y<-range(c(0, y), na.rm=T)
				ylab<-c("beta","p")
				yab<-c(0.05, 0)
     			titres<-c("Beta", "p", "population")
			}
			titre<-paste("Recherche de Seuil\nCox\nSurv(",nomvar[1], ",", nomvar[2],")~", nomvar[3], sep="")
		})

	if (roc && type==3) {
		plot(xxx, yy, xlim=c(0,1), ylim=c(0,1), xlab=xxxlab, ylab=yylab, lwd=2, type="b", col=coul[1])
		abline(a=0, b=1, col=coul[2])
	} else {
		plot(xx, y, xlim=xlim, xlab=xlab, ylim=ylim.y, ylab=ylab[1], lwd=2, type="b", col=coul[1])
		abline(h=yab[-1])
		abline(h=yab0, col="lightblue")
	}
	title(titres[1])
	plot(xx, p, xlim=xlim, xlab=xlab, ylim=ylim.p, ylab=ylab[2], lwd=2, type="b", col=coul[1])
	title(titres[2])
	abline(h=yab[1])

	hist(x$don$groupe, nclass=10, xlab=xlab, prob=T, ylab="%", main="", col=coul[2])
	title(titres[3])

	plot(c(0,1), c(0,1), type="n", axes=F, xlab="", ylab="")
	text(0.5,0.8, adj=0.5, cex=2,titre)

	invisible()
}
print.Seuil<-function(x,...) {

	switch(x$type,
		{xx<-round(x$seuil,5)},
		{xx<-round(x$seuil$tab,5)},
		{	ind<-match("test", names(x$seuil$tab.khi2))
			xx<-round(x$seuil$tab.khi2[,-ind],5)
			xx$test<- x$seuil$tab.khi2$test
		},
		{xx<-round(x$seuil,5)}
		)

	print(x$Call)
	if (x$type==3) {print(x$seuil$z)}
	print(xx)

	invisible()
}
seuil.lr.RR<-function(x) {
	if (prod(x, na.rm=T)>0) {
		y<-(x[1]/x[3])/(x[2]/x[4])
	} else {
		y<-NA
	}
	return(y)
}
seuil.lr<-function(tab,borne=NULL,delai="delsv",etat="etat", cov="age") {

	idelai<-match(delai, names(tab))
	if (is.na(idelai)) {stop("delai incorrect")}
	ietat<-match(etat, names(tab))
	if (is.na(ietat)) {stop("etat incorrect")}
	icov<-match(cov, names(tab))
	if (is.na(icov)) {stop("cov incorrect")}

	names(tab)[c(idelai, ietat, icov)]<-c("delai","etat","cov")

	covlim<-range(tab$cov)
	if (is.null(borne)) {
		borne<- quantile(tab$cov,p= seq(0.01,0.99, by=0.01))
	} else {
		borne<-sort(borne[borne>=covlim[1] & borne<=covlim[2]])
		if (length(borne)==0) {stop("bornes non correctes")}
	}
	maxcov<- covlim[2]+1
	mincov<- covlim[1]-1
#
# seuil logrank
#
	tab.lr<-matrix(NA, ncol=9, nrow=length(borne))
	for (i in 1:length(borne)) {
		cat(".")
		tab$g<-cut(tab$cov,breaks=c(mincov,borne[i],maxcov))
		x<-table(tab$g)
		if (all(x!=0)) {
			lr<-survdiff(Surv(delai, etat)~g,data=tab)
			tab.lr[i,-1]<-c(lr$n, lr$obs, lr$exp, lr$var[1,1], lr$chisq)
		}
	}
	cat("\n")

	tab.lr<-as.data.frame(tab.lr)
	names(tab.lr)<- c("bi","n (<=bi)","n (>bi)","O (<=bi)","O (>bi)", "E (<=bi)","E (>bi)","V","khi2")

	tab.lr$RR<-apply(tab.lr[,c(4:7)],1,seuil.lr.RR)
	tab.lr$p<-1-pchisq(tab.lr$khi2,1)
	tab.lr$bi<-borne
	tab<-tab.lr

	return(tab)
}
seuil.student<-function(tab,borne=NULL,grquant="deltrans", covquant="age") {
#grquant="groupe"; covquant="x"
	igrquant<-match(grquant, names(tab))
	if (is.na(igrquant)) {stop("grquant incorrect")}
	icovquant<-match(covquant, names(tab))
	if (is.na(icovquant)) {stop("covquant incorrect")}
	names(tab)[c(igrquant, icovquant)]<-c("groupe","cov")

	grlim<-range(tab$groupe)
	if (is.null(borne)) {
		borne<-quantile(tab$groupe, seq(0.01, 0.99, by=0.01))
	} else {
		borne<-sort(borne[borne>=grlim[1] & borne<=grlim[2]])
		if (length(borne)==0) {stop("bornes non correctes")}
	}

	tab.m<-matrix(NA, ncol=8, nrow=length(borne))
	for (i in 1:length(borne)) {
#         i<-1
		cat(".")
		tab$g<-ifelse(tab$groupe<=borne[i],0,1)
		x<-tab$cov[tab$g==0]
		y<-tab$cov[tab$g==1]
		if (length(x)>1 && length(y)>1) {
			z<-t.test(x,y)
			tab.m[i,-1]<-c(length(x), length(y),mean(x), mean(y), var(x), var(y), z$stat)
		}
	}
	cat("\n")

	tab.m<-as.data.frame(tab.m)
	names(tab.m)<-c("bi","n (<=bi)","n (>bi)","m (<=bi)","m (>bi)", "v (<=bi)","v (>bi)","t")
	tab.m$bi<-borne
#	borne<-borne[!is.na(tab.m$"n (<=bi)")]
#	tab.m<-tab.m[!is.na(tab.m$"n (<=bi)"),]

	ddl<-(tab.m$"n (<=bi)" + tab.m$"n (>bi)"-2)
	ddl<-max(ddl, na.rm=T)
	tab.m$res<-tab.m$"v (<=bi"*( tab.m$"n (<=bi"-1) + tab.m$"v (>bi"*( tab.m$"n (>bi"-1)
	tab.m$res<- tab.m$res/ddl
	tab.m$p<- 2*(1-pt(abs(tab.m$t),ddl))
	tab<-tab.m

     res<-list(tab=tab, ddl=ddl)
	return(res)
}
seuil.khi2.attendu<-function(x) {
	y<-x
	tot<-sum(x)
	y[1,1]<-sum(x[,1])*sum(x[1,])/tot
	y[1,2]<-sum(x[,2])*sum(x[1,])/tot
	y[2,1]<-sum(x[,1])*sum(x[2,])/tot
	y[2,2]<-sum(x[,2])*sum(x[2,])/tot
	return(y)
}
seuil.khi2<-function(tab,borne=NULL,covqual="etat", covquant="age") {

	icovqual<-match(covqual, names(tab))
	if (is.na(icovqual)) {stop("covqual incorrect")}
	icovquant<-match(covquant, names(tab))
	if (is.na(icovquant)) {stop("covquant incorrect")}
	names(tab)[c(icovqual, icovquant)]<-c("etat","cov")

	x<-table(as.vector(tab$etat))
	if (length(x)!=2) {stop("La variable qualitative n'a pas 2 classes")}
	valeuretat<-dimnames(x)[[1]]

	covlim<-range(tab$cov)
	if (is.null(borne)) {
		borne<-quantile(tab$cov,p=seq(0.01,0.99, by=0.01))
	} else {
		borne<-sort(borne[borne>=covlim[1] & borne<=covlim[2]])
		if (length(borne)==0) {stop("bornes non correctes")}
	}
	maxcov<- covlim[2]+1
	mincov<- covlim[1]-1


	tab.khi2<-matrix(NA, ncol=16, nrow=length(borne))
	for (i in 1:length(borne)) {
		cat(".")
		tab$g<-cut(tab$cov,breaks=c(mincov,borne[i],maxcov))
		x<-table(tab$etat, tab$g)
		y<-seuil.khi2.attendu(x)
		z<-3
		if (any(y<3)) {
			z<-1
			if (sum(x)<200) {
				test<-fisher.test(x)
				test<-test$p
			} else {
				test<-(-1)
			}
		} else {
			if (any(y<5)){
				z<-2
				test<- sum((abs(x-y)-0.5)^2/y)
			} else {
				test<- sum((x-y)^2/y)
			}
		}

		OR<-(x[2,2]*x[1,1])/(x[2,1]*x[1,2])
		Se<-x[2,2]/sum(x[2,])
		Sp<-x[1,1]/sum(x[1,])
		VPP<-x[2,2]/sum(x[,2])
		VPN<-x[1,1]/sum(x[,1])

		tab.khi2[i,-1]<-c(c(x),c(y),z,test, Se, Sp, VPP, VPN, OR)
	}
	cat("\n")

	tab.khi2<-as.data.frame(tab.khi2)
	names(tab.khi2)<-c("bi","Oa","Oc","Ob","Od", "Ea","Ec","Eb","Ed","test","khi2","Se","Sp", "VPP","VPN","OR")
	typetest<-c("F","y","K")
	tab.khi2$bi<- borne
	tab.khi2$test<-typetest[tab.khi2$test]
	tab.khi2$p<-ifelse(tab.khi2$test=="fisher",tab.khi2$khi2,1-pchisq(tab.khi2$khi2,1))

	tab.SeSp<-tab.khi2[,c(1,12:15)]
	tab.khi2<-tab.khi2[,c(-12:-15)]

	tab.khi2$OR[tab.khi2$OR ==Inf]<-NA
	tab.khi2$khi2[tab.khi2$khi2 <0]<-NA

	z<-matrix(paste("    O", letters[c(1,3,2,4)], "  ",sep=""), ncol=2)
	dimnames(z)<-list(paste(covqual, valeuretat, sep="="), paste(covquant, c("<=bi",">bi"), sep=""))
	class(z) <- "char.matrix"

	res<-list(tab.khi2=tab.khi2, tab.SeSp=tab.SeSp, z=z)
	return(res)
}
seuil.cox<-function(tab,borne=NULL,delai="delsv",etat="etat", cov="age") {

	idelai<-match(delai, names(tab))
	if (is.na(idelai)) {stop("delai incorrect")}
	ietat<-match(etat, names(tab))
	if (is.na(ietat)) {stop("etat incorrect")}
	icov<-match(cov, names(tab))
	if (is.na(icov)) {stop("cov incorrect")}

	names(tab)[c(idelai, ietat, icov)]<-c("delai","etat","cov")

	covlim<-range(tab$cov)
	if (is.null(borne)) {
		borne<-quantile(tab$cov,p=seq(0.01,0.99, by=0.01))
	} else {
		borne<-sort(borne[borne>=covlim[1] & borne<=covlim[2]])
		if (length(borne)==0) {stop("bornes non correctes")}
	}
	maxcov<- covlim[2]+1
	mincov<- covlim[1]-1
#
# seuil cox
#
     nbb<-length(borne)
	tab.cox<-matrix(NA, ncol=3, nrow=nbb)
	ncox<-rep(0,nbb)
	ncoxe<-ncox
	for (i in 1:nbb) {
		cat(".")
		tab$g<-cut(tab$cov,breaks=c(mincov,borne[i],maxcov))
		cg<-c(tab$g)-1
          ncox[i]<-sum(cg)
          ncoxe[i]<-sum(cg*tab$etat)
		cox<-coxph(Surv(delai, etat)~g,data=tab)
		tab.cox[i,-1]<-c(cox$coef, cox$var)
	}
	cat("\n")

	tab.cox<-as.data.frame(tab.cox)
	names(tab.cox)<-c("bi","beta","var")

     tab.cox$"n (>bi)"<-ncox
     tab.cox$"n (<=bi)"<-dim(tab)[1]-ncox
     tab.cox$"o (>bi)"<-ncoxe
     tab.cox$"o (<=bi)"<-sum(tab$etat)-ncoxe

	tab.cox$RR<-exp(tab.cox$beta)
	tab.cox$z<-ifelse(!is.na(tab.cox$var),tab.cox$beta/sqrt(tab.cox$var),NA)
	tab.cox$p<-2*(1-pnorm(abs(tab.cox$z)))
	tab.cox$bi<-borne
	tab<-tab.cox

	return(tab)
}
#
# ==============================================================================
# ==============================================================================
# la partie Joli pour afficher et ploter des trucs sympa ? partir de r?gressions
# pour le moment seulement coxph
# ==============================================================================
# ==============================================================================
#
Joli <- function(model, ...) {
     UseMethod("Joli", model)
}
Joli.default <- function(model, ...) {
  cat("'Joli' s'applique aux objets de classe 'coxph', pas aux objets de classe '", class(model), "'.\n", sep = "")
}
print.Joli <- function(x,...){
     ind<-match("cox", names(x))
     if (!is.na(ind)) {print.Joli.cox(x, ...)}
}
plot.Joli <- function(x,...){
     ind<-match("cox", names(x))
     if (!is.na(ind)) {plot.Joli.cox(x, ...)}
}
#
# ==============================================================================
# quelques outils
#
Joli.Petit.p<-function(p, plim=0.001) {
     dec<-abs(log10(plim))
     pu<-10^dec
     if (dec==1) {
          plimtxt<-"<0.1"
     } else {
          plimtxt<-paste("<0.", substring("0000000000000000000", 1, dec-1),"1", sep="")
     }
     x<-round(p*pu)
     y<-format(c(plim, x/pu))[-1]
     z<-ifelse(x==0, plimtxt, y)
     return(z)
}
Joli.enleve.espace<-function(v) {
     v2<-gsub(" ", "", v)
     while (any(v!=v2)) {
          v<-v2
          v2<-gsub(" ", "", v)
     }
     return(v)
}
#
# ==============================================================================
# coxph
#
Joli.coxph <- function(model, ...) {
#model<-cox;verbose<-T;indrdv<-T
     appel <- match.call()
     verbose<-F
          ind <- pmatch("verb", names(appel))
          if (!is.na(ind)) {
               verbose <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(verbose)) {verbose<-F}

     indrdv<-F
          ind <- pmatch("rdv", names(appel))
          if (!is.na(ind)) {
               indrdv <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(indrdv)) {indrdv<-F}


     if (verbose) {print(model$call)}
#
# ===== r?cup des formules
#
     if (verbose) {cat("R?cup des variables et des data\n")}
     wf<-as.character(model$formula)[-1]
     wf<-Joli.enleve.espace(wf)
     wf2<-strsplit(wf[2], "\\+")[[1]]
     wf1<-wf[1]
#Surv
     v<-wf1
     u<-"Surv(";nu<-nchar(u)
     vsu<-NULL;nbsu<-0
     i<-match(u, substring(v, 1, nu))
          if (!is.na(i)) {
               w<-strsplit(substring(v, nu+1, nchar(v)-1), "\\,")[[1]]
               vsu<-w
               nbsu<-length(vsu)
          }
#strata
     k<-0
     v<-wf2
     u<-"strata(";nu<-nchar(u)
     vst<-NULL;nbst<-0
     i<-match(u, substring(v, 1, nu))
          if (!is.na(i)) {
               w<-strsplit(substring(v[i], nu+1, nchar(v[i])-1), "\\,")[[1]]
               vst<-w
               nbst<-length(vst)
               k<-c(k,i)
          }

#interaction   : je suppose 2 variables
     i<-grep("\\*", wf2)
     j<-grep("\\:", wf2)
     ij<-sort(c(i, j))
     nbint<-length(ij)
     vint<-NULL
     if (nbint>0) {
          k<-c(k, ij)
          typint<-rep(0, nbint)
          typint[ij %in% i]<-1
          typint[ij %in% j]<-2

          typint<-rep(c(1,2), c(length(i), length(j)))
          w<-wf2[ij]
          w<-gsub("\\:", "\\*", w)
          vint<-strsplit(w, "\\*")

          if (any(sapply(vint, length)>2)) {
               stop("Plus de 2 nvx d'interaction : je ne sais pas faire\n")
          }
     }

#les autres
     if (length(k)>1) {
          k<-k[-1]
          vcov<-wf2[-k]
     } else {
          vcov<-wf2
     }

#les data
     vtout<-c(vsu, unique(c(vcov, vst, unlist(vint))))

     m<-model$call
     m<-m[match(c("", "data"), names(m), nomatch = 0)]
     data <- eval(m[[2]], parent.frame())

     i<-match(vtout, names(data))
     if (any(is.na(i))) {
          print(vtout[is.na(i)])
          stop("Il y a un truc pas pr?vu dans la formule du cox\n")
     }
     data<-data[, vtout]
     i<-complete.cases(data)*1
     if (any(i==0)) {
          e<-tapply(data[, 2], i, sum)
     } else {
          e<-c(0, sum(data[, 2]))
     }
     data<-data[i==1,]
     N<-length(i)
     n<-dim(data)[1]
#
# ===== on fab data.frame avec les classes
# ----- les classes
#
     if (verbose) {cat("Pr?pa du tableau des coef\n")}
     v<-vtout[-1:-nbsu]
     if (nbst>0) {
          v<-v[-match(vst, v)]
     }
#
# ----- rep?rage des var enterant dans les interactions
#
     Li<-rep(0, length(v))
     if (nbint>0) {
          for (i in 1:nbint) {
               j<-match(vint[[i]], v)
               Li[j]<-i
          }
     }

     if (length(v)>1) {
          L<-as.list(data[,v])
     } else {
          L<-list(data[, v])
          names(L)<-v
     }
     nbL<-length(L)
     vL<-names(L)
     Lclass<-sapply(L, function(x) if (any(class(x)=="factor")) return("factor") else return(class(x)))
     Lf<-lapply(L, function(x, num="") {if(any(class(x)=="factor")) {levels(x)} else {num}})
     nbLf<-sapply(Lf, length)

     Lfb<-Lf
     for (i in 1:nbL) {
          Lfb[[i]]<-paste(vL[i],Lf[[i]], sep="")
     }
#
# ----- ajout des variables interaction dans les classes
#
     i<-1
     vajoutI<-c()
     while (i<=nbint) {
          w<-vint[[i]]
          j<-match(w, v)

          wL<-paste(c("I", w), collapse=".")
          wL2<-paste(c("I", rev(w)), collapse=".")
          if (any(Lclass[j]=="factor")) {
               z<-list(Lf[[j[1]]],Lf[[j[2]]])
               z<-expand.grid(z)
               wf<-paste(z[, 1], z[,2], sep=":")
               wfb<-paste(paste(w[1],z[, 1], sep=""), paste(w[2],z[, 2], sep=""), sep=":")

               data$INTER.JOLI<-factor(paste(data[, w[1]], data[, w[2]], sep=":"))
               names(data)[match("INTER.JOLI", names(data))]<-wL
               vajoutI<-c(vajoutI, wL)
               vL<-c(vL, wL)
               Lclass<-c(Lclass, "factor")
               Lf[[nbL+1]]<-wf
               Lfb[[nbL+1]]<-wfb
               nbLf<-c(nbLf, length(wf))
               nbL<-nbL+1
               Li<-c(Li, -Li[j[1]])
          } else {
               wf<-""
               data$INTER.JOLI<-data[, w[1]]*data[, w[2]]
               names(data)[match("INTER.JOLI", names(data))]<-wL
               vajoutI<-c(vajoutI, wL)

               wfb<-paste(w, collapse=":")
               vL<-c(vL, wL)
               Lclass<-c(Lclass, "numeric")
               Lf[[nbL+1]]<-wf
               Lfb[[nbL+1]]<-wfb
               nbLf<-c(nbLf, 1)
               nbL<-nbL+1
               Li<-c(Li, -Li[j[1]])

               data$INTER.JOLI<-data[, wL]
               names(data)[match("INTER.JOLI", names(data))]<-wL2
               vajoutI<-c(vajoutI, wL2)
               wfb2<-paste(rev(w), collapse=":")
               vL<-c(vL, wL2)
               Lclass<-c(Lclass, "numeric")
               Lf[[nbL+1]]<-wf
               Lfb[[nbL+1]]<-wfb2
               nbLf<-c(nbLf, 1)
               nbL<-nbL+1
               Li<-c(Li, -Li[j[1]])
          }

          i<-i+1
     }
     names(Lf)<-vL
     names(Lfb)<-vL
     names(Lclass)<-vL

     classe<-data.frame(class=unlist(Lf), cov=rep(vL, nbLf), val=unlist(Lfb), type=rep(Lclass, nbLf))
     classe$i<-rep(1:nbL, nbLf)
     classe$j<-sequence(nbLf)
     classe$inter<-rep(Li, nbLf)
#
# ----- les coeff
#
     b<-model$coef
     beta<-data.frame(val=names(b), b=b, sb=sqrt(diag(model$var)))

     tab<-merge(beta, classe, by="val", all=T)
     tab<-tab[order(tab$i, tab$j),]
     tab<-tab[tab$inter>=0 | !is.na(tab$b),] #si interact et NA je garde pas sinon c'est une r?f
     tab$b[is.na(tab$b)]<-0
#
# ----- termes interactions en trop
#
     x<-tapply(tab$sb, tab$cov, mean, na.rm=T)
     if (any(is.nan(x))) {
          x<-names(x)[is.na(x)]
          tab<-tab[!(tab$cov %in% x),]
     }
     tab<-tab[order(tab$i, tab$j),]
     v<-(as.character(tab$cov))
     vu<-unique(v)
     tab$i<-match(v, vu)
     tab$j<-rep(0, dim(tab)[1])
     tab$j[1]<-1
     i<-2
     while (i <=dim(tab)[1]) {
          if (tab$i[i]==tab$i[i-1]) {tab$j[i]<-tab$j[i-1]+1} else {tab$j[i]<-1}
          i<-i+1
     }
     tabb<-tab
#
# ----- je soigne les data
#
     if (nbint>0) {
          i<-match(vajoutI, tabb$cov)
          if (any(is.na(i))) {
               vajoutI<-vajoutI[is.na(i)]
               data<-data[, -match(vajoutI, names(data))]
          }
     }
#
# ----- ajout du Wald
#
     tab<-tabb
     tab$wald<-tab$b/tab$sb
     tab$p<-2*(1-pnorm(abs(tab$wald)))

     tab<-tab[order(tab$i, tab$j),]

     tabw<-tab
#
# ----- le RdV global
#
     if (verbose) {cat("RdV global\n")}
     rdv<-list(lv0=model$loglik[1], lv1=model$loglik[2], ddl=length(model$coef))
     rdv$rdv<-abs(2*(rdv$lv0-rdv$lv1))
     rdv$p<-1-pchisq(rdv$rdv, rdv$ddl)
#
# ----- ajout du RdV
#
     tabr<-NULL
     if (indrdv) {
          if (verbose) {cat("RdV\n")}
          tab<-tabb
          v<-as.character(tab$cov[tab$j==1])
          nbv<-length(v)

          if (nbv>1) {
               f1<-paste("Surv(", paste(vsu, collapse=","), ")", sep="")
               sep<-"~"
               if (nbst>0) {
                    f1s<-paste("strata(", paste(vst, collapse=","), ")", sep="")
                    f1<-paste(f1, f1s, sep="~")
                    sep<-"+"
               }
               lv0<-1:nbv
               ddl0<-lv0
               for (i in 1:nbv) {
#             i<-8
#              i<-i+1
#               cat(i, "\n")
# je retire l'interaction si un var interaction est retir?e
                    j<-match(v[i], tab$cov)
                    if (tab$inter[j]>0) {
                         vmi<-v[-i]
                         u<-unique(as.character(tab$cov[tab$inter==-tab$inter[j]]))
                         vmi<-vmi[-match(u, vmi)]
                    } else {
                         vmi<-v[-i]
                    }
                    f2<-paste(vmi, collapse="+")
                    f<-paste(f1, f2, sep=sep)
#                  cat(i,f, "\n")
                    coxi<-coxph(formula(f), data=data)
                    lv0[i]<-max(coxi$loglik)
                    ddl0[i]<-length(coxi$coef)
               }

               lv1<-max(model$loglik)
               ddl1<-dim(beta)[1]

               i<-match(tab$cov, v)
               tab$lv0<-lv0[i]
               tab$ddl0<-ddl0[i]
               tab$lv1<-rep(lv1, dim(tab)[1])
               tab$ddl1<-rep(ddl1, dim(tab)[1])
               tab$rdv<-abs(2*(tab$lv1-tab$lv0))
               tab$ddl<-tab$ddl1-tab$ddl0
               tab$p.rdv<-1-pchisq(tab$rdv, tab$ddl)
               tab$rdv[tab$j!=1]<-NA
               tab$ddl[tab$j!=1]<-NA
               tab$p.rdv[tab$j!=1]<-NA
               tabr<-tab
          } else {
               n<-dim(tab)[1]
               tab$lv0<-rep(rdv$lv0, n)
               tab$ddl0<-rep(0, n)
               tab$lv1<-rep(rdv$lv1, n)
               tab$ddl1<-rep(rdv$ddl, n)
               tab$rdv<-abs(2*(tab$lv1-tab$lv0))
               tab$ddl<-tab$ddl1-tab$ddl0
               tab$p.rdv<-1-pchisq(tab$rdv, tab$ddl)
               tab$rdv[tab$j!=1]<-NA
               tab$ddl[tab$j!=1]<-NA
               tab$p.rdv[tab$j!=1]<-NA
               tabr<-tab
          }
     }
#
# ----- les param
#
     if (verbose) {cat("param\n")}
     p<-list(v=vtout)
     p$N<-c(N, n)
     p$evt<-e
     p$cox<-model$call
     p$strata<-vst
     param<-p
#
# ----- les scores
#
     if (verbose) {cat("Score\n")}
     tab<-tabw
     v<-(as.character(tab$cov[tab$j==1]));nbv<-length(v)
     D<-data[, c(v, vsu[1])]
     p<-array(0, dim=dim(D))
     for (i in 1:nbv) {
#         i<-1
#         i<-i+1
          tabi<-tab[tab$i==i,]
          x<-D[, i]
          if (tabi$type[1]=="numeric" || tabi$type[1]=="integer") {
               p[, i]<-tabi$b*x
          } else {
               p[, i]<-tabi$b[match(x, tabi$class)]
          }
     }
     p<-as.data.frame(p)
     names(p)<-c(v, "score")
     p<-apply(p, 1, sum)
     score<-p

#     x<-score$score
#     y<-model$linear+sum(model$mean*model$coef)
#
# ----- les profils
#
     if (verbose) {cat("Profils\n")}
     tab<-tabw
     v<-(as.character(tab$cov[tab$j==1]));nbv<-length(v)
     if (length(v)>1) {
          L<-as.list(data[,v])
     } else {
          L<-list(data[, v])
          names(L)<-v
     }
     p<-lapply(L, function(x) {if(any(class(x)=="factor")) {levels(x)} else {pretty(x)}})
     profil<-p
#
#     summary(x-y)
# ----- r?sultats
#
     nom<-c("c", "b", "sb", "classe", "cov", "type", "i", "j", "I")
     nom2<-c("i", "j", "I", "cov", "classe", "b", "sb")
     names(tabb)<-nom
     tabb<-tabb[, c(nom2, "type")]
     tabb<-tabb[order(tabb$i, tabb$j),]

     names(tabw)<-c(nom, "wald", "p")
     tabw<-tabw[, c(nom2, "wald", "p")]
     tabw<-tabw[order(tabw$i, tabw$j),]

     if (indrdv) {
          names(tabr)<-c(nom, "lv0", "ddl0", "lv1", "ddl1", "rdv", "ddl", "p")
          tabr<-tabr[, c(nom2, "lv0", "lv1", "rdv", "ddl", "p")]
          tabr<-tabr[order(tabr$i, tabr$j),]
     }

     res<-list(cox=model, param=param, data=data, tab=tabb, rdv=tabr, wald=tabw, rdvg=rdv, score=score, profil=profil)
#     res<-list(param=param, data=data, cox=cox, rdvg=rdv, tab=tabb, wald=tabw, rdv=tabr, score=score, profil=profil)
     class(res) <- "Joli"

     return(res)
}
print.Joli.cox <- function(x,...){
     appel <- match.call()
     alpha <- 0.05
          ind <- pmatch("alp", names(appel))
          if (!is.na(ind)) {
               alpha <- as.numeric(eval(appel[[ind]]))[1]
          }
          if (is.na(alpha) || alpha<=0.001 || alpha>=0.5) {alpha<-0.05}
     dec <- 3
          ind <- pmatch("dec", names(appel))
          if (!is.na(ind)) {
               dec <- round(as.numeric(eval(appel[[ind]]))[1])
          }
          if (is.na(dec) || dec<=0 || dec>=10) {dec<-3}
     rrdec <- 2
          ind <- pmatch("rrdec", names(appel))
          if (!is.na(ind)) {
               rrdec <- round(as.numeric(eval(appel[[ind]]))[1])
          }
          if (is.na(rrdec) || rrdec<=0 || rrdec>=10) {rrdec<-3}
     plim <- 0.001
          ind <- pmatch("plim", names(appel))
          if (!is.na(ind)) {
               plim <- as.numeric(eval(appel[[ind]]))[1]
          }
          if (is.na(plim) || plim<=0 || plim>=0.1) {plim<-0.001}
     rdv<-T
          ind <- pmatch("rdv", names(appel))
          if (!is.na(ind)) {
               rdv <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(rdv)) {rdv<-T}
     verbose<-F
          ind <- pmatch("verb", names(appel))
          if (!is.na(ind)) {
               verbose <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(verbose)) {verbose<-F}
     rr<-T
          ind <- pmatch("RR", names(appel))
          if (!is.na(ind)) {
               rr <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(rr)) {rr<-T}
     invrr<-F
          ind <- pmatch("invRR", names(appel))
          if (!is.na(ind)) {
               invrr <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(invrr)) {invrr<-F}
     coeff<-T
          ind <- pmatch("coef", names(appel))
          if (!is.na(ind)) {
               coeff <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(coeff)) {coeff<-T}
     wald<-F
          ind <- pmatch("wald", names(appel))
          if (!is.na(ind)) {
               wald <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(wald)) {wald<-F}

     rien<-F
          ind <- pmatch("rien", names(appel))
          if (!is.na(ind)) {
               rien <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(rien)) {rien<-F}
          if (rien) {
               wald<-F
               coeff<-F
               rdv<-F
               rr<-F
               invrr<-F
          }

     app<-F
          ind <- pmatch("app", names(appel))
          if (!is.na(ind)) {
               app <- as.logical(eval(appel[[ind]]))[1]
          }
          if (is.na(app)) {app<-F}
     fic<-NULL
          ind <- pmatch("fic", names(appel))
          if (!is.na(ind)) {
               fic <- as.character(eval(appel[[ind]]))[1]
          }
#
# ----- les choses existent ?
#
     if(is.null(x$rdv)) {rdv<-F}
#
# ----- les covariables
#
     if (verbose) {cat("Covariables\n")}
     tab<-x$tab
     y<-c("cov", as.character(tab$cov))
          j<-c(1, tab$j)
          y[j!=1]<-""
          y<-format(y, just="left")
          z<-y
          zt<-y
     y<-c("classe", as.character(tab$classe))
          y<-format(y, just="left")
          z<-paste(z, y, sep="  ")
          zt<-paste(zt, y, sep="\t")

          zb<-z
          ztb<-zt
#
# ----- les coeff
#
     if (verbose) {cat("Coeff\n")}
     if (coeff) {
          z<-zb
          zt<-ztb

          tab<-x$tab
          y<-round(tab$b, dec)
               y<-format(c(" b", format(y)))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-round(tab$sb, dec)
               i<-ifelse(is.na(y), 1, 0)
               y<-format(y)
               y[i==1]<-""
               y<-format(c(" sb", y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          zb<-z
          ztb<-zt
     }
#
# ----- les Wald
#
     if (verbose) {cat("Wald\n")}
     if (wald) {
          z<-zb
          zt<-ztb

          tab<-x$wald
          y<-round(tab$wald, dec)
               i<-ifelse(is.na(y), 1, 0)
               y<-format(y)
               y[i==1]<-""
               y<-format(c("wald", y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-tab$p
               i<-ifelse(is.na(y), 1, 0)
               y<-Joli.Petit.p(y, plim)
               y[i==1]<-""
               y<-format(c("  p", format(y, just="ri")))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")

          zb<-z
          ztb<-zt
     }
#
# ----- RR
#
     if (verbose) {cat("RR\n")}
     if (rr) {
          z<-zb
          zt<-ztb

          tab<-x$tab
          b<-tab$b
          sb<-tab$sb
          ici<-exp(qnorm(alpha/2, b, sb))
          ics<-exp(qnorm(1-alpha/2, b, sb))
          r<-round(cbind(exp(b), ici, ics), rrdec)
          rtxt<-format(r)

          y<-r[, 1]
               i<-ifelse(is.na(y), 1, 0)
               y<-rtxt[, 1]
               y[i==1]<-""
               y<-format(c(" RR", y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-r[, 2]+r[, 3]
               i<-ifelse(is.na(y), 1, 0)
               y<-paste("[", rtxt[, 2], "-", rtxt[, 3], "]", sep="")
               y[i==1]<-""
               y<-format(c(paste("IC(",1-alpha, ")", sep=""), y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")

          zb<-z
          ztb<-zt
     }
#
# ----- 1/RR
#
     if (verbose) {cat("invRR\n")}
     if (invrr) {
          z<-zb
          zt<-ztb

          tab<-x$tab
          b<-(-tab$b)
          sb<-tab$sb
          ici<-exp(qnorm(alpha/2, b, sb))
          ics<-exp(qnorm(1-alpha/2, b, sb))
          r<-round(cbind(exp(b), ici, ics), rrdec)
          rtxt<-format(r)

          y<-r[, 1]
               i<-ifelse(is.na(y), 1, 0)
               y<-rtxt[, 1]
               y[i==1]<-""
               y<-format(c(" 1/RR", y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-r[, 2]+r[, 3]
               i<-ifelse(is.na(y), 1, 0)
               y<-paste("[", rtxt[, 2], "-", rtxt[, 3], "]", sep="")
               y[i==1]<-""
               y<-format(c(paste("IC(",1-alpha, ")", sep=""), y))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")

          zb<-z
          ztb<-zt
     }
#
# ----- RdV
#
     if (verbose) {cat("RdV\n")}
     if (rdv) {
          z<-zb
          zt<-ztb

          tab<-x$rdv
          y<-round(tab$rdv, dec)
               y<-format(y)
               y[tab$j!=1]<-""
               y<-format(c("RdV", format(y, just="ri")))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-tab$ddl
               y<-format(y)
               y[tab$j!=1]<-""
               y<-format(c("ddl", format(y, just="ri")))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")
          y<-tab$p
               i<-ifelse(is.na(y), 1, 0)
               y<-Joli.Petit.p(y, plim)
               y[tab$j!=1]<-""
               y<-format(c("  p", format(y, just="ri")))
               z<-paste(z, y, sep="  ")
               zt<-paste(zt, y, sep="\t")

               zb<-z
               ztb<-zt
     }
#
# ----- des tirets
#
     n<-max(nchar(zb))
     j<-c(0, tab$j)
     tiret<-paste(paste(rep("-", max(n)), collapse=""), "\n ", sep="")
     tiret<-ifelse(j==1, tiret, "")
     zb<-paste(tiret, zb, sep="")
#
# ----- affichage des r?sultats
# ----- les param
#
     if (!is.null(fic)) {
          sink(fic, app=app)
          zb<-ztb
     }
     p<-x$param
     print(p$cox)
     if (p$N[1]==p$N[2]) {
          cat("\n", "Nombre de sujets    : ",p$N[2], "\n", sep="")
          cat(      "Nombre d'?v?nements : ",p$evt[2], "\n", sep="")
     } else {
          cat("\n", "Nombre de sujets    : ",p$N[2], " (",p$N[1]-p$N[2], " sujets retir?s)\n", sep="")
          cat(      "Nombre d'?v?nements : ",p$evt[2], " (",p$e[1], " evts retir?s)\n", sep="")
     }
     if (!is.null(p$strata)) {
          cat("\nStratification sur : ", paste(p$strata, collapse=", "), "\n", sep="")
     }

     y<-x$rdvg
     p<-Joli.Petit.p(y$p,plim/100)
     if (substring(p, 1, 1)!="<") {p<-paste("=", p, sep="")}
     cat("\nRapport de Vraisemblance du mod?le\n")
     cat("LV mod?le vide  : ",y$lv0, "\n", sep="")
     cat("LV mod?le plein : ",y$lv1, "\n", sep="")

     cat("RdV : ",y$rdv, " avec ", y$ddl, " ddl => p", p,"\n", sep="")

     if (coeff || wald || rr || rdv) {
          cat("\nCoefficients\n ", sep="")
          cat(paste(zb, collapse="\n "), "\n", sep="")
     }

     if (!is.null(fic)) {
          sink()
     }
     
}
plot.Joli.cox <- function(joli,...){
#joli<-j; profil<-p; tps=list(t=1, txt="S(1j)")
     appel <- match.call()


     titre<-"Nomogram"
          ind <- pmatch("tit", names(appel))
          if (!is.na(ind)) {
               titre <- as.character(eval(appel[[ind]]))[1]
          }
     dec <- 3
          ind <- pmatch("dec", names(appel))
          if (!is.na(ind)) {
               dec <- round(as.numeric(eval(appel[[ind]]))[1])
          }
          if (is.na(dec) || dec<=0 || dec>=10) {dec<-3}
     pch <- 15
          ind <- pmatch("pch", names(appel))
          if (!is.na(ind)) {
               pch <- round(as.numeric(eval(appel[[ind]]))[1])
          }
          if (is.na(pch) || pch<0 || pch>=21) {pch<-15}
     lwd <-c(2, 2, 2, 2)
          ind <- pmatch("lwd", names(appel))
          if (!is.na(ind)) {
               lwd <-as.numeric(eval(appel[[ind]]))
               lwd<-lwd[!is.na(lwd) & lwd>=0 & lwd<=10]
               if (length(lwd)==0) {lwd<-c(2, 2, 2, 2)}
               lwd<-rep(lwd, 4)[1:4]
          }
     cex<-c(0.9, 1, 1, 1)
          ind <- pmatch("cex", names(appel))
          if (!is.na(ind)) {
               cex <-as.numeric(eval(appel[[ind]]))
               cex<-cex[!is.na(cex) & cex>=0 & cex<=5]
               if (length(cex)==0) {cex<-c(0.9, 1, 1, 1)}
               cex<-rep(cex, 4)[1:4]
          }
     line<-(4)
          ind <- pmatch("line", names(appel))
          if (!is.na(ind)) {
               line <-(abs(as.numeric(eval(appel[[ind]]))))[1]
          }
          if (is.na(line) || line<0) {line<-(4)}
     coul<-c("blue", "magenta", "red", "orange")
          ind <- pmatch("col", names(appel))
          if (!is.na(ind)) {
               coul <-as.character(eval(appel[[ind]]))
               coul<-coul[!is.na(coul)]
               if (length(coul)==0){coul<-c("blue", "magenta", "red", "orange")}
               cex<-rep(cex, 4)[1:4]
          }
     tps<-list(t=365.25, txt="S(1an)")
          ind <- pmatch("tps", names(appel))
          if (!is.na(ind)) {
               tps <-as.list(eval(appel[[ind]]))
               if (any(is.na(match(names(tps), c("t", "txt")))) || is.null(names(tps))) {tps<-list(t=365.25, txt="S(1an)")}
          }
#          print(tps)

     profil<-NULL
          ind <- pmatch("pro", names(appel))
          if (!is.na(ind)) {
               profil <-as.list(eval(appel[[ind]]))
          }
#          print(profil)

     p<-joli$profil
     b<-joli$tab
     lgp<-sapply(p, length)
     v<-names(p);nbv<-length(v)
#
# ----- profil particulier : s'il existe, je prends le premier & je v?rifie qu'il existe
#
     V<-names(profil)
     j<-match(v, V)
     if (any(is.na(j))) {
          indprof<-F
     } else {
          indprof<-T
          prof<-list()
          vp<-c()
          for (i in 1:nbv) {
               prof[[i]]<-profil[[j[i]]][1]
               vp<-c(vp, V[j[i]])
          }
          names(prof)<-vp
          for (i in 1:nbv) {
               bi<-b[b$i==i,]
               if (bi$type[1]=="factor") {
                    j<-match(prof[[i]], p[[i]])
                    if (is.na(j)) {indprof<-F}
               } else {
                    if (prof[[i]]>max(p[[i]]) | prof[[i]]<min(p[[i]])) {indprof<-F}
               }
          }
     }
     if (indprof) {
          bprof<-prof
          vprof<-names(bprof)
     }

     y<-list()
     x<-list()
     for (i in 1:nbv) {
#         i<-1
          y[[i]]<-rep(i, lgp[i])

          xx<-p[[i]]
          bi<-b[b$cov==v[i],]
          if (bi$type[1]=="factor") {
               bb<-c(0, bi$b)
               j<-match(xx, bi$classe)+1
               j[is.na(j)]<-1
               x[[i]]<-bb[j]
               if (indprof) {
                    j<-match(prof[[i]], bi$classe)+1
                    j[is.na(j)]<-1
                    bprof[i]<-bb[j]
                    vprof[i]<-as.character(prof[[i]])
               }
          } else {
               x[[i]]<-bi$b*xx
               if (indprof) {
                    bprof[i]<-bi$b*prof[[i]]
                    vprof[i]<-as.character(round(prof[[i]], dec))
               }
          }
     }
     if (indprof) {
          scprof<-sum(unlist(bprof))
     }

     colv<-coul[1];lwdv<-lwd[1];cexv<-cex[1]
     colb<-coul[2];lwdb<-lwd[2];cexb<-cex[2]
     cols<-coul[3];lwds<-lwd[3];cexs<-cex[3]
     colS<-coul[4];lwdS<-lwd[4];cexS<-cex[4]

     yh<-0.1

     xr<-lapply(x, range)
     yr<-lapply(y, range)
     yn<-lapply(y, length)
#
# la ligne beta
#
     xb<-pretty(range(unlist(xr)))
     ybn<-length(xb)
     yb<-nbv+1.25*rep(1, ybn)
     xbr<-range(xb)
     ybr<-range(yb)
#
# la ligne score
#
     m<-sum(sapply(xr, min))
     M<-sum(sapply(xr, max))
     xsr<-c(m, M)
     ysr<-nbv+1.25*c(1,1)*2
     xs<-pretty(xsr)
     ysn<-length(xs)
     xsr<-range(xs)
     ys<-rep(ysr[1], ysn)
#
# la ligne survie
#
     scm<-sum(joli$cox$mean*joli$cox$coef)
     sc<-xs
     km<-survfit(joli$cox)
     km<-data.frame(t=km$time, s=km$surv)
     if (any(km$t<=tps$t)) {
          s<-min(km$s[km$t<=tps$t])
     } else {
          s<-1
     }
     s2<-s^exp(sc-scm)
     if (indprof) {sprof<-s^exp(scprof-scm)}
     xSr<-range(s2)
     ySr<-nbv+1.25*c(1,1)*3
     xS<-s2
     ySn<-length(xS)
     xSr<-range(xS)
     yS<-rep(ySr[1], ySn)
     xStxt<-format(round(xS, dec))

     droite<-1
     if (indprof) {droite<-2}
     oldpar<-par(mai=c(0.95, 1, 0.75, droite))
          xlim<-range(c(0,xb))
          ylim<-c(1-3*yh, yS[1]+yh*3)


          plot(xlim, ylim, ann=F, type="n", axes=F)
          title(titre)
          abline(v=xb, lty=2, col="lightblue")
          abline(v=0, lty=1, col=colv)
#
# les covariable & profil
#
          mtext(side=2, at=1:nbv, v, adj=1, las=2, col=colv)
          for (i in 1:nbv) {
               xx<-x[[i]];yy<-y[[i]];zz<-p[[i]]
               zz<-zz[order(xx)];yy<-yy[order(xx)];xx<-xx[order(xx)]
               sens<-rep(c(-1, 1), yn[[i]])[1:yn[[i]]]
               lines(xr[[i]], yr[[i]], col=colv, lwd=lwdv)
               segments(xx, yy, xx, yy+sens*yh, col=colv, lwd=lwdS)
               text(xx, yy+sens*2*yh, col=colv, adj=0.5, zz, cex=cexv)
          }
          if (indprof) {
               xx<-unlist(bprof)
               mtext(side=4, at=1:nbv, vprof, adj=0, las=2, col=colv)
               mtext(side=4, at=1:nbv, format(round(xx, dec)), adj=0, las=2, col=colv, line=line)
               points(xx, 1:nbv, pch=pch, col=colv)
          }
#
# la ligne beta
#
          xx<-xbr;yy<-ybr
          lines(xx, yy, col=colb, lwd=lwdb)
          xx<-xb;yy<-yb
          sens<-rep(c(-1, 1), length(xx))[1:length(xx)]
          segments(xx, yy, xx, yy+sens*yh, col=colb, lwd=lwdb)
          text(xx, yy+sens*2*yh, col=colb, adj=0.5, xb, cex=cexb)
          mtext(side=2, at=yb[1], expression(beta), adj=1, las=2, col=colb)
          if (indprof) {
               xx<-(unlist(bprof))
               points(xx, rep(yy[1], nbv), pch=pch, col=colb)
          }
#
# la ligne score
#
          xx<-((xsr-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
          yy<-ysr
          lines(xx, yy, col=cols, lwd=lwds)
          xx<-((xs-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
          yy<-ys
          sens<-rep(c(-1, 1), length(xx))[1:length(xx)]
          segments(xx, yy, xx, yy+sens*yh, col=cols, lwd=lwds)
          text(xx, yy+sens*2*yh, col=cols, adj=0.5, xs, cex=cexs)
          mtext(side=2, at=ys[1], "score", adj=1, las=2, col=cols)

          if (indprof) {
               xx<-scprof
               mtext(side=4, at=yy[1], round(xx,dec), adj=0, las=2, col=cols)
               xx<-((xx-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
               points(xx, yy[1], pch=pch, col=cols)
          }
#
# la ligne survie : elle s'attache ? la ligne de score
#
          xx<-((xsr-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
          yy<-ySr
          lines(xx, yy, col=colS, lwd=lwdS)
          xx<-((xs-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
          yy<-yS
          sens<-rep(c(-1, 1), length(xx))[1:length(xx)]
          segments(xx, yy, xx, yy+sens*yh, col=colS, lwd=lwdS)
          text(xx, yy+sens*2*yh, col=colS, adj=0.5, xStxt, cex=cexS)
          mtext(side=2, at=yS[1], tps$txt, adj=1, las=2, col=colS)
          if (indprof) {
               xx<-scprof
               xx<-((xx-xsr[1])/diff(xsr))*diff(xlim)+xlim[1]
               points(xx, yy[1], pch=pch, col=colS)
               mtext(side=4, at=yy[1], round(sprof,dec), adj=0, las=2, col=colS)
          }


     par(oldpar)

     invisible()
}


