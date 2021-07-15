#dev dans le repertoire lirepigasoumacro
# ================================================================================
# Sort les noms des CSV dans le repertoire par defaut
# en vue de lecture macro
# ================================================================================
#
MacroRepereCSV<-function() {
	f<-dir(pattern=".csv");fs<-strsplit(f, "\\.")
	nomf<-tolower(unlist(lapply(fs, MacroEltList, i=1)))
	n<-nchar(nomf)
	fin<-substring(nomf, n-3, n)
	nomf<-nomf[fin!="_clu" & fin!="_dlu"]
	fs<-strsplit(nomf, "_")
	nomf<-tolower(unlist(lapply(fs, MacroEltList, i=1)))
	nomf2<-tolower(unlist(lapply(fs, MacroEltList, i=2)))

	res<-list(nomf=nomf, nomf2=nomf2)
	return(res)
}
#
# ================================================================================
# Lecture du DLU et mise en place des visit & eform & question
# ================================================================================
#
MacroDLU<-function(ficdlu) {
	dlu<-read.table(ficdlu, sep=",", h=T, row=NULL, quote=c( "\""))
	x<-as.character(dlu$Visit.Form.Question)
	xs<-strsplit(x, "/")
	dlu$Visit<-unlist(lapply(xs, MacroEltList, i=1))
	dlu$Form<-unlist(lapply(xs, MacroEltList, i=2))
	dlu$Question<-unlist(lapply(xs, MacroEltList, i=3))
	dlu$VF<-paste(dlu$Visit, dlu$Form, sep=".")
	head(dlu)
	return(dlu)
}

#
# ================================================================================
# Lecture du CLU
# ================================================================================
#
MacroCLU<-function(ficclu) {
	clu<-read.table(ficclu, sep=",", h=T, row=NULL, quote=c( "\""))
	n<-dim(clu)[1]
#	if (n>0) {
#		clu.code<-tapply(clu$CatCode, clu$ShortCode, c)
#		clu.value<-tapply(as.character(clu$CatValue), clu$ShortCode, c)
#
#		fctpc<-function(x) {paste("c(", paste(x,collapse=", "), ")", sep="")}
#		fctpv<-function(x) {paste("c(\"", paste(x,collapse="\", \""), "\")", sep="")}
#		x<-lapply(clu.code, fctpc)
#		y<-lapply(clu.value, fctpv)
#		v<-paste("\tdata$", names(x),".f<-factor(data$", names(x),", ", unlist(x), ", ", unlist(y), ")", sep="")
#	}

	if (n==0) {
		cat("Pas de variable categorielle\n")
	}

	invisible(clu)
}
#
# ================================================================================
# extraction du ieme elements d'un list
# ================================================================================
#
MacroEltList<-function(l, i=1) {l[i]}
Macrodd<-function(ddeb, dfin) {
	d<-unlist(lapply(strsplit(c(ddeb, dfin), " "), MacroEltList, i=4))
	h<-as.numeric(unlist(lapply(strsplit(d, ":"), MacroEltList, i=1)))
	m<-as.numeric(unlist(lapply(strsplit(d, ":"), MacroEltList, i=2)))
	s<-as.numeric(unlist(lapply(strsplit(d, ":"), MacroEltList, i=3)))
	s<-(s+60*m)+60*h
	ds<-diff(s)

	m<-ds%/%60;s<-ds%%60
	h<-m%/%60;m<-m%%60
	
	txt<-paste("(",ds, ") : ",h,"h", m, "mn", s, "s", sep="")

	invisible(txt)
}
#
# ================================================================================
# Impression des param de lecture d'un csv
# ================================================================================
#
MacroprintP<-function(p) {
	cat(p$tps[3], "temps ecoule\n")
	cat(p$nblig, "lignes lues\n")
	cat(p$N, "dossiers\n")
	cat(p$V, "variables\n")
	cat(p$Vr, "variables (en comptant les rep et identifiants)\n")
	cat(p$rep[1], "visit rep\n")
	cat(p$rep[2], "form rep\n")
	if (p$rep[3]>0) {
     	cat(p$rep[3], "quest rep (au moins)\n")
     } else {
     	cat(p$rep[3], "quest rep\n")
     }
}
#
# ================================================================================
# Lire un fichier MACRO CSV (query Modul)
# ================================================================================
#
MacroLire<-function(fic, d, sep="", verbose=F, rep=NULL) {
#fic<-"sondur2"; d<-"20070618"; verbose<-T
#fic<-"essairep"; d<-"20070619"; verbose<-T
#fic<-"dprost";d<-"20070620";verbose<-T
#fic<-"lnhophtalmique";d<-"20070626";verbose<-F
#mr<-MacroLire("rar2", "20070824", rep="s:/juliette/rar/")

#fic<-"far"; d<-"20071109"; verbose<-T;rep<-NULL
#fic<-"sein81inclu"; d<-"20071122"; verbose<-T;rep<-NULL

	ddeb<-date()

	str<-fic
	strd<-d
	ficdlu<-paste(rep, fic, sep, d, "_DLU.csv", sep="")
	ficclu<-paste(rep, fic, sep, d, "_CLU.csv", sep="")
	fic<-paste(rep, fic, sep, d, ".csv", sep="")
	ie<-file.exists(c(fic, ficdlu, ficclu))

	if (!ie[1]) {
		stop("\n\tFichier *.csv inexistant\n\tA bientot")
	}
	if (!ie[2]) {
		stop("\n\tFichier *_DLU.csv inexistant\n\tA bientot")
	}
#
# ==================== lecture du DLU et DATA
#
	cat("lecture de", ficdlu, "\n")
	dlu<-MacroDLU(ficdlu)
	dlu$n<-1:dim(dlu)[1]

	cat("lecture de", fic, "\n")
	DATA<-read.table(fic, sep=",", h=T, row=NULL, quote=c( "\""))
		DATA$Trial<-NULL
		DATA$Site<-NULL
	nblig<-dim(DATA)[1]
#
	if (tolower(str)=="proton3") {
		names(DATA)[names(DATA)=="NA."]<-"NAc"
		dlu$ShortCode<-as.character(dlu$ShortCode)
		dlu$ShortCode[is.na(dlu$ShortCode)]<-"NAc"
		dlu$Question[is.na(dlu$Question)]<-"NAc"
	}
#
# ==================== decoupage en VF
#
	if (!verbose) {cat("Decoupage en VF\n")} else {cat("\n")}
#
# ----- le nom des variables par VF
#
	vf<-tapply(as.character(dlu$ShortCode), dlu$VF, c)
	nbvf<-length(vf)
	dvf<-list()
	vide<-rep(0, nbvf)
	for (i in 1:nbvf) {
	# i=1
		if (verbose) {cat(i, "decoupage en VF", names(vf)[i], "\n")}
#
# ----- je prends les datas du VF plus les identifiants
# ----- et j'enleve les lignes vides
#
		d<-DATA[, c("PersonId", "VisitCycle", "FormCycle", "RepeatNumber", vf[[i]])]

		cd<-apply(d, 2, as.character)
		M<-ifelse(is.na(cd), 0, 1)
		M[!is.na(cd) & cd==""]<-0

		sm<-apply(M, 1, sum)-4

		if (any(sm>0)) {
			dvf[[i]]<-d[sm>0,]
		} else {
			vide[i]<-1
		}
	}
#
# ==================== si des VF sont vides je les enleve
#
	u0<-unique(as.character(dlu$VF))
	if (any(vide==1)) {

		if (!verbose) {cat("Elimination des VF vides\n")} else {cat("\n")}
		k<-0
		for (i in 1:nbvf) {
			if (vide[i]==0) {
				k<-k+1
				dvf[[k]]<-dvf[[i]]
			} else {
				if (verbose) {cat(i, names(vf)[i], "eliminee car vide\n")}
				j<-match(vf[[i]], as.character(dlu$ShortCode))
				dlu<-dlu[-j,]
			}
		}
		while (length(dvf)>k) {
		   dvf[[length(dvf)]]<-NULL
		}
		vf<-tapply(as.character(dlu$ShortCode), dlu$VF, c)
		nbvf<-length(vf)
		names(dvf)<-names(vf)
	} else {
		names(dvf)<-names(vf)
	}
	u1<-unique(as.character(dlu$VF))
	iu<-match(u0, u1)
	vfe<-u0[is.na(iu)]
#
# ==================== mise en place des formats par VF
# ===== a priori seules les dates et les reels sont a convertir
#
	if (!verbose) {cat("Mise en place des formats\n")} else {cat("\n")}
	for (i in 1:nbvf) {
#		i<-i+1
		if (verbose) {cat(i, "formatage de", names(vf)[i], "\n")}

		d<-dvf[[i]];N<-dim(d)[1]

		dlui<-dlu[dlu$VF==names(dvf)[i],]
		ok<-F
#
# ----- les dates
#
		v<-as.character(dlui$ShortCode[dlui$Type=="Date"])
		nv<-length(v)
		c(N, nv)
#		match(v, w)
		if (nv>0) {
			r<-d[, v]
			if (nv>1) {
				cr<-unlist(lapply(r, as.character))
				x<-as.Date(cr, format="%d/%m/%Y")
				X<-array(x, dim=c(N,nv))
			} else  {
				cr<-as.character(r)
				x<-as.Date(cr, format="%d/%m/%Y")
				X<-x
			}
			d[, v]<-X
			for (j in 1:nv) {
				class(d[,v[j]])<-"Date"
			}
			ok<-T
		}
#
# ----- les reels : pb de decimale avec , et sep avec ,
#
		v <-as.character(dlui$ShortCode[dlui$Type=="Real"])
		nv <-length(v)
		if (nv>0) {
               r <- d[, v]
               if (nv==1) {
                    cr<-as.character(r)
               } else {
                    cr <- unlist(lapply(c(r), as.character))
               }
               cr<-gsub(",", ".", cr)
               x<-as.numeric(cr)

			if (nv>1) {
				X<-array(x, dim=c(N,nv))
			} else  {
				X<-x
			}
			d[, v]<-X
			ok<-T
		}
		if (ok) {dvf[[i]]<-d}
	}
	names(dvf)<-names(vf)
#
# ==================== j'ajoute une variable qui indique si c'est rempli ou non
#
	if (!verbose) {cat("Ajout d'une variable VF\n")} else {cat("\n")}
     vf.var<-names(dvf)
     vf.var<-gsub("\\.", "_", vf.var)
     vf.var<-paste("VF", vf.var, sep="_")
     for (i in 1:nbvf) {
#		i<-14
#		i<-i+1
		if (verbose) {cat(i, " ", names(dvf)[i], " : ajout de ",vf.var[i],"\n", sep="")}
          d<-dvf[[i]]
               d$vf.var.x<-rep(1, dim(d)[1])
               names(d)[dim(d)[2]]<-vf.var[i]
               dvf[[i]]<-d

          j<-match(names(d)[5],dlu$ShortCode);dlu2<-dlu[j,];dlu2
          dlu2$ShortCode<-vf.var[i]
          dlu2$Description<-"Variable VF"
          dlu2$Type<-"IntegerData"
          dlu2$Description<-"Variable VF"
          dlu2$Question<-vf.var[i]
          dlu2$n<-max(dlu$n)+1

          dlu<-rbind(dlu, dlu2)
     }
#
# ==================== je coupe les questions rep et les mets en ligne
#
	if (!verbose) {cat("Repetitions des Questions en ligne\n")} else {cat("\n")}
	for (i in 1:nbvf) {
#		i<-0
#		i<-i+1
		d<-dvf[[i]]
		any(d$RepeatNumber>1)
		if (any(d$RepeatNumber>1)) {
			if (verbose) {cat(i, " ", names(dvf)[i], " : question repetitive\n", sep="")}
			x<-d[d$RepeatNumber>1, ]
			cd<-apply(x, 2, as.character)
			M<-ifelse(is.na(cd), 0, 1)
			M[!is.na(cd) & cd==""]<-0
			if (dim(x)[1]>1) {
				sM<-apply(M, 2, sum)
			} else {
				sM<-rbind(M)
			}

			dq<-d[, sM>0]
			sM[1:4]<-0
			dpq<-d[d$RepeatNumber==1, sM==0]

			r<-max(dq$RepeatNumber);r
			for (ir in 1:r) {
#				ir<-1
				dq2<-dq[dq$RepeatNumber==ir,]
				names(dq2)[-1:-4]<-paste(names(dq2)[-1:-4], ir, sep=".q")
				if (ir==1) {
					Dq2<-dq2
				} else {
					dq2$RepeatNumber<-NULL
					Dq2<-merge(Dq2, dq2, by=c("PersonId", "VisitCycle", "FormCycle"), all=T)
				}
			}

			dvf[[i]]<-merge(dpq, Dq2, by=c("PersonId", "VisitCycle", "FormCycle", "RepeatNumber"), all=T)
		}
	}
#
# ==================== je coupe les form rep et les mets en ligne
#
	if (!verbose) {cat("Repetitions des eForm en ligne\n")} else {cat("\n")}
	for (i in 1:nbvf) {
#		i<-1
#		i<-i+1
		d<-dvf[[i]]
		any(d$FormCycle>1)
		if (any(d$FormCycle>1)) {
			if (verbose) {cat(i, " ", names(dvf)[i], " : eForm repetitif\n", sep="")}
			dq<-d

			r<-max(dq$FormCycle);r
			for (ir in 1:r) {
#				ir<-1
				dq2<-dq[dq$FormCycle==ir,]
				names(dq2)[-1:-4]<-paste(names(dq2)[-1:-4], ir, sep=".f")
				if (ir==1) {
					Dq2<-dq2
				} else {
					dq2$RepeatNumber<-NULL
					dq2$FormCycle<-NULL
					Dq2<-merge(Dq2, dq2, by=c("PersonId", "VisitCycle"), all=T)
				}
			}
			dvf[[i]]<-Dq2
		}
	}
#
# ==================== je coupe les v rep et les mets en ligne
# attention des Visites peuvent intervenir dans plusieurs VF
#
	if (!verbose) {cat("Regroupement VF en Visit\n")} else {cat("\n")}
	names(dvf)
	v<-unlist(lapply(strsplit(names(dvf), "\\."), MacroEltList, i=1))
	v<-tapply(names(dvf), v, c)
	nbv<-length(v)

	dv<-list()
	for (i in 1:nbv) {
#		i<-1
#		i<-i+1
		if (length(v[[i]])>1) {
			if (verbose) {cat(i, " ", names(v)[i], " regroupement des eForm\n", sep="")}
			for (j in 1:length(v[[i]])) {
#				j<-1
				dq<-dvf[[v[[i]][j]]]
				if (j==1) {
					Dq2<-dq
				} else {
					dq$FormCycle<-NULL
					dq$RepeatNumber<-NULL
					Dq2<-merge(Dq2, dq, , by=c("PersonId", "VisitCycle"), all=T)
				}
			}
			dv[[i]]<-Dq2
		} else {
			if (verbose) {cat(i, " ", names(v)[i], " pas de regroupement necessaire\n", sep="")}
			dv[[i]]<-dvf[[v[[i]][1]]]
		}
	}
	names(dv)<-names(v)
	dvf<-NULL

	if (!verbose) {cat("Repetitions des Visit en ligne\n")} else {cat("\n")}
	for (i in 1:nbv) {
#		i<-1
#		i<-i+1
		d<-dv[[i]]
		any(d$VisitCycle>1)
		if (any(d$VisitCycle>1)) {
			if (verbose) {cat(i, " ", names(dv)[i], " : Visit rep\n", sep="")}
			dq<-d

			r<-max(dq$VisitCycle);r
			for (ir in 1:r) {
#				ir<-1
				dq2<-dq[dq$VisitCycle==ir,]
				names(dq2)[-1:-4]<-paste(names(dq2)[-1:-4], ir, sep=".v")
				if (ir==1) {
					Dq2<-dq2
				} else {
					dq2$RepeatNumber<-NULL
					dq2$FormCycle<-NULL
					dq2$VisitCycle<-NULL
					Dq2<-merge(Dq2, dq2, by=c("PersonId"), all=T)
				}
			}
			dv[[i]]<-Dq2
		} else {
			if (verbose) {cat(i, " ", names(dv)[i], " : Visit non rep\n", sep="")}
		}
	}
#
# ==================== merge en 1 seul data frame Par PersonId
#
	if (!verbose) {cat("Merge de l'ensemble\n")} else {cat("\n")}
	for (i in 1:nbv) {
		if (verbose) {cat(i, " merge de ", names(dv)[i], sep="", "\n")}
		d<-dv[[i]]
		d$FormCycle<-NULL
		d$VisitCycle<-NULL
		d$RepeatNumber<-NULL
		if (i==1) {
			DATA<-d
		} else {
			DATA<-merge(DATA, d, by="PersonId", all=T)
		}
	}
#
# ==================== les rep dans le dlu
#
	v<-names(DATA)[-1]

	vs<-strsplit(v, "\\.")
	w1<-unlist(lapply(vs, MacroEltList, i=1))
	w2<-unlist(lapply(vs, MacroEltList, i=2))
	w3<-unlist(lapply(vs, MacroEltList, i=3))
	w4<-unlist(lapply(vs, MacroEltList, i=4))

	w2c<-substring(w2, 1,1);w2n<-as.numeric(substring(w2, 2,nchar(w2)))
	w3c<-substring(w3, 1,1);w3n<-as.numeric(substring(w3, 2,nchar(w3)))
	w4c<-substring(w4, 1,1);w4n<-as.numeric(substring(w4, 2,nchar(w4)))

	rv<-ifelse(w2c=="v" & !is.na(w2c), w2n, 0)
	rv<-ifelse(w3c=="v" & !is.na(w3c), w3n, rv)
	rv<-ifelse(w4c=="v" & !is.na(w4c), w4n, rv)

	rf<-ifelse(w2c=="f" & !is.na(w2c), w2n, 0)
	rf<-ifelse(w3c=="f" & !is.na(w3c), w3n, rf)
	rf<-ifelse(w4c=="f" & !is.na(w4c), w4n, rf)

	rq<-ifelse(w2c=="q" & !is.na(w2c), w2n, 0)
	rq<-ifelse(w3c=="q" & !is.na(w3c), w3n, rq)
	rq<-ifelse(w4c=="q" & !is.na(w4c), w4n, rq)


	v<-data.frame(var=v, ShortCode=w1, rv=rv, rf=rf, rq=rq)

	r<-tapply(v$rv, v$ShortCode, max);r<-data.frame(ShortCode=names(r), rv=r)
		dlu<-merge(dlu, r, by="ShortCode", all=T)
	r<-tapply(v$rf, v$ShortCode, max);r<-data.frame(ShortCode=names(r), rf=r)
		dlu<-merge(dlu, r, by="ShortCode", all=T)
	r<-tapply(v$rq, v$ShortCode, max);r<-data.frame(ShortCode=names(r), rq=r)
		dlu<-merge(dlu, r, by="ShortCode", all=T)

	dlu$r<-ifelse(dlu$rv+dlu$rf+dlu$rq>0, 1, 0)
	dlu<-dlu[order(dlu$n),]
#
# ==================== lecture du CLU
#
	if (ie[3]) {
		if (!verbose) {cat("Lecture du CLU & fab du script\n")} else {cat("\n")}
		clu<-MacroCLU(ficclu)
	} else {
		cat("CLU inexistant\n")
		clu<-NULL
	}
#
# ==================== je bidouille les variables VF
# ===== et j'ajoute une var VF_ nombre de repe
#
	if (!verbose) {cat("Mise a 0/1 des variable VF\n")} else {cat("\n")}

     m<-MacroSelectionneShortCode(vf.var, dlu, names(DATA))
     v<-tapply(m$names, m$ShortCode, as.character)
     lgv<-sapply(v, length)
     SX<-list()
     for (i in 1:length(v)) {
#		i<-2
#		i<-i+1
		if (verbose) {cat(i, " Mise a 0/1 de la variable VF ", names(v)[[i]], "\n", sep="")}

          x<-DATA[,  v[[i]]]
          x[is.na(x)]<-0
          DATA[,  v[[i]]]<-x

          if (lgv[i]>1) {
               sx<-apply(x, 1, sum)
               SX[[length(SX)+1]]<-sx
          }

     }
     if (length(SX)>0) {
          names(SX)<-names(v)[lgv>1]
          for (i in 1:length(SX)) {
               DATA$VF_ydr<-SX[[i]]
               names(DATA)[match("VF_ydr", names(DATA))]<-names(SX)[i]
          }
     }
#     w<-sort(unique(c(unlist(v), vf.var)))
#     summary(DATA[, unlist(w)])
#     summary(DATA[, unlist(w)])

#
# ==================== quelques comptes
#
#dlu<-mr$dlu

     lettre<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2:9)

     np<-15                                       #nbre de phrases
     tp<-sample(seq(5, 50, by=5), np, replace=T)  #nbre de mots par phrase
     nmt<-sum(tp)                                 #nbre de mots au total
     tm<-sample(1:10, nmt, replace=T)             #taille des mots
     m<-sample(lettre, nmt, replace=T)
     m<-rep(m, tm)
     im<-rep(1:nmt, tm)
     ip<-rep(rep(1:np, tp), tm)
     m<-tapply(m, ip, c)

	srv<-sum(ifelse(unlist(tapply(dlu$rv, dlu$Visit, max))>0, 1, 0))
	rf<-tapply(dlu$rf, dlu$VF, c)
          srf<-unlist(lapply(rf, max))
          srf<-sum(ifelse(srf>0,1,0))
          nrf<-unlist(lapply(rf, length))
     srq<-0
     if (any(dlu$rq>0)) {
          rq<-tapply(dlu$rq, dlu$VF, as.numeric)
          rql<-list()
          for (i in 1:length(rq)) {
               rqi<-rq[[i]]
               rqid<-diff(c(0, rqi))
               rqid<-rqid[rqi!=0]
               rqid<-rqid[rqid!=0]
               rql[[i]]<-rqid
          }
          srq<-sum(unlist(lapply(rql, length)))
     }


	dfin<-date()
	tps<-Macrodd(ddeb, dfin)
	p<-list(tps=c(ddeb, dfin, tps))
	p$nblig<-nblig
	p$N<-dim(DATA)[1]
	p$V<-dim(dlu)[1]
	p$Vr<-dim(DATA)[2]
	p$rep<-c(srv, srf, srq)
#
# ===== je crache les resultats
#
	res<-list(str=str, date=strd, dlu=dlu, data=DATA, p=p, vfe=vfe, clu=clu)

	cat("\n")
	MacroprintP(res$p)

	return(res)
}
#
# ================================================================================
# Pour trier les eForm repetitifs d'un dataframe obtenu par MacroLire
# ================================================================================
# la variable tri doit etre une variable de l'eform 
#                 et ne doit pas etre repetitive en Question
#
MacroTriEform<-function(data, dlu, vf="Reconstr.DEPOSE", vt="DABL") {

	i<-match(vf, dlu$VF)
	if (is.na(i)) {
		stop("\n\tVF inexistant\n\tA bientot")
	}

	w<-names(data)
	dlut<-dlu[dlu$VF==vf,]
	dlut
	v<-as.character(dlut$ShortCode)

	r<-max(dlut$rf)
	if (r==0) {
		stop("\n\tPas de repetition\n\tA bientot")
	}
	rv<-max(dlut$rv)

	it<-match(vt, v);it
	if (is.na(it)) {
		stop("\n\tLa variable de tri n'est pas dans le VF\n\tA bientot")
	}

	if (dlut$rq[it]>0) {
		stop("\n\tLa variable de tri est une Question repetitive\n\tA bientot")
	}

	w<-names(data)
	ws<-unlist(lapply(strsplit(w, "\\."), MacroEltList, i=1))
	i<-match(ws, v)
	w<-w[!is.na(i)]
	datat<-data[, w]

	N<-dim(data)[1]
	i1<-min((1:dim(data)[2])[!is.na(i)])

	nbvtot<-length(w)		#nb variables a trainer dans le tri
	nbvf<-length(dlut$rf)		#nb variables dans chaque eform
	txt<-c("nb variables a trainer dans le tri",
		"nb variables dans chaque eform",
		"nb repetition",
		"nb de visit repetitives")
	e<-c(nbvtot, nbvf, r, rv)
	cat(paste(paste(format(txt, just="left"), format(e, just="right"), sep=" : "), collapse="\n"), "\n")
#
# ===== je decoupe en visit au cas ou la visit soit repetitive
#
	if (rv>0) {	
		datav<-list()
		fin<-0
		for (i in 1:rv) {
			deb<-fin+1
			fin<-deb+(nbvf*r)-1
			datav[[i]]<-datat[, deb:fin]
		}
		names(datav)<-paste("V", 1:rv, sep="")
	} else {
		datav<-list(datat)
		names(datav)<-"V0"
		rv<-1
	}
	ind<-array(i1+1:nbvtot-1, dim=c(nbvf, r, rv))	#les variables se suivent
#
# ===== je tri les eform
#
	n<-10
	if (r>=10) {n<-100}
	if (r>=100) {n<-1000}	#!!!!

	atrier<-array(0, dim=c(N, rv))	
	for (iv in 1:rv) {
		d<-datav[[iv]]
		dvt<-d[,  ((1:r)-1)*nbvf+it]
		dvto<-t(apply(dvt, 1, order))

		o<-apply(sweep(dvto, 2, n^(0:(r-1)), "*"), 1, sum)
		bo<-sum((1:r)*(n^(0:(r-1))))
		atrier[,iv]<-(o!=bo)*1		

		if (any(o!=bo)) {
			cat("tri necessaire\n")
#
# ===== mise en list de la fiche
#
			dl<-list()
			fin<-0
			for (ir in 1:r) {
				deb<-fin+1
				fin<-deb+nbvf-1
				dl[[ir]]<-d[, deb:fin]
			}
#
# ===== tri du list
#
			dlt<-dl
			for (ir in 1:r) {
				o<-dvto[, ir]
				ou<-sort(unique(o))
				for (io in 1:length(ou)) {
					jr<-ou[io]
					dlt[[ir]][o==jr,]<-dl[[jr]][o==jr,]
				}
			}

#		verif<-array(0, dim=c(N, r))
#		verift<-array(0, dim=c(N, r))
#		for (ir in 1:r) {
#			verif[,ir]<-dl[[ir]][, it]
#			verift[,ir]<-dlt[[ir]][, it]
#		}
#		verif
#		verift

#
# ===== remise en place dans le data d'origine
#
			fin<-0
			for (ir in 1:r) {
				deb<-fin+1
				fin<-deb+nbvf-1
				d[, deb:fin]<-dlt[[ir]]
			}

			data[, names(d)]<-d
		} else {
			cat("pas de tri necessaire\n")
		}
	}

	res<-list(data=data, atrier=atrier, ind=ind)
	return(res)
}
#
# ================================================================================
# Pour trier les Visit repetitifs d'un dataframe obtenu par MacroLire
# ================================================================================
# la variable tri doit etre une variable de la visit
#                 et ne doit pas etre repetitive en eForm
#
MacroTriVisit<-function(data, dlu, visit="Reconstr", vt="DABL") {
#data<-a$data; dl<-a$dlu;visit<-"V11";vt<-"Q0016"

	i<-match(visit, dlu$Visit)
	if (is.na(i)) {
		stop("\n\tVisit inexistant\n\tA bientot")
	}

	w<-names(data)
	dlut<-dlu[dlu$Visit==visit,]
	dlut
	v<-as.character(dlut$ShortCode)

	r<-max(dlut$rv)
	if (r==0) {
		stop("\n\tPas de repetition\n\tA bientot")
	}

	it<-match(vt, v);it
	if (is.na(it)) {
		stop("\n\tLa variable de tri n'est pas dans le Visit\n\tA bientot")
	}

	if (dlut$rf[it]>0) {
		stop("\n\tLa variable de tri est dans un eForm repetitif\n\tA bientot")
	}
	if (dlut$rq[it]>0) {
		stop("\n\tLa variable de tri est une Question repetitive\n\tA bientot")
	}

	w<-names(data)
	ws<-unlist(lapply(strsplit(w, "\\."), MacroEltList, i=1))
	i<-match(ws, v)
	w<-w[!is.na(i)]
	datat<-data[, w]

	N<-dim(data)[1]
	i1<-min((1:dim(data)[2])[!is.na(i)])

	nbvtot<-length(w)		#nb variables a trainer dans le tri
	nbvv<-length(dlut$rv)		#nb variables dans chaque visit
	txt<-c("nb variables a trainer dans le tri",
		"nb variables dans chaque visit",
		"nb repetition")
	e<-c(nbvtot, nbvv, r)
	cat(paste(paste(format(txt, just="left"), format(e, just="right"), sep=" : "), collapse="\n"), "\n")


	ind<-array(i1+1:nbvtot-1, dim=c(nbvv, r))	#les variables se suivent
#
# ===== je tri les eform
#
	n<-10
	if (r>=10) {n<-100}
	if (r>=100) {n<-1000}	#!!!!

	atrier<-rep(0, N)

		d<-data[, c(ind)]
		dvt<-d[,  ((1:r)-1)*nbvv+it]
		dvto<-t(apply(dvt, 1, order))

		o<-apply(sweep(dvto, 2, n^(0:(r-1)), "*"), 1, sum)
		bo<-sum((1:r)*(n^(0:(r-1))))
		atrier<-(o!=bo)*1		

		if (any(o!=bo)) {
			cat("tri necessaire\n")
#
# ===== mise en list de la fiche
#
			dl<-list()
			fin<-0
			for (ir in 1:r) {
				deb<-fin+1
				fin<-deb+nbvv-1
				dl[[ir]]<-d[, deb:fin]
			}
#
# ===== tri du list
#
			dlt<-dl
			for (ir in 1:r) {
				o<-dvto[, ir]
				ou<-sort(unique(o))
				for (io in 1:length(ou)) {
					jr<-ou[io]
					dlt[[ir]][o==jr,]<-dl[[jr]][o==jr,]
				}
			}

#			verif<-array(0, dim=c(N, r))
#			verift<-array(0, dim=c(N, r))
#			for (ir in 1:r) {
#				verif[,ir]<-dl[[ir]][, it]
#				verift[,ir]<-dlt[[ir]][, it]
#			}
#			verif
#			verift

#
# ===== remise en place dans le data d'origine
#
			fin<-0
			for (ir in 1:r) {
				deb<-fin+1
				fin<-deb+nbvv-1
				d[, deb:fin]<-dlt[[ir]]
			}

			data[, names(d)]<-d
		} else {
			cat("pas de tri necessaire\n")
		}

	res<-list(data=data, atrier=atrier, ind=ind)
	return(res)
}
#
# ================================================================================
# Pour trier les Question Group repetitifs d'un dataframe obtenu par MacroLire
# ================================================================================
#
MacroTriQuestion<-function(data, dlu, vq=c("Q0041", "Q0051"), vt="Q0041") {
#data<-a$data; dlu<-a$dlu;vq<-c("DPSA", "EPSA");vt<-"DPSA"

	w<-names(data)
	i<-match(vq, dlu$ShortCode)
	if (any(is.na(i))) {
		stop("\n\tQuestions inexistantes\n\tA bientot")
	}
	dlut<-dlu[i,]
	v<-as.character(dlut$ShortCode)

	r<-max(dlut$rq)
	if (any(dlut$rq==0)) {
		stop("\n\tAu moins une des Question n'est pas repetitive\n\tA bientot")
	}
	rf<-max(dlut$rf)
	rv<-max(dlut$rv)

	it<-match(vt, v);it
	if (is.na(it)) {
		stop("\n\tLa variable de tri n'est pas dans les Questions\n\tA bientot")
	}

	w<-names(data)
	ws<-unlist(lapply(strsplit(w, "\\."), MacroEltList, i=1))
	i<-match(ws, v)
	w<-w[!is.na(i)]
	datat<-data[, w]

	N<-dim(data)[1]
	i1<-min((1:dim(data)[2])[!is.na(i)])

	nbvtot<-length(w)		#nb variables a trainer dans le tri
	nbvq<-length(dlut$rq)		#nb variables dans chaque question
	txt<-c("nb variables a trainer dans le tri",
		"nb variables dans chaque Question Group",
		"nb repetition",
		"nb de eform repetitif",
		"nb de visit repetitives")
	e<-c(nbvtot, nbvq, r, rf, rv)
	cat(paste(paste(format(txt, just="left"), format(e, just="right"), sep=" : "), collapse="\n"), "\n")
#
# ===== je decoupe en vf au cas ou la visit et/ou eform soit repetitif
#
	rv<-max(c(rv, 1))
	rf<-max(c(rf, 1))

		datavf<-list()
		fin<-0
		k<-0
		for (i in 1:rv) {
			for (j in 1:rf) {
				k<-k+1
				deb<-fin+1
				fin<-deb+(nbvq*r)-1
				datavf[[k]]<-datat[, deb:fin]
			}
		}

	ind<-array(i1+1:nbvtot-1, dim=c(nbvq, r, rf, rv))	#les variables se suivent
#
# ===== je tri les questions
#
	n<-10
	if (r>=10) {n<-100}
	if (r>=100) {n<-1000}	#!!!!

	atrier<-array(0, dim=c(N, rf, rv))
	k<-0
	for (iv in 1:rv) {
		for (ir in 1:rf) {
			k<-k+1
			d<-datavf[[k]]
			dvt<-d[,  ((1:r)-1)*nbvq+it]
			dvto<-t(apply(dvt, 1, order))

			o<-apply(sweep(dvto, 2, n^(0:(r-1)), "*"), 1, sum)
			bo<-sum((1:r)*(n^(0:(r-1))))
			atrier[,ir, iv]<-(o!=bo)*1		

			if (any(o!=bo)) {
				cat("tri necessaire\n")
#
# ===== mise en list de la fiche
#
				dl<-list()
				fin<-0
				for (iq in 1:r) {
					deb<-fin+1
					fin<-deb+nbvq-1
					dl[[iq]]<-d[, deb:fin]
				}
#
# ===== tri du list
#
				dlt<-dl
				for (iq in 1:r) {
					o<-dvto[, iq]
					ou<-sort(unique(o))
					for (io in 1:length(ou)) {
						jq<-ou[io]
						dlt[[iq]][o==jq,]<-dl[[jq]][o==jq,]
					}
				}

#				verif<-array(0, dim=c(N, r))
#				verift<-array(0, dim=c(N, r))
#				for (ir in 1:r) {
#					verif[,ir]<-dl[[ir]][, it]
#				verift[,ir]<-dlt[[ir]][, it]
#				}
#				verif
#				verift

#
# ===== remise en place dans le data d'origine
#
				fin<-0
				for (iq in 1:r) {
					deb<-fin+1
					fin<-deb+nbvq-1
					d[, deb:fin]<-dlt[[iq]]
				}
	
				data[, names(d)]<-d
			} else {
				cat("pas de tri necessaire\n")
			}
		}
	}

	res<-list(data=data, atrier=atrier, ind=ind)
	return(res)
}
#
# ================================================================================
# Lire du Macro Query Module en Batch
# ================================================================================
#
MacroLireBatch<-function(fic, d, sep="", rep=NULL) {
#fic<-"colon2"; d<-"20070705"
	ficb<-paste(rep, fic, sep, d, "BatchData.csv", sep="")
	ok<-file.exists(ficb)
	if (!ok) {
		stop("\n\tFichier non trouve\n\n")
	}

	b<-read.table(ficb, sep=",", quote="\"", h=F)

	names(b)<-c("trial", "site", "personid", "vna", "visit", "vr", "fna", "form", "fr", "qna", "question", "qr", "value")
	return(b)
}
#
# ================================================================================
# Pour fabriquer un data.frame necessaire pour fabriquer un fichier pour BatchDataEntry
# ================================================================================
#
MacroBatchUneVariable<-function(id, x, rv, rf, rq, Visit, Form, Question, trial, site) {
	n<-length(x)
	b<-data.frame(trial=rep(trial, n), site=rep(site,n))
		b$PersonId<-id
		b$Vna<-rep("", n)
			b$Visit<-rep(Visit, n)
			b$Vr<-rep(rv,n)
		b$Fna<-rep("", n)
			b$Form<-rep(Form, n)
			b$Fr<-rep(rf,n)
		b$Qna<-rep("", n)
			b$Question<-rep(Question, n)
			b$Qr<-rep(rq,n)
		b$val<-as.character(x)

		return(b)
}
#
# =====================================================================================
# a partir d'une liste de variables en ShortCode
# sort les shortcodes avec les repetitions
# =====================================================================================
#
MacroSelectionneShortCode<-function(v, dlu, nom) {
#dlu<-tv
#v<-as.character(tv$ShortCode[tv$Type=="Date"]);v<-c(v, as.character(tvi$ShortCode[tvi$Type=="Date"]))
#v<-c("essai", v)
#nom<-names(sein)
#v<-v[1]

	r<-apply(dlu[, c("rv", "rf", "rq")], 2, max)
	z<-list(rv=0:r[1], rf=0:r[2], rq=0:r[3])
	z<-expand.grid(z)
	z$rv<-ifelse(z$rv==0, "", paste(".v", z$rv, sep=""))
	z$rf<-ifelse(z$rf==0, "", paste(".f", z$rf, sep=""))
	z$rq<-ifelse(z$rq==0, "", paste(".q",z$rq, sep=""))
	z<-paste(z$rq, z$rf, z$rv, sep="")

	V<-outer(v, z, paste, sep="")
	i<-array(match(V, nom), dim=dim(V))
	j<-ifelse(is.na(i), 0, 1)
	sj<-apply(j, 1, sum)

	v<-data.frame(v=v, trouvee=ifelse(sj>0, 1, 0))

	iV<-row(V)[!is.na(i)]
	V<-V[!is.na(i)]
	V<-data.frame(names=V, ShortCode=v$v[iV])
	V<-merge(V, v, by.x="ShortCode", by.y="v",all=T)

	return(V)
}
#
# =====================================================================================
# a partir d'une liste de variables en Question
# sort les shortcodes avec les repetitions
# =====================================================================================
#
MacroSelectionneQuestion<-function(v, dlu, nom) {
#v<-c("EXTR", "CONC", "PQ18");dlu<-mr$dlu;nom<-names(mr$data)

     i<-match(v, dlu$Question);summary(i)
     if (all(is.na(i))) {
          stop("\n\tAucune Question n'existe\n\n")
     }
     w1<-v[is.na(i)]
     if (any(is.na(i))) {
          w1<-data.frame(ShortCode=w1, names=rep(NA, length(w1)), trouvee=rep(0, length(w1)))
     } else {
          w1<-data.frame(ShortCode=v[1], names=NA, trouvee=0)[-1,]
     }
     w2<-v[!is.na(i)]

     j<-match(dlu$Question, w2);summary(j)
     w<-unique(as.character(dlu$ShortCode[!is.na(j)]))

     m<-MacroSelectionneShortCode(w, dlu, nom)
     m<-rbind(m, w1)
     m<-merge(m, dlu[, c("ShortCode", "Visit", "Form", "Question")], by="ShortCode", all.x=T, all.y=F)

	m$Question[m$trouvee==0]<-as.character(m$ShortCode)[m$trouvee==0]
	m$ShortCode[m$trouvee==0]<-NA
	m
}
#
# =====================================================================================
# Fabrique la fonction EnFacteur a partir du CLU
# je suppose que le code n'est que numerique
# =====================================================================================
#
MacroMetEnFacteur<-function(v, dlu, clu, nom) {
fct.cq<-function(x) {paste("c(\"", x, "\")", sep="")}
fct.c<-function(x) {paste("c(", x, ")", sep="")}

     lesv<-unique(as.character(clu$ShortCode))
     lesv<-MacroSelectionneShortCode(lesv, dlu, nom)
     iv<-match(lesv$ShortCode, v)
     if (all(is.na(iv))) {
          stop("\n\ Aucune variable categorielle selectionnee\n")
     }
     lesv<-lesv[!is.na(iv),]
     if (all(lesv$trouvee==0)) {
          stop("\n\ Les variables sont vides\n")
     }
     lesv<-lesv[lesv$trouvee==1,]


     v1<-lesv

     v1<-merge(v1, clu, by="ShortCode", all.x=T, all.y=F)
     v1<-v1[order(v1$CatCode),]

     cod1<-tapply(as.character(v1$CatCode), as.character(v1$names), c)
     cod1<-lapply(lapply(cod1, paste, collapse=", "), fct.c)

     val1<-tapply(as.character(v1$CatValue), as.character(v1$names), c)
     val1<-lapply(lapply(val1, paste, collapse="\", \""), fct.cq)
     
     x<-paste(unlist(cod1), unlist(val1), sep=", ")
     nomx<-names(cod1)
     x<-paste("\tdata$", nomx, ".f<-factor(data$", nomx, ", ", x, ")",sep="")

     x<-c("EnFacteur<-function(data) {", x, "\return(data)", "}")
     x<-paste(x, collapse="\n")
     
     fic<-paste(c(sample(LETTERS, 6), ".tmp"), collapse="")
     sink(fic)
          cat(x, "\n", sep="")
     sink()
     source(fic)
     file.remove(fic)
     
     cat("Pour creer les variables tapez\n\ndata<-EnFacteur(data)\n\n\n", sep="")

     invisible()
}
#
# =====================================================================================
# =====================================================================================
#
MacroRenommeShortCode<-function(v, new, dlu, nom) {
     if (length(new)!=length(v)) {
          stop("\n\tIl faut autant de v que de new\n")
     }
#
# les v existe en shortcode + recup des repet
#
     mssc<-MacroSelectionneShortCode(v, dlu, nom)
     if (all(mssc$trouvee==0)) {
          stop("\n\tAucun v n'est un ShortCode\n")
     }
#
# les news ne doivent pas deja exister
#
     vn<-data.frame(ShortCode=v, new=new)
     i<-match(vn$new, dlu$ShortCode)
     if (all(!is.na(i))) {
          stop("\n\tTous les new sont deja des ShortCodes\n")
     }
     vn$existe<-rep(1, dim(vn)[1])
     vn$existe[is.na(i)]<-0
#
# merge pour pouvoir ajouter les rep
#
     mssc<-merge(mssc, vn, by="ShortCode", all.x=T, all.y=F)
     if (any(mssc$trouvee==1 & mssc$existe==0)) {
          mssc$names<-as.character(mssc$names)
          mssc$ShortCode<-as.character(mssc$ShortCode)

          n1<-nchar(mssc$ShortCode)
          n2<-nchar(mssc$names)
          r<-substring(mssc$names, n1+1, n2)
          r<-ifelse(mssc$trouvee==1 & n2>n1, r, "")
          mssc$new.r<-ifelse(mssc$existe==0 & mssc$trouvee==1, paste(mssc$new, r, sep=""), "")
          mssc$id<-1:dim(mssc)[1]
#
# les noms sont changes
#
          msscok<-mssc[mssc$trouvee==1 & mssc$existe==0,]
          msscok$inom<-match(msscok$names, nom)
          nom[msscok$inom]<-msscok$new.r
          mssc<-merge(mssc, msscok[, c("id", "inom")], by="id", all=T)
#
# les ShortCodes dans dlu sont changes
#
          msscok<-mssc[mssc$trouvee==1 & mssc$existe==0,]
          msscok$idlu<-match(msscok$ShortCode, dlu$ShortCode)
          idlu<-unique(msscok$idlu)
          msscok<-msscok[match(idlu, msscok$idlu),]
          dlu$ShortCode<-as.character(dlu$ShortCode)
          dlu$ShortCode[msscok$idlu]<-as.character(msscok$new)

          mssc<-merge(mssc, msscok[, c("id", "idlu")], by="id", all=T)

          cat("\n")
          if (any(mssc$trouvee==0)) {cat("\tDes ShortCode n'ont pas ete trouves\n")}
          if (any(mssc$existe==1)) {cat("\tDes new existent deja\n")}
          res<-list(sc=mssc, nom=nom, dlu=dlu)

     } else {
          cat("\n\tpas de changement possible\n")
          res<-list(sc=mssc)
     }
     return(res)
}
