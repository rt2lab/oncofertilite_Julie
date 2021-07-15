
library("reshape2")


##################################################
### Univariée  #################################
##################################################


### Mettre en forme les donnees d'une univariee sur donnees de survie/ logistique
mise_en_forme <- function(x, nom, var_analyse, data, exit.p.value) {
  
  ## les fonctions à utiliser :
  tabuler_survie <- function(x) {
    cbind( x$ic$conf.int[, -2, drop = FALSE], x$ic$sctest[3]) # Recuere maitenant le test du score, d'une part pour avoir equivalence avec le log rank d'autre part pour gerer de faon approprie les variables numeriques
  }	
  
  ### Gestion des noms de variables
  noms_en_forme <- function(x, var_analyse,  nom, data) {
    if (! is.null(x)) {
      
      
      numero <- which(nom == var_analyse) ## du coup pour l'instant on ne peut plus utiliser les noms
      
      ligne <- cbind(categorie = gsub("\\.variable", "", rownames(x)), x)
      
      
      ligne <- cbind( variable = nom[numero], ligne)
      rownames(ligne) <- NULL
      
      
      if(class(data[, nom[numero]]) %in% c("factor", "ordered")) {
        premiere <- c(variable = nom[numero], categorie = levels(data[, nom[numero]])[1], OR = 1, ic_bas = "", ic_haut = "", p_value = "")
        ligne <- rbind(premiere, ligne)
        ligne[1, "p_value"] <- ligne[2, "p_value"]
      }
      # rownames(ligne)[1] <- nom[numero]
      # if (nrow(ligne) > 1) {
      # rownames(ligne)[2 : nrow(ligne)] <-  paste(nom[numero], 2 : nrow(ligne), sep = "")
      # }
      ##gsub("variable[[:print:]]*", "", names(tab)[[ii]], perl = TRUE)
    } else {
      ligne <- NULL
    }
    return(ligne)
  }
  
  ### recuperer liste avec uniquements les donnes interessantes
  if (class(x[[1]][[1]]) == "survfit") {
    tab <- lapply(x, FUN = tabuler_survie)
    nom_or <- "HR"
  } else {
    tab <- lapply(x, FUN = "[[", 1)
    nom_or <- "OR"
  }
  
  ## Mise en forme de tableau
  
  
  tab2 <- mapply(noms_en_forme, tab, var_analyse, MoreArgs = list(nom = nom, data = data), SIMPLIFY = FALSE)
  
  mat  <- do.call(rbind, tab2)
  
  ## Mise en forme finale
  colnames(mat) <- c("Variable", "Categorie", nom_or, "IC Bas", "IC haut", "p_value")
  if (! exit.p.value) mat[, "p_value"] <- mise_en_forme_p(as.numeric(mat[, "p_value"]))
  mat[, 3:5] <- as.character(round(as.numeric(mat[, 3:5]), 2))
  rownames(mat) <- NULL
  mat <- as.data.frame(mat, stringsAsFactors = FALSE)
  #mat[, 6] <- round(as.numeric(mat[, 6]), 4)	
  mat[, 3] <- paste(mat[, 3], " [", mat[, 4], "-", mat[, 5], "]", sep ="")
  mat <- mat[, -c(4, 5)]
  mat[, nom_or] <- gsub(" [NA-NA]", "", mat[, nom_or], fixed = TRUE)
  mat$p_value[duplicated(mat$Variable)] <- ""
  mat$Variable[duplicated(mat$Variable)] <- ""
  return(mat)
}	



#### Fonction de survie
survie <- function (variable, formule, data, nom, courbes = FALSE) {
  formule <- update(formule, ". ~ . + .variable")
  data$`.variable` <- variable
  
  survie <- survfit(formule, data=data)  # delaidfs dfsstatus
  table(variable) -> a
  if (any(a==0)) {
    return(survie)
  } else {
    lr <- survdiff(formule, data=data)
    test <- round(pchisq(lr$chisq, df = (dim(lr$n)-1), lower.tail=F),5)
    #ose <- lr$o/lr$e
    #rr <- ose/min(ose)
    ic <- summary(coxph(formule, data=data))
    out <- list("Survie"=survie, "Log Rank"=test, "ic"=ic)
    if (test<0.1 & courbes) {
      par(oma = c(4,0,0,2))
      survplot(survie, conf="none", label.curves=list(keys="lines"), levels.only=T, col=c("red", "blue", "black", "green3" ), time.inc=6, xlim = c(0,60), xlab="Mois", n.risk=T, y.n.risk=-0.4, cex.n.risk=1)
      numero <- as.numeric(gsub("\\D","", deparse(substitute(variable)), perl=T))
      titre <- paste("Overall survival : ", nom[numero])
      text(paste("Log Rank : ",test), x= 80, y=0.95)
      text(titre, xpd=T, x=36,y=1.1, font=2)
      text("Number at risk", x=0, y=-0.23, xpd=TRUE, font=2)
    }
    return(out)
  }
}

#### Fonction logistique
logistique <- function (variable, formule, data, nom) {
  ## changer la formule et inclure la variable
  formule <- update(formule, "~ .variable")
  data$`.variable` <- variable
  
  ## regression logistique et extraction des valeurs
  if(any(table(variable)== 0) ) {
    or <- NULL
  } else {	
    mod <- glm(formule, data = data, family = binomial)
    mod_sum <- summary(mod)$coefficients
    #conf <- confint(mod)
    conf <- cbind(mod_sum[, 1] - qnorm(0.975) * mod_sum[, 2] , mod_sum[, 1] + qnorm(0.975) * mod_sum[, 2] )
    or <- cbind(exp(mod_sum[, 1]), exp(conf), mod_sum[, 4])
    colnames(or)<- c("OR", "ic_bas", "ic_haut", "p_value")
    or <- or[-1, ,drop = FALSE]
  }
  ## les effectifs 
  formule2 <- as.formula(paste("~ .variable +", all.vars(formule)[1]))
  effectif <- xtabs(formule2, data = data)
  
  ## changer les dimnames
  numero <- as.numeric(gsub("\\D","", deparse(substitute(variable)), perl=T))
  names(dimnames(effectif))[1] <- nom[numero]
  
  ## les pourcentages
  pourcent <- round(100* prop.table(effectif, 2), 1)
  
  ## preparation de la sortie
  out <- list(or = or, Effectif = effectif, Pourcentage = pourcent)
  return(out)
}



#######################
### univariee ultime
#######################

#la formule ne doit pas ?tre mise entre guillemet

univariee <- function(var_analyse, FUN = c(survie, logistique), formule, data, nom = NULL, exit.p.value = FALSE,...) {
  if (is.null(nom)) nom <- var_analyse
  FUN <- match.fun(FUN)
  univ <- lapply(data[, var_analyse], FUN, formule, data = data, nom, ...)
  tab <- mise_en_forme(univ, nom, var_analyse, data = data, exit.p.value)
  return(tab)
}


##################################################################
### Mise en forme p-value	     #################################
##################################################################

mise_en_forme_p <- function(p) {
  out <- vector(length = length(p))
  for (ii in 1:length(p)) {
    if (p[ii] > 0.1) out[ii] <- paste(round(p[ii], 2))
    if (p[ii] <= 0.1 & p[ii] > 0.05) out[ii] <- paste(round(p[ii], 3))
    if (p[ii] <= 0.05 & p[ii] >= 0.0001) out[ii] <- paste(signif(p[ii], 2))
    if (p[ii] < 0.0001) out[ii] <- "<0.0001"
  }
  return(out)
}


##################################################################
### Tabuler	     #################################
##################################################################



### version n colonnes : tabuler ultime
tabuler <- function(vars, groupe = NULL, noms = NULL, typenum = "normal", avec_manquant = FALSE, tous = FALSE, IQR = FALSE, tester = TRUE, data = NULL, n_chiffre_virgule = 1)   {
  ## Vérifier les input
  if (class(vars) != "data.frame") vars <- data[, vars]
  if (class(groupe) == "character") groupe <- data[, groupe]
  nvar <- ncol(vars)
  if (is.null(nvar)) {
    nvar <- 1
    vars <- as.data.frame(vars)
  }
  if (is.data.frame(groupe)) groupe <- groupe[, 1]
  
  if (is.null(noms)) noms <- names(vars)
  
  ## initialiser les valeurs
  totalclass <- 0
  noms_bis <- vector("character", length = 0)
  type <- vector("character", length = nvar)
  
  ## préparer les noms et le nombre total de classe et le type de classe
  for (ii in 1:nvar) {
    
    type[ii] <- class(vars[, ii])[1]
    if(! type[ii] %in% c("factor", "ordered", "numeric", "integer")) stop(paste("Variable non prise en charge :", noms[ii], "(type : ", type[ii], ")"))
    if (type[ii] %in% c("factor", "ordered")) {
      if (avec_manquant == FALSE) {
        nbreclass <- length(levels(vars[, ii]))
        noms_bis_plus <- paste(noms[ii], ":", levels(vars[, ii]))
      } else {
        nbreclass <- length(levels(vars[, ii])) + 1
        noms_bis_plus <- paste(noms[ii], ":", c(levels(vars[, ii]), "Missing"))	
      }
    }
    if (type[ii] %in%  c("numeric", "integer")) {
      if (avec_manquant == FALSE) {
        nbreclass <- 1
        noms_bis_plus <- paste(noms[ii], ":")
      } else {
        nbreclass <- 2
        noms_bis_plus <- paste(noms[ii], c(":", ": Missing"))
      }
    }
    totalclass <- totalclass + nbreclass
    noms_bis <- c(noms_bis, noms_bis_plus)
  }
  
  ## si une variable de groupe est choisi
  if(! is.null(groupe)) {
    totalcol <- length(levels(groupe))
    mat <- matrix(NA, ncol = (totalcol * 2) + 1, nrow = totalclass)
    colnames(mat)[c(seq(1, 2 * totalcol - 1, by = 2), 2 * totalcol + 1)] <- c(paste(levels(groupe)) ,"p-value") 
    
    compteur <- 1
    nom <- rep(NA, nvar)
    for (ii in 1:nvar) {
      
      if (type[ii] %in% c("factor", "ordered")) {
        tab <- table(vars[ ,ii], groupe)
        if(avec_manquant == TRUE) tab <- table(vars[ ,ii], groupe, useNA = "always")
        tab2 <- round(100 * prop.table(tab, margin=2), n_chiffre_virgule)
        tab_test <- table(vars[ ,ii], groupe)
        if(tester == TRUE) {
          test <- chisq.test(tab_test, correct=FALSE)
          if ( all(test$expected >= 5)) {
            test <- chisq.test(tab_test, correct=FALSE)
            test <- test$p.value
          } else {
            if (all(test$expected > 3)) {
              test <- chisq.test(tab_test, correct=TRUE)
              test <- test$p.value
            } else {
              safe_fisher.test <- failwith(NULL, fisher.test, quiet = TRUE)
              test <- safe_fisher.test(tab_test)
              if(is.null(test)) test <- chisq.test(tab_test, correct=FALSE, simulate.p.value = TRUE, B = 10000)
              test <- test$p.value
            }
          }
        } else {
          test <- NA
        }
        ## Partie qui range tout dans les bonnes cases
        if (avec_manquant == FALSE) boucle2 <- length(levels(vars[ , ii]))
        if (avec_manquant == TRUE) boucle2 <- length(levels(vars[ , ii])) + 1
        for (jj in 1:boucle2) {
          for (kk in seq(1, totalcol)) {					
            mat[compteur , 2 * kk - 1] <- tab[jj, kk]
            mat[compteur , 2 * kk ] <- paste("(", tab2[jj, kk], "%)", sep="")
          }				
          mat[compteur , (2 * totalcol) + 1] <- test
          compteur <- compteur + 1
        }
      }
      if (type[ii] %in% c("numeric","integer")) {
        var1 <- vars[, ii]
        groupe2 <- levels(groupe)
        
        if (avec_manquant == TRUE) {
          manq_nbre <- tapply(is.na(var1), groupe, sum)
          manq_pourc <- round(100 * manq_nbre / table(groupe) , n_chiffre_virgule)
        }
        
        if (typenum  != "normal") {			
          for (kk in seq(1, totalcol)) {
            assign(paste("med", kk, sep =""), median(var1[groupe == groupe2[kk]], na.rm = TRUE))
            assign(paste("ran", kk, sep =""), range(var1[groupe == groupe2[kk]], na.rm = TRUE))
            if (IQR == TRUE) assign(paste("ran", kk, sep =""), quantile(var1[groupe == groupe2[kk]], probs = c(0.25, 0.75), na.rm = TRUE))
          }							
          if (tester == TRUE) {
            test2 <- kruskal.test(var1, groupe)
            test <- test2$p.value 
          } else {
            test <- NA
          }
          for (kk in seq(1, totalcol)) {					
            mat[compteur, 2 * kk - 1] <- get(paste("med", kk, sep =""))
            mat[compteur, 2 * kk] <- paste("(range ", get(paste("ran", kk, sep =""))[1], "-", get(paste("ran", kk, sep =""))[2], ")", sep="")
            if (IQR == TRUE) mat[compteur, 2 * kk] <- paste("(IQR ", get(paste("ran", kk, sep =""))[1], "-", get(paste("ran", kk, sep =""))[2], ")", sep="")
            mat[compteur, (2 * totalcol) + 1] <- test			
          }
        }		
        if (typenum == "normal") {
          
          for (kk in seq(1, totalcol)) {
            assign(paste("moy", kk, sep =""), signif(mean(var1[groupe == groupe2[kk]], na.rm = TRUE), 4))
            assign(paste("et", kk, sep =""), signif(sd(var1[groupe == groupe2[kk]], na.rm = TRUE), 3))
          }
          
          if( tester == TRUE) {
            test1 <- lm(var1 ~ groupe)
            test1 <- anova(test1)
            test <- test1[["Pr(>F)"]][1] 
          } else {
            test <- NA
          }
          
          for (kk in seq(1, totalcol)) {
            mat[compteur, 2 * kk - 1] <- get(paste("moy", kk, sep =""))
            mat[compteur, 2 * kk] <- paste("(+/- ", get(paste("et", kk, sep ="")), ")", sep="")
            mat[compteur, (2 * totalcol) + 1] <- test				
          }
        }
        if (avec_manquant == TRUE){
          for (kk in seq(1, totalcol)) {
            mat[compteur + 1, 2 * kk - 1] <- manq_nbre[kk]
            mat[compteur + 1, 2 * kk] <- paste("(", manq_pourc[kk], "%)", sep ="")
            mat[compteur + 1, (2 * totalcol) + 1] <- test
          }
          compteur <- compteur + 2
        }
        if (avec_manquant == FALSE) {
          compteur <- compteur + 1
        }
        
      }	
    }
    
    
    ## fusionner les résultats moy/N et sd/% en une seule colonne
    for (kk in seq(1, 2 * totalcol - 1, by = 2)) {
      mat[, kk] <- paste(mat[, kk], mat[, kk + 1])
    }
    mat <- mat[, c(seq(1, 2 * totalcol - 1, by = 2), 2 * totalcol + 1)]
    
    ## Gérer les noms de variable
    mat2 <- colsplit(noms_bis, ":", names = c("Variable", "Category"))
    
    ## Gérer les p_values
    if (tester == TRUE) {
      mat[, "p-value"] <- mise_en_forme_p(as.numeric(mat[, "p-value"]))
      mat[duplicated(mat2[, 1]), "p-value"] <- ""
    } else {
      mat <- mat[ , ! colnames(mat) %in% "p-value"]
    }
    
    ## Gérer les duplications de noms
    mat2[duplicated(mat2[, 1]), 1] <- ""
    
    ## fusionner noms et valeurs
    mat <- cbind(mat2, mat)	
  } else {
    ## Gérer les noms de variable
    mat <- colsplit(noms_bis, ":", names = c("Variable", "Category"))
    ## Gérer les duplications de noms
    mat[duplicated(mat[, 1]), 1] <- ""
    ## passage de force de tous à TRUE sinon le programme ne renvoie aucune données
    tous <- TRUE
  }
  
  ## ajouter éventuellement la colonne total
  if(tous == TRUE) {
    mat3 <- vector(length = 0)
    for (ii in 1:nvar) {
      if (type[ii] %in% c("numeric", "integer")) {
        if (typenum == "normal") {
          mat3 <- c(mat3, paste(round(mean(vars[, ii], na.rm = TRUE), n_chiffre_virgule), " (+/- ", round(sd(vars[, ii], na.rm = TRUE), n_chiffre_virgule), ")", sep = ""))
        } else {
          if (IQR == FALSE) {
            mat3 <- c(mat3, paste(median(vars[, ii], na.rm = TRUE), " (range ", range(vars[, ii], na.rm = TRUE)[1], "-", range(vars[, ii], na.rm = TRUE)[2], ")", sep = ""))
          } else { 
            mat3 <- c(mat3, paste(median(vars[, ii], na.rm = TRUE), " (IQR ", quantile(vars[, ii], probs = 0.25, na.rm = TRUE), "-", quantile(vars[, ii], probs = 0.75, na.rm = TRUE), ")", sep = ""))
            
          }
        }
        if (avec_manquant == TRUE) {
          manq_nbre <- sum(is.na(vars[, ii]))
          manq_pourc <- round(100 * manq_nbre / length(vars[, ii]), n_chiffre_virgule )
          mat3 <- c(mat3, paste(manq_nbre, "(", manq_pourc, "%)", sep =""))
        }
        
      }
      if (type[ii] %in% c("factor","ordered")) {
        if (avec_manquant == FALSE) tab1 <- table(vars[ ,ii])
        if (avec_manquant == TRUE) tab1 <- table(vars[ ,ii], useNA = "always")				
        tab2 <- round(100 * tab1 / sum(tab1), n_chiffre_virgule)
        tab3 <- paste(tab1, " (", tab2, "%)", sep = "")
        mat3 <- c(mat3, tab3)
      }
    }
    mat <- cbind(mat, "ALL" = mat3)
  }
  
  ## dernier détails esthétiques
  mat[is.na(mat$Category), "Category"] <- ""
  rownames(mat) <- NULL
  
  return(mat)
}




##### Realisation de tableau quand la variable à predire est continue

num_predite <- function(y, x) {
  type <- class(x)
  
  ## Cas numeric
  
  if (type %in% c("numeric", "integer")) {
    mod <- lm(y ~ x)  
    sum_mod <- summary(mod)$coefficients[2, ]
    
    out <- matrix(nrow = 1, ncol = 2, 
                  c(paste0(round(sum_mod[1], 2), "(+/-", round(sum_mod[2], 2), ")"),
                    mise_en_forme_p(sum_mod[4])
                  )
    )
    
    out <- cbind("", out)
    
  }
  
  ## Cas des facteurs
  
  if (type %in% c("factor", "ordered")) { 
    a <- tapply(y, x, mean, na.rm = TRUE)
    b <- tapply(y, x, sd, na.rm = TRUE)
    d <- matrix(paste0(round(a, 1), "(+/-", round(b, 1), ")"), ncol = 1)
    d <- cbind(levels(x), d)
    #p <- t.test(y ~ x)$p.value  # ne marche que si deux groupes
    p <- anova(lm(y ~ x))[1, 5]
    out <- cbind(d, "")
    out[1, 3] <- mise_en_forme_p(p) 
    
  }  
  
  return(out)
  
}


tab_num <- function(predite, predicteurs, donnees) {
  
  res <- lapply(predicteurs, function(xs) {
    
    num_predite(y = donnees[, predite], x = donnees[, xs])
  }
  )
  
  out <- mapply(cbind, predicteurs, res)
  
  out <- do.call(rbind, out)
  
  out[duplicated(out[, 1]), 1] <- ""
  colnames(out) <- c("Variable", "Categories", "Moy_Coeff", "p_value")
  
  return(out)
} 






##############################################################################
## Fonctions de presentations des resultats de multivariee ###################
##############################################################################
glm_summary <- function(x) {
  y <- summary(x)
  z <- data.frame("Odds Ratio" = exp(coef(x)), "IC_inf." = exp(confint.default(x)[, 1]), "IC_sup." = exp(confint.default(x)[, 2]), "P_value" = y$coefficients[,4])
  z <- z[-1, ]
  z[, 1:3] <- round(z[, 1:3], 2)
  z[, 4] <- signif(z[, 4], 4)
  return(z)
}
glm_summary2 <- function(x) {
  ## le programme retourne la p value de la variable globale d'apres drop1 (erreur possible si valeurs manquantes...)
  
  ## A faire : permettre de passer comme arguments les noms des variables
  mod <- x$model[, -1] ## les variables utilisees
  type <- sapply(mod, class)
  y <- summary(x)
  z <- data.frame("Odds Ratio" = exp(coef(x)), "IC_inf." = exp(confint.default(x)[, 1]), "IC_sup." = exp(confint.default(x)[, 2]), "P_value" = y$coefficients[,4])
  z <- z[-1,]
  z[, 1:3] <- round(z[, 1:3], 2)
  w <- data.frame("OR" = paste(z[, 1], " [IC 95% ", z[, 2], "-", z[, 3], "]", sep = ""), "p-value" = mise_en_forme_p(z[, 4]), stringsAsFactors = FALSE)
  
  a <- drop1(x, test = "Chisq")
  p <- mise_en_forme_p(a[[5]][-1])
  
  out <- data.frame("Variable" = NA, "Categorie" = NA, "OR" = NA, "p" = NA)
  compteur <- 1
  for (ii in 1: length(type)) {
    if (type[ii] %in% c("numeric", "integer")) {
      out2 <- cbind(colnames(mod)[ii], "+ 1 ", w[compteur, ])
      out2[4] <- p[ii] 
      compteur <- compteur + 1
    }
    if (type[ii] %in% c("factor", "ordered")) {
      out2 <- data.frame("Variable" = rep(colnames(mod)[ii], length(levels(mod[, ii]))), "Categorie" = levels(mod[, ii]), "OR" = NA, "p-value" = p[ii], stringsAsFactors = FALSE)
      out2[1, 3] <- "1"
      out2[-1, 3:4] <- w[compteur:(compteur + length(levels(mod[, ii])) - 2), ]
      compteur <- compteur + length(levels(mod[, ii])) - 1 
    }
    names(out2) <- names(out)
    out <- rbind(out, out2)
  }
  
  ## Mise en forme
  out <- out[-1, ]
  rownames(out) <- NULL
  out[duplicated(out[, 1]), c(1, 4)] <- ""
  out[out$p != "", 4] <- p
  return(out)
}

lm_summary2 <- function(x) {
  ## le programme retourne la p value de la variable globale d'apres drop1 (erreur possible si valeurs manquantes...)
  
  ## A faire : permettre de passer comme arguments les noms des variables
  mod <- x$model[, -1] ## les variables utilisees
  type <- sapply(mod, class)
  y <- summary(x)
  z <- data.frame("Coef" = coef(x), "IC_inf." = confint.default(x)[, 1], "IC_sup." = confint.default(x)[, 2], "P_value" = y$coefficients[,4])
  z <- z[-1,]
  z[, 1:3] <- round(z[, 1:3], 2)
  w <- data.frame("Coef" = paste(z[, 1], " [IC 95% ", z[, 2], "-", z[, 3], "]", sep = ""), "p-value" = mise_en_forme_p(z[, 4]), stringsAsFactors = FALSE)
  
  a <- drop1(x, test = "Chisq")
  p <- mise_en_forme_p(a[[5]][-1])
  
  out <- data.frame("Variable" = NA, "Categorie" = NA, "Coef" = NA, "p" = NA)
  compteur <- 1
  for (ii in 1: length(type)) {
    if (type[ii] %in% c("numeric", "integer")) {
      out2 <- cbind(colnames(mod)[ii], "+ 1 ", w[compteur, ])
      out2[4] <- p[ii] 
      compteur <- compteur + 1
    }
    if (type[ii] %in% c("factor", "ordered")) {
      out2 <- data.frame("Variable" = rep(colnames(mod)[ii], length(levels(mod[, ii]))), "Categorie" = levels(mod[, ii]), "Coef" = NA, "p-value" = p[ii], stringsAsFactors = FALSE)
      out2[1, 3] <- "0"
      out2[-1, 3:4] <- w[compteur:(compteur + length(levels(mod[, ii])) - 2), ]
      compteur <- compteur + length(levels(mod[, ii])) - 1 
    }
    names(out2) <- names(out)
    out <- rbind(out, out2)
  }
  
  ## Mise en forme
  out <- out[-1, ]
  rownames(out) <- NULL
  out[duplicated(out[, 1]), c(1, 4)] <- ""
  out[out$p != "", 4] <- p
  return(out)
}

cox_summary2 <- function(x) { # x un objet de class copxh
  ## x <- coxph(Surv(delaios, status) ~ age + mnbrecl + msync + mtaillecl, data = periop)
  
  ## Attention au calcul des p-values si valeurs manquantes....
  
  y <- summary(x)
  z <- data.frame("Hazard_Ratio" = y$conf.int[, 1, drop = FALSE], "IC_inf." = y$conf.int[, 3], "IC_sup." = y$conf.int[, 4], "P_value" = y$coefficients[, 5])	
  z[, 1:3] <- round(z[, 1:3], 2)
  z[, 4] <- signif(z[, 4], 4)
  
  w <- data.frame("HR" = paste(z[, 1], " [IC 95% ", z[, 2], "-", z[, 3], "]", sep = ""), "p-value" = mise_en_forme_p(z[, 4]), stringsAsFactors = FALSE)
  
  ## les classes des variables 
  types <- attr(x$terms, "dataClasses")[- 1]
  variable <- names(types)
  ## les levels des facteurs (sous forme de liste)
  lev <- x$xlevels
  
  ## les p-values correct pour les facteurs ?? plus de 2 classes
  a <- drop1(x, test = "Chisq")
  p <- mise_en_forme_p(a[[4]][-1])
  
  ## la longeur du tableau
  longeur <- sum(types %in% c("numeric", "integer")) + sum(sapply(lev, length))
  
  out <- matrix(vector(mode = "character", length = 0), ncol = 4)
  colnames(out) <- c("Variable", "Category", "HR", "p")
  compteur <- 1
  for (ii in seq_along(variable)) {
    if(types[ii] %in% c("numeric", "integer")) {
      out2 <- cbind("Variable" = variable[ii], "Category" = "+1", w[compteur, ])
      colnames(out2) <- colnames(out)
      compteur <- compteur + 1
    }
    if(types[ii] %in% c("factor", "ordered")) {
      long <- length(lev[[variable[ii]]])
      out2 <-  matrix(c(rep(variable[ii], long), lev[[variable[ii]]], c("1",w[(compteur: (compteur + long - 2)), 1]), c("42",w[(compteur: (compteur + long - 2)), 2])), ncol = 4)
      compteur <- compteur + long - 1
      colnames(out2) <- colnames(out)
    }
    if(grepl("nmatrix\\.[[:digit:]]", types[ii])) {
      long <- as.numeric(gsub("nmatrix\\.([[:digit:]])",  "\\1", types[ii],))
      out2 <-  matrix(c(rep(variable[ii], long), c(paste("Segment", 1:long)), c(w[compteur:(compteur + long - 1), 1]), c(w[compteur:(compteur + long - 1), 2])), ncol = 4)
      compteur <- compteur + long
      colnames(out2) <- colnames(out)
    }
    out <- rbind(out, out2)
  }
  
  ## Mise en forme finale
  out[, c(1:2, 4)] <- apply(out[, c(1,2, 4)], 2, as.character) ## sinon probleme de levels de facteur inconnue à la ligne suivante
  out[duplicated(out[, 1]), c(1, 4)] <- ""
  out[out[, 4] != "", 4] <- p
  
  return(out)
}


convertisseur <- function(y) {
  out  <- matrix(vector(mode = "character", length = 0), ncol = 3)
  for (ii in 1:nrow(y)) {
    if (y[ii, 1] != "") {
      out2 <- matrix(c(y[ii, 1], "", y[ii, 4], y[ii, 2], y[ii, 3], ""), ncol = 3, byrow = TRUE)
      
    } else {
      out2 <- c(y[ii, 2], y[ii, 3], "")
    }
    out <- rbind(out, out2)		
  }
  rownames(out) <- NULL
  colnames(out) <- c("Variable", names(y)[3], "p")
  out <- as.data.frame(out)
  return(out)
}



km_summary2 <- function(km, time = NULL, units = "Mois") {
  
  
  ## A faire : prévoir le cas ou une strate est vide (tous mort)
  
  if(is.null(time)) {
    out <- suivi_median_summary(km)
  } else {
    mediane <- suivi_median_summary(km)
    temps <- summary(km, time = time)
    
    ## calcul et mise en forme des survies
    surv_ic <- cbind(temps$surv, temps$lower, temps$upper)
    surv_ic <- round(100 * surv_ic, 1)
    surv_ic <- paste(surv_ic[, 1], "[", surv_ic[, 2], "-", surv_ic[, 3], "]", sep ="")
    
    if (is.null(temps$strata)) {
      tab1 <- data.frame("Strata" = "Tous", "time" = temps$time, "survie" = surv_ic)
      tab2 <- data.frame("Strata" = "Tous", "time" = -1, "survie" = c(mediane))			
    } else {
      tab1 <- data.frame("Strata" = gsub("[[:alnum:][:punct:]]+=", "", temps$strata, perl = TRUE), "time" = temps$time, "survie" = surv_ic)
      tab2 <- data.frame("Strata" = rownames(mediane), "time" = -1, "survie" = c(mediane))
    }
    
    
    tab <- rbind(tab1, tab2) 
    
    out <- dcast(tab, Strata ~ time, value.var = "survie")
    colnames(out) <- c("Strata", "Mediane", paste(time, units))
    
  }
  return(out)
}

suivi_median_summary <- function(km) {
  ### definition d'une fonction generique
  medianise <- function(tab) {
    a <- with(tab, c(time[surv <= 0.5][1], time[lower <= 0.5][1], time[upper <= 0.5][1]))
    a <- round(a, 1)
    a <- paste(a[1], "[", a[2], "-", a[3], "]", sep ="")
    return(a)
  }
  
  ## Application diffèrente selon le nombre de strate
  
  if(is.null(km$strata)) {
    tab <- data.frame("time" = km$time, "surv" = km$surv, "upper" = km$upper, "lower" = km$lower)
    out <- medianise(tab)
  } else {
    tab <- data.frame("time" = km$time, "surv" = km$surv, "upper" = km$upper, "lower" = km$lower)
    n <- length(km$strata)
    groupe <- rep(1:n, km$strata)
    
    tab_split <- split(tab, groupe)
    out <- lapply(tab_split, medianise)
    names(out) <- gsub("[[:alnum:][:punct:]]+=", "", names(km$strata), perl = TRUE)
    
    out <- do.call(rbind, out)
    colnames(out) <- "Mediane"
    
  }
  return(out)
}



##############################################################################
### Recherche automatique de seuil		 ###################################
##############################################################################

## version pour 2 seuils : sorties completes avec graph et HR et interval

recherche.seuil <- function(seuils, formula, data) {
  
  # Initialisation des valeurs
  p_seuils <- seuils * 0
  conf_seuils <- matrix(rep(seuils, 3), ncol = 3)
  effectifs_seuils <- matrix(c(p_seuils, p_seuils), ncol = 2)
  rownames(effectifs_seuils) <- seuils
  colnames(effectifs_seuils) <- c("Inferieur au seuil", "Superieur au seuil")
  
  ## Manipulation de la formula
  variable <- all.vars(formula)[3] ## adapt? uniquement au modele de Cox pour le moment
  formula <- update(formula, . ~ groupe_seuil)
  
  # Boucle calculant les param�tres
  for (ii in seq_along(seuils)) {
    ## s�paration des groupes
    data$groupe_seuil <- ifelse(data[variable] < seuils[ii], 0, 1)
    effectifs_seuils[ii, ] <- table(data$groupe_seuil)
    
    # realisation du modele de Cox et export des parametres d'interets
    model <- coxph(formula, data = data)
    conf_seuils[ii, 1] <- coef(model)
    conf_seuils[ii, 2:3] <- confint(model)
    p_seuils[ii] <- pchisq(model$score, df = 1, lower.tail = FALSE)
  }
  
  ## Mise en forme des donnees
  conf_seuils <- exp(conf_seuils)
  out <- data.frame(seuils, effectifs_seuils, conf_seuils, p = p_seuils, signif = NA)
  out$signif <- factor(ifelse(out$p < 0.05, "Significatif", "Non significatif"))
  colnames(out)[4:6] <- c("HR", "conf.inf", "conf.sup")
  
  ## R�alisation des graphiques
  graph_pvalue <- ggplot(out, aes(x = seuils, y = p )) + geom_point(aes(color = signif)) + geom_smooth(se= FALSE, method = loess) + geom_hline(aes(yintercept = 0.05)) + labs( x = "Seuil", y = "P-value", color = "Significativit�") + scale_colour_brewer(palette = "Set1")
  graph_hr <- ggplot(out, aes(x = seuils, y = HR )) + geom_point(aes(color = signif)) + geom_smooth(se= FALSE, method = loess) + geom_hline(aes(yintercept = 1)) + geom_errorbar(aes(ymin = conf.inf, ymax = conf.sup, color = signif)) + labs( x = "Seuil", y = "HR (IC 95%)", color = "Significativit�") + scale_colour_brewer(palette = "Set1")
  
  multiplot(graph_hr, graph_pvalue)
  
  ## export du r�sultat final
  out <- list(tableau = out, "graph_pvalue" = graph_pvalue, "graph_hr" = graph_hr)
  return(out)
}

## version �tendu � n classe mais sortie plus limit� par impossibilit� de repr�senter facilement les r�sultats ....

recherche.seuil3 <- function(seuils, nclasse = 3, formula, data, effectif_mini = 10, parallel = FALSE) {
  
  ## Manipulation de la formula
  variable <- all.vars(formula)[3] ## adapté uniquement au modele de Cox pour le moment
  formula <- update(formula, . ~ groupe_seuil)
  
  ## "Boucle" calculant les points de coupure
  coupures <- eval(parse(text = paste("expand.grid(", paste(rep("seuils", nclasse - 1), collapse =","), ")", sep ="")))
  idx <- apply(coupures, MARGIN = 1, function(x) all(diff(x) >= 2))
  coupures <- coupures[idx, ]
  
  mini <- min(data[, variable], na.rm = TRUE)
  maxi <- max(data[, variable], na.rm = TRUE) + 1
  coupures2 <- data.frame(rep(mini, nrow(coupures)), coupures, rep(maxi, nrow(coupures) ))
  
  # Boucle calculant les paramètres
  coupes_plusieurs <- function(x, data , formula , variable) {
    ## séparation des groupes
    data$groupe_seuil <- cut(data[, variable], breaks = x, include.lowest = TRUE)
    effectifs_seuils <- table(data$groupe_seuil)
    
    if( any(effectifs_seuils < effectif_mini)) {
      return(c(effectifs_seuils, rep(NA, nclasse - 1), NA, NA))
    } else {
      # realisation du modele de Cox et export des parametres d'interets
      model <- coxph(formula, data = data)
      hr <- exp(coef(model))
      hr_moy <- mean(hr)
      #conf_seuils[ii, 2:3] <- confint(model)
      p_seuils <- pchisq(model$score, df = nclasse - 1, lower.tail = FALSE)
      
      return(c(effectifs_seuils, hr, hr_moy, p_seuils))
    }
  } 
  if (parallel) {
    res <- t(parApply(cl, coupures2, MARGIN = 1, FUN = coupes_plusieurs, data, formula, variable))
  } else {
    res <- t(apply(coupures2, MARGIN = 1, FUN = coupes_plusieurs, data, formula, variable))
  }
  out <- cbind(coupures, res)
  colnames(out) <- c(paste("seuil", 1:(nclasse - 1), sep = ""),
                     paste("effectif_groupe", 1:nclasse, sep = ""), paste("hr", 1:(nclasse - 1), sep = ""),"hr_moyen", "p_value") 
  rownames(out) <- NULL
  out <- as.data.frame(out)
  out <- out[! is.na(out$p_value),]
  out$signif <- factor(ifelse(out$p_value < 0.05, "Significatif", "Non significatif"))
  
  graph <- ggplot(out, aes(x = seuil1, y = seuil2, size = 1 - p_value, color = 1 - p_value)) + geom_point() + scale_colour_gradient()
  print(graph)
  #	
  #	graph <- ggplot(out, aes(x = seuil1, y = seuil2, size = 1 - p_value, color = signif)) + geom_point() + scale_colour_brewer(palette = "Set1")
  #	print(graph)
  
  return(out)
  
}




recherche.seuil.glm <- function(seuils, formula, data) {
  
  # Initialisation des valeurs
  p_seuils <- seuils * 0
  conf_seuils <- matrix(rep(seuils, 3), ncol = 3)
  effectifs_seuils <- matrix(c(p_seuils, p_seuils), ncol = 2)
  rownames(effectifs_seuils) <- seuils
  colnames(effectifs_seuils) <- c("Inferieur au seuil", "Superieur au seuil")
  
  ## Manipulation de la formula
  variable <- all.vars(formula)[2] ## adapt? uniquement au modele de Cox pour le moment
  formula <- update(formula, . ~ groupe_seuil)
  
  # Boucle calculant les param?tres
  for (ii in seq_along(seuils)) {
    ## s?paration des groupes
    data$groupe_seuil <- ifelse(data[variable] < seuils[ii], 0, 1)
    effectifs_seuils[ii, ] <- table(data$groupe_seuil)
    
    # realisation du modele de Cox et export des parametres d'interets
    
    
    model <- glm(formula, data = data, family = "binomial")
    conf_seuils[ii, 1] <- coef(model)[2]
    conf_seuils[ii, 2:3] <- confint(model)[2, ]
    p_seuils[ii] <- summary(model)$coefficients[2, 4]
  }
  
  ## Mise en forme des donnees
  conf_seuils <- exp(conf_seuils)
  out <- data.frame(seuils, effectifs_seuils, conf_seuils, p = p_seuils, signif = NA)
  out$signif <- factor(ifelse(out$p < 0.05, "Significatif", "Non significatif"))
  colnames(out)[4:6] <- c("HR", "conf.inf", "conf.sup")
  
  ## R?alisation des graphiques
  graph_pvalue <- ggplot(out, aes(x = seuils, y = p )) + geom_point(aes(color = signif)) + geom_smooth(se= FALSE, method = loess) + geom_hline(aes(yintercept = 0.05)) + labs( x = "Seuil", y = "P-value", color = "Significativit?") + scale_colour_brewer(palette = "Set1")
  graph_hr <- ggplot(out, aes(x = seuils, y = HR )) + geom_point(aes(color = signif)) + geom_smooth(se= FALSE, method = loess) + geom_hline(aes(yintercept = 1)) + geom_errorbar(aes(ymin = conf.inf, ymax = conf.sup, color = signif)) + labs( x = "Seuil", y = "HR (IC 95%)", color = "Significativit?") + scale_colour_brewer(palette = "Set1")
  
  multiplot(graph_hr, graph_pvalue)
  
  ## export du resultat final
  out <- list(tableau = out, "graph_pvalue" = graph_pvalue, "graph_hr" = graph_hr)
  return(out)
}






##############################################################################
## Affichage de plusieurs plot ggplot 2 ###################################
##############################################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




######################################################################
### 			ROC POWER 	################################################
######################################################################


rocpower  <- function(auc1, auc2, alpha = 0.05, beta = 0.2) {
  ## calcul le nombre de sujet a inclure PAR GROUPE
  ## pour la comparaison de 2 courbes ROC dont l'AUC estime est donnee en entree
  ## le test est non param�trique bas� sur le Wilcoxon selon Hanley et Mac Neil, Radiology 1982
  ## par d�faut un test bilateral est fait
  
  q11 <- auc1 / (2  - auc1)
  q12 <- 2 * (auc1 ^2) / ( 1 + auc1)
  q21 <- auc2 / (2  - auc2)
  q22 <- 2 * (auc2 ^2) / ( 1 + auc2)
  sigma <- auc2 - auc1
  v1 <- q11 + q12 - 2 * auc1^2
  v2 <- q21 + q22 - 2 * auc2^2
  n <- ((qnorm(alpha / 2, lower.tail = FALSE) * sqrt(2 * v1) + qnorm(beta, lower.tail = FALSE) * sqrt(v1 + v2) )/ (auc1 - auc2))^2
  return(ceiling(n))
} 



binconf2 <- function(x, succes = "oui", ...) {
  a <- binconf(sum(x == succes), length(x), include.x =  FALSE, return.df=TRUE)
  names(a) <- c("y", "ymin", "ymax")
  return(a)
}




###################################
####### Puissance survie  #########
###################################

sample_survie <- function (pourcent_survie1, pourcent_survie2 = NULL, hr = NULL, teval, ttotal, taccrual, alpha = 0.05, beta = 0.2, k = 1, gamma = 0.0000001)  {
  
  ### On peut préciser 3 temps :
    ## teval = le temps auquel on calcul le CPJ
    ## ttotal = la durée totale de l'esaai
    ## taccrual = sur combien de temps on inclus
  ## on précise la survie dans le groupe controle et soit le HR soit la survie dans le groupe expérimental
  
  ### gamma = 0 = entrée uniforme mais ne peut pas être 0 stricte sinon NaN
  
  if(is.null(pourcent_survie2) & is.null(hr)) stop("IL faut remplir HR ou pourencet survie 2")
  
  lam1 <- - log(pourcent_survie1) / teval
  
  if(is.null(pourcent_survie2)) {
    lam2 <- lam1 * hr
    pourcent_survie2 <- - exp(lam2 * teval)
  }
  
  
  if(is.null(hr)) {
    lam2 <- - log(pourcent_survie2) / teval
    hr <- lam2 / lam1
  }
  
  
  
  variance1 <- lam1^2 * (1 + gamma * exp(-lam1 * ttotal) * 
                           (1 - exp((lam1 - gamma) * taccrual))/((lam1 - gamma) * 
                                                                   (1 - exp(-gamma * taccrual))))^-1
  variance2 <- lam2^2 * (1 + gamma * exp(-lam2 * ttotal) * 
                           (1 - exp((lam2 - gamma) * taccrual))/((lam2 - gamma) * 
                                                                   (1 - exp(-gamma * taccrual))))^-1

  n2 = (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 * (variance1/k + 
                                                     variance2)/(lam1 - lam2)^2
  
  
  out <- data.frame("Survie groupe 1" = pourcent_survie1, "Survie groupe 2" = pourcent_survie2, "HR" = hr,  "Total" = ceiling(n2) + ceiling(n2 * k) ,"Groupe_control" = ceiling(n2), "Groupe_Experimental" = ceiling(n2 * k))
  print(out)
  
}

cox_summary <- function(x) {
  y <- summary(x)
  z <- data.frame("Hazard_Ratio" = y$conf.int[, 1, drop = FALSE], "IC_inf." = y$conf.int[, 3], "IC_sup." = y$conf.int[, 4], "P_value" = y$coefficients[, 5])
  z[, 1:3] <- round(z[, 1:3], 2)
  z[, 4] <- signif(z[, 4], 4)
  return(z)
}
