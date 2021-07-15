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
              safe_fisher.test <- possibly(fisher.test, NULL,quiet = TRUE)
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

