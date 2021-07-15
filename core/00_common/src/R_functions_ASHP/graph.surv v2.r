# del : variable délais
# evt : variable événement
# gp : variable de stratification (pour logrank éventuel)
# tps : séquence des temps à afficher sur le graphique et ds les données
# col : séquence des couleurs à attribuer aux courbes

# DESCRIPTION :
# trace une ou des courbes de survie en affichant sous le graphique les
# effectifs à risque à chaque temps

# pour faire apparaitre les IC: ajouter IC=c(), avec les temps où l'on veut les IC dans les arguments de la fonction quand on la lance


graph.surv<-function(del,evt,     # del : variable délais;   evt : variable événement
                     gp = 1,      # gp : variable de stratification (pour logrank éventuel)
                     tps = NULL,  # tps : séquence des temps à afficher sur le graphique et ds les données
                     col = 1:length(gp), lwd = 2, lty = 1, # col : séquence des couleurs à attribuer aux courbes
                     IC = NULL, decalIC = 0,
                     legend = NULL, xlab = "", ylab = "", 
                     explab = "number at risk", ...)

     {
            require(survival)

            if (all(gp == 1)) {
              nb <- 1
              calc <- survfit(Surv(del, evt) ~ 1)
            }else{
              nb <- dim(table(gp))
              calc <- survfit(Surv(del, evt) ~ gp)
            }

            temps <- calc$time
            t.sort <- sort(temps)

            if (is.null(tps)) {
              tps <- seq(0, t.sort[length(t.sort)], (t.sort[length(t.sort)])%/%10)
            }

            if (length(col) != nb) {
              col <- rep(col, length.out = nb)
            }

            if (length(lwd) != nb) {
              lwd <- rep(lwd, length.out = nb)
            }

            if (length(lty) != nb) {
              lty <- rep(lty, length.out = nb)
            }

            plot(calc, conf = F, col = col,
                 xlim=c(-tps[length(tps)]/7,tps[length(tps)]+tps[length(tps)]/20),
                 xaxt = "n", ylim = c(-0.3 - 0.1 * nb, 1), yaxt = "n",
                 xlab = "", ylab = "", lwd = lwd, lty = lty,
                 mark.time = F, frame = F, ...)

            n <- summary(calc, time = tps)
            lev <- levels(n$strata)
            risk <- NULL
            if (nb == 1) {
              risk <- n$n.risk
            }else {
              for (i in 1:nb) {
                if (length(n$n.risk[n$strata==lev[i]])<length(tps)) {
                  risk<-c(risk, n$n.risk[n$strata == lev[i]],
                          rep(0,length(tps)-length(n$n.risk[n$strata==lev[i]])))
                }else{
                  risk <- c(risk, n$n.risk[n$strata == lev[i]])
                }
              }
            }

            expo <- matrix(risk, nb, (length(tps)), byrow = T)
            axis(1, tps, pos = c(0, 0), cex.axis=0.7)  # pos indique la position de l'axe
            #axis(2,c(0,0.2,0.4,0.6,0.8,1),c("0","20","40","60","80","100")
            axis(2,c(0,0.2,0.4,0.6,0.8,1),c("0","0.2","0.4","0.6","0.8","1"),
                 pos = c(0, 0), las = 2, cex.axis=0.7)   

            text(tps[length(tps)]/2, -0.25, xlab, font = 3, cex = 0.7)       #text(tps[length(tps)]/2, -0.2, xlab, font = 3) pour positionner le label de l'axe des x
            text(-tps[length(tps)]/8, 1.2, ylab, font = 3, srt = 0, cex = 0.7) 


            if (!is.null(IC)) {
              con <- summary(calc, time = IC)
              for (i in 1:length(IC)) {
                up <- con$upper[con$time == IC[i]]
                low <- con$lower[con$time == IC[i]]
                for (j in 1:length(up)) {
                  segments(IC[i]+decalIC*(j-1), low[j], IC[i]+decalIC*(j-1),up[j],
                           col=col[j], lwd=lwd[j], lty=lty[j])
                }
              }
            }

            text(0, -0.3, explab, font = 2, adj = 0.2, cex=0.8)        #### Pour positionner le texte "nomber at risk": modifier adj
            for (i in 1:length(tps)) {
              for (j in 1:nb) {
                text(tps[i], (-0.3 - 0.1 * j), expo[j, i], col = col[j], cex=0.8)  # Taille de police du nbre de sujets à risque : modifier cex ici
                segments(-tps[length(tps)]/10, (-0.3-0.1*j), -tps[length(tps)]/20,
                         (-0.3-0.1*j), col=col[j], lwd=lwd[j], lty=lty[j])
              }
            }

            if (!is.null(legend)) {
              cat("\n")
              cat("Où voulez-vous placer la légende ?", "\n")
              legend(locator(1),legend, col=col, lwd=lwd, lty=lty, bg="gray90")
            }

            if (nb > 1) {
              lr <- survdiff(Surv(del,evt) ~ gp)
              lr$p.value<-pchisq(lr$chisq,df=nb-1,lower=F)
              mtext(paste("Logrank à ",nb-1," ddl:\n p = ",round(lr$p.value,3)),
              line=-5,side=4,outer=F,las=1)

            }

     }         # fin de la fonction
