odds.ratio <- function(reg, level=0.95, digits=3) {
		        if ("glm" %in% class(reg)) {
	                if(reg$family$family == "binomial"){
		                        r <- cbind(exp(coef(reg)),exp(confint(reg,level=level)),summary(reg)$coefficients[,4])
		                        r[,1:3] <- round(r[,1:3],digits=digits)
		                        colnames(r)[1] <- "OR"
		                        colnames(r)[4] <- "p"
		                        printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
		                } else {
		                        stop('reg should be a glm with family=binomial or the result of multinom.')
		                }
		        } else if ("multinom" %in% class(reg)) {
		                coef <- summary(reg)$coefficients
		                ci <- confint(reg,level=level)
		                # From http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
		                z <-summary(reg)$coefficients/summary(reg)$standard.errors
		                p <- p <- (1 - pnorm(abs(z), 0, 1)) * 2
		                d <- dim(ci)
		                r <- array(NA,c(d[1]*d[3],d[2]+2))
		                dimnames(r)[[1]]<-rep("",d[1]*d[3])
		                for (i in 1:d[3]) {
		                        fl <- (i-1)*d[1] + 1 #first line
		                        ll <- i*d[1] #last line
		                        r[fl:ll,] <- cbind(coef[i,],ci[,,i],p[i,])
		                        rownames(r)[fl:ll] <- paste0(rownames(coef)[i],"/",colnames(coef))
		                }
		                r[,1:3] <- round(r[,1:3],digits=digits)
		                colnames(r) <- c("OR",dimnames(ci)[[2]],"p")
		                printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
		        }
		        else
		                stop('reg should be a glm with family=binomial or the result of multinom.')
		}
