plotMclust <- function (mc, data) 
{
    nb.mix = mc$G
    p <- seq(min(data), to = max(data), length = 1000)
    d <- cdens(modelName = mc$modelName, data = p, parameters = mc$parameters)
    if (nb.mix == 1) {
        points(p, d[, 1], type = "l", col = "blue", lty = 1)
    }
    else if (nb.mix == 2) {
        temp1 <- mc$parameters$pro[1]
        temp2 <- mc$parameters$pro[2]
        points(p, d[, 1] * temp1, type = "l", col = "green3",lty = 1)
        points(p, d[, 2] * temp2, type = "l", col = "blue", lty = 1)
    }
    else if (nb.mix == 3) {
        temp1 <- mc$parameters$pro[1]
        temp2 <- mc$parameters$pro[2]
        temp3 <- mc$parameters$pro[3]
        points(p, d[, 1] * temp1, type = "l", col = "green3",lty = 1)
        points(p, d[, 2] * temp2, type = "l", col = "blue", lty = 1)
        points(p, d[, 3] * temp3, type = "l", col = "magenta", lty = 1)
    }
    return(NULL)
}
fct.selectionTNBC <- function(normData, path, file)
{
	require(mclust)

	#ER#
	mclust_er <- Mclust(data=normData["205225_at",], G=2, modelNames="V")

	#HER#
	mclust_her <- Mclust(data= normData["216836_s_at",], G=2, modelNames="V")

	#PR#
	med_pr <- median(normData["208305_at",])

	#sélection tn#
	er_neg <- names(mclust_er$classification)[mclust_er$classification==1]
	her_neg <- names(mclust_her$classification)[mclust_her$classification==1]
	pr_neg <- colnames(normData)[which(normData["208305_at",] < med_pr)]
	tnbc <- intersect(intersect(er_neg, her_neg), pr_neg)

	med_pr_bis <- median(normData["208305_at", intersect(er_neg, her_neg)])
	pr_neg_bis <- colnames(normData)[which(normData["208305_at",] < med_pr_bis)]
	tnbc_bis <- intersect(intersect(er_neg, her_neg), pr_neg_bis)
	
	
	pdf(paste0(path, "hist_er_her_pr", file, ".pdf"))
	##plotMclust ER
	hist(normData["205225_at",], freq=FALSE, xlim=c(0,16), breaks=50, 
	main=paste("Histogram of ER distribution in", file), ylab="Density", xlab="")
	plotMclust(mc=mclust_er, data=normData["205225_at",])
	
	##plotMclust HER
	hist(normData["216836_s_at",], freq=FALSE, xlim=c(0,16), breaks=50,
	main=paste("Histogram of HER distribution in", file), ylab="Density", xlab="")
	plotMclust(mc=mclust_her, data=normData["216836_s_at",])
	
	##plot ER/HER
	plot(normData["205225_at",], normData["216836_s_at",], pch=19, cex=0.5,
    main=paste("Plot ER_HER", file), ylab="", xlab="")
	
	##hist PR
	hist(normData["208305_at",], freq=FALSE, xlim=c(0,16), breaks=50,
	main=paste("Histogram of PR distribution in", file), ylab="Density", xlab="")
	abline(v= med_pr, col="green")
	abline(v= med_pr_bis, col="blue")	
	dev.off()
	
	write.table(tnbc, file=paste0(path, "tnbcSamples_", file, ".txt"), col.names=FALSE, row.names=FALSE)
	write.table(tnbc_bis, file=paste0(path, "tnbcSamples-bis_", file, ".txt"), col.names=FALSE, row.names=FALSE)
}
