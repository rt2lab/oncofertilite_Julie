brewpal <- function(n){
   return(brewer.pal(n,"Set3"))
						}

my.pal <- colorRampPalette(unique(c(brewer.pal(12,"Set3"), brewer.pal(8,"Dark2"), brewer.pal(9,"Pastel1"), brewer.pal(9, "RdGy"))))

## normData = matrix : matrix normalisee
## normData.cl = vecteur : vecteur de classe (ordonnée selon les samples) == code un peu simple ne fonctionne que pour 1 label
## dataName = character : nom du dataset

exploratoryBRAINStem <- function(normData, normData.cl, dataName, ellipse=FALSE){

   ## palette pal
   pal <- ifelse(length(unique(normData.cl)) <= 12, "brewpal", "my.pal")


   ## Histogram of intensity
   print("Histogram of intensity")
   print("...")
   png(paste("Results/ExploreDataRMA/", dataName, "histIntensity.png", sep=""), res=300, height=1600, width=1600)
   par(font=2, font.axis=2)
   hist(normData, breaks=200, xlab="Expression Level (log2)", main=paste("Histogram of Expression Level ",  dataName, " dataset", sep=""))    
   dev.off()

   ## Boxplots of intensity
   print("Boxplots of intensity")
   print("...")
   png(paste("Results/ExploreDataRMA/", dataName, "boxplotsIntensity.png", sep=""), res=300, height=1600, width=2600)
   par(mar=c(9.1,3.1,3.1,1.1), font=2, font.axis=2)
   boxplot(as.data.frame(normData), names=colnames(normData), las=2, cex.axis=0.5, col="linen", main=paste("Intensity Boxplots of normalized data from ",  dataName, " dataset", sep=""))
   dev.off()

   ## PCA on all genes
   print("PCA on all genes")
   print("...")
   res.pca <- PCA(t(normData), graph=FALSE)
   if(ellipse){
       png(paste("Results/ExploreDataRMA/explAna_pca_" , dataName, ".png", sep=""), res=300, height=2400, width=4800)
       par(font.main=2, font=2, font.axis=2, mfrow=c(1,2))
   } else {
       png(paste("Results/ExploreDataRMA/explAna_pca_" , dataName, ".png", sep=""), res=300, height=2400, width=2400)
   }
   plotSample(res.pca, lab = normData.cl, palette=pal, lab.title="Samples Groups")
   title(paste("PCA on all genes for ",  dataName, " dataset", sep=""))
   if(ellipse){
       plotSample(res.pca, lab = normData.cl, palette=pal, lab.title="Samples Groups", ellipse=TRUE)
       title(paste("PCA on all genes for ",  dataName, " dataset", sep=""))
   }
   dev.off()

   ## Clustering on all genes
   print("Clustering on all genes")
   print("...")
   c <- clustering(normData, metric="euclidean", method="ward")
   png(paste("Results/ExploreDataRMA/explAna_clust_", dataName, ".png", sep=""), res=300, height=2400, width=2600)
   clustering.plot(c, lab=normData.cl, palette=pal, title=paste("Hierarchical Clustering on all genes for ",  dataName, " dataset", sep=""))
   dev.off()

}
