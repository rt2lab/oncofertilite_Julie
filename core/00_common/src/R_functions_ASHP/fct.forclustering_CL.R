clust.dist <-
function(mat, meth.dis="euclidean") { 
  require(Hmisc)
  require(cluster)
  
  MEASURE <- c("euclidean", "manhattan", "pearson", "pearsonabs", "spearman", "spearmanabs", "jaccard", "dice")
  mea <- pmatch(meth.dis, MEASURE)
  
  if (is.na(mea)) stop("Error :Unknown Metric.")
  if (mea==1) DIS <- as.matrix(daisy(t(mat), metric="euclidean"))      
  if (mea==2) DIS <- as.matrix(daisy(t(mat), metric="manhattan"))    
  if (mea==3) DIS <- (1-rcorr(mat, type="pearson")$r)/2
  if (mea==4) DIS <- 1-abs(rcorr(mat, type="pearson")$r)
  if (mea==5) DIS <- (1-rcorr(mat, type="spearman")$r)/2
  if (mea==6) DIS <- 1-abs(rcorr(mat, type="spearman")$r)
  if (mea==7) DIS <- jaccard(mat)                                    
  if (mea==8) DIS <- dice(mat)                                        
  attr(DIS,"Metric")<-meth.dis

  return( DIS )
}


trim.heatmap <- 
function(data, trim) {

    ## data <- data - mean(data, na.rm = TRUE)
    data = t(scale(t(data)))
    q <- quantile(data, c((1 - trim), trim), na.rm = TRUE)
    data[data < q[1]] = q[1]
    data[data > q[2]] = q[2]
    maxi <- max(data, na.rm = TRUE)
    mini <- min(data, na.rm = TRUE)
    data[!is.na(data) & data > 0] <- data[!is.na(data) &  data > 0]/maxi
    data[!is.na(data) & data < 0] <- -data[!is.na(data) &  data < 0]/mini

    return(data)

}


clustering <- 
function (data, metric = "pearson", method = "ward", nb) {
    METHOD <- c("average", "single", "complete", "ward", "weighted", 
        "diana", "kcentroids")
    mea <- pmatch(method, METHOD)
    if (is.na(mea)) 
        stop("Error : Unknown Linkage.")
    if (!is.na(mea) && mea != 7) 
        DIS <- clust.dist(data, metric)
    if (mea <= 5) 
        HIERA <- agnes(DIS, method = method, diss = TRUE, keep.diss = TRUE)
    else if (mea == 6) 
        HIERA <- diana(DIS, diss = TRUE)
    else if (mea == 7) {
        if (missing(nb)) 
            stop("Number of clusters 'k' have to be selected for kcentroids algorithm")
        HIERA <- list()
        if (metric == "euclidean") {
            for (i in nb) HIERA <- c(HIERA, km = kmeans(t(data), centers = i)$cluster)
        }
        else {
            DIS <- clust.dist(data, metric)
            for (i in nb) HIERA <- c(HIERA, pm = pam(DIS$DIS, k = i, diss = TRUE)$cluster)
        }
    }
    attr(HIERA$diss, "Metric") <- attr(DIS, "Metric")
    return(HIERA)
}
