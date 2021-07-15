
source('~/RT2Lab/R/R functions/distancePointLine_BS.R', chdir = TRUE)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}




select.mvgenes <- function(data,measure=c('sd','iqr')){
switch (measure,
"sd" = {
x <- apply(data,1,sd,na.rm=TRUE)
    },
    "iqr" = {
x <- apply(data,1,IQR,na.rm=TRUE)
    }
)

xx <- x[complete.cases(x)]
oo <- order(xx, decreasing=TRUE)

## line between the two extreme sensitivity values
dd <- cbind("y"=xx[oo][c(1, length(oo))], "x"=c(1, length(oo)))
rr <- lm(y ~ x, data=data.frame(dd))
## compute distance from sensitivity values and the line between the two extreme sensitivity values
ddi <- apply(cbind(1:length(oo), xx[oo]), 1, function(x, slope, intercept) {
    return(distancePointLine(x=x[1], y=x[2], slope=slope, intercept=intercept))
  }, slope=rr$coefficients[2], intercept=rr$coefficients[1])
  
    ## identify cutoff as the maximum distance
    cutoff <- which.max(abs(ddi))
    cutoffn <- names(ddi)[cutoff]
  
    mycol <- rep("grey", length(xx))
    names(mycol) <- names(xx)
    mypch <- rep(16, length(xx))
    names(mypch) <- names(xx)
    mypch[cutoffn] <- 19
        
    mvgenes <- names(which(xx > xx[cutoffn]))
    
#     par(mfrow=c(2,1))
#     plot(xx[oo], col=mycol[oo], pch=mypch[oo], ylab=measure,main=paste('Cutoff at',round(xx[cutoffn],3),',', length(mvgenes),'genes selected'))
#     points(x=cutoff, y=xx[cutoffn], pch=mypch[cutoffn], col='red')
#     abline(a=rr$coefficients[1], b=rr$coefficients[2], lwd=2, col="grey66")
#     lines(x=c(cutoff, cutoff), y=c(par("usr")[3], xx[cutoffn]), col="red")
#     lines(x=c(par("usr")[1], cutoff), y=c(xx[cutoffn], xx[cutoffn]), col="red")
# 
# hist(x,xlab=measure,main=paste('Histogram of',measure,'for all genes'),breaks=100,prob=TRUE)
# lines(density(x),col=gg_color_hue(3)[2])
# abline(v=xx[cutoffn],col='red',lty=3)
return(mvgenes)  
}
