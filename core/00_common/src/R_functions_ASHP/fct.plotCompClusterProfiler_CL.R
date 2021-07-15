
require(plyr)
require(ggplot2)

plotCompGO <- function(resComp, showCategory=5, grps=NULL,
                       sortBy=c("pvalue", "Count"),
                       by="percentage",
                       title="",
                       font.size=12){
    clProf.df <- summary(resComp)
    sortBy <- match.arg(arg=sortBy , choices=c("pvalue", "Count"))
    if (is.null(grps)) grps <- unique(clProf.df$Cluster)
    Cluster <- NULL # to satisfy codetools
    result <- ddply(.data = clProf.df, .variables = .(Cluster),.fun = function(df, N) {
        if (length(df$Count) > N) {
            if (sortBy == "Count"){
                idx <- order(df$Count, decreasing=TRUE)[1:N]
            } else
                idx <- order(df$pvalue,decreasing=FALSE)[1:N]
            return(df[idx,])
        } else {
            return(df)
        }
    }, N=showCategory )
    result$Description <- as.character(result$Description) ## un-factor
    GOlevel <- result[,c(2,3)] ## GO ID and Term
    GOlevel <- unique(GOlevel)
    
    result <- result[result$Count != 0, ]
    result$Description <- factor(result$Description, levels=rev(GOlevel[,2]))
    if (by=="percentage") {
        Description <- Count <- NULL # to satisfy codetools
        result <- ddply(result, .(Description), transform,
                        Percentage = Count/sum(Count),
                        Total = sum(Count))
        
        ## label GO Description with gene counts.
        x <- mdply(result[, c("Description", "Total")], paste, sep=" (")
        y <- sapply(x[,3], paste, ")", sep="")
        result$Description <- y
        
        ## restore the original order of GO Description
        xx <- result[,c(2,3)]
        xx <- unique(xx)
        rownames(xx) <- xx[,1]
        Termlevel <- xx[as.character(GOlevel[,1]),2]
        
        ##drop the *Total* column
        result <- result[, colnames(result) != "Total"]
        
        result$Description <- factor(result$Description,
                                     levels=rev(Termlevel))
        
    }
    if (by == "percentage")
        p <- ggplot(result, aes(x = Cluster, y = Description, size = Percentage))
    if (by == "count")
        p <- ggplot(result, aes(x = Cluster, y = Description, size = Count))
    p <- p +  geom_point() + aes(color=pvalue) +
        scale_colour_gradient(low="red", high="blue") +
            xlim(grps) + xlab("") + ylab("") +
                theme(axis.text.x = element_text(colour="black", size=font.size, vjust = 1)) +
                    theme(axis.text.y = element_text(colour="black", size=font.size, hjust = 1)) +
                        labs(title=title) + theme_bw()
    return(p)
}






barplotEnrichGO <- function(resEnrich, showCategory=5, 
                            title="",
                            order = FALSE,
                            drop = FALSE,
                            font.size=12){
    
    res <- summary(resEnrich)
    if (drop == TRUE) {
        res <- res[res$Count != 0, ]
    }
    if (showCategory <= nrow(res)) {
        res <- res[1:showCategory, ]
    }
    if (order == TRUE) {
        idx <- order(res$Count)
        res <- res[idx, ]
    }
    res$Description <- factor(res$Description, levels = as.character(res$Description))
    Description <- Count <- NULL
    p <- ggplot(res, aes(x = Description, y = Count)) + geom_bar() + 
        coord_flip() + xlab("") + ylab("") + theme_bw() +
            theme(axis.text.x = element_text(colour="black", size=font.size, vjust = 1)) +
                theme(axis.text.y = element_text(colour="black", size=font.size, hjust = 1)) +
                    labs(title=title) 
    
    if ("pvalue" %in% colnames(res)) {
        pvalue <- NULL
        p <- p + aes(fill = pvalue) +
            scale_fill_continuous(low = "red", high = "blue")
    }
    return(p)
}









