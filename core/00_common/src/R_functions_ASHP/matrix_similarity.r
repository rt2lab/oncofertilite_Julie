

similarityMatrix <- function(dataA, dataB){
## Calcule la proportion de genes qui se clusterisent ensemble entre 2 clustering

    ## Mettre un 1 quand 2 probesets sont dans le mm cluster

    #chaque element de la liste est un cluster de gene
    cls <- lapply(unique(dataA), function(i) names(dataA[dataA %in%  i]))
    #creer la matrix
    m.A <- matrix(ncol=length(dataA),nrow=length(dataA),0)
    rownames(m.A) <- colnames(m.A) <- names(dataA)
    for(i in 1:length(unique(dataA))){
        nelts <- 1:length(dataA)
        cl <- as.numeric(names(dataA) %in% cls[[i]])
       updt <- outer(cl, cl)
        m.A <- m.A + updt
        }
    
    
    cls <- lapply(unique(dataB), function(i) names(dataB[dataB %in%  i]))
    m.B <- matrix(ncol=length(dataB),nrow=length(dataB),0)
    rownames(m.B) <- colnames(m.B) <- names(dataB)
    for(i in 1:length(unique(dataB))){
        nelts <- 1:length(dataA)
        cl <- as.numeric(names(dataB) %in% cls[[i]])
          updt <- outer(cl, cl)
        m.B <- m.B + updt
        }

    ## Additionne les 2 matrices, 
    ## la ou il y a un 2 = les 2 probeset sont dans le mm cluster dans les 2 clustering
    ## les 1 indique same set in A but different in B et reciproquement
    ## element différent dans A et dans B ?
    M <- m.A + m.B
    return(M)    
}

