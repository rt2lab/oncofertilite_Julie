### plot function for histogram + density from mclust model
plot.hist.density <- function(x, modelName, ...)
  {
    hist(x, freq=FALSE, ...)
    rug(x, ticksize = 0.01, quiet = TRUE)
    newx <- seq(from = min(x), to = max(x), length = 500)
    dens.value <- dens(modelName = modelName$modelName, data = newx, parameters = modelName$parameters)
    lines(newx, dens.value, col="blue")        
  }

