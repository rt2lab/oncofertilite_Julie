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
