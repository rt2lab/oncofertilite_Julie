# NE JAMAIS OUBLIER DE TRIMMER  !!!! 
trim.heatmap <- function(data, trim){
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
