trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# substitute levels with pretty levels and save prety names as attributes
mappingNames <- function(dataset, dataDictionary, map_var_names=F) {
   
   datasetMapped = as.data.frame(dataset)
   for(i in 1:ncol(datasetMapped)){
      # we do not want to map levels if variable is always NA
      if(!is.null(datasetMapped[,i])){
         if(colnames(datasetMapped)[i] %in% dataDictionary$var){
            pos = which(dataDictionary$var == colnames(datasetMapped)[i])
            if(!is.na(dataDictionary$levels[pos]) & dataDictionary$levels[pos] != "" & !grepl("txt",colnames(datasetMapped)[i])){
               lev = unlist(strsplit(as.character(dataDictionary$levels[pos]),"\\|"))
               #datasetMapped[,i] = as.character(datasetMapped[,i])
               vec1 = c()
               vec2 = c()
               for(l in lev){
                  val1 = trim(unlist(strsplit(as.character(l),","))[1])
                  val2 = trim(unlist(strsplit(as.character(l),","))[2])
                  vec1 = c(vec1,val1)
                  vec2 = c(vec2, val2)
               }
               datasetMapped[,i] = factor(datasetMapped[,i],levels = vec1, labels = vec2)
               if(dataDictionary$var_type[pos] == c("integer")){ #For status_rfs etc... 
                  datasetMapped[,i]  <- as.integer(as.character(datasetMapped[,i] ))
               }
            }
            if(map_var_names == T){
               if(as.character(dataDictionary$names_var[pos]) != ""){
                  attr(datasetMapped[,i], "label") = as.character(dataDictionary$names_var[pos])
               }
            }
         }
      }
   }
   
   # print(unlist(lapply(1:ncol(datasetMapped),function(i){v = attr(datasetMapped[,i], "label");if(is.null(v)) "" else v})))
   return(datasetMapped)
}


# retrieve the pretty names that are stored as attributes
get_pretty_colnames <-function(data){
   prettyColNames = unlist(lapply(1:ncol(data),function(i){v = attr(data[,i], "label");if(is.null(v)) "" else v}))
   return(prettyColNames)
}



get_levels_labels_name <- function(var,data_dictionnary){
   pos = which(data_dictionnary$var == var)
   if(length(pos)==0){print("The variable does not exist") 
            return(list(
                        labels = NULL,
                        name = NULL)) }
   if(!is.na(data_dictionnary$levels[pos]) & data_dictionnary$levels[pos] != ""){
      lev = unlist(strsplit(as.character(data_dictionnary$levels[pos]),"\\|"))
      levels = sapply(lev,function(l) trim(unlist(strsplit(as.character(l),","))[1]))
      labels = sapply(lev,function(l) trim(unlist(strsplit(as.character(l),","))[2]))
      names(labels) = as.vector(levels)
      return(list(labels = labels,
                  name = as.character(data_dictionnary$names_var[pos])))
   }
   return(list(
          labels = NULL,
          name = as.character(data_dictionnary$names_var[pos])))
}


transformToFactor <- function(dataset, dataDictionary){
   for(i in 1:ncol(dataset)){
      if(colnames(dataset)[i] %in% dataDictionary$var){
         pos = which(dataDictionary$var == colnames(dataset)[i])
         if(!is.na(dataDictionary$levels[pos]) & dataDictionary$levels[pos] != ""){
            if(class(dataset[,i]) != 'factor'){
               dataset[,i] = as.factor(dataset[,i])
            }
         }
      }
   }
      
   return(dataset)
}
