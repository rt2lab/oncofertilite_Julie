

#######################################
## Map gene Symbol into String id
######################################
StringMap <- function (my_data_frame, my_data_frame_id_col_names=NA, aliasFile, 
    removeUnmappedRows = FALSE, quiet = FALSE) {
    ## See string_db$map
    "\nDescription:\n  Maps the gene identifiers of the input dataframe to STRING identifiers.\n  It returns the input dataframe with the \"STRING_id\" additional column.\n\nInput parameters:\n  \"my_data_frame\"                 data frame provided as input. \n  \"my_data_frame_id_col_names\"    vector contatining the names of the columns of \"my_data_frame\" that have to be used for the mapping.\n  \"takeFirst\"                     boolean indicating what to do in case of multiple STRING proteins that map to the same name. \n                                      If TRUE, only the first of those is taken. Otherwise all of them are used. (default TRUE)\n  \"removeUnmappedRows\"            remove the rows that cannot be mapped to STRING \n                                      (by default those lines are left and their STRING_id is set to NA)\n  \"quiet\"                         Setting this variable to TRUE we can avoid printing the warning relative to the unmapped values.\n\nAuthor(s):\n   Andrea Franceschini\n"
    aliasDf2 = NULL
    aliasDf <- aliasFile    
    aliasDf = renameColDf(aliasDf, "protein_id", "STRING_id")
    aliasDf2 = subset(aliasDf, select = c("STRING_id", "alias"))
    
    tempDf = multi_map_df(my_data_frame, aliasDf2, my_data_frame_id_col_names, 
        "alias", "STRING_id")
    naDf = subset(tempDf, is.na(STRING_id))
    if (nrow(naDf) > 0 & !quiet) 
        cat(paste("Warning:  we couldn't map to STRING ", as.integer((nrow(naDf)/nrow(tempDf)) * 
            100), "% of your identifiers", sep = ""))
    if (removeUnmappedRows) 
        tempDf = subset(tempDf, !is.na(STRING_id))
        
    
    return(tempDf)
}

STRING.function <- function(version,genes,annot,data=NA,input_directory,output_directory,output_name){

#version : version of string to use
#input_directory : where STRING files should be store after dwl
#classVect : vector contening the classe of each genes with theire probe names
#annot : contenaing the correspondance probeset to symbol. Need a 'jetset.symbol' column
#data : expression data
## version='current' ## 9_01 / Last Modified 26-Mar-2014 22:19
# Beware with version and protein_aliases_tf.tsv.gz file
require(STRINGdb)
species=9606; 

## Links btw StringID
linksdb = downloadAbsentFileSTRING(paste("http://string.uzh.ch/permanent/string/", 
                version, "/protein_links/", species, "__protein_links.tsv.gz", 
                sep = ""), oD = input_directory)
linksDf <- read.table(linksdb, sep = " ", header = TRUE, quote = "", stringsAsFactors = FALSE, fill = TRUE)

## Alias of StringID
aliasdb = downloadAbsentFileSTRING(paste("http://string.uzh.ch/permanent/string/", 
                version, "/protein_aliases/", species, "__protein_aliases_tf.tsv.gz", 
                sep = ""), oD = input_directory)
aliasDf <- read.table(aliasdb, sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, fill = TRUE)
		
				# "Convert probe id to symbol
				# "symbolGeneList <- data.frame('genes'=annot[genes,'jetset.symbol'],
				# "'iqr'=apply(data[genes,],1,IQR))
				# "symbolGeneList[,1] <- as.character(symbolGeneList[,1])

symbolGeneList <- cbind(genes,genes)
colnames(symbolGeneList) <- c('genes','toto')
## Map genes to StringID
print('Map genes')
mvgenes_mapped <- StringMap(my_data_frame=symbolGeneList,my_data_frame_id_col_names='genes', 
aliasFile=aliasDf,removeUnmappedRows = TRUE, quiet = FALSE)
## Links for our mapped genes
itr <- linksDf[which(linksDf[,'protein1'] %in% mvgenes_mapped[,'STRING_id']),]
itr <- itr[which(itr[,'protein2'] %in% mvgenes_mapped[,'STRING_id']),]
itr <- itr[which(itr$combined_score>400),]
itr$symbol1 <- mvgenes_mapped[match(itr[,'protein1'],mvgenes_mapped[,'STRING_id']),'genes']
itr$symbol2 <- mvgenes_mapped[match(itr[,'protein2'],mvgenes_mapped[,'STRING_id']),'genes']

if(!is.na(data)){
	## Data frame with String links and correlation btw both genes
	StringCorr <- cbind(itr[,c('symbol1','symbol2','combined_score')],rep(NA,nrow(itr)))
	colnames(StringCorr) <- c('symbol1','symbol2','str','cor')
	StringCorr[,'symbol1'] <- toupper(StringCorr[,'symbol1'])
	StringCorr[,'symbol2'] <- toupper(StringCorr[,'symbol2'])
	d <- data[genes,]
	rownames(d) <- toupper(annot[genes,'jetset.symbol']) # solve case sensitive probleme
	cc <- cor(t(d),method='pearson')
	for(j in 1:nrow(StringCorr)){
	StringCorr[j,'cor'] <- cc[StringCorr[j,'symbol1'],StringCorr[j,'symbol2']]
	}
}else{StringCorr <- itr[,c('symbol1','symbol2','combined_score')]}

## Compute gene degree in the string graph
geneDrg <- table(c(as.character(itr[,'symbol1']),as.character(itr[,'symbol2'])))

print('Write results')
	# write.table(as.character(symbolGeneList[,1]),
	# file=paste0(output_directory,'/',output_name,'.txt'),
	# sep='\n',row.names=FALSE,col.names=FALSE)

write.table(itr,
file=paste0(output_directory,'/',output_name,'.string.txt')
,row.names=FALSE,col.names=TRUE)
if(is.na(data)){
write.table(StringCorr,
file=paste0(output_directory,'/',output_name,'.StrClean.txt'),
row.names=FALSE,col.names=TRUE)
}else{write.table(StringCorr,
file=paste0(output_directory,'/',output_name,'.StrCorr.txt'),
row.names=FALSE,col.names=TRUE)
}

return(list("geneDrg"=geneDrg,"symbolGeneList"=symbolGeneList,'StringCorr'=StringCorr))

}