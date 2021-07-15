				# Create a metagene according to the median
				source('~/RT2Lab/R/R functions/trim_BS.R', chdir = TRUE)
				library(gplots)
				library(EMA)
				library(mclust)

				source('~/RT2Lab/R/R functions/plot.hist.density.R')

				# Define : 

		# 				GeneList							<- common_ASHP_HBK 	 
		# 				GeneListName						<- "common_ASHP_HBK"	 
		# 				pub_expression_dataset 				<- REMAGUS_gen_exp_common
				
				# Creates expression matrix
				mat_expression_all_gene			=pub_expression_dataset[intersect(rownames(pub_expression_dataset),GeneList	)	,]
					
				
				#  How many genes found on the list?
				print(paste(length(intersect(rownames(pub_expression_dataset),GeneList	)),"/", length(GeneList)))
				# Represent heatmap
 				pdf(paste0("results/heatmap_",GeneListName,".pdf"))
				heatmap.2(trim.heatmap(mat_expression_all_gene,0.99),cexCol = 0.5,dendrogram="column",Rowv=FALSE,Colv=TRUE, cexRow = 0.7,trace='none',scale='none',col=myPalette(low='green',high ='red',mid ='black',k=50),main=GeneListName)
				dev.off()
				print("fuck")

				# Create metagene 
				metagene						<- apply(mat_expression_all_gene,2,median)			

				# Check metagene distribution ?
				
				metageneName			<- paste0("metagene_",GeneListName)
				pdf(paste0("results/",metageneName,".pdf"))
				hist(metagene,breaks=100)	

# 				probe[which(probe$symbol=="ESR1"),]  #205225_at
# 				p <-"205225_at"
# 				data <-herA_FilteredOutliers		
# 				hist(data[p,],breaks=100)	

				clust <- Mclust(as.matrix(metagene), modelNames="E", G=2)				
				plot.hist.density(as.matrix(metagene), clust, breaks=100) # , main='ESR1')
				dev.off()

						# If do not wan't to cut to the median, but rather to a bimodal distribution : 
						# 						class.bin 			<- ifelse(clust$classification==1,0,1)
						# 						variable_2class 	<- ifelse (class.bin=="0","class negative","class positive") #194
						# 						names(variable_2class)==colnames(mat_expression_all_gene)
				
				median_metagene					<- median(metagene)
				metagene_LH						<- as.matrix(ifelse(metagene		 <= median_metagene	,"low", "high"))	
				metagene_LH_fact				<-	relevel(as.factor(metagene_LH),ref="low")
				names(metagene_LH_fact)			<- rownames(metagene_LH)
				
				# Save metagene with the right names
				metageneList 					<-list(	GeneList					= GeneList,
														GeneListName				= GeneListName,
														mat_expression_all_gene		= mat_expression_all_gene,
														metagene					= metagene,
														median_metagene				= median_metagene,
														metagene_LH					= metagene_LH, 
														metagene_LH_fact			= metagene_LH_fact)
																		
				save(metageneList, file=paste0("data/processed/metageneList_",GeneListName,".RData"))
				
				# Then put into a get load
								# common_ASHP_HBK				<- get(load(file='~/Documents/PROFESSIONNEL/STATISTIQUES/REMAGUS_2015_Patricia/data/metageneList_common_ASHP_HBK.RData'))
								# and retrieve the six objects
								# common_ASHP_HBK[["GeneList				"]]
								# common_ASHP_HBK[["GeneListName			"]]
								# common_ASHP_HBK[["mat_expression_all_gene	"]]
								# common_ASHP_HBK[["metagene				"]]
								# common_ASHP_HBK[["median_metagene			"]]
								# common_ASHP_HBK[["metagene_LH				"]]
								# common_ASHP_HBK[["metagene_LH_fact		"]]


				