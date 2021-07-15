# compList <- list("allDownBCUO142"=resDiffDownBCUO$EntrezID,
#              "downB278"=resDiffDownB$EntrezID,
#              "downC458"=resDiffDownC$EntrezID,
#              "downU90"=resDiffDownU$EntrezID,
#              "downO139"=resDiffDownO$EntrezID
# )

grps <- names(compList)
source("http://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")
require(clusterProfiler)

comp <- compareCluster(compList,
                       fun="enrichGO",
                       ont="BP",
                       OrgDb='org.Hs.eg.db',
                       pvalueCutoff=0.05,
                       qvalueCutoff=0.1)

p <- plotCompGO(comp, showCategory=20, grps=grps,
                sortBy=c("pvalue"), by="percentage", title="GO - BP", font.size=12)
pdf("results/diffana/GOenrich/enrichDownGOBPpval.pdf", title="enrichDownGOBPpval.pdf", width=10, height=10)
print(p);dev.off()


enrich <- enrichGO(compList[[1]], ont="BP", organism="human", pvalueCutoff=0.05, qvalueCutoff=0.1)
p <- barplotEnrichGO(enrich, showCategory=20, title="DOWN ALL", order=TRUE, font.size=12)
pdf("results/diffana/GOenrich/barplotEnrichDownGOBP_BCUO.pdf", title="downGOBP_BCUO", width=10)
print(p);dev.off()
