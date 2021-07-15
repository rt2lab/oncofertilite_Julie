
# si nécessaire de mettre la courbe dans l'article, releveliser pour mettre la courbe chimiothérapie en classe de référence
# Description de la population en fonction de la chimio

## Lignes codes Matahi pour enregistrer dans un .csv
# Si on veut l'enregistrer dans un tableau, 
# file.table = file(description="test.csv", open="a")
# Header = c("",levels(tnadj[,group_ref]), "Total","Pvalue")
# write.table(matrix(Header,nrow=1),file="test.csv",sep=";",col.names=F,row.names=F,append=T, quote=F)
# for (group in group_sep)
# {
#   Dat.case <- tab2[[group]]$pcase
#   Dat.test <- tab2[[group]]$test
#   Dat.case <- cbind(Dat.case,Dat.test$p)
#   #write.table(Dat.case, file=paste0(group,'_vs_',group_ref,'_table.csv'),append=T,row.names=T)
#   write.table(Dat.case, file="test.csv",append=T,row.names=T,col.names=F,sep=";")
#   #write.csv(Dat.case, file=paste0(group,'_vs_',group_ref,'_test.csv'))
# }
# close(file.table)