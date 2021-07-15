convert_ensembl_to_symbol <- function(ens_symb_matrix, geneList, df){
  geneList_ensembl_id     <- ens_symb_matrix[match(geneList, ens_symb_matrix$external_gene_name  ),"ensembl_gene_id"] ; 
  pos_gene_na             <- which(is.na(ens_symb_matrix[match(geneList, ens_symb_matrix$external_gene_name  ),"ensembl_gene_id"]) )
  geneList_ensembl_id     <- na.omit(geneList_ensembl_id) ; class(geneList_ensembl_id)
  mat_geneList            <- df[geneList_ensembl_id,]
  rownames(mat_geneList)  <- geneList[-pos_gene_na]
  return(mat_geneList)                  }
