load("/Users/ahamypet/RT2Lab/NEOREP/NEOREP2/data/raw/nip_1146_neorep2.RData")
length(nip_1146_neorep2)

load("/Users/ahamypet/RT2Lab/oncofertilite/oncofertilite_2011_2017/data/processed/nip_oncofertilite.RData")
length(nip_oncofertilite)

nip_deja_actu_oncofert_aullene <- intersect(nip_1146_neorep2,nip_oncofertilite)
write.csv2(nip_deja_actu_oncofert_aullene, file="/Users/ahamypet/RT2Lab/NEOREP/NEOREP2/data/raw/nip_deja_actu_oncofert_aullene.csv")

setdiff(nip_1146_neorep2,nip_oncofertilite)

108/117