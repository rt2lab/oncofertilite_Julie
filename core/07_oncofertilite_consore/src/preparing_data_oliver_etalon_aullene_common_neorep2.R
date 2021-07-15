library(readxl)
matrice_globale_short_annot_5 <- read_excel("oncofertilite/oncofertilite_2011_2017/data/raw/matrice_globale_short_annot_5.xls", 
                                            col_types = c("numeric", "text", "numeric", 
                                                          "numeric", "text", "text", "text", 
                                                          "numeric", "date", "date", "numeric", 
                                                          "text", "date", "date", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "date", "date", 
                                                          "text", "numeric", "date", "date", 
                                                          "numeric", "text", "text", "text", 
                                                          "numeric", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "numeric", "text", "text", "text", 
                                                          "numeric", "text", "text")) %>% as.data.frame()
head(matrice_globale_short_annot_5)
tail(matrice_globale_short_annot_5)
d1 <- matrice_globale_short_annot_5
d1$nip          <- gsub("\\*","",d1$numdos7)
d1$numdos_curie <- formatC(as.integer(d1$nip),width = 7, flag = "0")

load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/raw/nip_oncofertilite_aullene_no_refusal.RData")
d1_1366 <- d1 %>% filter(numdos7 %in% nip_oncofertilite_aullene_no_refusal)
head(d1_1366)
d1_1366$date_1st_chir_a_checker
d1_1366$date_prem_biop_a_check
d1_1366$Date_debut_consore_a_check

d1_1366 %>% filter(is.na(d1_1366$date_1st_chir_a_checker)) %>% head()

205/517
