load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/07_oncofertilite_consore_preprocessed_labels_deident.RData")
head(database_preprocessed_labels_deident)
base_marcel <- database_preprocessed_labels_deident %>% select(-contains("old"), - contains ("corrected"), -contains("is_"), -date_preg_desire) 
head(base_marcel)

save(base_marcel, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/base_marcel.RData")
write.csv2(base_marcel, file = "/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/processed/base_marcel.csv")
