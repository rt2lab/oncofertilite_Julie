######################## Time-to-pregnancy project###################

#### On utilise la base oncofertilite_consore... C:\Users\Julie\Desktop\Institut Curie\07_oncofertilite_consore_preprocessed_labels.csv


setwd(dir ="C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/")
data_dir = 'data/'


## téléchargement de la base de données 

data = read.csv2("C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/oncofertilite_Borghese_stat_des.R")

summary(data)