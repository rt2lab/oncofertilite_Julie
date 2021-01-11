######################## Time-to-pregnancy project###################

#### On utilise la base oncofertilite_consore... C:\Users\Julie\Desktop\Institut Curie\07_oncofertilite_consore_preprocessed_labels.csv


setwd(dir ="C:/Users/Julie/Desktop/Institut Curie/")
data_dir = 'data/'


## téléchargement de la base de données 

data = read.csv2("C:/Users/Julie/Desktop/Institut Curie/07_oncofertilite_consore_preprocessed_labels.csv")

summary(data)


#library 
install.packages('tidyverse')
library(tidyverse)

