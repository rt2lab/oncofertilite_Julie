######################## Time-to-pregnancy project###################

#### On utilise la base oncofertilite_consore... C:\Users\Julie\Desktop\Institut Curie\07_oncofertilite_consore_preprocessed_labels.csv


setwd(dir ="C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/")
data_dir = 'data/'


## téléchargement de la base de données 

data = read.csv2("C:/Users/Julie/Documents/GitHub/oncofertilite_Julie/Institut Curie/07_oncofertilite_consore_preprocessed_labels.csv")

summary(data)


#library 

library(httr)
library(arsenal)
library(dplyr)
library(reshape2)
install.packages('ggplot2')

library(labelled)


# première description de la base de donnée 

summary(data)

str(data)

# on a 1357 observations et 274 variables 
# on remarque dans un premier temps qu'il y a un nombre de Na très important
# que fait-on des NA ? 
library(psych)
describe(data)

# ##################### Première analyse : découverte de la base############################

########################## Distribution de l'âge ############################## 

# A priori on a 1357 observations donc 1357 patientes. 

describe(data$age)
# en moyenne, les femmes de l'échantillon ont 37,8 ans (38 ans); la médiane est à 38 ans. 
# la plus jeune a un peu plus de 22 ans et la plus âgée a presque 44 ans. 

hist(data$age, main = "Distribution du nombre de patients en fonction de l'âge", xlab = "Âge", ylab = "Effectif")

#il faut que je crée une variable numérique catégorielle d'âge ...

hist(data$age_young_cl, main = "Distribution du nombre de patients en fonction de la classe d'âge", xlab = "Âge", ylab = "Effectif")

table(data$age_cl_10_1)

##### [0 -30) [30 -40) [40 -50) 
#########95      706      554 

