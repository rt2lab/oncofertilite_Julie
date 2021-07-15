# 

load("/Users/ahamypet/RT2Lab/REMAGUS_2015/Data/Processed/clinique_2015_3.RData")

head(clinique)
table(clinique$patcar.f,exclude=NULL)
table(clinique$coeur,exclude=NULL)
table(clinique$poum.f,exclude=NULL)
table(clinique$neuro.f,exclude=NULL)
table(clinique$patdig.f,exclude=NULL)
table(clinique$autr.f,exclude=NULL)

