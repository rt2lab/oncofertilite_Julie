# source("C:/yann/PGM/biostat/fonctions_macro_R.r")

source('~/RT2Lab/R/R functions/fonctions_macro_R_ASHP.r')


#wd<-setwd("C:/yann/PGM/S/lirePigasoumacro")
#x<-MacroLire("essairep", "20070619")
#edit(file="Categorie_essairep.r")
#setwd(wd)

#v<-c("Q004", "Q0040", "Q0041")
#v<-MacroSelectionneShortCode(v, x$dlu, names(x$data))
#v

fct<-c("MacroRepereCSV", "MacroDLU", "MacroEltList", "Macrodd", "MacroLire")
fct<-c(fct, "MacroprintP", "MacroTriEform", "MacroTriVisit", "MacroTriQuestion")
fct<-c(fct, "MacroCLU")
fct<-c(fct, "MacroLireBatch", "MacroBatchUneVariable")
fct<-c(fct, "MacroSelectionneShortCode", "MacroSelectionneQuestion", "MacroMetEnFacteur")
fct<-c(fct, "MacroRenommeShortCode")


package.skeleton(name = "macro", list = fct, path = "c:/yann/pgm/biostat")

#c:\yann\pgm\biostat\modif_path

search()
detach(2)
remove.packages("macro")
install.packages("s:/pgm/macro_1.4-0.zip",repos = NULL)

library(macro)

x<-MacroLire("far", "20070711", rep="C:/yann/PGM/biostat/")

x$str
x$date
head(x$dlu)
head(x$dlu[x$dlu$r!=0,])
x$p
x$vfe

head(x$data)


v<-c("EXRD", "UNILA", "SDJ", "SBJ")
v<-MacroSelectionneQuestion(v, x$dlu, names(x$data))

v<-c("EXRD", "UNILA", "SDJ", "SBJ")
v<-MacroSelectionneShortCode(v, x$dlu, names(x$data))


far<-x$data

v<-c("EXRD", "UNILA", "SDJ", "SBJ")
MacroMetEnFacteur(v, x$dlu, x$clu, names(far))

data<-EnFacteur(far)




m<-MacroTriEform(far, x$dlu, vf="V1.GENET", vt="DATPG")



detach(2)
remove.packages("macro")


install.packages("c:/yann/pgm/biostat/library/macro_1.1-0.zip",repos = NULL)
library(macro)
help.search("macro")
?MacroLire








function(data) {
        data$CANCERG.f1.f<-factor(data$CANCERG.f1, c(0, 1), c("0 : non", "1 : oui"))
        data$CANCERG.f2.f<-factor(data$CANCERG.f2, c(0, 1), c("0 : non", "1 : oui"))
        data$CANCERG.f3.f<-factor(data$CANCERG.f3, c(0, 1), c("0 : non", "1 : oui"))
        data$CANCERG.f4.f<-factor(data$CANCERG.f4, c(0, 1), c("0 : non", "1 : oui"))
        data$TMRPG.f1.f<-factor(data$TMRPG.f1, c(0, 1, 2, 3, 4), c("0 : non", "1 : simple", "2 : echo", "3 : IRM", "4 : scanner"))
        data$TMRPG.f2.f<-factor(data$TMRPG.f2, c(0, 1, 2, 3, 4), c("0 : non", "1 : simple", "2 : echo", "3 : IRM", "4 : scanner"))
        data$TMRPG.f3.f<-factor(data$TMRPG.f3, c(0, 1, 2, 3, 4), c("0 : non", "1 : simple", "2 : echo", "3 : IRM", "4 : scanner"))
        data$TMRPG.f4.f<-factor(data$TMRPG.f4, c(0, 1, 2, 3, 4), c("0 : non", "1 : simple", "2 : echo", "3 : IRM", "4 : scanner"))
        return(data)
}




m<-MacroLire("rar2", "20070824", rep="s:/juliette/rar/")

v<-c("essai", "essai", paste("NUMPA", 1:12, sep=""))
new<-c("NUMPATT","essaibis", paste("NUMPATT", 1:12, sep=""))          #pour être conforme au questionnaire

mrsc<-MacroRenommeShortCode(v, new, m$dlu, names(m$data))
mrsc$sc                  #pour voir si tous les ShortCodes existent
                         #pour voir si tous les news sont des nouveaux noms

names(m$data)<-mrsc$nom  #changement effectif des noms de variables
m$dlu<-mrsc$dlu          #changement effectif des ShortCode dans le dlu


v<-c( paste("NUMPA", 1:12, sep=""))
new<-c(paste("NUMPATT", 1:12, sep=""))          #pour être conforme au questionnaire
mrsc<-MacroRenommeShortCode(v[1], new[1], m$dlu, names(m$data))
mrsc$sc




#
# ==============================================================================
# tester un pb dans colon2
# ==============================================================================
#
setwd("c:/yann/pgm/biostat")
library(macro)

m<-MacroLire("colon2", "20080108", verbose=T)

