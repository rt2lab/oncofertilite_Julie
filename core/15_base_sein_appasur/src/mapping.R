#Util libraries
library(tidyr)
library(dplyr)

#Util functions
#Two functions which may be useful in the mapping process

###################################################################
#Function n1 : na_factor
#Transform a column into a factor with specified NA values
#Input : col : a column, 
#        na_string : the list of strings to be transformed into NA
#        levels : the list of levels (optional)
#        labels : the list of labels (optional)
#Output : col : a factor column
#Example of use: 
#col_to_test = c(rep("Not specified",10),rep(5,15),rep(0,2),rep("NA",4))
#na_factor(col_to_test,c("Not specified","NA"))
#na_factor(col_to_test,c("Not specified","NA"),labels = c("Zero","Five"))
na_factor <- function(col,na_string,levels = NULL,labels = NULL){
  col[which(col %in% na_string)] <- NA
  levels = if (is.null(levels)) setdiff(unique(col),NA) else levels
  labels = if (is.null(labels)) setdiff(unique(col),NA) else labels
  col <- factor(col,levels=levels,labels=labels)
  return(col)
}
###################################################################

###################################################################
#Function n2 : regroup
#Regroup levels of a factor variable to create a new factor variable
#Input : col : a column, 
#        input : the list of values in the input
#        output : the list of values in which the input is converted
#        base : the value given for values not in input, default is NA
# Output : col : a factor column
#Example of use: 
#col_to_test = c(rep("Not specified",10),rep(5,15),rep(0,2),rep("NA",4))
#regroup(col_to_test,list(c("Not specified","NA"),c("0","5")),c("Not filled","Filled"))
#regroup(col_to_test,list(c("0"),c("5")),c("Low","High"),base="Not filled")
regroup <- function(col,input,output, base=NA){
  col_res <- rep(base, length(col))
  if(length(input) != length(output)){stop("Error : the two vectors are not of equal size")}
  for(i in 1:length(input)){
    col_res[which(col %in% input[i][[1]])] <- output[i]
  }
  col_res <- factor(col_res)
  return(col_res)
}
###################################################################

###################################################################
#Step 1
#Define your Mapping Class
MappingSein <- setClass( 
  "MappingSein",

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "15_base_sein_appasur", 
    database = openxlsx::read.xlsx(file.path(
      Sys.getenv("PROJECT_PATH"),
      "core/15_base_sein_appasur/data/EXTRACTION_119_20200717_data_avecidentifiants.xlsx" #Ce fichier depuis 20 Juillet 2020
    ), na.strings = c("NA",""), detectDates = T), 
    dat_censor = as.Date("2099-12-31")
  ),
  contains = "Mapping"
)
###################################################################

###################################################################
#Step 2
#Mapping Initial is all preprocessing step not related to generic or derived variables
#It can be use to : suppress columns, correct values
#If the preprocessing is to long, you can source another file which contains a function returning self.
#Beware : you need to return the whole object of class MappingMyBase (self)
setMethod(f="mapping_initial",
          signature="MappingSein", 
          definition=function(self)
          {
            self@database <- self@database %>% dplyr::select(-starts_with("NUMANA"),
                                                      -starts_with("LANA"),
                                                      -starts_with("CHIRMET"),
                                                      -starts_with("DATANA"),
                                                      -starts_with("CIMORG"),
                                                      -starts_with("NVRE"),
                                                      -starts_with("MR"), 
                                                      -starts_with("HTMET"),
                                                      -starts_with("RTMET"),
                                                      -starts_with("CTMET"),
                                                      -starts_with("TYPDK"), 
                                                      -starts_with("SIEGDK"),
                                                      -starts_with("ACTANA"),
                                                      -starts_with("TTMET"),
                                                      -starts_with("TTRLOC"),
                                                      -starts_with("TCMET"), 
                                                      -starts_with("RTRG"),
                                                      -starts_with("TTRGG"),
                                                      -starts_with("CHIRR"))
            
            #Rem637662ove controlateral or second cancer for patients
            self@database <- self@database %>% filter(numcs == 1)
            
            #Corrections dates d'hercpetin pour certaines patientes 
            #0880797 faute de frappe (2009 au lieu de 2008)
            self@database$DADTCIBL.f1[self@database$NUMDOS == 880797] <- "2008-07-11"
            #0780633 erreur sur la date d'herceptin
            self@database$DATDTCIBL.f1[self@database$NUMDOS == 780633] <- "2007-07-23"

            #Correction date de chirurgie
            #0385372 faute de frappe (1 mois de difference)
            self@database$DATCHIR.f1[self@database$NUMDOS == 385372] <- "2011-10-18"
            
            #On supprime la patiente dont la date de diagnostic est en 2012. (decision taken with ASHP on 22nd of July 2020)
            self@database <- self@database %>% filter(as.Date(DATDIAG) < "2012-01-01")
            
            #On supprime 2 patientes méta d'emblée et une patiente qui a progressé sous neo-adjuvant (entraîne des problèmes de délais à la chirugie négatif)
            self@database <- self@database %>% filter(!NUMDOS %in% c("1113429","989663","1117888"))
            #On supprime 0703163 car cancer du sein non incident (premier CS en 1977)
            self@database <- self@database %>% filter(!NUMDOS %in% c("703163"))
            #On supprime 0903230 et 0908397 car tumeur phyllode 
            self@database <- self@database %>% filter(!NUMDOS %in% c("903230","908397"))
            #On supprime 0200600, 0302695, 0304498 car CS non incident
            self@database <- self@database %>% filter(!NUMDOS %in% c("200600","302695","304498"))
            #On supprime 1005631 car CS non incident
            self@database <- self@database %>% filter(!NUMDOS %in% c("1005631"))
            #On supprime un numdos parce que c'est un homme 
            self@database <- self@database %>% filter(!NUMDOS %in% c("1100750"))
            
            #Correction date de naissance base sein
            self@database$DATNAI[self@database$NUMDOS == 890130] <- "1951-06-01" 
            self@database$DATNAI[self@database$NUMDOS == 1107796] <- "1951-01-01" 
            self@database$DATNAI[self@database$NUMDOS == 1116364] <- "1964-09-05" 
            self@database$DATNAI[self@database$NUMDOS == 1092615] <- "1960-02-01"
            self@database$DATNAI[self@database$NUMDOS == 886735] <- "1959-02-01"
            
            #Correction date de diagnostic 
            self@database$DATDIAG[self@database$NUMDOS == 7088] <- "2008-12-11"
            self@database$DATDIAG[self@database$NUMDOS == 1007573] <- "2010-09-15"
            
            #Correction dates de chimio
            self@database$DATDCT.f1[self@database$NUMOS == 1086120] <- "2010-09-02"
            
            
            
            return(self)
          }
)
###################################################################

###################################################################
#Step 3 : 
#Redefine the methods mapping_family_var

#Patient id
#Check n5 : done
#Patiente 9405583 : dat de diagnostic en 2012 (2012-01-17)
#Mail envoyé à Linda pour correction le 20 Juillet 2012. 
#Réponse : il est possible que la date de diagnostic ait été corrigée dans la base sein après l'export de la cohorte. 
#Decision : on supprime la patiente (fait dans mapping initial)
setMethod(f="mapping_patient_id",
          signature="MappingSein", 
          definition=function(self)
          {
            center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3))
            numdos_curie = as.character(formatC(self@database$NUMDOS,width = 7, flag = "0",format="d"))
            
            return(list(
              database = rep(1,nrow(self@database)),
              numdos_curie = numdos_curie,
              cletri = self@database$num,
              side  = factor(self@database$cote, levels = c("G","D"), labels = c(1,2)),
              dat_birth = as.Date(self@database$DATNAI),
              dat_bc_diagnosis = as.Date(ifelse(is.na(self@database$DATDIAG), as.character(self@database$DATCHIR.f1), as.character(self@database$DATDIAG)),origin = "1900-01-01"),
              center_curie = center_curie,
              center = factor(ifelse(center_curie == 3,2,1)),
              refusal_data_use = rep(2,nrow(self@database))
            ))
          }
)

#Patient char
#Check 5 : done 
setMethod(f="mapping_patient_char",
          signature="MappingSein", 
          definition=function(self)
          {
            return(list(
              age_menarche       = as.numeric(self@database$AGEPR),
              #Prev pregnancy is a derived variable
              prev_pregnancy     = na_factor(self@database$GROS, c("non précisé"),levels=c("non","oui"),labels = c(0,1)),
              menop              = na_factor(self@database$MENOP,c("non précisé"),levels=c("non","oui"),labels = c(0,1)),
              age_menop          = as.numeric(self@database$AGMENOP),
              hrt                = na_factor(self@database$THS,c("non précisé"),levels=c("non","oui"),labels = c(0,1)),
              fam_history        = na_factor(self@database$ANTFAM,c("non précisé"),levels=c("non","oui"),labels = c(0,1)),
              brca_screen        = factor(self@database$RECMUT, levels = c("non","oui"), labels = c(0,1)),
              brca_mut           = factor(self@database$RESMUT, levels = c("non","oui"), labels = c(0,1)),
              weight             = as.numeric(self@database$POIDS),
              size               = as.numeric(self@database$TAILLE)/100
            ))
          }
)

#Bc_diagnosis
#Check 5 : done 
setMethod(f="mapping_bc_diagnosis",
          signature="MappingSein", 
          definition=function(self)
          {
            return(list(
              bilat_bc           = factor(self@database$bilate), 
              inflammatory_BC    = factor(ifelse(self@database$TUICC  %in% c("T4c","T4d"), 1,0)),
              moddiag            = factor(self@database$MODDIAG, levels = c("radiologique","clinique"),labels=c(0,1)),
              multifocality_clin = na_factor(self@database$MULTIF, c("non précisé"),levels = c("non","oui"),labels=c(0,1)),
              tclin              = as.numeric(self@database$TCLIN),
              ctuicc_5cl          = regroup(self@database$TUICC , list(c("T0","Tis"),c("T1"),c("T2"),c("T3"),c("T4","T4a","T4b","T4c","T4d")), c(0,1,2,3,4)),
              cnuicc_4cl          = regroup(self@database$NUICC , list(c("N0"),c("N1"),c("N2"),c("N3")), c(0,1,2,3)),
              muicc              = rep(0,nrow(self@database)) #No distant metastasis at origin in database (inclusion criteria)
            ))
          }
)

#Bc_biology
#Check 5 : done 
#### QUESTIONS ### 
#mail Linda envoye, pq 1100 NA dans INFILT alors que grade rempli le 22 Juillet 2020. 
#en attente de reponse 
#TODO : laisse variable brute BS (attendre reponse Linda)
# INFILT                   EE                n
# <chr>                    <chr>         <int>
#   1 CCIS                   NA             1214
# 2 CCIS + micro infiltrant  NA             294
# 3 infiltrant               1              2061
# 4 infiltrant               2              3710
# 5 infiltrant               3              1840
# 6 infiltrant               Non évaluable    48
# 7 infiltrant               Non précisé      21
# 8 infiltrant               NA                2
# 9 maladie de Paget stricte NA                8
## 10 NA                       1                75
## 11 NA                       2               455
## 12 NA                       3               547 ###Devraient etre remplies pour INFILT
# 13 NA                       Non évaluable     7
# 14 NA                       Non précisé      16

#Old questions : A) Je ne comprends pas le lien entre er_percentage, er_intensity et er_status
#           er_intensity
#er_status    1    2    3   <NA>
#        0    82   20    8  1231
#        1    497 2099 4623  219
#      <NA>    0    1    1  1517
#Notamment, est-ce normal que des patientes avec er_status 0 ou NA aient des valeurs d'intensité? OK 
#er_status == 1 <=> er_percentage > 10% (par def)
# B) Même question que A) avec pr. OK
# C) er and pr_allred are missing : should we construct it? One day
# D) histo_5cl : beaucoup d'autres codes dans self@database$CIMO1 : normal qu'on ne les utilisent pas? OK
# E) Pas de variable pour p53, str_til_perc, it_til_perc, lvi_biop (la variable EMBV concerne l'anapath de la chir et pas de la biopsie n'est-ce pas?) OK
# F) NBMIT : number of mitoses in mm2. In data dictionnary : in 2 mm2.  #Bea to check. Ok pour le code 
setMethod(f="mapping_bc_biology",
          signature = "MappingSein", 
          definition = function(self)
          {
            
            her2_status                                                                                                <- NA
            her2_status[self@database$HERIHC == "+++" | self@database$HERA == "oui"]                                   <- 1
            her2_status[self@database$HERIHC == "+" | self@database$HERIHC == "négatif" |self@database$HERA == "non"]  <- 0
            
            #TODO : check this code with ASHP.
            grade_3cl          = regroup(self@database$EE,list(c("1"),c("2"),c("3")),c(1,2,3))
            invasive_or_dcis   = factor(self@database$INFILT, levels = c("infiltrant", "CCIS","CCIS + micro infiltrant","maladie de Paget stricte"), labels=c(1,2,1,2)) 
            invasive_or_dcis[!is.na(grade_3cl)] = 1 #Plus que 23 NA
            
            return(list(
              er_status          = factor(self@database$ROCL, levels = c("RO -", "RO +"), labels=c(0,1)),
              pr_status          = factor(self@database$RPCL, levels = c("RP -", "RP +"), labels=c(0,1)),
              er_intensity       = factor(self@database$ROINT, levels = c("faible", "modéré","fort"), labels=c(1,2,3)),
              pr_intensity       = factor(self@database$RPINT, levels = c("faible", "modéré","fort"), labels=c(1,2,3)),
              er_percentage      = as.integer(self@database$ROPCT),
              pr_percentage      = as.integer(self@database$RPPCT),
              her2_status        = factor(her2_status), #Double check done here
              histo_5cl          = regroup(self@database$CIMO1,list(c("85003"),c("85203"),c("84803"),c("82113")),c(1,2,3,4),base=9), 
              grade_3cl          = grade_3cl,
              ki67_perc          = as.integer(self@database$KI67PCT),
              mitotic_index      = as.integer(self@database$NBMIT),
              dcis_component     = factor(self@database$INSITU, levels = c("non","oui"), labels=c(0,1)),
              invasive_or_dcis   = invasive_or_dcis
            ))
      }
)

#Surgery
#Check 5 : Done
#Old Questions : A) Toutes les patientes ont une date de chirurgie (DATCHIR.f1) ==> donc toutes les patientes ont la variable derived breast_surgery qui vaut 1.
#Mais 4 patientes ont "pas de geste mammaire NA NA" dans TYPCHIR. 
#Les NIPs des 4 patientes : "0704164" "0788572" (chir mammaire et axillaire en fait (erreur base sein)) (check fait par Aullene 16 Juin)
# "0982367" "1103334" geste axillaire seul, pas d'erreur. 
#Answer : On corrige pour les deux patientes  "0704164" "0788572" 
#B) mastectomie si au moins une fois mastectomie? OK
setMethod(f="mapping_surgery",
          signature="MappingSein", 
          definition=function(self)
          {
            TYPCHIR            <- paste(self@database$TYPCHIR.f1,self@database$TYPCHIR.f2, self@database$TYPCHIR.f3)
            
            breast_surgery_3cl <- rep(NA,nrow(self@database))
            breast_surgery_3cl [ which(TYPCHIR %in% c("NA NA NA") |
                                          TYPCHIR %in% c("pas de geste mammaire NA NA"))]  <- 9
            breast_surgery_3cl [grep("tumorectomie",TYPCHIR) ]                          <- 1
            breast_surgery_3cl [grep("mastectomie",TYPCHIR)  ]                          <- 2
            #Error corrections : 
            breast_surgery_3cl[self@database$numdos_curie == "0704164"]                 <- 2
            breast_surgery_3cl[self@database$numdos_curie == "0788572"]                 <- 1

            axillary_surgery_4cl                                                                      <- rep(9,nrow(self@database))
            axillary_surgery_4cl[which(self@database$GS =="oui")]                                     <- 1
            axillary_surgery_4cl[which(self@database$CAX == "oui")]                                   <- 2
            axillary_surgery_4cl[which(self@database$GS =="oui" & self@database$CAX == "oui") ]       <- 3

            return(list(
             dat_first_surg       = as.Date(self@database$DATCHIR.f1),
             breast_surgery_3cl   = factor(breast_surgery_3cl), 
             axillary_surgery_4cl = factor(axillary_surgery_4cl)
            ))
          }
)

#Treatments binary
#Check 5 : done
setMethod(f="mapping_treatments_binary",
          signature="MappingSein",
          definition=function(self)
          {
            ht               = ifelse(!is.na(self@database$DATDHT.q1),1,0)
            
            TYPHT            =  paste(self@database$TYPHT.q1,self@database$TYPHT.q2) 
            TYPHT[which(as.character(self@database$DATDHT.q1) != as.character(self@database$DATDHT.q2))]  <- NA
             
            ht_type_5cl                                                              <- rep(NA,nrow(self@database))
            ht_type_5cl[which(ht == 1)]                                              <- 5
            ht_type_5cl[which(self@database$TYPHT.q1== "Anti oestrogènes")]          <- 1
            ht_type_5cl[which(self@database$TYPHT.q1== "Inhibiteur Aromatase")]      <- 2
            ht_type_5cl[which(TYPHT %in% c("Agoniste LH RH Anti oestrogènes","Anti oestrogènes Agoniste LH RH"))]      <- 3
            ht_type_5cl[which(TYPHT %in% c("Inhibiteur Aromatase Agoniste LH RH","Agoniste LH RH Inhibiteur Aromatase"))]  <- 4
             
            antiher2         =  ifelse(self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB"),1,0) #793
            dat_first_antiher2                 <- rep(as.Date(NA),nrow(self@database))
            dat_first_antiher2[which(antiher2 == 1)] <- as.Date(self@database[which(antiher2 == 1),][["DATDTCIBL.f1"]], format="%Y-%m-%d")
            
            #Useful for the following
            neo_antiher2 <- rep(0,nrow(self@database))
            neo_antiher2[which(dat_first_antiher2 < self@database$dat_first_surg)] <- 1 #204 neo antiher2 
            
            source(paste(Sys.getenv("PROJECT_PATH"),'core/', opt$db_name,'/src/process_CT_base_sein.R', sep="/"))
            df_all_tcibl_adj <- process_df_all_tcibl_adj(self@database) #Il n'y a pas que antiher2 mais aussi tc_others dans la database. Au total 820 patientes
            numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
            df_all_tcibl_adj_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_tcibl_adj,by="numdos_curie_side")
            
            #Correction on antiher2
            #Decision taken on 2nd of July : 1 year instead of 6 months (pour inclure les patientes dans des protocoles particulier)
            #Some patients are corrected (see mapping_initial)
            antiher2[which(df_all_tcibl_adj_ordered$antiher2_1year_post_surgery == 0 & neo_antiher2==0)]     <- 0 #2 en moins : les deux sont des erreurs, données corrigées dans mapping initial
            dat_first_antiher2[which(antiher2==0)] <- NA
            
                  ###QC####
            #dat_surg_correction_antiher2 <- self@database[which(df_all_tcibl_adj_ordered$antiher2_1year_post_surgery == 0 & neo_antiher2==0 & self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB")), c("numdos_curie","dat_first_surg")] 
            #correction_antiher2 <- cbind(dat_surg_correction_antiher2,df_all_tcibl_adj_ordered[which(df_all_tcibl_adj_ordered$antiher2_1year_post_surgery == 0 & neo_antiher2==0 & self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB")),]) 
            #0603400 : Herceptin demarrée apres les rayons (et pas en même temps que la chimiothérapie) ==> la patiente est dans un protocole. 
            #correction_antiher2$delay <- as.Date(correction_antiher2$dat_first_antiher2_adj) - as.Date(correction_antiher2$dat_first_surg)
            #View(self@database %>% dplyr::select(numdos_curie,TYPTCIBL.q1.f1))
            #0880797 : 508 jours de difference. 2008 et pas 2009 : faute de frappe. 
            #0780633 : 373 jours de délai : erreur (en fait la patiente commence son herceptin plus tôt). 
            
            #TC other
            tc_other         =   regroup(self@database$TYPTCIBL.q1.f1 , list(c("HERCEPTIN","PERJETA","TYVERB",NA),c("Autres","AVASTIN")), c(0,1)) 
            dat_first_tc_other                 <- rep(as.Date(NA),nrow(self@database))
            dat_first_tc_other[which(tc_other == 1)] <- as.Date(self@database[which(tc_other == 1),][["DATDTCIBL.f1"]] )
            
            return(list(
              ct               =  factor(ifelse(!is.na(self@database$DATDCT.f1),1,0)),
              dat_first_ct     =  as.Date(self@database$DATDCT.f1),
              dat_end_first_ct =  as.Date(self@database$DATFCT.f1),
              rt               =  factor(ifelse(!is.na(self@database$DATDRT),1,0)),
              dat_first_rt     =  as.Date(self@database$DATDRT),
              ht               = factor(ht),
              dat_first_ht     =  as.Date(self@database$DATDHT.q1),
              antiher2         =  factor(antiher2),
              dat_first_antiher2 = as.Date(dat_first_antiher2),
              tc_other           = factor(tc_other),
              dat_first_tc_other = dat_first_tc_other,
              ht_type_5cl       = factor(ht_type_5cl)
            ))
          }
)

#Neoadj or not
#Check 5 : done
setMethod(f="mapping_neoadj_or_not",
          signature="MappingSein",
          definition=function(self)
          {
            neo_ct       <- rep(0,nrow(self@database))
            neo_ht       <- rep(0,nrow(self@database))
            neo_rt       <- rep(0,nrow(self@database))
            neo_antiher2 <- rep(0,nrow(self@database))
            neo_tc_other <- rep(0,nrow(self@database))

            neo_ct[which(as.Date(self@database$dat_first_ct) < as.Date(self@database$dat_first_surg))]             <- 1
            neo_ht[which(as.Date(self@database$dat_first_ht) < as.Date(self@database$dat_first_surg))]             <- 1
            neo_rt[which(as.Date(self@database$dat_first_rt) < as.Date(self@database$dat_first_surg))]             <- 1
            neo_antiher2[which(as.Date(self@database$dat_first_antiher2) < as.Date(self@database$dat_first_surg))] <- 1
            neo_tc_other[which(as.Date(self@database$dat_first_tc_other) < as.Date(self@database$dat_first_surg))] <- 1
            
            ###QC 
            #Mapping initial : correction de la date de radiothérapie de la patiente 0385372 (1 mois ds'erreur) #Done dans mapping initial
            #En fait une seule patiente ==> erreur de codage dans le mois (1 mois d'ecart entre les deux en fait). 
            #self@database$numdos_curie[which(as.Date(self@database$dat_first_rt) == as.Date(self@database$dat_first_surg))]
            #self@database$numdos_curie[which(as.Date(self@database$dat_first_ht) == as.Date(self@database$dat_first_surg))]
          
            dat_first_neo_ct         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_ht         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_rt         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_antiher2   <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_tc_other   <- rep(as.Date(NA),nrow(self@database))

            dat_first_neo_ct[which(neo_ct      ==1)]        <- self@database$dat_first_ct[which(neo_ct==1)]
            dat_first_neo_ht[which(neo_ht      ==1)]        <- self@database$dat_first_ht[which(neo_ht      ==1)]
            dat_first_neo_rt[which(neo_rt      ==1)]        <- self@database$dat_first_rt[which(neo_rt      ==1)]
            dat_first_neo_antiher2[which(neo_antiher2==1)]  <- self@database$dat_first_antiher2[which(neo_antiher2==1)]
            dat_first_neo_tc_other[which(neo_tc_other==1)]  <- self@database$dat_first_tc_other[which(neo_tc_other==1)]

            return(list(
              neo_ct = factor(neo_ct),
              neo_ht = factor(neo_ht),
              neo_rt = factor(neo_rt),
              neo_antiher2 = factor(neo_antiher2),
              neo_tc_other = factor(neo_tc_other),
              dat_first_neo_ct = as.Date(dat_first_neo_ct),
              dat_first_neo_ht = as.Date(dat_first_neo_ht),
              dat_first_neo_rt = as.Date(dat_first_neo_rt),
              dat_first_neo_antiher2 = as.Date(dat_first_neo_antiher2),
              dat_first_neo_tc_other = as.Date(dat_first_neo_tc_other)
            ))
          }
)

#Neoadjuvant_ct_antiher2
#Check 5 : done
###BEWARE : neo_ct_regimen : pb on devrait avoir 10* plus de 2 ()!!!!!!
#Erreur de code : j'ai retrouve qq anthra mais toujours tres peu : c'est possible? 
#check_pb_chemo_BS_elise.R (22/07/2020) : verif ok. APPASUR2 tres enrichie en anthra-taxanes et tres peu de anthra seules. 
setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingSein", 
          definition=function(self)
          {
              source(paste(Sys.getenv("PROJECT_PATH"),'core', opt$db_name,'src/process_CT_base_sein.R', sep="/"))
              path_mapping_ct = file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name ,"docs/file_mapping_chemotherapy_base_sein_annot.csv")
              list_chemo_processed <- process_CT_base_sein(self@database,path_mapping_ct)
              numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
              
              #Il faut garder uniquement les chimiothérapies neoadjuvantes : 
              #2 possibilites : a) chimio neoadj en .f1 uniquement et dans ce cas on prend le regime de .f1
              # b) chumio neoadj en .f1 et en .f2 (3 patientes) et dans ce cas on prend un résumé des deux. 
              df_all_chemo_wide_neo_2_casea <- list_chemo_processed$df_all_chemo_wide_2 %>% 
                filter(sequence_ct_each_f_f1 == "neoadjuvant" & 
                       sequence_ct_each_f_f2 %in% c(NA,"adjuvant","neo and adjuvant")) %>%
                dplyr::select(numdos_curie_side,recap_regimen_each_f_f1, date_end_f1, pluriseq_ct_f1) %>%
                rename(regimen_ct_each_pat_side = recap_regimen_each_f_f1,
                       date_end_f1f2 = date_end_f1,
                       pluriseq_ct_each_pat_side = pluriseq_ct_f1) #825 patients
              
              df_all_chemo_wide_neo_2_caseb <- list_chemo_processed$df_all_chemo_wide_2 %>% 
                filter(sequence_ct_each_f_f1 == "neoadjuvant" & 
                         sequence_ct_each_f_f2 == "neoadjuvant") %>%
                dplyr::select(numdos_curie_side,regimen_ct_each_pat_side, date_end_f1f2, pluriseq_ct_each_pat_side) #3 patients
              
              df_all_chemo_wide_neo_2 <- rbind(df_all_chemo_wide_neo_2_casea,df_all_chemo_wide_neo_2_caseb)
              
              #828 neo-adjuvant dedans (sur 859). 
              #31 neoadjuvant qui disparaissent : checker pourquoi. 
              ##### 24 ne sont pas dans df_all_chemo_wide_2 car polychimio ######
              #neo_ct_self <-paste0(self@database$numdos_curie[self@database$neo_ct == 1], "_", self@database$side[self@database$neo_ct == 1])
              #length(intersect(nip_chimio_TYPCTCL,neo_ct_self))
              ##### 2 qui sont en neo and adjuvant pour .f1. #####
              ##### 5 qui sont en NA's pour sequence_ct_each_f_f1 car dat_end_f1 non renseignée #####
              
              #TO CHECK NEOADJ REGIMEN
              # neo_regimen_to_check <- self@database %>% filter(!is.na(neo_ct_regimen)) %>% select(numdos_curie, neo_ct_regimen, neo_ct_sequence)
              # save(neo_regimen_to_check, 
              #      file = file.path(Sys.getenv("PROJECT_PATH"),
              #                                     "core",
              #                                     opt$db_name,
              #                                     "data",
              #                                     "neo_regimen_to_check.RData"))
              
              df_all_chemo_wide_neo_ordered_2 <- left_join(as.data.frame(numdos_curie_side),df_all_chemo_wide_neo_2,by="numdos_curie_side")
              
              #Monosequential 
              #Ok en fait (verif dans NEOREP) ==> pas besoin.
              
              return(list(
                neo_ct_regimen = recode(df_all_chemo_wide_neo_ordered_2$regimen_ct_each_pat_side,
                                        "anthra-taxanes" = "1", "anthra"="2", "taxanes"="3","others"="4"),
                nb_cycles_neo_ct = rep(NA,nrow(self@database)), 
                nb_cycles_neo_ct_anthra = rep(NA,nrow(self@database)),
                nb_cycles_neo_ct_taxanes = rep(NA,nrow(self@database)),
                dat_end_neo_ct = as.Date(df_all_chemo_wide_neo_ordered_2$date_end_f1f2),
                neo_ct_sequence = factor(df_all_chemo_wide_neo_ordered_2$pluriseq_ct_each_pat_side,
                                         levels=c("monosequential","bi-sequential","plurisequential"),
                                         labels=c("1","2","3"))   
                ######### neo antiher2 regimen ???? #######
                ))
          }
)

#Check 5 : done
#Adjuvant ct antiher2
###Old : Attention probleme : pas possible d'avoir seulement 2 neoadj and adj : bcp trop peu!!! ==> Probleme regle avec arrivée de .f2
setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingSein", 
          definition=function(self)
          {
            
            ###QC
              #3025 patientes avec chimio adjuvante dans df_all_chemo_wide_2
              #list_chemo_processed$df_all_chemo_wide_2 %>% filter(seq_ct_each_pat_side == "adjuvant") %>% dplyr::select(numdos_curie_side) %>% unique() %>% summarise(n())
              #124 patientes avec chimio néo et adjuvante (both) dans df_all_chemo_wide_2
              #list_chemo_processed$df_all_chemo_wide_2 %>% filter(seq_ct_each_pat_side == "neoadjuvant and adjuvant") %>% dplyr::select(numdos_curie_side) %>% unique() %>% summarise(n())
            
            adj_ct    <- rep(0,nrow(self@database))
            adj_ct[which(as.Date(self@database$DATDCT.f1)> as.Date(self@database$dat_first_surg) |
                         as.Date(self@database$DATDCT.f2)> as.Date(self@database$dat_first_surg) |
                         as.Date(self@database$DATFCT.f1)> as.Date(self@database$dat_first_surg) |
                         as.Date(self@database$DATFCT.f2)> as.Date(self@database$dat_first_surg))] <- 1
            #Attention !! 
            #128 patientes dans neo_ct et adj_ct contre 124 : ok car neo_ct ne depend que de dat_first_ct.

            adj_antiher2  <- rep(0,nrow(self@database))
            adj_antiher2[which(  as.Date(self@database$DATDTCIBL.f1)>  as.Date(self@database$dat_first_surg) |
                                   as.Date(self@database$DATDTCIBL.f2)>  as.Date(self@database$dat_first_surg)|
                                   as.Date(self@database$DATDTCIBL.f3)>  as.Date(self@database$dat_first_surg) |
                                   as.Date(self@database$DATFTCIBL.f1)>  as.Date(self@database$dat_first_surg) |
                                   as.Date(self@database$DATFTCIBL.f2)>  as.Date(self@database$dat_first_surg) |
                                   as.Date(self@database$DATFTCIBL.f3)>  as.Date(self@database$dat_first_surg))] <- 1
            
            source(paste(Sys.getenv("PROJECT_PATH"),'core', opt$db_name, 'src/process_CT_base_sein.R',sep="/"))
            path_mapping_ct = file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "docs/file_mapping_chemotherapy_base_sein_annot.csv")
            list_chemo_processed <- process_CT_base_sein(self@database,path_mapping_ct)
            numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
            
            #Plusieurs cas : a) .f1 est deja adjuvant : alors on prend resume de .f1 et .f2
            df_all_chemo_wide_adj2_casea <- list_chemo_processed$df_all_chemo_wide_2 %>% 
              filter(sequence_ct_each_f_f1 == "adjuvant") %>% 
              dplyr::select(numdos_curie_side,regimen_ct_each_pat_side, date_start_f1f2, date_end_f1f2, pluriseq_ct_each_pat_side) #3025 patientes
            
            #b) .f1 est neoadjuvant et .f2 est adjuvante : on prend .f2 uniquement
            df_all_chemo_wide_adj2_caseb <- list_chemo_processed$df_all_chemo_wide_2 %>% 
              filter(sequence_ct_each_f_f1 == "neoadjuvant" & 
                       sequence_ct_each_f_f2 == "adjuvant") %>%
              dplyr::select(numdos_curie_side,recap_regimen_each_f_f2, date_start_f2, date_end_f2, pluriseq_ct_f2) %>%
              rename(regimen_ct_each_pat_side = recap_regimen_each_f_f2,
                     date_start_f1f2 = date_start_f2,
                     date_end_f1f2 = date_end_f2,
                     pluriseq_ct_each_pat_side = pluriseq_ct_f2) #121 patientes
            
            #c) .f1 est NA ou neoadj et adj et .f2 est adjuvant : on considere que pas fiable donc on ne prend pas.
            # Sur les 3185 adj patientes : je recupere le regiment de 3146 : il en manque 39.
            # Ok car : polychimio + case c. 
            
            df_all_chemo_wide_adj2 <- rbind(df_all_chemo_wide_adj2_casea,df_all_chemo_wide_adj2_caseb)
            df_all_chemo_wide_adj_ordered2 <- left_join(as.data.frame(numdos_curie_side),df_all_chemo_wide_adj2,by="numdos_curie_side")
            
            df_all_tcibl_adj <- process_df_all_tcibl_adj(self@database) 
            df_all_tcibl_adj_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_tcibl_adj,by="numdos_curie_side")
            adj_antiher2[which(df_all_tcibl_adj_ordered$antiher2_1year_post_surgery == 0)] <- 0
            
            #On fait la correction aussi pour les dates. 
            dat_first_antiher2_adj = as.Date(df_all_tcibl_adj_ordered$dat_first_antiher2_adj)
            dat_first_antiher2_adj[which(df_all_tcibl_adj_ordered$antiher2_6months_post_surgery == 0)] <- NA 

            return(list(
              adj_ct = factor(adj_ct),
              adj_ct_regimen =  factor(recode(df_all_chemo_wide_adj_ordered2$regimen_ct_each_pat_side,
                                       "anthra-taxanes" = "1", "anthra"="2", "taxanes"="3","others"="4")), 
              nb_cycles_adj_ct_taxanes = rep(NA,nrow(self@database)),
              nb_cycles_adj_ct_anthra = rep(NA,nrow(self@database)),
              adj_ct_sequence = factor(recode(df_all_chemo_wide_adj_ordered2$pluriseq_ct_each_pat_side,
                                       "monosequential" = "1", "bi-sequential"="2", "plurisequential"="3")),
              nb_cycles_adj_ct = rep(NA,nrow(self@database)), 
              dat_first_adj_ct = as.Date(df_all_chemo_wide_adj_ordered2$date_start_f1f2), # ok pour NA's
              dat_end_adj_ct = as.Date(df_all_chemo_wide_adj_ordered2$date_end_f1f2), # 19 NA's en plus : ok 
              adj_antiher2 = factor(adj_antiher2),
              dat_first_adj_antiher2 = dat_first_antiher2_adj 
            ))
          }
)

#Tumor char surg
#Check 5 : done 
###discussion sur WA (RT2_young) 13/03/2020 13h : toutes les variables de cette family ne sont remplies que si chir d'emblee. Sinon pas de sens. 
setMethod(f="mapping_tumor_char_surg",
          signature="MappingSein", 
          definition=function(self)
          {
            #We need to create primary_ttt
            breast_surgery = ifelse(!is.na(self@database$dat_first_surg),1,0) 
            primary_ttt = rep(1,nrow(self@database))
            primary_ttt[breast_surgery==0] = 9
            primary_ttt[unique(c(which(self@database$neo_ct == 1),
                                 which(self@database$neo_ht == 1),
                                 which(self@database$neo_rt == 1),
                                 which(self@database$neo_antiher2 == 1),
                                 which(self@database$neo_tc_other == 1)))] = 2
            
            NBGSPOS_2 <- ifelse(is.na(as.integer(self@database$NBGSPOS)),0,as.integer(self@database$NBGSPOS))
            NBGSPOS_2[which(primary_ttt != 1)] <- NA 
            #0883587 : 39 ganglions ==> ok 
            
            NBCAPOS_2 <- ifelse(is.na(as.integer(self@database$NBCAPOS)),0,as.integer(self@database$NBCAPOS))
            NBCAPOS_2[which(primary_ttt != 1)] <- NA 
            
            histo_size = as.integer(self@database$TINF)
            histo_size[which(primary_ttt != 1)] <- NA #15 NA de plus
            
            ptuicc_5cl = regroup(self@database$PTUICC , list(c("pT0","pTis"),c("pT1a","pT1b","pT1c","pT1Mic","pT1"),
                                                             c("pT2"),c("pT3"),c("pT4b","pT4","pT4a","pT4c")), c(0,1,2,3,4))
            ptuicc_5cl[which(primary_ttt != 1)] <- NA #8 NA de plus
            
            lvi     = factor(self@database$EMBV,levels = c("non","oui"), labels = c(0,1))
            lvi[which(primary_ttt != 1)] <- NA
            
            multifocality_histo = factor(self@database$MULTHIST,levels = c("non","oui"), labels = c(0,1))
            multifocality_histo[which(primary_ttt != 1)]  <- NA
            
            return(list(
              nbggpos = NBGSPOS_2 + NBCAPOS_2,
              histo_size = histo_size ,
              ptuicc_5cl = ptuicc_5cl,
              lvi     = lvi,
              multifocality_histo = multifocality_histo
            ))
          }
)

#Tumor char neo
#Check 5 : done 
setMethod(f="mapping_tumor_char_neo",
          signature="MappingSein",
          definition=function(self)
          {
            variables_ini_neoadj	<- c("CTPMM","CTPMMRES","CTPGG","CTPGGRES","BIOPRETT","LIEUBIOP")
            variables_post_neoadj	<- c("PTUICC","YPTUICC","YPNUICC","PNUICC","CHEVALIE","SATALOFT","SATALOFG")
            
            breast_res_infiltr   <- rep(NA,nrow(self@database))
            breast_res_infiltr[self@database$YPTUICC %in% c("pT0","pTis")]                                                          <- 0 #134 
            breast_res_infiltr[self@database$SATALOFT %in% c("TA effet thérapeutique total ou presque")]                            <- 0 #35
            breast_res_infiltr[self@database$CHEVALIE %in% c("G1 aucun reliquat sein et ggl",
                                                             "G2 in situ strict sein - pas de reliquat ggl")]                       <- 0 #148
            breast_res_infiltr[self@database$YPTUICC %in% c(c("pT1","pT1a","pT1b","pT1c","pT1Mic","pT2","pT3","pT4","pT4b","pT4d"))]<- 1 #471
            breast_res_infiltr[self@database$SATALOFT %in% c("TB effet thérapeutique de plus de 50 pourcents",
                                                             "TC effet thérapeutique moins de 50 pourcents",
                                                             "TD pas effet thérapeutique")]                                         <- 1 #36
            
            #Last check neoadj
            trace_post_NAC_stage <- ifelse(  !is.na(self@database$YPTUICC) |  
                                             !is.na(self@database$YPNUICC) |
                                             !is.na(self@database$CHEVALIE)|
                                             !is.na(self@database$SATALOFT)|
                                             !is.na(self@database$SATALOFG),1,0) #759 yes.
            
            neoadj_check <- rep(NA,nrow(self@database))
            neoadj_check[which(self@database$neo_ct == 1 & trace_post_NAC_stage == 1)]     <- 1
            neoadj_check[which(self@database$neo_ht == 1 & trace_post_NAC_stage == 1)]     <- 1
            neoadj_check[which(self@database$neo_rt == 1 & trace_post_NAC_stage == 1)]     <- 1
            neoadj_check[which(self@database$neo_antiher2 == 1 & trace_post_NAC_stage == 1)]     <- 1
            
            neoadj_check[which(self@database$neo_ct == 0 & trace_post_NAC_stage == 0)]     <- 0
            neoadj_check[which(self@database$neo_ht == 0 & trace_post_NAC_stage == 0)]     <- 0
            neoadj_check[which(self@database$neo_rt == 0 & trace_post_NAC_stage == 0)]     <- 0
            neoadj_check[which(self@database$ct == 0 & trace_post_NAC_stage == 0)]     <- 0
            neoadj_check[which(self@database$neo_antiher2 == 0 & trace_post_NAC_stage == 0)]     <- 0
            
            neoadj_check[which(is.na(self@database$dat_first_surg))]     <- 9
            
            #Correction on breast res infiltr to have only neoadj patients
            breast_res_infiltr[neoadj_check == 0 | is.na(neoadj_check)] <- NA
            
            #Nbggpos vaut 0 si jamais pas surg primary ttt.
            #On doit donc reconstruire la variable nbggpos. 
            NBGSPOS_2 <- ifelse(is.na(as.integer(self@database$NBGSPOS)),0,as.integer(self@database$NBGSPOS))
            NBGSPOS_2[which(neoadj_check ==0  | is.na(neoadj_check))] <- NA 
            
            NBCAPOS_2 <- ifelse(is.na(as.integer(self@database$NBCAPOS)),0,as.integer(self@database$NBCAPOS))
            NBCAPOS_2[which(neoadj_check ==0  | is.na(neoadj_check))] <- NA 
            
            nbggpos_postneo_ct <- as.numeric(NBGSPOS_2 + NBCAPOS_2)
            nbggpos_postneo_ct[neoadj_check == 0 | is.na(neoadj_check)] <- NA
            
            pcr                                                               <- rep(NA,nrow(self@database))
            pcr[which(breast_res_infiltr == 0 & self@database$nuicc_4cl==0)]  <- 1 
            pcr[which(breast_res_infiltr == 1 | self@database$nuicc_4cl %in% c("1","2","3"))]  <- 0 
            pcr[which(neoadj_check == 1 & is.na(pcr) &
                        self@database$CHEVALIE %in% c("G3 carcinome inf modifié sein et/ou ggl",
                                                      "G4 carcinome inf non ou peu modifié sein et/ou ggl") )] <- 0
            pcr[which(neoadj_check ==0 | is.na(neoadj_check))] <- NA
            #78 patientes en pcr. 671 non pcr. 

            return(list(
              #Je suis obligée de modifier le nom de breast_res_infiltr sinon la variable pcr va etre renommee en pcr_old.
              breast_res_infiltr_raw = factor(breast_res_infiltr),
              nbggpos_postneo = nbggpos_postneo_ct,
              pcr = factor(pcr) 
            ))
          }
)

#Events and censor
#Check 5 : done
setMethod(f="mapping_events_and_censor",
          signature="MappingSein", 
          definition=function(self)
          {
            
            source(paste(Sys.getenv("PROJECT_PATH"),'core/', opt$db_name,'/src/prepare_curation_file.R', sep="/"))
            cleaned_records <- left_join(as.data.frame(as.character(self@database$numdos_curie)),
                                         prepare_cleaned_records("Curationbaseseinappa-ExportElise_DATA_2020-11-27_1422.csv"),
                                         by = c("as.character(self@database$numdos_curie)" = "numdos_curie")
            ) %>%
              rename(numdos_curie = `as.character(self@database$numdos_curie)`)

            #Test sur 8901809	ok
            
            ev_prog_neo    <- rep(NA,nrow(self@database))
            dat_prog_neo   <- rep(as.Date(NA),nrow(self@database)) 
            
            ev_recloc      <- pmax(ifelse(!is.na(self@database$DATRLOC.f1),1,0), cleaned_records$ev_recloc_cleaned, na.rm = T)
            dat_recloc     <- pmin(as.Date(self@database$DATRLOC.f1), cleaned_records$dat_recloc_cleaned, na.rm = T )
            print("Ev recloc")
            summary(factor(ifelse(!is.na(self@database$DATRLOC.f1),1,0)))
            summary(factor(ev_recloc))

            ev_recreg      <- pmax(ifelse(!is.na(self@database$DATRGG.f1),1,0), cleaned_records$ev_recreg_cleaned, na.rm = T)
            dat_recreg     <- pmin(as.Date(self@database$DATRGG.f1), cleaned_records$dat_recreg_cleaned, na.rm = T )
            print("Ev recreg")
            summary(factor(ifelse(!is.na(self@database$DATRGG.f1),1,0)))
            summary(factor(ev_recreg))
            
            ev_meta      <- pmax(ifelse(!is.na(self@database$DATMET.f1),1,0), cleaned_records$ev_meta_cleaned, na.rm = T)
            dat_meta     <- pmin(as.Date(self@database$DATMET.f1), cleaned_records$dat_meta_cleaned, na.rm = T )
            print("Ev meta")
            summary(factor(ifelse(!is.na(self@database$DATMET.f1),1,0)))
            summary(factor(ev_meta))
            
            ev_contro      <- pmax(ifelse(is.na(self@database$datecontro),0,1), cleaned_records$ev_contro_cleaned, na.rm = T)
            dat_contro     <- pmin(as.Date(self@database$datecontro), cleaned_records$dat_contro_cleaned, na.rm = T )
            print("Ev contro")
            summary(factor(ifelse(!is.na(self@database$datecontro),1,0)))
            #summary(factor(ifelse(!is.na(self@database$datecontro[self@database$numdos_curie %in% cleaned_records1$numdos_curie]),1,0)))
            summary(factor(ev_contro))
            
            ev_secondk      <- pmax(ifelse(!is.na(self@database$DATDK.f1),1,0), cleaned_records$ev_secondk_cleaned, na.rm = T)
            dat_secondk     <- pmin(as.Date(self@database$DATDK.f1), cleaned_records$dat_secondk_cleaned, na.rm = T )
            print("Ev secondk")
            summary(factor(ifelse(!is.na(self@database$DATDK.f1),1,0)))
            #summary(factor(ifelse(!is.na(self@database$DATDK.f1[self@database$numdos_curie %in% cleaned_records1$numdos_curie]),1,0)))
            summary(factor(ev_secondk))
            
            status_vital      <- pmax(as.integer(as.character(factor(self@database$ETATDN, levels = c("vivant", "décédé"), labels=c(0,1)))), 
                                      cleaned_records$status_vital_cleaned,
                                      na.rm = T)
            status_vital[is.na(status_vital)] <- 0 #You cannot have a NA for status vital
            print("Status vital")
            summary(factor(factor(self@database$ETATDN, levels = c("vivant", "décédé"), labels=c(0,1))))
            #summary(factor(factor(self@database$ETATDN[self@database$numdos_curie %in% cleaned_records1$numdos_curie], levels = c("vivant", "décédé"), labels=c(0,1))))
            summary(factor(status_vital))

            dat_last_news     <- pmax(as.Date(self@database$DATDN), as.Date(cleaned_records$dat_last_news_cleaned), na.rm = T )
            print("Dat last news")
            summary(as.Date(self@database$DATDN))
            summary(dat_last_news)
            
            dat_last_update     <- pmax(as.Date(self@database$DATDN2,origin = "1899-12-30"), as.Date(cleaned_records$dat_last_update_cleaned), na.rm = T )
            print("Dat last update")
            summary(as.Date(self@database$DATDN2,origin = "1899-12-30"))
            summary(dat_last_update)
            
            #3 pour qui pas de causes de décès renseignée?
            cause_death <- regroup(self@database$CAUSDC , list(c("cancer du sein","iatrogène"),c("2ème cancer","autre","maladie intercurrente","non précisé")), c(1,2))
            cause_death[which(status_vital==0)] <- NA
            
            ev_prog_neo[which(as.Date(dat_prog_neo) > self@dat_censor)] <- 0
            ev_recloc[which(dat_recloc > self@dat_censor)]     <- 0
            ev_recreg[which(dat_recreg > self@dat_censor)]     <- 0
            ev_meta[which(dat_meta > self@dat_censor)]         <- 0
            ev_contro[which(dat_contro > self@dat_censor)]     <- 0
            ev_secondk[which(dat_secondk > self@dat_censor)]       <- 0
            #Elise : pour l'instant on ne censure pas
            #status_vital[which(as.Date(dat_last_news) > self@dat_censor & status_vital== 1 )]       <- 0
            
            return(list(
              dat_censor_database = rep(self@dat_censor,nrow(self@database)),
              ev_prog_neo = factor(ev_prog_neo),
              dat_prog_neo = as.Date(dat_prog_neo),
              ev_recloc = factor(ev_recloc),
              dat_recloc = as.Date(dat_recloc),
              ev_recreg = factor(ev_recreg),
              dat_recreg = as.Date(dat_recreg),
              ev_meta = factor(ev_meta),
              dat_meta = as.Date(dat_meta),
              ev_contro = factor(ev_contro),
              dat_contro = as.Date(dat_contro),
              ev_secondk = factor(ev_secondk),
              dat_secondk = as.Date(dat_secondk),
              status_vital = factor(status_vital),
              dat_last_news = as.Date(dat_last_news),
              dat_last_update = as.Date(dat_last_update), 
              cause_death = factor(cause_death)
            ))
          }
)

#Comedication
setMethod(f="mapping_comedication",
          signature="MappingSein", 
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Comorbidity
setMethod(f="mapping_comorbidity",
          signature="MappingSein", 
          definition=function(self)
          {
            return(list(
            ))
          }
)
