#List of needed libraries : library(tidyr) ; library(dplyr)
library(tidyr)
library(dplyr)

#Util functions
na_factor <- function(col,na_string,levels,labels){
  col[which(col %in% na_string)] <- NA
  col <- factor(col,levels=levels,labels=labels)
  return(col)
}

regroup <- function(col,input,output, base=NA){
  col_res <- rep(base, length(col))
  if(length(input) != length(output)){stop("Error : the two vectors are not of equal size")}
  for(i in 1:length(input)){
    col_res[which(col %in% input[i][[1]])] <- output[i]
  }
  col_res <- factor(col_res)
  return(col_res)
}

#Define MappingSein Class
MappingSein <- setClass(
  "MappingSein",

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "sein",
    database = read_xlsx(file.path(
      Sys.getenv("PROJECT_PATH"),
      "core/15_base_sein_appasur/data/base_sein.xlsx"
    ), na = "NA"),
    dat_censor = as.Date("2020-12-31")
  ),

  contains = "Mapping"
)

setMethod(f="mapping_initial",
          signature="MappingSein",
          definition=function(self)
          {
            self@database <- self@database %>% select(-starts_with("NUMANA"),
                                                      -starts_with("LANA"),
                                                      -starts_with("CHIRMET"), -starts_with("DATANA"),-starts_with("CIMORG"),-starts_with("NVRE"),
                                                      -starts_with("MR"), -starts_with("HTMET"),-starts_with("RTMET"),-starts_with("CTMET"),
                                                      -starts_with("TYPDK"), -starts_with("SIEGDK"),-starts_with("ACTANA"),
                                                      -starts_with("TTMET"),-starts_with("TTRLOC"),
                                                      -starts_with("TCMET"), -starts_with("RTRG"),-starts_with("TTRGG"),-starts_with("CHIRR"))

            return(self)
          }
)

setMethod(f="mapping_patient_id",
          signature="MappingSein",
          definition=function(self)
          {
            center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3))
            return(list(
              database = rep(1,nrow(self@database)),
              numdos_curie = self@database$NUMDOS,
              cletri = rep("toto",nrow(self@database)),
              side  = factor(self@database$cote, levels = c("G","D"), labels = c(1,2)),
              dat_birth = as.Date(self@database$DATNAI),
              dat_bc_diagnosis = as.Date(ifelse(is.na(self@database$DATDIAG), as.character(self@database$DATCHIR.f1), as.character(self@database$DATDIAG)),origin = "1900-01-01"),
              center = factor(ifelse(center_curie == 3,2,1))
             #base_cletri = paste0(self@database$database, "_",self@database$cletri) #Pas dans le data dictionnary
            ))
          }
)

setMethod(f="mapping_patient_char",
          signature="MappingSein",
          definition=function(self)
          {
            return(list(
              age_menarche       = as.numeric(self@database$AGEPR),
              prev_pregnancy     = na_factor(self@database$GROS, c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              menop              = na_factor(self@database$MENOP,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              age_menop          = as.numeric(self@database$AGMENOP),
              hrt                = na_factor(self@database$THS,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              fam_history        = na_factor(self@database$ANTFAM,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              brca_screen        = factor(self@database$RECMUT, levels = c("non","oui"), labels = c(0,1)),
              brca_mut           = factor(self@database$RESMUT, levels = c("non","oui"), labels = c(0,1)),
              weight             = as.numeric(self@database$POIDS),
              size               = as.numeric(self@database$TAILLE)/100
            ))
          }
)

setMethod(f="mapping_bc_diagnosis",
          signature="MappingSein",
          definition=function(self)
          {
            return(list(
              inflammatory_BC    = factor(ifelse(self@database$TUICC  %in% c("T4c","T4d"), 1,0)),
              moddiag            = factor(self@database$MODDIAG, levels = c("radiologique","clinique"),labels=c(0,1)),
              clin_multifocality = factor(self@database$MULTIF, levels = c("non","oui"),labels=c(1,2)),
              tclin              = as.numeric(self@database$TCLIN),
              tuicc_5cl          = regroup(self@database$TUICC , list(c("T0","Tis"),c("T1"),c("T2"),c("T3"),c("T4a","T4b","T4c","T4d")), c(0,1,2,3,4)),
              nuicc_4cl          = regroup(self@database$NUICC , list(c("N0"),c("N1"),c("N2"),c("N3")), c(0,1,2,3)),
              muicc              = factor(self@database$MUICC, levels = c("M0","M1"), labels = c(0,1))
            ))
          }
)

setMethod(f="mapping_bc_biology",
          signature = "MappingSein",
          definition = function(self)
          {
            her2_status                                                                                                <- NA
            her2_status[self@database$HERIHC == "+++" | self@database$HERA == "oui"]                                   <- 1
            her2_status[self@database$HERIHC == "+" | self@database$HERIHC == "negatif" |self@database$HERA == "non"]  <- 0
            return(list(
            er_status          = factor(self@database$ROCL, levels = c("RO -", "RO +"), labels=c(0,1)),
            pr_status          = factor(self@database$RPCL, levels = c("RP -", "RP +"), labels=c(0,1)),
            er_intensity       = factor(self@database$ROINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
            pr_intensity       = factor(self@database$RPINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
            er_percentage      = as.integer(self@database$ROPCT),
            pr_percentage      = as.integer(self@database$RPPCT),
            her2_status        = factor(her2_status),
            histo_5cl          = regroup(self@database$CIMO1,list(c("85003"),c("85203"),c("84803"),c("82113")),c(1,2,3,4),base=9), #???
            grade_3cl          = regroup(self@database$EE,list(c("1"),c("2"),c("3")),c(1,2,3)),
            Ki67_perc          = as.integer(self@database$KI67PCT),
            mitotic_index      = as.integer(self@database$NBMIT),
            invasive_or_dcis   = factor(self@database$INFILT, levels = c("infiltrant", "CCIS","CCIS + micro infiltrant"), labels=c(1,2,1)),
            dcis_component     = factor(self@database$INSITU, levels = c("non","oui"), labels=c(0,1))
            ))
          }
)

setMethod(f="mapping_surgery",
          signature="MappingSein",
          definition=function(self)
          {
            TYPCHIR            <- paste(self@database$TYPCHIR.f1,self@database$TYPCHIR.f2, self@database$TYPCHIR.f3)
            breast_surgery_3cl <- rep(NA,nrow(self@database))
            breast_surgery_3cl [ which(TYPCHIR %in% c("NA NA NA") |
                                       TYPCHIR %in% c("pas de geste mammaire NA NA"))]  <- 0
            breast_surgery_3cl [grep("tumorectomie",TYPCHIR) ]                          <- 1
            breast_surgery_3cl [grep("mastectomie",TYPCHIR)  ]                          <- 2

            axillary_surgery_4cl                                                                      <- rep(0,nrow(self@database))
            axillary_surgery_4cl[which(self@database$GS =="oui")]                                     <- 1
            axillary_surgery_4cl[which(self@database$CAX == "oui")]                                   <- 2
            axillary_surgery_4cl[which(self@database$GS =="oui" & self@database$CAX == "oui") ]       <- 3

            return(list(
              dat_first_surg      = as.Date(self@database$DATCHIR.f1),
              breast_surgery_3cl  = factor(breast_surgery_3cl),
              axillary_surgery_4cl= factor(axillary_surgery_4cl)
            ))
          }
)

setMethod(f="mapping_treatments_binary",
          signature="MappingSein",
          definition=function(self)
          {
            TYPHT            =  paste(self@database$TYPHT.q1,self@database$TYPHT.q2)
            TYPHT[ grep("NA",TYPHT) ]                   <- NA
            TYPHT[which(as.character(self@database$DATDHT.q1) != as.character(self@database$DATDHT.q2))]  <- NA

            ht               = ifelse(!is.na(self@database$DATDHT.q1),1,0)

            ht_type_5cl                                                              <- rep(NA,nrow(self@database))
            ht_type_5cl[which(ht == 1)]                                              <- 5
            ht_type_5cl[which(self@database$TYPHT.q1== "Anti-oestrogenes")]          <- 1
            ht_type_5cl[which(self@database$TYPHT.q1== "Inhibiteur Aromatase")]      <- 2
            ht_type_5cl[which(TYPHT %in% c("Agoniste LH RH Anti-oestrogenes","Anti-oestrogenes Agoniste LH RH"))]      <- 3
            ht_type_5cl[which(TYPHT %in% c("Inhibiteur Aromatase Agoniste LH RH","Agoniste LH RH Inhibiteur Aromatase"))]  <- 4

            antiher2         =  ifelse(self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB"),1,0)
            dat_first_antiher2                 <- rep(as.Date(NA),nrow(self@database))
            dat_first_antiher2[which(antiher2 == 1)] <- as.Date(self@database[which(antiher2 == 1),][["DATDTCIBL.f1"]], format="%Y-%m-%d")

            neo_antiher2 <- rep(0,nrow(self@database))
            neo_antiher2[which(dat_first_antiher2 < self@database$dat_first_surg)] <- 1

            source(paste(Sys.getenv("PROJECT_PATH"),'core/01_base_sein/src/process_CT_base_sein.R',sep="/"))
            df_all_tcibl_adj <- process_df_all_tcibl_adj(self@database)
            numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
            df_all_tcibl_adj_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_tcibl_adj,by="numdos_curie_side")
            antiher2[which(df_all_tcibl_adj_ordered$antiher2_6months_post_surgery == 0 & neo_antiher2==0)]     <- 0
            dat_first_antiher2[which(antiher2==0)] <- NA

            tc_other         =  ifelse(!self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB",NA),1,0) #Gestion des NAs à revoir
            dat_first_tc_other                 <- rep(as.Date(NA),nrow(self@database))
            dat_first_tc_other[which(tc_other == 1)] <- as.Date(self@database[which(tc_other == 1),][["DATDTCIBL.f1"]] )

            return(list(
                ct               =  factor(ifelse(!is.na(self@database$DATDCT.f1),1,0)), #Pas dans le data dictionnary
                dat_first_ct     =  as.Date(self@database$DATDCT.f1),
                dat_end_first_ct =  as.Date(self@database$DATFCT.f1),
                rt               =  factor(ifelse(!is.na(self@database$DATDRT),1,0)),
                dat_first_rt     =  as.Date(self@database$DATDRT),
                ht               =  factor(ifelse(!is.na(self@database$DATDHT.q1),1,0)),
                dat_first_ht     =  as.Date(self@database$DATDHT.q1),
                antiher2         =  factor(antiher2),
                dat_first_antiher2 = as.Date(dat_first_antiher2),
                ht = factor(ht),
                tc_other = factor(tc_other),
                dat_first_tc_other = dat_first_tc_other,
                ht_type_5cl       = factor(ht_type_5cl)
             ))
          }
)

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

            primary_ttt                              <- rep(1,nrow(self@database))
            primary_ttt[which(self@database$breast_surgery==0)] <- 9
            primary_ttt[which( self@database$neo_ct == 1 |
                        self@database$neo_ht      == 1 |
                        self@database$neo_rt      == 1 |
                        self@database$neo_antiher2== 1 |
                        self@database$neo_tc_other== 1 )]       <- 2

            dat_first_neo_ct         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_ht         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_rt         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_antiher2   <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_tc_other   <- rep(as.Date(NA),nrow(self@database))

            dat_first_neo_ct[which(neo_ct      ==1)]        <- self@database[which(neo_ct      ==1),"dat_first_ct"]
            dat_first_neo_ht[which(neo_ht      ==1)]        <- self@database[which(neo_ht      ==1),"dat_first_ht"]
            dat_first_neo_rt[which(neo_rt      ==1)]        <- self@database[which(neo_rt      ==1),"dat_first_rt"]
            dat_first_neo_antiher2[which(neo_antiher2==1)]  <- self@database[which(neo_antiher2==1),"dat_first_antiher2"]
            dat_first_neo_tc_other[which(neo_tc_other==1)]  <- self@database[which(neo_tc_other==1),"dat_first_tc_other"]

            return(list(
              neo_ct = factor(neo_ct),
              neo_ht = factor(neo_ht),
              neo_rt = factor(neo_rt),
              neo_antiher2 = factor(neo_antiher2),
              neo_tc_other = factor(neo_tc_other),
              primary_ttt = factor(primary_ttt), #Not in data dictionnary
              dat_first_neo_ct = as.Date(dat_first_neo_ct),
              dat_first_neo_ht = as.Date(dat_first_neo_ht),
              dat_first_neo_rt = as.Date(dat_first_neo_rt),
              dat_first_neo_antiher2 = as.Date(dat_first_neo_antiher2),
              dat_first_neo_tc_other = as.Date(dat_first_neo_tc_other)
            ))
          }
)

setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingSein",
          definition=function(self)
          {
            source(paste(Sys.getenv("PROJECT_PATH"),'core/01_base_sein/src/process_CT_base_sein.R', sep="/"))
            path_mapping_ct = file.path(Sys.getenv("PROJECT_PATH"), "core/01_base_sein/docs/file_mapping_chemotherapy_base_sein_annot.csv")
            list_chemo_processed <- process_CT_base_sein(self@database,path_mapping_ct)
            numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
            df_all_chemo_wide_neo <- list_chemo_processed$df_all_chemo_wide_2 %>% filter(seq_ct_each_pat_side == "neoadjuvant")
            df_all_chemo_wide_neo_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_chemo_wide_neo,by="numdos_curie_side")

            return(list(
               neo_ct_regimen = recode(df_all_chemo_wide_neo_ordered$regimen_ct_each_pat_side,
                                       "anthra-taxanes" = "1", "anthra"="2", "taxanes"="3","others"="4"),
               nb_cycles_neo_ct = df_all_chemo_wide_neo_ordered$sum_nbcy_q1q2_f1f2,
               dat_end_neo_ct = ad.Date(f_all_chemo_wide_neo_ordered$date_end_f1f2),
               neo_ct_sequence = factor(df_all_chemo_wide_neo_ordered$pluriseq_ct_each_pat_side,levels=c("monosequential","bi-sequential","plurisequential"),labels=c("1","2","3"))
               #dat_first_antiher2_neo?
               ))
          }
)

setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingSein",
          definition=function(self)
          {
            adj_ct    <- rep(0,length(self@database))
            adj_ct[which(as.Date(self@database$DATDCT.f1)> as.Date(self@database$dat_first_surg) |
                       as.Date(self@database$DATFCT.f1)> as.Date(self@database$dat_first_surg) |
                       as.Date(self@database$DATDCT.f2) > as.Date(self@database$dat_first_surg) |
                       as.Date(self@database$DATFCT.f2) > as.Date(self@database$dat_first_surg) |
                       as.Date(self@database$DATDCT.f3) > as.Date(self@database$dat_first_surg) |
                       as.Date(self@database$DATFCT.f3) > as.Date(self@database$dat_first_surg))] <- 1

            adj_antiher2  <- rep(0,nrow(self@database))
            adj_antiher2[which(  as.Date(self@database$DATDTCIBL.f1)>  as.Date(self@database$dat_first_surg) |
                                 as.Date(self@database$DATDTCIBL.f2)>  as.Date(self@database$dat_first_surg)|
                                 as.Date(self@database$DATDTCIBL.f3)>  as.Date(self@database$dat_first_surg) |
                                 as.Date(self@database$DATFTCIBL.f1)>  as.Date(self@database$dat_first_surg) |
                                 as.Date(self@database$DATFTCIBL.f2)>  as.Date(self@database$dat_first_surg) |
                                 as.Date(self@database$DATFTCIBL.f3)>  as.Date(self@database$dat_first_surg))] <- 1

            source(paste(Sys.getenv("PROJECT_PATH"),'core/01_base_sein/src/process_CT_base_sein.R',sep="/"))
            path_mapping_ct = file.path(Sys.getenv("PROJECT_PATH"), "core/01_base_sein/docs/file_mapping_chemotherapy_base_sein_annot.csv")
            list_chemo_processed <- process_CT_base_sein(self@database,path_mapping_ct)
            numdos_curie_side  <- paste0(self@database$numdos_curie,"_",self@database$side)
            df_all_chemo_wide_adj <- list_chemo_processed$df_all_chemo_wide_2 %>% filter(seq_ct_each_pat_side == "adjuvant")
            df_all_chemo_wide_adj_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_chemo_wide_adj,by="numdos_curie_side")

            ####Attention probleme possible ####
            df_all_tcibl_adj <- process_df_all_tcibl_adj(self@database)
            df_all_tcibl_adj_ordered <- left_join(as.data.frame(numdos_curie_side),df_all_tcibl_adj,by="numdos_curie_side")
            adj_antiher2[which(df_all_tcibl_adj_ordered$antiher2_6months_post_surgery == 0)] <- 0

            return(list(
              adj_ct = factor(adj_ct),
              adj_ct_regimen =  recode(df_all_chemo_wide_adj_ordered$regimen_ct_each_pat_side,
                                       "anthra-taxanes" = "1", "anthra"="2", "taxanes"="3","others"="4"),
              adj_ct_sequence = recode(df_all_chemo_wide_adj_ordered$pluriseq_ct_each_pat_side,
                                       "monosequential" = "1", "bi-sequential"="2", "plurisequential"="3"),
              nb_cycles_adj_ct = df_all_chemo_wide_adj_ordered$sum_nbcy_q1q2_f1f2,
              dat_first_adj_ct = as.Date(df_all_chemo_wide_adj_ordered$date_start_f1f2),
              dat_end_adj_ct = as.Date(df_all_chemo_wide_adj_ordered$date_end_f1f2),
              adj_antiher2 = factor(adj_antiher2),
              dat_first_antiher2_adj = as.Date(df_all_tcibl_adj_ordered $dat_first_antiher2_adj)
            ))
          }
)

setMethod(f="mapping_tumor_char_surg",
          signature="MappingSein",
          definition=function(self)
          {
            NBGSPOS_2 <- ifelse(is.na(as.integer(self@database$NBGSPOS)),0,as.integer(self@database$NBGSPOS))
            NBGSPOS_2[which(self@database$primary_ttt != 1)] <- NA
            NBCAPOS_2 <- ifelse(is.na(as.integer(self@database$NBCAPOS)),0,as.integer(self@database$NBCAPOS))
            NBCAPOS_2[which(self@database$primary_ttt != 1)] <- NA
            multifocality_clin_histo                                                                  <- rep(0,nrow(self@database))
            multifocality_clin_histo[which(self@database$neoadj==1 & self@database$MULTIF == "oui")]  <- 1
            multifocality_clin_histo[which(self@database$MULTHIST == "oui")]                          <- 1
            multifocality_clin_histo[which(self@database$primary_ttt != 1)] <- NA

            return(list(
              nbggpos = NBGSPOS_2 + NBCAPOS_2,
              histo_size = as.integer(self@database$TINF) ,
              ptuicc_5cl = regroup(self@database$PTUICC , list(c("pT0","pTis"),c("pT1a","pT1b","pT1c","pT1Mic","pT1"),
                                                               c("pT2"),c("pT3"),c("pT4b","pT4","pT4a","pT4c")), c(0,1,2,3,4)),
              lvi     = factor(self@database$EMBV,levels = c("non","oui"), labels = c(0,1)),
              multifocal_histo = factor(self@database$MULTHIST,levels = c("non","oui"), labels = c(0,1)),
              multifocality_clin_histo = factor(multifocality_clin_histo)
            ))
          }
)

setMethod(f="mapping_tumor_char_neo",
          signature="MappingSein",
          definition=function(self)
          {
            variables_ini_neoadj	<- c("CTPMM","CTPMMRES","CTPGG","CTPGGRES","BIOPRETT","LIEUBIOP")
            variables_post_neoadj	<- c("PTUICC","YPTUICC","YPNUICC","PNUICC","CHEVALIE","SATALOFT","SATALOFG")

            breast_res_infiltr   <- rep(NA,nrow(self@database))
            breast_res_infiltr[self@database$YPTUICC %in% c("pT0","pTis")]                                                  <- 0
            breast_res_infiltr[self@database$SATALOFT %in% c("TA effet therapeutique total ou presque")]                    <- 0
            breast_res_infiltr[self@database$CHEVALIE %in% c("G1 aucun reliquat sein et ggl",
                                                     "G2 in situ strict sein - pas de reliquat ggl")]                       <- 0
            breast_res_infiltr[self@database$YPTUICC %in% c(c("pT1","pT1a","pT1b","pT1c","pT1Mic","pT2","pT3","pT4","pT4b","pT4d"))]<- 1
            breast_res_infiltr[self@database$SATALOFT %in% c("TB effet therapeutique de plus de 50 %",
                                                    "TC effet therapeutique moins de 50 %","TD pas effet therapeutique")]  <- 1

            #Last check neoadj
            trace_post_NAC_stage <- ifelse(!is.na(self@database$YPTUICC) |!is.na(self@database$YPNUICC) |
                                                                !is.na(self@database$CHEVALIE)|!is.na(self@database$SATALOFT)|
                                                                !is.na(self@database$SATALOFG),1,0)
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
            neoadj_check[which(self@database$breast_surgery == 0)]     <- 9

            nbggpos_postneo_ct   <- rep(NA,nrow(self@database))
            nbggpos_postneo_ct[which(neoadj_check==1)]  <- as.numeric(self@database[which(neoadj_check==1),"nbggpos"])
            
            pCR                                                         <- rep(NA,nrow(self@database))
            pCR[which(breast_res_infiltr == 0 & self@database$nuicc_4cl==0)]  <- 1 #A revoir
            pCR[which(breast_res_infiltr == 1 | self@database$nuicc_4cl %in% c("1","2","3"))]  <- 0 #A revoir
            pCR[which(neoadj_check == 1 & is.na(pCR) &
                        self@database$CHEVALIE %in% c("G3 carcinome inf modifie sein et/ou ggl",
                                          "G4 carcinome inf non ou peu modifie sein et/ou ggl") )] <- 0
            return(list(
              breast_res_infiltr = factor(breast_res_infiltr),
              nbggpos_postneo = nbggpos_postneo_ct,
              pCR = factor(pCR)
            ))
          }
)

setMethod(f="mapping_events_and_censor",
          signature="MappingSein",
          definition=function(self)
          
            
          {
            
            #Adding relapses preprocessed by Victoire and Mathilde
            new_relapses <- read_xlsx(paste(Sys.getenv("PROJECT_PATH"),'core/15_base_sein_appasur/data/relapses_mathilde_victoire.xlsx',sep="/"))
            
            new_relapses_wide <- new_relapses %>% pivot_wider(id_cols = c("NUMDOS"),
                                                              names_from = "RELAPSE_TYPE",
                                                               values_from = "DATE") %>%
                                                  mutate(ETATDN = ifelse(is.na(DEATH),0,1))

            #DATRGG.f1
            DATRGG.f1_updated <- self@database$DATRGG.f1
            DATMET.f1_updated <- self@database$DATMET.f1
            DATDK.f1_updated  <- self@database$DATDK.f1
            ETATDN_updated    <- factor(self@database$ETATDN, levels = c("vivant", "décédé"), labels=c(0,1))
            DATDN_updated     <- self@database$DATDN
            
            #TODO : meta 
            #Check 2 patients
            # meta_new_relapses <- new_relapses %>% filter(RELAPSE_TYPE=="META") %>% dplyr::select(NUMDOS)
            # meta_new_relapses <- unique(meta_new_relapses$NUMDOS)
            # base_sein_meta <- self@database[which(!is.na(self@database$DATMET.f1)),"NUMDOS"]$NUMDOS
            # intersect(meta_new_relapses,base_sein_meta)
            #906622 : meta relapses base sein in 2017, meta relapses internes in 2010.
            #1102734  base sein : meta relapse 07/2017 ; internes : meta 2014, death 2017.
            
            # TODO : loco 
            # No patient to check 
            # loco_new_relapses <- new_relapses %>% filter(RELAPSE_TYPE=="LOCO") %>% dplyr::select(NUMDOS)
            # loco_new_relapses <- unique(loco_new_relapses$NUMDOS)
            # base_sein_loco <- self@database[which(!is.na(self@database$DATRGG.f1)),"NUMDOS"]$NUMDOS
            # intersect(loco_new_relapses,base_sein_loco)
            
            # TODO : deuxk
            # No patient to check
            # secondk_new_relapses <- new_relapses %>% filter(RELAPSE_TYPE=="DEUXK") %>% dplyr::select(NUMDOS)
            # secondk_new_relapses <- unique(secondk_new_relapses$NUMDOS)
            # base_sein_secondk <- self@database[which(!is.na(self@database$DATDK.f1)),"NUMDOS"]$NUMDOS
            # intersect(loco_new_relapses,base_sein_secondk)
            
            # TODO : status vital
            # 1 patient to check 
            # 1102734 (same than one meta)
            # etatdn_new_relapses <- new_relapses %>% filter(RELAPSE_TYPE=="DEATH") %>% dplyr::select(NUMDOS)
            # etatdn_new_relapses <- unique(etatdn_new_relapses$NUMDOS)
            # base_sein_etatdn <- self@database[which(self@database$ETATDN=="décédé"),"NUMDOS"]$NUMDOS
            # intersect(etatdn_new_relapses,base_sein_etatdn)
            
            for (to_update in 1:nrow(new_relapses_wide)){
              numdos_to_update = as.character(new_relapses_wide[to_update,"NUMDOS"])
              location = which(self@database$NUMDOS == numdos_to_update)
              DATRGG.f1_updated[location] <- pmin(self@database$DATRGG.f1[location],new_relapses_wide$LOCO[to_update],na.rm=T)
              DATMET.f1_updated[location] <- pmin(self@database$DATMET.f1[location],new_relapses_wide$META[to_update],na.rm=T)
              DATDK.f1_updated[location]  <- pmin(self@database$DATDK.f1[location],new_relapses_wide$DEUXK[to_update],na.rm=T)
              ETATDN_updated[location]    <- pmax(as.integer(as.character(ETATDN_updated[location])),new_relapses_wide$ETATDN[to_update],na.rm=T)
              DATDN_updated[location]     <- pmax(DATDN_updated[location],DATRGG.f1_updated[location],
                                                  DATMET.f1_updated[location],DATDK.f1_updated[location],
                                                  new_relapses_wide$DEATH[to_update],na.rm=T)
              }

             ev_prog_neo    <- rep(0,nrow(self@database))
             dat_prog_neo   <- rep(as.Date(NA),nrow(self@database)) #A revoir ASHP
             ev_recloc      <- ifelse(!is.na(self@database$DATRLOC.f1),1,0)
             dat_recloc     <- as.Date(self@database$DATRLOC.f1)
             ev_recreg      <- ifelse(!is.na(DATRGG.f1_updated),1,0)
             dat_recreg     <- as.Date(DATRGG.f1_updated)
             ev_meta        <- ifelse(!is.na(DATMET.f1_updated),1,0)
             dat_meta       <- as.Date(DATMET.f1_updated)
             ev_contro      <- rep(0,nrow(self@database))
             dat_contro     <- rep(as.Date(NA),nrow(self@database)) #A revoir ASHP
             ev_deuxk       <- ifelse(!is.na(DATDK.f1_updated),1,0)
             dat_deuxk      <- as.Date(DATDK.f1_updated)
             status_vital   <- factor(ETATDN_updated)
             dat_last_news  <- as.Date(DATDN_updated)
             ev_prog_neo[which(as.Date(dat_prog_neo) > self@dat_censor)] <- 0
             ev_recloc[which(dat_recloc > self@dat_censor)]     <- 0
             ev_recreg[which(dat_recreg > self@dat_censor)]     <- 0
             ev_meta[which(dat_meta > self@dat_censor)]         <- 0
             ev_contro[which(dat_contro > self@dat_censor)]     <- 0
             ev_deuxk[which(dat_deuxk > self@dat_censor)]       <- 0
             status_vital[which(as.Date(dat_last_news) > self@dat_censor & status_vital==1 )]       <- 0

             return(list(
                dat_censor = rep(self@dat_censor,nrow(self@database)),
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
                ev_deuxk = factor(ev_deuxk),
                dat_deuxk = as.Date(dat_deuxk),
                status_vital = status_vital,
                dat_last_news = as.Date(dat_last_news)
            ))
          }
)
