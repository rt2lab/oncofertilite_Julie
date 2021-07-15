#Classs

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

MappingpSein_tabac <- setClass(
  "MappingpSein_tabac",
  
  # Define the slots
  
  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "sein_tabac",
      database = read.csv("/Users/nadir/Desktop/projects/postdoc/DATABASES/databases/core/17_sein_tabac/data/Tabac-Complete_DATA_2020-06-22_0917.csv", sep=';') ###A revoir####
  ),
  
  contains = "Mapping"
)

setMethod(f="mapping_patient_id",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            database = 3
            source('/Users/nadir/Desktop/projects/postdoc/DATABASES/databases/core/17_sein_tabac/src/Tabac-Complete_R_2020-06-22_0917.r')
            return(list(
              database = 3,
              # numdos_curie = self@database$PATID,
              # cletri = self@database$,
              # side  = factor(self@database$cote, levels = c("G","D"), labels = c(1,2)),
              # dat_birth = as.Date(ifelse(is.na(self@database$DATDIAG), self@database$datechir, self@database$DATDIAG), origin = "1970-01-01"),
              # dat_bc_diagnosis = self@database$dat_bc_diagnosis,
              # center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3)),
              # center = ifelse(self@database$center_curie == 3,2,1),
              # base_cletri = paste0(database, "_",self@database$PATID)
            ))
          }
)

setMethod(f="mapping_patient_char",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            drinking_alcohol    = self@database$alcool
            drinking_alcohol[which(drinking_alcohol > 0)] = 1
            
            return(list(
              # age              = self@database$age,
              #age_menarche       = as.numeric(self@database$AGEPR), #vérifier avec Anne-Sophie
              #prev_pregnancy     = na_factor(self@database$GROS, c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              # menop              = menop,
              #age_menop          = as.numeric(self@database$AGMENOP), 
              #hrt                = na_factor(self@database$THS,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              #fam_history        = na_factor(self@database$ANTFAM,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              #brca_screen        = factor(self@database$RECMUT, levels = c("non","oui"), labels = c(0,1)),
              #brca_mut           = factor(self@database$RESMUT, levels = c("non","oui"), labels = c(0,1)),
              weight              = as.numeric(self@database$poids),
              size                = as.numeric(self@database$taille),
              smoking             = factor(self@database$tabac_life),
              drinking_alcohol    = factor(drinking_alcohol)
              # bmi                = as.numeric(self@database$bmi)
            ))
          }
)

setMethod(f="mapping_bc_diagnosis",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            return(list(
              #inflammatory_BC    = ifelse(self@database$TUICC  %in% c("T4c","T4d"), 1,0),
              #moddiag            = factor(self@database$MODDIAG, levels = c("radiologique","clinique"),labels=c(0,1)),
              clin_multifocality  = factor(self@database$bifocal),
              tclin              =  as.numeric(self@database$taille_infiltrant)
              # tuicc_5cl           = na_factor(self@database$tumstat, na_string = "8", levels = c(0,1,2,3,4), labels = c(0,1,2,3,4)), 
              # nuicc_4cl           = na_factor(self@database$nodstat, na_string = "8", levels = c(0,1,2,3), labels = c(0,1,2,3)), 
              # muicc               = factor(self@database$metstat, levels = c("0","1"), labels = c(0,1))
            ))
          }
)

setMethod(f="mapping_bc_biology",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            er_status = rep(NA, nrow(self@database))
            er_status[which(self@database$rec_est___0==1)] = 1
            er_status[which(self@database$rec_est___1==1)] = 0
            
            pr_status = rep(NA, nrow(self@database))
            pr_status[which(self@database$recep_prog___0==1)] = 1
            pr_status[which(self@database$recep_prog___1==1)] = 0
            
            her2_status = rep(NA, nrow(self@database))
            her2_status[which(self@database$stat_her2___0==1)] = 1
            her2_status[which(self@database$stat_her2___1==1)] = 0
            
            grade_3cl = rep(NA, nrow(self@database))
            grade_3cl[which(self@database$grade___0==1)] = 1
            grade_3cl[which(self@database$grade___1==1)] = 2
            grade_3cl[which(self@database$grade___2==1)] = 3
            
            histo_5cl = rep(NA, nrow(self@database))
            histo_5cl[which(self@database$type_histo___0) == 1] = 1
            histo_5cl[which(self@database$type_histo___2) == 1] = 1
            histo_5cl[which(self@database$type_histo___1) == 1] = 2
            histo_5cl[which(self@database$type_histo___3) == 1] = 2
            histo_5cl[which(self@database$type_histo___4) == 1] = 3
            
            return(list(
              er_status           = factor(er_status),
              pr_status           = factor(pr_status),
              #er_intensity       = factor(self@database$ROINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
              #pr_intensity       = factor(self@database$RPINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
              #er_percentage      = as.integer(self@database$ROPCT),
              #pr_percentage      = as.integer(self@database$RPPCT),
              her2_status         = factor(her2_status),
              histo_5cl           = factor(histo_5cl)),
              grade_3cl           = factor(grade_3cl),
              # p53                 = na_factor(self@database$p53, na_string = "0", levels = c("1","2"), labels=c(1,2))
              #ki67_perc          = as.integer(self@database$KI67PCT),
              #mitotic_index      = as.integer(self@database$NBMIT),
              #mitotic_index_class= na_factor(self@database$IM,c("non precise"),levels = c("faible", "moyen","fort"), labels=c(1,2,3)),
              #invasive_or_dcis    = factor(self@database$INFILT, levels = c("infiltrant", "CCIS","CCIS + micro infiltrant"), labels=c(1,2,1)) ,
              #dcis_component      = factor(self@database$INSITU, levels = c("non","oui"), labels=c(0,1))
            ))
          }
)

setMethod(f="mapping_surgery",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            # breast_surgery_3cl  = self@database$surg4
            # breast_surgery_3cl[which(self@database$surg3==0)] = 9
            
            return(list(
              # dat_first_surg      = self@database$DATCHIR.f1, 
              breast_surgery      = ffactor(self@database$ttt_kr___0),
              # TYPCHIR             = paste(self@database$TYPCHIR.f1,self@database$TYPCHIR.f2, self@database$TYPCHIR.f3), ###Normal pour les NAs??? 
              # breast_surgery_3cl  = factor(breast_surgery_3cl, levels = c(0,1,2), labels = c(0,1,2))
              # axillary_surgery_4cl= factor(axillary_surgery_4cl)  # AXILLARY IS TO DO
            ))
          }
)

setMethod(f="mapping_treatments_binary",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            
            dat_first_ht     =  unlist(lapply(self@database$date_hormono, function(x){if(as.character(x)=='') NA else{ paste0(substr(x,1,unlist(gregexpr('/',x))[[2]]),'20',
                                                                                                                             substr(x, (nchar(as.character(x))-1), nchar(as.character(x))) )} })) 
            
            dat_first_ct     =  unlist(lapply(self@database$date_chimio, function(x){if(as.character(x)=='') NA else{ paste0(substr(x,1,unlist(gregexpr('/',x))[[2]]),'20',
                                                                                                                              substr(x, (nchar(as.character(x))-1), nchar(as.character(x))) )} })) 
            
                                             
            
            return(list(
              ct               =  factor(self@database$ttt_kr___1),
              dat_first_ct     =  as.Date(dat_first_ct,format='%d/%m/%Y'),
              # dat_end_first_ct =  self@database$DATFCT.f1,
              rt               =  factor(self@database$ttt_kr___2),
              # dat_first_rt     =  self@database$DATDRT,
              ht               =  factor(self@database$ttt_kr___4),
              # ht_type_3cl      =  factor(ifelse((self@database$nyarin==1),2,NA)), # tamoxifen?
              dat_first_ht     =  as.Date(dat_first_ht,format='%d/%m/%Y'),
              # TYPHT            =  factor(TYPHT),
              antiher2         =  factor(self@database$ttt_kr___3)
              # dat_first_antiher2 = dat_first_antiher2,
              #tc_other         =  ifelse(!self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB"),1,0),
              #dat_first_tc_other = dat_first_tc_other
            ))
          }
)

setMethod(f="mapping_neoadj_or_not",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            neo_ct      = rep(0, nrow(self@database))     
            neo_ct[which(self@database$seq_chimio==1)] = 1
            # neo_ht      = factor(self@database$ttt_kr___5)
            # neo_rt      =  self@database$rt_surg_inelig
            # neo_rt[which(is.na(neo_rt))] = 0
            return(list(
              neo_ct = neo_ct,
              neo_ht = factor(self@database$hormono_neo)
            ))
          }
)

setMethod(f="mapping_neoadj_or_not",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            return(list(
              # neo_ct = factor(neo_ct),
              # neo_ht = factor(neo_ht),
              # neo_rt = factor(ifelse(!is.na(self@database$rt_surg_inelig),1,0))
              # neo_antiher2 = factor(neo_antiher2),
              # neo_tc_other = factor(neo_tc_other),
              # primary_ttt = factor(primary_ttt),
              # dat_first_neo_ct = dat_first_neo_ct,
              # dat_first_neo_ht = dat_first_neo_ht,
              # dat_first_neo_rt = dat_first_neo_rt,
              # dat_first_neo_antiher2 = dat_first_neo_antiher2,
              # dat_first_neo_tc_other = dat_first_neo_tc_other
            ))
          }
)

setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            return(list(
              # neo_ct_regimen = factor(self@database$trt_arm, levels = c("1","2"),labels=c(2,3)),
              # nb_cycles_neo_ct = self@database$totrt
            ))
          }
)

setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            return(list(
            ))
          }
)

setMethod(f="mapping_tumor_char_surg",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            return(list(
              nbggps = self@database$nb_gg,
              # multifocal_histo  = factor(self@database$surg13, levels = c("1","2"),labels=c(0,1))
              # histo_size <- as.integer(d1$TINF) ,
              # ptuicc_5cl <- regroup(self@database$PTUICC , list(c("T0","Tis"),c("T1"),c("T2"),c("T3"),c("T4a","T4b","T4c","T4d")), c(0,1,2,3,4)),
              # embols     <- factor(self@database$EMBV,levels = c("non","oui"), labels = c(0,1)),
              # multifocality_clin_histo <- factor(multifocality_clin_histo)
            ))
          }
)

setMethod(f="mapping_tumor_char_neo",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            
            return(list(
              # breast_res_insitu = na_factor(self@database$pCR2 , na_string = "9", levels = c(0,1), labels = c(0,1)),
              # breast_res_infiltr = na_factor(self@database$pCR4 , na_string = "9", levels = c(0,1), labels = c(0,1)),
              # breast_res_infiltr <- factor(breast_res_infiltr),
              # nbggpos_postneo_ct <- factor(nbggpos_postneo_ct),
              # pCR <- factor(self@database$pathcr)
            ))
          }
)

setMethod(f="mapping_delays_pathways",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            
            return(list(
              # delay_diag_to_surg = self@database$dsurg1 # this is from randomization and not from diagnosis
            ))
          }
)

setMethod(f="mapping_events_and_censor",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            # status_vital = rep(NA, nrow(self@database))
            # status_vital[which(self@database$causdeat == 1)] = 0
            
            # ev_prog_neo = self@database$prog_trt_inelig	
            # ev_prog_neo[which(is.na(ev_prog_neo))] = 0
            
            return(list(
              # ev_meta = factor(self@database$dmfs, levels = c("1","2"),labels=c(0,1)),
              ev_recloc = factor(self@database$recidiv_cancer)
              # ev_secondk = factor(self@database$new_primary, levels = c("0","1"),labels=c(0,1)),
              # status_vital = factor(self@database$ss, levels = c("1","2"),labels=c(0,1)),
              # ev_prog_neo = ev_prog_neo
            ))
          }
)

setMethod(f="mapping_evol",
          signature="MappingpSein_tabac",
          definition=function(self)
          {
            
            return(list(
              
              # status_drfs = factor(self@database$dmfs_pcr, levels = c("1","2"),labels=c(0,1)), 
              # status_drfs_diag = factor(self@database$dmfs, levels = c("1","2"),labels=c(0,1)), 
              # status_efs_diag = self@database$pfs,
              # delay_efs_diag = self@database$dpfs,
              # delay_drfs_diag = self@database$ddmfs / 30.4375,
              # delay_drfs = self@database$ddmfs_pcr / 30.4375,
              # delay_os_diag = self@database$dsur / 30.4375,
              # delay_os = self@database$dsur_pcr / 30.4375,
              
            ))
          }
)