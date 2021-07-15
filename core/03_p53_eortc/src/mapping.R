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

Mappingp53_eortc <- setClass(
  "Mappingp53_eortc",
  
  # Define the slots
  
  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "p53",
    database = read.csv("/Users/nadir/Desktop/projects/postdoc/DATABASES/databases/core/03_p53_eortc/data/data_pt.csv") ###A revoir####
  ),
  
  contains = "Mapping"
)

setMethod(f="mapping_patient_id",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            database = 3
            
            return(list(
              database = rep(3, nrow(self@database)),
              # numdos_curie = self@database$PATID,
              cletri = self@database$PATID,
              # side  = factor(self@database$cote, levels = c("G","D"), labels = c(1,2)),
              # dat_birth = as.Date(ifelse(is.na(self@database$DATDIAG), self@database$datechir, self@database$DATDIAG), origin = "1970-01-01"),
              # dat_bc_diagnosis = self@database$dat_bc_diagnosis,
              # center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3)),
              # center = ifelse(self@database$center_curie == 3,2,1),
              base_cletri = paste0(database, "_",self@database$PATID)
            ))
          }
)

setMethod(f="mapping_patient_char",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            menop              = self@database$mstat
            menop[which(menop==3)] = 1 # assign age < 50 to pre-menopausal
            menop[which(menop==4)] = 2 # and age >= 50 to post-menopausal
            menop = na_factor(menop,".",levels=c(1,2),labels = c(0,1)) 
            return(list(
              age              = self@database$age,
              #age_menarche       = as.numeric(self@database$AGEPR), #vérifier avec Anne-Sophie
              #prev_pregnancy     = na_factor(self@database$GROS, c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              menop              = menop,
              #age_menop          = as.numeric(self@database$AGMENOP), 
              #hrt                = na_factor(self@database$THS,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              #fam_history        = na_factor(self@database$ANTFAM,c("non precise"),levels=c("non","oui"),labels = c(0,1)),
              #brca_screen        = factor(self@database$RECMUT, levels = c("non","oui"), labels = c(0,1)),
              #brca_mut           = factor(self@database$RESMUT, levels = c("non","oui"), labels = c(0,1)),
              weight             = as.numeric(self@database$baseweight),
              size               = as.numeric(self@database$acthei),
              bmi                = as.numeric(self@database$bmi)
            ))
          }
)

setMethod(f="mapping_bc_diagnosis",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            return(list(
              #inflammatory_BC    = ifelse(self@database$TUICC  %in% c("T4c","T4d"), 1,0),
              #moddiag            = factor(self@database$MODDIAG, levels = c("radiologique","clinique"),labels=c(0,1)),
              # clin_multifocality  = factor(self@database$surg13, levels = c("1","2"),labels=c(0,1)),
              #tclin              = as.numeric(self@database$TCLIN),
              ctuicc_5cl           = na_factor(self@database$tumstat, na_string = "8", levels = c(0,1,2,3,4), labels = c(0,1,2,3,4)), 
              cnuicc_4cl           = na_factor(self@database$nodstat, na_string = "8", levels = c(0,1,2,3), labels = c(0,1,2,3)), 
              muicc               = na_factor(self@database$metstat, na_string = "8", levels = c(0,1,2,3), labels = c(0,1,2,3))
            ))
          }
)

setMethod(f="mapping_bc_biology",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            return(list(
              er_status           = na_factor(self@database$erstat, na_string = "9", levels = c("1","2"), labels=c(0,1)),
              pr_status           = na_factor(self@database$pgrstat, na_string = "9", levels = c("1","2"), labels=c(0,1)),
              #er_intensity       = factor(self@database$ROINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
              #pr_intensity       = factor(self@database$RPINT, levels = c("faible", "modere","fort"), labels=c(1,2,3)),
              #er_percentage      = as.integer(self@database$ROPCT),
              #pr_percentage      = as.integer(self@database$RPPCT),
              her2_status         = na_factor(self@database$herres_bin, na_string = "9", levels = c("1","2"), labels=c(0,1)),
              histo_3cl           = factor(self@database$surg19, levels = c("1","2","8"), labels=c(1,2,9)),
              grade_3cl           = na_factor(self@database$htgrad, na_string = "0", levels = c("1","2","3"), labels=c(1,2,3)),
              p53                 = na_factor(self@database$p53, na_string = "0", levels = c("1","2"), labels=c(1,2))
              #ki67_perc          = as.integer(self@database$KI67PCT),
              #mitotic_index      = as.integer(self@database$NBMIT),
              #mitotic_index_class= na_factor(self@database$IM,c("non precise"),levels = c("faible", "moyen","fort"), labels=c(1,2,3)),
              #invasive_or_dcis    = factor(self@database$INFILT, levels = c("infiltrant", "CCIS","CCIS + micro infiltrant"), labels=c(1,2,1)) ,
              #dcis_component      = factor(self@database$INSITU, levels = c("non","oui"), labels=c(0,1))
            ))
          }
)

setMethod(f="mapping_surgery",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            breast_surgery_3cl  = self@database$surg4
            breast_surgery_3cl[which(self@database$surg3==0)] = 9
            
            return(list(
              # dat_first_surg      = self@database$DATCHIR.f1, 
              breast_surgery        = factor(self@database$surg3, levels = c(0,1), labels = c(0,1)),
              # TYPCHIR             = paste(self@database$TYPCHIR.f1,self@database$TYPCHIR.f2, self@database$TYPCHIR.f3), ###Normal pour les NAs??? 
              breast_surgery_3cl    = breast_surgery_3cl,
              axillary_surgery_3cl  = factor(self@database$surg27, levels = c(0,1,2), labels = c(4,2,1))

            ))
          }
)

setMethod(f="mapping_treatments_binary",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            ht_type_3cl      =  factor(self@database$nyarin, levels = c(0,1), labels = c(0,2))
            ht_type_3cl[which(ht_type_3cl!=2)] = NA
            
            return(list(
              ct               =  factor(ifelse(!is.na(self@database$trt_arm),1,0)),
              # dat_first_ct     =  self@database$DATDCT.f1,
              # dat_end_first_ct =  self@database$DATFCT.f1,
              # rt               =  ifelse(!is.na(self@database$DATDRT),1,0),
              # dat_first_rt     =  self@database$DATDRT,
              ht               =  factor(self@database$endotrt, levels = c(0,1,8), labels = c(0,1,NA)), # tamoxifen?
              ht_type_3cl = ht_type_3cl,
              # dat_first_ht     =  self@database$DATDHT.q1,
              # TYPHT            =  factor(TYPHT),
              antiher2         =  na_factor(self@database$hertrt , na_string = "8", levels = c(0,1), labels = c(0,1))
              # dat_first_antiher2 = dat_first_antiher2,
              #tc_other         =  ifelse(!self@database$TYPTCIBL.q1.f1 %in% c("HERCEPTIN","PERJETA","TYVERB"),1,0),
              #dat_first_tc_other = dat_first_tc_other
            ))
          }
)

setMethod(f="mapping_neoadj_or_not",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            neo_rt      =  self@database$rt_surg_inelig
            neo_rt[which(is.na(neo_rt))] = 0
            return(list(
              neo_rt = neo_rt,
              neo_antiher2 = rep(0, nrow(self@database))
            ))
          }
)


setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            return(list(
              neo_ct_regimen = factor(self@database$trt_arm, levels = c("1","2"),labels=c(2,3)),
              nb_cycles_neo_ct = self@database$totrt
            ))
          }
)

setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            return(list(
              adj_antiher2 = na_factor(self@database$hertrt , na_string = "8", levels = c(0,1), labels = c(0,1))
            ))
          }
)


setMethod(f="mapping_tumor_char_surg",
          signature="Mappingp53_eortc",
          definition=function(self)
          {
            return(list(
              nbggpos = self@database$surg30 + self@database$surg31,
              multifocality_histo  = factor(self@database$surg13, levels = c("1","2"),labels=c(0,1))
              # histo_size <- as.integer(d1$TINF) ,
              # ptuicc_5cl <- regroup(self@database$PTUICC , list(c("T0","Tis"),c("T1"),c("T2"),c("T3"),c("T4a","T4b","T4c","T4d")), c(0,1,2,3,4)),
              # embols     <- factor(self@database$EMBV,levels = c("non","oui"), labels = c(0,1)),
              # multifocality_clin_histo <- factor(multifocality_clin_histo)
            ))
          }
)

setMethod(f="mapping_tumor_char_neo",
          signature="Mappingp53_eortc",
          definition=function(self)
          {

            return(list(
              # breast_res_insitu = regroup(self@database$surg7 , list(c(0),c(1,3),c(2)), c(0,1,0)),
              # breast_res_infiltr = regroup(self@database$surg7 , list(c(0),c(2,3),c(1)), c(0,1,0)),
              pcr = regroup(self@database$pCR4 , list(c(0),c(1), c(9)), c(0,1,NA))
            ))
          }
)


setMethod(f="mapping_events_and_censor",
          signature="Mappingp53_eortc",
          definition=function(self)
          {

            cause_death = regroup(self@database$causdeat , list(c(0,9),c(1),c(2,3,4,5,6,7,8)), c(NA,1,2))
            ev_prog_neo = self@database$prog_trt_inelig
            ev_prog_neo[which(is.na(ev_prog_neo))] = 0

            return(list(
              cause_death = cause_death,
              status_vital = factor(self@database$ss, levels = c("1","2"),labels=c(0,1)),
              ev_prog_neo = ev_prog_neo
            ))
          }
)

setMethod(f="mapping_evol",
          signature="Mappingp53_eortc",
          definition=function(self)
          {

            return(list(

              status_drfs = factor(self@database$dmfs_pcr, levels = c("1","2"),labels=c(0,1)),
              status_drfs_diag = factor(self@database$dmfs, levels = c("1","2"),labels=c(0,1)),
              status_efs_diag = self@database$pfs,
              delay_efs_diag = (self@database$dpfs + (self@database$diagc * 7)) / 30.4375,
              delay_drfs_diag = (self@database$ddmfs + (self@database$diagc * 7)) / 30.4375,
              delay_drfs = self@database$ddmfs_pcr / 30.4375,
              delay_os_diag = (self@database$dsur + (self@database$diagc * 7)) / 30.4375,
              delay_os = self@database$dsur_pcr / 30.4375

            ))
          }
)