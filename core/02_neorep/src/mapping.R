# rm(list = ls())

library(dplyr)

source('~/RT2Lab/databases/core/02_neorep/src/rajout_bases_satellites.R', local = TRUE)
source('~/RT2Lab/databases/core/02_neorep/src/1_merge_all_database_NEOREP_for_RT2clean.R', local = TRUE)

# See preprocessing in merge_all_database_NEOREP_for_RT2clean.R
load(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "data","neorep_before_mapping.Rdata"))
#head(neorep_before_mapping)
d1 <- neorep_before_mapping

####### TESTS #######
# dim(self@database)
####### END TESTS #######

#Class
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

MappingNeorep <- setClass(
  "MappingNeorep",
  # Define the slots
  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "neorep",
    database = d1) ,
  contains = "Mapping"
)

setMethod(f="mapping_initial",
          signature="MappingNeorep",
          definition=function(self)
          {
            #####
            #  correct numdos and remove doublons, correct wrong numdos
            # Ne connait pas neorep
            # self a 2 arg : name_database, database
            # l'objet qu'il prend c'est self@database
            
            # database <- neorep
            # self@database$numdos7		<- formatC(self@database$numdos,width = 7, flag = "0")
            # self@database$numdos7		<- self@database$numdos7
            self@database            <- self@database[self@database$donon==1 & !is.na(self@database$donon),]; nrow(self@database) # 1199
            # Introduce a cletri (we keep the old way we generated the cletri, because we previously generated 
            # this way for all collaborative projects such as RCB, TILs, Desmedt and co.
            self@database$cletri <- paste0("Pt","_",as.character(c(1:nrow(self@database))))
            # Correct wrong numdos from Curie
            #source('~/RT2Lab/databases/core/02_neorep/src/1_Corrections_wrong_numdos_NEOREP.R')
            # source(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "src/1_Corrections_wrong_numdos_NEOREP.R"))
            # Correct baseline errors in the raw datas
            # self <- correction_wrong_numdos_neorep(self)
            
            #source('~/RT2Lab/databases/core/02_neorep/src/2_Corrections_added_in_NEOREP_root.r')
            # source(file.path(Sys.getenv("PROJECT_PATH"), "core", opt$db_name, "src/2_Corrections_added_in_NEOREP_root.r"))
            # 
            # self <- correction_root_neorep(self)
            # cf variable margins ; remains to correct ...
             return(self)
           }
 )

setMethod(f="mapping_patient_id",
          signature="MappingNeorep",
          definition=function(self)
          {
            # self@database$numdos7 <- formatC(self@database$numdos,width = 7, flag = "0")
            # self@database$numdos7 <- self@database$numdos7
            side_tmp             = factor(self@database$cote,     levels = c(1,2), labels = c("droit","gauche")) # BEWARE  !!
            
            return(list(
              database         = rep(1,nrow(self@database)),
              numdos_curie     = self@database$numdos7,
              side             = factor(side_tmp, levels = c("droit","gauche"), labels = c(2,1)), # levels side to invert !!
              dat_birth        = self@database$datnai,
              dat_bc_diagnosis = as.Date(ifelse(is.na(self@database$datdeb), self@database$datexam,
                                                self@database$datdeb), origin = "1970-01-01"), # date biopsy, if NA, 1st exam
              center_curie     = factor(self@database$centre),
              center           = rep(1,nrow(self@database)) 
              # base_cletri      = paste0(self@database$database, "_",self@database$cletri)
            ))
          }
)

setMethod(f="mapping_patient_char",
          signature="MappingNeorep",
          definition=function(self)
          {
            return(list(
              menop              = factor(self@database$menop),
              fam_history        = factor(self@database$antfam),
              brca_screen        = factor(self@database$brca_screen),
              brca_mut           = factor(self@database$brca_mut),
              brca_1_2_mut       = factor(self@database$brca_1_2_mut),
              weight             = as.numeric(self@database$poids),
              size               = as.numeric(self@database$taille)/100,
              smoking_3cl        = factor(self@database$smoking_3cl),
              smoking            = factor(self@database$smoking)
            ))
          }
)

setMethod(f="mapping_comedication",
          signature="MappingNeorep",
          definition=function(self)
          {
            return(list(
              comedic                     = factor(self@database$comedic                    ), 
              comedic_n_nervous_system    = factor(self@database$comedic_n_nervous_system   ), 
              comedic_c_cardiovascular    = factor(self@database$comedic_c_cardiovascular   ), 
              comedic_a_alimentary_metabo = factor(self@database$comedic_a_alimentary_metabo), 
              comedic_h_hormonal_prep     = factor(self@database$comedic_h_hormonal_prep    ), 
              comedic_others              = factor(self@database$comedic_others             )
            ))
          }
)

setMethod(f="mapping_comorbidity",
          signature="MappingNeorep",
          definition=function(self)
          {
            return(list(
              comorbidity                      = factor(self@database$comorbidity_bin            ), 
              comor_hypertension_heart_disease = factor(self@database$comor_hypertension_heart_disease),
              comor_depression_anxiety         = factor(self@database$comor_depression_anxiety        ),
              comor_dyslipidemia               = factor(self@database$comor_dyslipidemia              ),
              comor_diabete                    = factor(self@database$comor_diabete                   ),
              comor_ulcere_gastritis           = factor(self@database$comor_ulcere_gastritis          ),
              comor_thyroid_disorders          = factor(self@database$comor_thyroid_disorders         ),
              comor_others_grouped             = factor(self@database$comor_others_grouped            )
            ))
          }
)

setMethod(f="mapping_bc_diagnosis",
          signature="MappingNeorep",
          definition=function(self)
          {
            ctuicc_5cl          = ifelse(self@database$tuicc == 4,NA, self@database$tuicc)

            return(list(
              bilat_bc           = rep(0,nrow(self@database)), # Exclusion criteria of the database NEOREP
              inflammatory_BC    = rep(0,nrow(self@database)), # Exclusion criteria of the database NEOREP
              multifocality_clin = rep(0,nrow(self@database)), # Exclusion criteria of the database NEOREP
              tclin              = as.numeric(self@database$tclin),
              ctuicc_5cl          = ctuicc_5cl,
              cnuicc_4cl          = factor(self@database$nuicc),
              muicc              = rep(0,nrow(self@database)) ,              # Exclusion criteria of the database NEOREP
              dat_first_biopsy   = self@database$datdeb
            ))
          }
)


setMethod(f="mapping_bc_biology",
          signature="MappingNeorep",
          definition=function(self)
          {
            lvi_biop                                  <- NA
            lvi_biop[self@database$lvi_biop == "yes"] <- 1
            lvi_biop[self@database$lvi_biop == "no"]  <- 0
            
            histo_3cl = ifelse(self@database$typana %in% c(1,2), self@database$typana, 3)
            grade_3cl = ifelse(self@database$ee %in% c(1,2,3), self@database$ee, NA)
            # dcis_component = ifelse(self@database$dcis_component == "no",0,1)

            return(list(
              er_status          = factor(self@database$roec),
              pr_status          = factor(self@database$proc),
              er_percentage      = as.integer(self@database$roep),
              pr_percentage      = as.integer(self@database$prop),
              er_allred          = as.integer(self@database$allreds),
              pr_allred          = as.integer(self@database$allredsp),
              her2_status        = factor(self@database$surex),
              # Beware, in NEOREP, the original variable has : 1- canalaire 2- lobulaire 3- medullaire 4- autre
              # So we can not map to histo_5cl 1,NST|2,Lobular|3,Mucinous|4,Tubulous|9,Others  	Histological type (5 classes)
              # Have to map to derived variable : 
              histo_3cl          = histo_3cl,
              grade_3cl          = grade_3cl,
              dcis_component     = as.integer(self@database$dcis_component),
              ki67_perc          = as.integer(self@database$kip),
              mitotic_index      = as.integer(self@database$indexm),
              # BEWARE, il  faudra corriger cette variable avec les variables mitotic index relues
              invasive_or_dcis   = rep(1,nrow(self@database))  , # All cancers in NEOREP are invasive
              str_til_perc       =  as.integer(self@database$str_til_perc), 
              it_til_perc        =  as.integer(self@database$it_til_perc),
              lvi_biop           =  lvi_biop,
              tumor_cellularity  = self@database$perc_cellules_tumorales  
              ))
          }
)

setMethod(f="mapping_surgery",
          signature="MappingNeorep",
          definition=function(self)
          {
            breast_surgery      <- rep(0,nrow(self@database))
            breast_surgery [ which(self@database$chir == 1)]  <- 1
            
            breast_surgery_3cl    = factor(ifelse(is.na(self@database$typchir),9,self@database$typchir))
            axillary_surgery_4cl  = factor(ifelse(is.na(self@database$typax),9,self@database$typax)) 

            return(list(
              breast_surgery        = factor(breast_surgery),
              breast_surgery_3cl    = breast_surgery_3cl,
              axillary_surgery_4cl  = axillary_surgery_4cl,
              dat_first_surg        = self@database$datchir
            ))
          }
)

setMethod(f="mapping_treatments_binary",
          signature="MappingNeorep",
          definition=function(self)
          {
            ht_type_5cl                                                                            <- rep(NA,nrow(self@database))
            ht_type_5cl[which(is.na(self@database$hormo))]                                         <- 9
            ht_type_5cl[which(self@database$typhor == 1)]                                          <- 1
            ht_type_5cl[which(self@database$typhor == 2)]                                          <- 5
            ht_type_5cl[which(self@database$typhor == 3)]                                          <- 3
            ht_type_5cl[which(self@database$typhor == 4)]                                          <- 2
            ht_type_5cl[which(self@database$typhor == 5)]                                          <- 4
            
            antiher2                                    <- rep(0,nrow(self@database))
            antiher2[which(self@database$hercep == 1)]  <- 1
            antiher2[which(self@database$heradj == 1)]  <- 1
            
            dat_first_antiher2                 <- pmin(self@database$hercdeb,self@database$debher , na.rm = TRUE)

            return(list(
              ct                 =  rep(1,nrow(self@database)), # Inclusion criteria neorep
              dat_first_ct       =  self@database$neodeb.q1,
              # @ASHP : HAVE TO PROCESS file with chemotherapy separately
              # dat_end_first_ct =  self@database$DATFCT.f1,
              rt                 =  ifelse(is.na(self@database$rth) | self@database$rth == 0 ,0,1),
              dat_first_rt       =  self@database$rtdeb,
              ht                 =  ifelse(is.na(self@database$hormo) | self@database$hormo == 0 ,0,1),
              dat_first_ht       =  self@database$dathor,
              ht_type_5cl        = factor(ht_type_5cl),
              antiher2           = factor(antiher2),
              dat_first_antiher2 = dat_first_antiher2
            ))
          }
)

setMethod(f="mapping_neoadj_or_not",
          signature="MappingNeorep",
          definition=function(self)
          {
            neo_rt       <- rep(0,nrow(self@database))
            neo_ht       <- rep(0,nrow(self@database))
            neo_antiher2 <- rep(0,nrow(self@database))
            
            neo_rt[which(self@database$dat_first_rt < self@database$dat_first_surg)]             <- 1
            neo_ht[which(self@database$dat_first_ht < self@database$dat_first_surg)]             <- 1 
            neo_antiher2     <- self@database$hercep
            
            dat_first_neo_rt         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_ht         <- rep(as.Date(NA),nrow(self@database))
            dat_first_neo_antiher2   <- rep(as.Date(NA),nrow(self@database))  
            
            dat_first_neo_rt[which(neo_rt      ==1)]              <- 
               self@database[which(neo_rt      ==1),"dat_first_rt"]
            dat_first_neo_ht[which(neo_ht      ==1)]              <- 
               self@database[which(neo_ht      ==1),"dat_first_ht"]
            dat_first_neo_antiher2[which(neo_antiher2==1)]  <-
                     self@database[which(neo_antiher2==1),"hercdeb"] # Variable raw, specific to neorep
            
            primary_ttt                                           <- rep(2,nrow(self@database)) 
            primary_ttt[which(self@database$breast_surgery == 0)] <- 9
            
            return(list(
              neo_ct                 = rep(1,nrow(self@database)),
              neo_ht                 = factor(neo_ht),
              neo_rt                 = factor(neo_rt),
              neo_antiher2           = self@database$hercep,
              primary_ttt            = factor(primary_ttt),
              dat_first_neo_ct       = self@database$dat_first_ct,
              dat_first_neo_ht       = dat_first_neo_ht,
              dat_first_neo_rt       = dat_first_neo_rt,
              dat_first_neo_antiher2 = self@database$hercdeb
            ))
          }
)

setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingNeorep",
          definition=function(self)
          {

            return(list(
              neo_ct_regimen           = self@database$neo_ct_regimen,
              nb_cycles_neo_ct         = self@database$nb_cycles_neo_ct,
              nb_cycles_neo_ct_anthra  = self@database$nb_cycles_neo_ct_anthra,
              nb_cycles_neo_ct_taxanes = self@database$nb_cycles_neo_ct_taxanes,
              dat_end_neo_ct           = self@database$neofin,
              neo_ct_sequence          = self@database$neo_ct_sequence
              ))
          }
)

setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingNeorep",
          definition=function(self)
          {
            adj_ct            = ifelse(is.na(self@database$chimadj) | self@database$chimadj == 0 ,0,1)
            # adj_ct_sequence = nb_sequences_adj  # See later, variable not right in july 
            # adj_ct_regimen   # See later, variable not right in july 
            # nb_cycles_adj_ct_taxanes   # See later, variable not right in july 
            # nb_cycles_adj_ct_anthra   # See later, variable not right in july 
            dat_end_adj_ct    = pmax (self@database$adjfin.q1,self@database$adjfin.q2, na.rm = TRUE)
            
            return(list(
              adj_ct                 = adj_ct,
              nb_cycles_adj_ct       = self@database$nb_total_cycles_adj,
              dat_first_adj_ct       = self@database$adjdeb.q1,
              dat_end_adj_ct         = dat_end_adj_ct,
              adj_antiher2           = self@database$heradj,
              dat_first_adj_antiher2 = self@database$debher
            ))
          }
)


setMethod(f="mapping_tumor_char_neo",
          signature="MappingNeorep",
          definition=function(self)
          {
            breast_res_infiltr                            <- NA
            breast_res_infiltr[self@database$absinf == 1] <- 0
            breast_res_infiltr[self@database$absinf == 0] <- 1
            
            # For in situ post NAC, beware, we do not take the original variable from database
            # instead, we take DCIS reread by Than - and for 718 patients, reread by Marick;
            # table(self@database$breast_res_insitu,self@database$absins,exclude=NULL)   
            # BEWARE, no discrepancy, which means we do not have the right file coming from marick....

            lvi_postneo                                     <- NA
            lvi_postneo[self@database$lvi_postneo == "yes"] <- 1
            lvi_postneo[self@database$lvi_postneo == "no"]  <- 0
            
             # rcb_index             <- as.numeric(self@database$RCB_index)
            # str_til_perc_postneo  <- as.numeric(self@database$str_til_perc_postneo)
            # it_til_perc_postneo   <- as.numeric(self@database$it_til_perc_postneo)
            
            return(list(
              breast_res_infiltr   = factor(breast_res_infiltr),
              breast_res_insitu    = self@database$breast_res_insitu,
              nbggpos_postneo      = self@database$ggenv,
              lvi_postneo          = factor(lvi_postneo),
              rcb_index            = self@database$RCB_index,
              # self@database$rcb_index
              str_til_perc_postneo = self@database$str_til_perc_postneo,
              it_til_perc_postneo  = self@database$it_til_perc_postneo,
              mitotic_index_postneo = self@database$mitotic_index_postneo,
              tumor_cellularity_postneo = self@database$perc_cellules_tumorales2  
            ))
          }
)


setMethod(f="mapping_events_and_censor",
          signature="MappingNeorep",
          definition=function(self)
          {
            dat_censor_database      <- rep("2019-03-01",nrow(self@database))
            dat_last_update          <- rep("2019-03-01",nrow(self@database))
            ev_prog_neo              <- rep(0,nrow(self@database)) #
            dat_prog_neo             <- rep(as.Date(NA),nrow(self@database))
            cause_death    <- rep(NA,nrow(self@database))
            cause_death    <- case_when(self@database$causdc == 1 ~ 1,
                                        self@database$causdc == 2 ~ 1,
                                        self@database$causdc %in% c(3,4,5,6,8) ~ 2,
                                        self@database$causdc == 7 ~ 3) 
            return(list(
              dat_censor_database      = dat_censor_database,
              dat_last_update = dat_last_update, 
              ev_prog_neo = ev_prog_neo,
              dat_prog_neo = dat_prog_neo ,
              ev_recloc = self@database$ev_recloc,
              dat_recloc = self@database$dat_recloc,
              ev_recreg = self@database$ev_recreg,
              dat_recreg = self@database$dat_recreg,
              ev_meta = self@database$ev_meta,
              dat_meta = self@database$dat_meta,
              ev_contro = self@database$ev_contro,
              dat_contro = self@database$dat_contro,
              ev_secondk = self@database$ev_secondk,
              dat_secondk = self@database$dat_secondk,
              status_vital = self@database$status_vital,
              cause_death  = cause_death,
              dat_last_news = self@database$dat_last_news
            ))
          }
)

