    
    #Load dataset
    load(file.path(Sys.getenv("PROJECT_PATH"), "core/05_feeric/data/df_complet_cas.RData"),verbose = T)
    
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
    
MappingFeeric <- setClass(
  "MappingFeeric",
  
  # Define the slots
  
  # Set the default values for the slots.
  prototype=list(
    name_database = "feeric",
    database = df_complet_cas 
  ) ,
  contains = "Mapping"
)
    
    #self = MappingFeeric() 
    
    setMethod(f="mapping_initial",
              signature="MappingFeeric",
              definition=function(self)
              {
                self@database$dat_first_adj_antiher2 <- as.Date(self@database$dat_first_adj_antiher2,format= "%Y-%m-%d") + 2 
                self@database$dat_first_adj_ct <- as.Date(self@database$dat_first_adj_ct,format= "%Y-%m-%d") + 2 
                self@database$dat_first_adj_rt <- as.Date(self@database$dat_first_adj_rt,format= "%Y-%m-%d") + 3
                self@database$dat_first_adj_ht <- as.Date(self@database$dat_first_adj_ht,format= "%Y-%m-%d") + 4
                return(self)
              }
    )
    
    
    
    setMethod(f="mapping_patient_id",
              signature="MappingFeeric",
              definition=function(self)
              {
                return(list(
                  database         = rep(5,nrow(self@database)),
                  center_curie     = regroup(self@database$recrut_center , list(c("IC Paris"),c("IC Saint Cloud"),c("Louis Pasteur Le Coudray Chartres",
                                                                                                                    "Institut Bergonié de Bordeaux",
                                                                                                                    "CMS St Louis",
                                                                                                                    "IC Montpellier",
                                                                                                                    "CGFL de Dijon",
                                                                                                                    "CLB de Lyon")), c(1,2,3)),
                  center           = regroup(self@database$recrut_center , list(c("IC Paris","IC Saint Cloud"),c("Louis Pasteur Le Coudray Chartres",
                                                                                                                 "Institut Bergonié de Bordeaux",
                                                                                                                 "CMS St Louis",
                                                                                                                 "IC Montpellier",
                                                                                                                 "CGFL de Dijon",
                                                                                                                 "CLB de Lyon")), c(1,2)),
                  base_cletri      = self@database$PatientID,
                  cletri           = self@database$PatientID
                ))
              }
    )
    
    
    setMethod(f="mapping_patient_char",
              signature="MappingFeeric",
              definition=function(self)
              {
                return(list(
                  age                = factor(self@database$age),
                  fam_history        = regroup(self@database$fam_history_bin, list(c("Yes"),c("No")),c(1,0)), 
                  #weight             = as.numeric(self@database$weight),
                  #size               = as.numeric(self@database$size)/100,
                  nb_preg            = as.integer(self@database$preg_numb),
                  #nb_child           = # à créer à partir de preg_his Y/N et outcome
                  breast_feed        = regroup(self@database$brfeed_his, list(c("No"),c("Yes")), c(0,1)),
                  brca_mut           = regroup(self@database$brca_mut, list(c("Yes"),c("No")),c(1,0)), # chez moi 3 levels No, Yes, Ongoing > ici NA créés automatiquement pour Ongoing
                  brca_1_2_mut       = regroup(self@database$brca_1_2_mut , list(c("BRCA1"),c("BRCA2"),c("Others"),c("No")), c(1,2,3,4)),
                  smoking_3cl        = regroup(self@database$smoking_3cl, list(c("Never"),c("Current"),c("Former")), c(1,2,3)),
                  smoking            = regroup(self@database$smoking, list(c("No"),c("Yes")),c(0,1))
                ))
              }
    )
    
    # Clara : Je rajoute comedication and comorbidity 
    
setMethod(f="mapping_comorbidity",
          signature="MappingFeeric",
          definition=function(self)
          {
            return(list(
              comorbidity = regroup(self@database$comorbidity_bin, list(c("No"),c("Yes")),c(0,1))
            ))
          }
)
    
    
    setMethod(f="mapping_comedication",
              signature="MappingFeeric",
              definition=function(self)
              {
                return(list(
                  comedic        = regroup(self@database$comedic, list(c("No"),c("Yes")), c(0,1))
                ))
              }
    )
    
    
    setMethod(f="mapping_bc_diagnosis",
              signature="MappingFeeric",
              definition=function(self)
              {
                return(list(
                ))
              }
    )
    
    setMethod(f="mapping_surgery",
              signature="MappingFeeric",
              definition=function(self)
              {
                #Beware dates : there are some in 2019. 
                #TODO : mettre NA si non opere
                #Dat first surg are inferred on the 16th of month. 
                dat_first_surg <- as.Date(self@database$first_bc_ttt_date,format= "%Y-%m-%d") + 1
                neoadj_patient = which(self@database$neoadj_chemo == "Yes" |
                                         self@database$neoadj_rt == "Yes" |
                                         self@database$neoadj_hormon == "Yes" |
                                         self@database$neoadj_hcp == "Yes")
                dat_first_surg[neoadj_patient] <- pmax(as.Date(self@database$dat_first_neo_ct[neoadj_patient],format= "%Y-%m-%d"),
                                                       as.Date(self@database$dat_first_neo_ht[neoadj_patient],format= "%Y-%m-%d"),
                                                       as.Date(self@database$dat_first_neo_rt[neoadj_patient],format= "%Y-%m-%d"),
                                                       as.Date(self@database$dat_first_neo_antiher2[neoadj_patient],format= "%Y-%m-%d"),
                                                       na.rm = T) + 1 
                
                return(list(
                  breast_surgery_3cl    = regroup(self@database$breast_surgery_3cl, list(c("Lumpectomy"),c("Mastectomy")),c(1,2)),
                  axillary_surgery_4cl  = regroup(self@database$axillary_surgery_2cl, list(c("Sentinel Node biopsy"),
                                                                                           c("Axillary Node dissection")), c(1,2)),
                  dat_first_surg        =  dat_first_surg
                ))
              }
    )
    
    setMethod(f="mapping_treatments_binary",
              signature="MappingFeeric",
              definition=function(self)
              {
                
                #Date of treatments are inferred to 17th of the month if adjuvant 
                #15th otherwise
                
                #Dat first chemotherapy
                dat_first_ct <- pmin(as.Date(self@database$dat_first_neo_ct,format="%Y-%m-%d"),
                                     as.Date(self@database$dat_first_adj_ct,format="%Y-%m-%d") ,
                                     na.rm = T) 
                
                dat_first_rt <- pmin(as.Date(self@database$dat_first_neo_rt,format="%Y-%m-%d"),
                                     as.Date(self@database$dat_first_adj_rt,format="%Y-%m-%d")  ,
                                     na.rm = T)
                
                dat_first_ht <- pmin(as.Date(self@database$dat_first_neo_ht,format="%Y-%m-%d"),
                                     as.Date(self@database$dat_first_adj_ht,format="%Y-%m-%d"),
                                     na.rm = T)
                
                dat_first_antiher2 <- pmin(as.Date(self@database$dat_first_neo_antiher2,format="%Y-%m-%d"),
                                     as.Date(self@database$dat_first_adj_antiher2,format="%Y-%m-%d"),
                                     na.rm = T)
                
                return(list(
                  ct                 =  regroup(self@database$ct, list(c("No"),c("Yes")), c(0,1)),
                  dat_first_ct       =  dat_first_ct,
                  rt                 =  regroup(self@database$rt, list(c("No"),c("Yes")), c(0,1)),
                  dat_first_rt       =  dat_first_rt,
                  ht                 =  regroup(self@database$ht, list(c("No"),c("Yes")), c(0,1)),
                  dat_first_ht       =  dat_first_ht,
                  #ht_type_5cl        = # à créer car chez moi neoadj ou adj ht type 
                  antiher2           = regroup(self@database$antiher2, list(c("No"),c("Yes")), c(0,1)),
                  dat_first_antiher2 = dat_first_antiher2
                ))
              }
    )
    
    
    setMethod(f="mapping_neoadj_or_not",
              signature="MappingFeeric",
              definition=function(self)
              {
                return(list(
                  neo_ct                 = regroup(self@database$neoadj_chemo, list(c("No"),c("Yes")), c(0,1)),
                  neo_ht                 = regroup(self@database$neoadj_hormon, list(c("No"),c("Yes")), c(0,1)),
                  neo_rt                 = regroup(self@database$neoadj_rt, list(c("No"),c("Yes")), c(0,1)),
                  neo_antiher2           = regroup(self@database$neoadj_hcp, list(c("No"),c("Yes")), c(0,1)),
                  dat_first_neo_ct       = self@database$dat_first_neo_ct,
                  dat_first_neo_ht       = self@database$dat_first_neo_ht,
                  dat_first_neo_rt       = self@database$dat_first_neo_rt,
                  dat_first_neo_antiher2 = self@database$dat_first_neo_antiher2
                ))
              }
    )
    
    
    # fertility_pregnancy à faire plus tard avec ASHP
    
    #preg_dg = self@database$dg_preg
    #ovarian_cryopreservation = self@database$ovarian_cryopreservation
    
