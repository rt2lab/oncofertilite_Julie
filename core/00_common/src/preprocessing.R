library(data.table)

#Util functions
one_missing <- function(names_cols_df,df,col_preprocessed=list()){
  missing_preprocessed = sapply(col_preprocessed,is.null)
  missing_cols_df = sapply(names_cols_df,function(x){!x %in% colnames(df)})
  return(any(missing_cols_df) | any(missing_preprocessed))
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


#Create class preprocessing
Preprocessing <- setClass(
  "Preprocessing",

  # Define the slots
  slots = c(
    name_database = "character",
    database = "data.frame",
    dat_censor = "Date"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "",
    database = data.frame(),
    dat_censor = as.Date("2020-01-01")
  )

)

setGeneric(name="preprocessing_patient_id",
           def=function(self)
           {
             standardGeneric("preprocessing_patient_id")
           }
)

setMethod(f="preprocessing_patient_id",
          signature="Preprocessing",
          definition=function(self)
          {

            if(one_missing(c("dat_bc_diagnosis"),self@database)){
              year_diag = self@database$year_diag
              period_diag = self@database$period_diag
            }else{
              year_diag  = lubridate::year(self@database$dat_bc_diagnosis)
              period_diag = cut(year_diag,  seq( from = 1970, to = 2020, by = 5),  right=FALSE,labels = FALSE)
            }

            if(one_missing(c("database","cletri"),self@database)){
              base_cletri = self@database$base_cletri
            }else{
              base_cletri = paste0(self@database$database, "_",self@database$cletri)
            }

            if(one_missing(c("side"),self@database,list(base_cletri))){
              base_cletri_side = self@database$patient_side
            }
            else{
              base_cletri_side = paste0(base_cletri,"_",self@database$side)
            }
            
            if(one_missing(c("cletri","side"),self@database)){
              patient_side = self@database$patient_side
            }
            else{
              patient_side = paste0(self@database$cletri,"_",self@database$side)
            }

            if(one_missing(c("dat_birth"),self@database)){
              year_birth = self@database$year_birth
            }else{
              year_birth     = lubridate::year(self@database$dat_birth)
            }
            
            if(one_missing(c("numdos_curie"),self@database)){
              is_base_sein = self@database$is_base_sein
              is_neorep = self@database$is_neorep
              is_appasur1 = self@database$is_appasur1
              is_appasur2 = self@database$is_appasur2
              is_base_sein_ybcp_florence = self@database$is_base_sein_ybcp_florence
              is_neocheck = self@database$is_neocheck
              is_appasur_snds = self@database$is_appasur_snds
              is_comedic_snds = self@database$is_comedic_snds
              
            }else{
              
              #Hash numdos Curie
              source(file.path(Sys.getenv("PROJECT_PATH"), "core/00_common/src/hashing.R"))
              numdos_curie_hash = simplify2array(lapply(self@database$numdos_curie,hash_curie))
              
              #Base sein
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_base_sein.RData"
              ),verbose = T) #71 784 numdos
              is_base_sein = ifelse(numdos_curie_hash %in% nip_base_sein_hash, 1, NA)
              
              #Neorep
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_neorep.RData"
              ),verbose = T)
              is_neorep = ifelse(numdos_curie_hash %in% nip_neorep_hash, 1, NA)
              
              #Appasur1 
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_appasur1.RData"
              ),verbose = T)
              is_appasur1 = ifelse(numdos_curie_hash %in% nip_appasur1_hash, 1, NA)
              
              #Appasur2
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_appasur2.RData"
              ),verbose = T)
              is_appasur2 = ifelse(numdos_curie_hash %in% nip_appasur2_hash, 1, NA)
              
              #Base sein Florence YBCP
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_base_sein_ybcp_florence.RData"
              ),verbose = T)
              is_base_sein_ybcp_florence = ifelse(numdos_curie_hash %in% nip_base_sein_ybcp_florence_hash, 1, NA)
              
              #Neocheck
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_neocheck.RData"
              ),verbose = T)
              is_neocheck = ifelse(numdos_curie_hash %in% nip_neocheck_hash, 1, NA)
              
              #APPASUR SNDS
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_appasur_snds.RData"
              ),verbose = T)
              is_appasur_snds = ifelse(numdos_curie_hash %in% nip_appasur_snds_hash, 1, NA)
              
              #APPASUR SNDS
              load(file.path(
                Sys.getenv("PROJECT_PATH"),
                "core/00_common/data_hash/nip_comedic_snds.RData"
              ),verbose = T)
              is_comedic_snds = ifelse(numdos_curie_hash %in% nip_comedic_snds_hash, 1, NA)
        
            }
    
            return(list(
              base_cletri    = base_cletri,
              patient_side   = patient_side,
              year_birth     = year_birth,
              year_diag      = year_diag,
              period_diag    = factor(period_diag),
              is_base_sein      = is_base_sein,
              is_neorep         = is_neorep,
              is_appasur1       = is_appasur1,
              is_appasur2       = is_appasur2,
              is_base_sein_ybcp_florence = is_base_sein_ybcp_florence,
              is_neocheck       = is_neocheck,
              is_appasur_snds   = is_appasur_snds,
              is_comedic_snds   = is_comedic_snds
            ))
          }
)

setGeneric(name="preprocessing_patient_char",
           def=function(self)
           {
             standardGeneric("preprocessing_patient_char")
           }
)

setMethod(f="preprocessing_patient_char",
          signature="Preprocessing",
          definition=function(self)
          {
            #age cl_10_1 peut-etre calcule quand meme.. #a revoir. 
            if(one_missing(c("dat_bc_diagnosis","dat_birth"),self@database)){
              age          = as.numeric(as.character(self@database$age))
            }else{
              age          = round(as.numeric(as.Date(self@database$dat_bc_diagnosis) -
                                             as.Date(self@database$dat_birth))/365.25,1)
            }
            if(one_missing(c(),self@database,list(age))){
              age_cl_10_1  = self@database$age_cl_10_1
              age_cl_10_2  = self@database$age_cl_10_2
              age_cl_3_cl  = self@database$age_cl_3_cl
              age_cl_5_cl  = self@database$age_cl_5_cl
              age_young_cl = self@database$age_young_cl
              age_young_cl_30_bin = self@database$age_young_cl_30_bin
              age_young_cl_40_bin = self@database$age_young_cl_40_bin
              age_young_cl_45_bin = self@database$age_young_cl_45_bin
              age_young_cl_50_bin = self@database$age_young_cl_50_bin
            }else{
              age_cl_10_1  = cut (age, c(0,30,40,50,60,70,80,Inf), right=FALSE,labels = FALSE)
              age_cl_10_2  = cut (age, c(0,40,50,60,70,Inf), right=FALSE,labels = FALSE)
              age_cl_3_cl  = cut (age, c(0,50,60,Inf), right=FALSE,labels = FALSE)
              age_cl_5_cl  = cut (age, c(0,40,50,60,75,Inf), right=FALSE,labels = FALSE)
              age_young_cl = cut (age, c(0,30,35,40,Inf), right=FALSE,labels = FALSE)
              age_young_cl_30_bin = cut (age, c(0,30,Inf), right=FALSE,labels = FALSE)
              age_young_cl_40_bin = cut (age, c(0,40,Inf), right=FALSE,labels = FALSE)
              age_young_cl_45_bin = cut (age, c(0,45,Inf), right=FALSE,labels = FALSE)
              age_young_cl_50_bin = cut (age, c(0,50,Inf), right=FALSE,labels = FALSE)
            }

            if(one_missing(c("nb_preg"),self@database)){
              nb_preg_3cl    = self@database$nb_preg_3cl
              prev_pregnancy = self@database$prev_pregnancy
            }else{
              nb_preg_3cl    = ifelse(self@database$nb_preg == 0,0, ifelse(self@database$nb_preg == 1,1,2))
              prev_pregnancy = ifelse(self@database$nb_preg == 0,0,1)
            }

            if(one_missing(c("nb_child"),self@database)){
              nb_child_3cl = self@database$nb_child_3cl
              nb_child_2cl   = self@database$nb_child_2cl
            }else{
              nb_child_3cl = ifelse(self@database$nb_child == 0,0,ifelse(self@database$nb_child == 1,1,2))
              nb_child_2cl   = ifelse(self@database$nb_child_2cl == 0,0,1)
            }

            if(one_missing(c("weight","size"),self@database)){
              bmi     = self@database$bmi
            }else{
              bmi     =  round(self@database$weight/(self@database$size^2),1)
            }
            
            if(one_missing(c(),self@database, list(bmi))){
              bmi_2cl = self@database$bmi_2cl
              bmi_3cl = self@database$bmi_3cl
              bmi_4cl = self@database$bmi_4cl
              bmi_5cl = self@database$bmi_5cl
            }else{
              bmi_2cl =  cut(bmi, c(0,24.9999,100), right = FALSE, labels = FALSE)
              bmi_3cl =  relevel(as.factor(cut(bmi, c(0,18.5,24.9999,100), right = FALSE, labels = FALSE)),ref=2)
              bmi_4cl =  relevel(as.factor(cut (bmi, c(0, 18.5,24.9999,29.99999,100), right = FALSE, labels = FALSE)), ref = 2)
              bmi_5cl =  relevel(as.factor(cut (bmi, c(0, 18.5,24.9999,29.99999,35,100), right = FALSE, labels = FALSE) ), ref = 2)
            }
            
            return(list(
              age = age,
              age_cl_10_1   =  factor(age_cl_10_1),
              age_cl_10_2   =  factor(age_cl_10_2),
              age_cl_3_cl   =  factor(age_cl_3_cl),
              age_cl_5_cl   =  factor(age_cl_5_cl),
              age_young_cl  =  factor(age_young_cl),
              age_young_cl_30_bin = factor(age_young_cl_30_bin),
              age_young_cl_40_bin = factor(age_young_cl_40_bin),
              age_young_cl_45_bin = factor(age_young_cl_45_bin),
              age_young_cl_50_bin = factor(age_young_cl_50_bin),
              nb_preg_3cl   =  factor(nb_preg_3cl),
              prev_pregnancy=  factor(prev_pregnancy),
              nb_child_3cl  =  factor(nb_child_3cl),
              nb_child_2cl    =  factor(nb_child_2cl),
              bmi           =  bmi,
              bmi_2cl       =  bmi_2cl,
              bmi_3cl       =  bmi_3cl,
              bmi_4cl       =  bmi_4cl,
              bmi_5cl       =  bmi_5cl
            ))
          }
)

setGeneric(name="preprocessing_bc_diagnosis",
           def=function(self)
           {
             standardGeneric("preprocessing_bc_diagnosis")
           }
)

setMethod(f="preprocessing_bc_diagnosis",
          signature="Preprocessing",
          definition=function(self)
          {

            if(one_missing(c("ctuicc_5cl"),self@database)){
              ctuicc_4cl = self@database$ctuicc_4cl
            }else{
              ctuicc_4cl = ifelse(as.integer(as.character(self@database$ctuicc_5cl)) == 0, 1 , as.integer(as.character(self@database$ctuicc_5cl)))
            }
            
            #Si on a la taille clinique mais pas le decoupage en 5 classes on se raccroche ici :
            # on prend plutot la valeur du découpage de la taille clinique pour 3cl
            if(one_missing(c(),self@database,list(ctuicc_4cl))){
              if(!one_missing(c("tclin"),self@database)){ 
                ctuicc_3cl = cut(self@database$tclin, c(0,21,51,Inf), right = FALSE, labels = FALSE)
              }else{
                ctuicc_3cl = self@database$ctuicc_3cl
              }
            }else{
              ctuicc_3cl = ifelse(ctuicc_4cl==4, 3, ctuicc_4cl)
            }
            
            if(one_missing(c(),self@database,list(ctuicc_3cl))){
              ctuicc_2cl = self@database$ctuicc_2cl
            }else{
              ctuicc_2cl = ifelse(ctuicc_3cl==2, 1, ctuicc_3cl)
              ctuicc_2cl = ifelse(ctuicc_2cl==3, 2, ctuicc_2cl)
            }
            
            if(one_missing(c("cnuicc_4cl"),self@database)){
              cnuicc_2cl = self@database$cnuicc_2cl
            }else{
              cnuicc_2cl = ifelse(as.integer(as.character(self@database$cnuicc_4cl))==0, 0, 1)
            }
            
            #QUESTION pour patiente chir d'emblee avec NA dans multifocal_histo : est-ce que l'on prend la valeur de clin_multifocality ou alors on met NA
            #Pleins de questions sur Teams pour cette variable.
            #Pas fiable du tout pour l'instant
            if(one_missing(c("multifocality_clin","multifocality_histo","neo_ct"),self@database)){
              multifocality_clin_histo = self@database$multifocality_clin_histo
            }else{
              multifocality_clin_histo = ifelse(as.integer(as.character(self@database$neo_ct))==1, 
                                                as.integer(as.character(self@database$multifocality_clin)), #If neo_ct patients, multifocal clin
                                                ifelse(is.na(self@database$multifocality_histo), #Otherwise multifocal
                                                       as.integer(as.character(self@database$multifocality_clin)),
                                                       as.integer(as.character(self@database$multifocality_histo))
                                                )
                                        )
            }

            return(
              list(
                ctuicc_4cl = factor(ctuicc_4cl),
                ctuicc_3cl = factor(ctuicc_3cl),
                ctuicc_2cl = factor(ctuicc_2cl),
                cnuicc_2cl = factor(cnuicc_2cl),
                multifocality_clin_histo = factor(multifocality_clin_histo)
            ))
          }
)

setGeneric(name="preprocessing_bc_biology",
           def=function(self)
           {
             standardGeneric("preprocessing_bc_biology")
           }
)

setMethod(f="preprocessing_bc_biology",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("er_status"),self@database)){
              if(!one_missing(c("hr_status"),self@database)){
                hr_status <- self@database$hr_status
              }else{
                hr_status <- self@database$pr_status
                hr_status[hr_status == 0] <- NA
              }
            }else if(one_missing(c("pr_status"),self@database)){
              if(!one_missing(c("hr_status"),self@database)){
                hr_status <- self@database$hr_status
              }else{
                hr_status <- self@database$er_status
                hr_status[hr_status == 0] <- NA
              }
            }else{
              hr_status <- rep(NA,nrow(self@database))
              hr_status[self@database$er_status==1 | self@database$pr_status==1] <- 1
              hr_status[self@database$er_status==0 & self@database$pr_status==0] <- 0
            }

            if(one_missing(c(),self@database,list(hr_status))){
              if(!one_missing(c("luminal"),self@database)){
                luminal <- self@database$luminal
              }else{
                luminal <- rep(NA,nrow(self@database))
                luminal[self@database$her2_status==1] <- 0
              }
            }else if(one_missing(c("her2_status"),self@database)){
              if(!one_missing(c("luminal"),self@database)){
                luminal <- self@database$luminal
              }else{
                luminal <- rep(NA,nrow(self@database))
                luminal[hr_status==0] <- 0
              }
            }else{
              luminal <- rep(NA,nrow(self@database))
              luminal[hr_status==1 & self@database$her2_status==0] <- 1
              luminal[hr_status==0 | self@database$her2_status==1] <- 0
            }

            if(one_missing(c(),self@database,list(hr_status))){
              if(!one_missing(c("tnbc"),self@database)){
                tnbc <- self@database$tnbc
              }else{
                tnbc <- rep(NA,nrow(self@database))
                tnbc[self@database$her2_status==1] <- 0
              }
            }else if(one_missing(c("her2_status"),self@database)){
              if(!one_missing(c("tnbc"),self@database)){
                tnbc <- self@database$tnbc
              }else{
                tnbc <- rep(NA,nrow(self@database))
                tnbc[hr_status==1] <- 0
              }
            }else{
              tnbc <- rep(NA,nrow(self@database))
              tnbc[hr_status==0 & self@database$her2_status==0] <- 1
              tnbc[hr_status==1 | self@database$her2_status==1] <- 0
            }

            if(one_missing(c("her2_status"),self@database,list(hr_status)) &
               !one_missing(c("subtype"),self@database)){
              subtype <- self@database$subtype
            }else{
              subtype                     <- rep(NA,nrow(self@database))
              subtype[luminal==1]         <- 1
              subtype[tnbc==1]            <- 2
              subtype[self@database$her2_status==1]     <- 3
            }

            if(one_missing(c(),self@database,list(hr_status,subtype)) &
               !one_missing(c("subtype4"),self@database)){
              subtype4 <- self@database$subtype4
            }else{
              subtype4    <- subtype
              subtype4[which(subtype==3 & hr_status==1)] <- 3
              subtype4[which(subtype==3 & hr_status==0)] <- 4
            }

            if(one_missing(c("er_percentage"),self@database)){
              er_status_1_perc <- self@database$er_status_1_perc
            }else{
              er_status_1_perc <- ifelse(self@database$er_percentage>1,1,0)
            }

            if(one_missing(c("pr_percentage"),self@database)){
              pr_status_1_perc <- self@database$pr_status_1_perc
            }else{
              pr_status_1_perc <- ifelse(self@database$pr_percentage>1,1,0)
            }

            if(one_missing(c(),self@database,list(er_status_1_perc))){
              if(!one_missing(c("hr_status_1_perc"),self@database)){
                hr_status_1_perc <- self@database$hr_status_1_perc
              }else{
                hr_status_1_perc <- pr_status_1_perc
                hr_status_1_perc[hr_status_1_perc == 0] <- NA
              }
            }else if(one_missing(c(),self@database,list(pr_status_1_perc))){
              if(!one_missing(c("hr_status_1_perc"),self@database)){
                hr_status_1_perc <- self@database$hr_status_1_perc
              }else{
                hr_status_1_perc <- self@database$er_status_1_perc
                hr_status_1_perc[hr_status_1_perc == 0] <- NA
              }
            }else{
              hr_status_1_perc <- rep(NA,nrow(self@database))
              hr_status_1_perc[er_status_1_perc==1 | pr_status_1_perc==1] <- 1
              hr_status_1_perc[er_status_1_perc==0 & pr_status_1_perc==0] <- 0
            }

            if(one_missing(c(),self@database,list(hr_status_1_perc))){
              if(!one_missing(c("luminal_1_perc"),self@database)){
                luminal_1_perc <- self@database$luminal_1_perc
              }else{
                luminal_1_perc <- rep(NA,nrow(self@database))
                luminal_1_perc[self@database$her2_status==1] <- 0
              }
            }else if(one_missing(c("her2_status"),self@database)){
              if(!one_missing(c("luminal_1_perc"),self@database)){
                luminal_1_perc <- self@database$luminal_1_perc
              }else{
                luminal_1_perc <- rep(NA,nrow(self@database))
                luminal_1_perc[hr_status_1_perc==0] <- 0
              }
            }else{
              luminal_1_perc <- rep(NA,nrow(self@database))
              luminal_1_perc[hr_status_1_perc==1 & self@database$her2_status==0] <- 1
              luminal_1_perc[hr_status_1_perc==0 | self@database$her2_status==1] <- 0
            }

            if(one_missing(c(),self@database,list(hr_status_1_perc))){
              if(!one_missing(c("tnbc_1_perc"),self@database)){
                tnbc_1_perc <- self@database$tnbc_1_perc
              }else{
                tnbc_1_perc<- rep(NA,nrow(self@database))
                tnbc_1_perc[self@database$her2_status==1] <- 0
              }
            }else if(one_missing(c("her2_status"),self@database)){
              if(!one_missing(c("tnbc_1_perc"),self@database)){
                tnbc_1_perc <- self@database$tnbc_1_perc
              }else{
                tnbc_1_perc <- rep(NA,nrow(self@database))
                tnbc_1_perc[hr_status_1_perc==1] <- 0
              }
            }else{
              tnbc_1_perc <- rep(NA,nrow(self@database))
              tnbc_1_perc[hr_status_1_perc==0 & self@database$her2_status==0] <- 1
              tnbc_1_perc[hr_status_1_perc==1 | self@database$her2_status==1] <- 0
            }

            if(one_missing(c("her2_status"),self@database,list(hr_status_1_perc)) &
               !one_missing(c("subtype_1_perc"),self@database)){
              subtype_1_perc <- self@database$subtype_1_perc
            }else{
              subtype_1_perc                     <- rep(NA,nrow(self@database))
              subtype_1_perc[luminal_1_perc==1]         <- 1
              subtype_1_perc[tnbc_1_perc==1]            <- 2
              subtype_1_perc[self@database$her2_status==1]     <- 3
            }

            if(one_missing(c(),self@database,list(hr_status,subtype_1_perc)) &
               !one_missing(c("subtype4_1_perc"),self@database)){
              subtype4_1_perc <- self@database$subtype4_1_perc
            }else{
              subtype4_1_perc    <- subtype_1_perc
              subtype4_1_perc[which(subtype_1_perc==3 & hr_status_1_perc==1)] <- 3
              subtype4_1_perc[which(subtype_1_perc==3 & hr_status_1_perc==0)] <- 4
            }

            if(one_missing(c("histo_5cl"),self@database)){
              if(one_missing(c("histo_4cl"),self@database)){
                if(!one_missing(c("histo_3cl"),self@database)){
                  histo_4cl = NULL
                  histo_3cl = self@database$histo_3cl
                  histo_2cl <- ifelse(as.integer(as.character(histo_3cl)) == 9, 2, as.integer(as.character(histo_3cl)))
                }else{
                  histo_4cl = NULL
                  histo_3cl = NULL
                  histo_2cl <-NULL
                }
              } else {
                histo_4cl = self@database$histo_4cl
                histo_3cl <- ifelse(as.integer(as.character(histo_4cl)) != 3, as.integer(as.character(histo_4cl)),9)
                histo_2cl <- ifelse(as.integer(as.character(histo_3cl)) == 9, 2, as.integer(as.character(histo_3cl)))
              }
            }else{
              histo_4cl <- factor(ifelse(as.integer(as.character(self@database$histo_5cl)) != 4, as.integer(as.character(self@database$histo_5cl)),9))
              histo_3cl <- ifelse(as.integer(as.character(histo_4cl)) != 3, as.integer(as.character(histo_4cl)),9)
              histo_2cl <- ifelse(as.integer(as.character(histo_3cl)) == 9, 2, as.integer(as.character(histo_3cl)))
            }

            if(one_missing(c("grade_3cl"),self@database)){
              grade_2cl <- self@database$grade_2cl
            }else{
              grade_2cl <- regroup(self@database$grade_3cl,list(NA, c(1,2), 3), c(NA,1,2))
            }

            if(one_missing(c("ki67_perc"),self@database)){
              ki67_cl <- self@database$ki67_cl
            }else{
              ki67_cl <- cut (self@database$ki67_perc, c(0,10,20,101), right = FALSE, labels = FALSE)
            }

            #Revoir les conditions si une variable est manquante
            if(one_missing(c("invasive_or_dcis","dcis_component"),self@database)){
              inv_dcis_4cl <- self@database$inv_dcis_4cl
            }else{
              inv_dcis_4cl     <- rep(NA,nrow(self@database))
              inv_dcis_4cl[self@database$invasive_or_dcis==2] <- 3
              inv_dcis_4cl[self@database$invasive_or_dcis==1 & self@database$dcis_component==0 ] <- 1
              inv_dcis_4cl[self@database$invasive_or_dcis==1 & self@database$dcis_component==1]  <- 2
              inv_dcis_4cl[self@database$invasive_or_dcis==1 & is.na(self@database$dcis_component)] <- 4
            }

            # ref TILs
            # 1. https://pubmed.ncbi.nlm.nih.gov/30650045-tumor-infiltrating-lymphocytes-and-prognosis-a-pooled-individual-patient-analysis-of-early-stage-triple-negative-breast-cancers/?from_single_result=loi+JCO+2019
            # Loi, JCO 2019, sTIL, categorical <30% / ≥30%
            # 2. DENKERT Lancet 2018:  Denkert et al., “Tumour-Infiltrating Lymphocytes and Prognosis in Different Subtypes of Breast Cancer.”
            # TILs were analyzed both as a continuous parameter and in three predefined groups of low (0%–10% immune cells in stromal tissue within the tumor), intermediate (11%–59%), and high TILs (≥ 60%).
            if(one_missing(c("str_til_perc"),self@database)){
              str_til_perc_30 <- self@database$str_til_perc_30
              str_til_perc_by_10 <- self@database$str_til_perc_by_10
              str_til_denkert <- self@database$str_til_denkert
            }else{
              str_til_perc_30 <- cut (self@database$str_til_perc, c(0,30,101), right = FALSE, labels = FALSE)
              str_til_perc_by_10 <- cut (self@database$str_til_perc, c(0,10,20,30,40,50,60,70,80,90,101), right = FALSE, labels = FALSE)
              str_til_denkert <- cut (self@database$str_til_perc, c(0,11,60,101), right = FALSE, labels = FALSE)
            }
            
            if(one_missing(c("it_til_perc"),self@database)){
              it_til_perc_by_5 <- self@database$it_til_perc_5
            }else{
              it_til_perc_by_5 <- cut (self@database$it_til_perc, c(0,5,10,15,20,25,30,40,45,101), right = FALSE, labels = FALSE)
            }
            
            #Mitotic index class
            if(one_missing(c("mitotic_index"),self@database)){
              mitotic_index_cl <- self@database$mitotic_index_cl
            }else{
              mitotic_index_cl <- cut (self@database$mitotic_index, c(0,10,20,101), right = FALSE, labels = FALSE)
            }

            return(
              list(
                hr_status = factor(hr_status),
                luminal = factor(luminal),
                tnbc = factor(tnbc),
                subtype = factor(subtype),
                subtype4 = factor(subtype4),
                subtype5 = rep(NA,nrow(self@database)), # Definition of luminal A versus B / reference?
                # @ASHP Attente mail Bea
                er_status_1_perc = factor(er_status_1_perc),
                pr_status_1_perc = factor(pr_status_1_perc),
                hr_status_1_perc = factor(hr_status_1_perc),
                luminal_1_perc = factor(luminal_1_perc),
                tnbc_1_perc = factor(tnbc_1_perc),
                subtype_1_perc = factor(subtype_1_perc),
                subtype4_1_perc = factor(subtype4_1_perc),
                subtype5_1_perc = rep(NA,nrow(self@database)), # @ASHP a revoir (def luminal A / B)
                histo_4cl = factor(histo_4cl),
                histo_3cl = factor(histo_3cl),
                histo_2cl = factor(histo_2cl),
                grade_2cl = factor(grade_2cl),
                ki67_cl = factor(ki67_cl),
                inv_dcis_4cl = factor(inv_dcis_4cl),
                str_til_perc_30 = factor(str_til_perc_30),
                str_til_perc_by_10 = factor(str_til_perc_by_10),
                it_til_perc_by_5 = factor(it_til_perc_by_5),
                str_til_denkert = factor(str_til_denkert),
                mitotic_index_cl = factor(mitotic_index_cl)
              ))
          }
)

setGeneric(name="preprocessing_surgery",
           def=function(self)
           {
             standardGeneric("preprocessing_surgery")
           }
)

setMethod(f="preprocessing_surgery",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("dat_first_breast_surg"),self@database)){
              breast_surgery = self@database$breast_surgery
            }else{
              breast_surgery = ifelse(!is.na(self@database$dat_first_breast_surg),1,0)
            }
            
            if(one_missing(c("dat_first_axillar_surg"),self@database)){
              axillary_surgery = self@database$axillary_surgery
            }else{
              axillary_surgery = ifelse(!is.na(self@database$dat_first_axillar_surg),1,0)
            }
            
            if(one_missing(c("dat_first_axillar_surg","dat_first_breast_surg"),self@database)){
              dat_first_cancer_surg = self@database$dat_first_cancer_surg
            }else{
              dat_first_cancer_surg = pmin(self@database$dat_first_axillar_surg, self@database$dat_first_breast_surg, na.rm = T)
            }
            
            if(one_missing(c(),self@database, list(dat_first_cancer_surg))){
              if(!one_missing(c(),self@database, list(breast_surgery,axillary_surgery))){
                cancer_surgery = pmax(as.integer(as.character(breast_surgery)), as.integer(as.character(axillary_surgery)), na.rm = T)
              }else{
                cancer_surgery = self@database$cancer_surgery
              }
            }else{
              cancer_surgery = ifelse(!is.na(self@database$dat_first_cancer_surg),1,0)
            }

            if(one_missing(c("axillary_surgery_4cl"),self@database)){
              axillary_surgery_3cl = self@database$axillary_surgery_3cl
            }else{
              axillary_surgery_3cl = ifelse(self@database$axillary_surgery_4cl %in% c(2,3),2,self@database$axillary_surgery_4cl)
            }
            
            if(one_missing(c(),self@database, list(axillary_surgery_3cl))){
              axillary_surgery_2cl = self@database$axillary_surgery_2cl
            }else{
              axillary_surgery_2cl = ifelse(axillary_surgery_3cl %in% c(1,2),axillary_surgery_3cl,NA)
            }
            
            #if(one_missing(c(),self@database, list(axillary_surgery_2cl))){
            #  axillary_surgery = self@database$axillary_surgery
            #}else{
            #  axillary_surgery = ifelse(is.na(axillary_surgery_2cl),0,1)
            #}
            
            if(one_missing(c("breast_surgery_3cl"),self@database)){
              breast_surgery_2cl = self@database$breast_surgery_2cl
            }else{
              breast_surgery_2cl = ifelse(as.character(self@database$breast_surgery_3cl) == "9",NA,as.character(self@database$breast_surgery_3cl))
            }

            return(
              list(
                breast_surgery = factor(breast_surgery),
                axillary_surgery = factor(axillary_surgery),
                cancer_surgery  = factor(cancer_surgery), 
                dat_first_cancer_surg = dat_first_cancer_surg,
                axillary_surgery_3cl = factor(axillary_surgery_3cl),
                axillary_surgery_2cl = factor(axillary_surgery_2cl), 
                breast_surgery_2cl = factor(breast_surgery_2cl)
              ))
          }
)

setGeneric(name="preprocessing_treatments_binary",
           def=function(self)
           {
             standardGeneric("preprocessing_treatments_binary")
           }
)

setMethod(f="preprocessing_treatments_binary",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("ht_type_5cl"),self@database)){
              ht_type_3cl = self@database$ht_type_3cl
            }else{
              ht_type_3cl	= ifelse(self@database$ht_type_5cl %in% c(1,2,NA), self@database$ht_type_5cl, 3)
            }

            return(
              list(
                ht_type_3cl	= factor(ht_type_3cl)
              ))
          }
)

setGeneric(name="preprocessing_neoadj_or_not",
           def=function(self)
           {
             standardGeneric("preprocessing_neoadj_or_not")
           }
)

setMethod(f="preprocessing_neoadj_or_not",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("breast_surgery","neo_ct","neo_ht","neo_rt","neo_antiher2","neo_tc_other"),self@database)){
                primary_ttt = self@database$primary_ttt
              }else{
                primary_ttt = rep(1,nrow(self@database))
                primary_ttt[self@database$breast_surgery==0] = 9
                primary_ttt[unique(c(which(self@database$neo_ct == 1),
                                     which(self@database$neo_ht == 1),
                                     which(self@database$neo_rt == 1),
                                     which(self@database$neo_antiher2 == 1),
                                     which(self@database$neo_tc_other == 1)))] = 2
              }

              if(one_missing(c("neo_ct","neo_ht","neo_rt","neo_antiher2","neo_tc_other"),self@database, list(primary_ttt))){
                  primary_ttt_5cl = self@database$primary_ttt_5cl
              }else{
                primary_ttt_5cl <- case_when(
                  is.na(primary_ttt) ~ NA_real_,
                  primary_ttt==1     ~ 1,
                  primary_ttt==9     ~ 9,
                  primary_ttt==2 & 
                      self@database$neo_ct == 1 &
                      (self@database$neo_ht==0 | is.na(self@database$neo_ht)) & 
                      (self@database$neo_rt==0 | is.na(self@database$neo_rt)) &
                      (self@database$neo_tc_other==0 | is.na(self@database$neo_tc_other)) ~ 2, 
                  primary_ttt==2 & 
                    self@database$neo_ht == 1 &
                    (self@database$neo_rt==0 | is.na(self@database$neo_rt)) & 
                    (self@database$neo_ct==0 | is.na(self@database$neo_ct)) &
                    (self@database$neo_tc_other==0 | is.na(self@database$neo_tc_other)) ~ 3, 
                  TRUE ~ 4
                )
              }


            if(one_missing(c(),self@database,list(primary_ttt_5cl))){
              primary_ttt_3cl = self@database$primary_ttt_3cl
            }else{
              primary_ttt_3cl = ifelse(primary_ttt_5cl %in% c(1,2),primary_ttt_5cl,3) #Gestion des NA à revoir
            }

            return(
              list(
                primary_ttt = factor(primary_ttt),
                primary_ttt_5cl = factor(primary_ttt_5cl),
                primary_ttt_3cl = factor(primary_ttt_3cl)
              ))
          }
)

setGeneric(name="preprocessing_tumor_char_surg",
           def=function(self)
           {
             standardGeneric("preprocessing_tumor_char_surg")
           }
)

setMethod(f="preprocessing_tumor_char_surg",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("ptuicc_5cl"),self@database)){
              ptuicc_4cl = self@database$ptuicc_4cl
            }else{
              ptuicc_4cl =  regroup(self@database$ptuicc_5cl, list(c(0,1), 2,3,4), c(1,2,3,4))
            }

            if(one_missing(c(),self@database,list(ptuicc_4cl ))){
              ptuicc_3cl = self@database$ptuicc_3cl
            }else{
              ptuicc_3cl = ifelse(as.integer(as.character(ptuicc_4cl))==4, 3, as.integer(as.character(ptuicc_4cl)))
            }

            if(one_missing(c("nbggpos"),self@database)){
              pnuicc_4cl  = self@database$pnuicc_4cl
            }else{
              pnuicc_4cl = cut (self@database$nbggpos, c(0,1,4,10,60), right=FALSE,labels = FALSE)
            }

            if(one_missing(c(),self@database,list(pnuicc_4cl))){
              pnuicc_3cl = self@database$pnuicc_3cl
            }else{
              pnuicc_3cl = ifelse(pnuicc_4cl< 4,pnuicc_4cl,3)
            }

            if(one_missing(c(),self@database,list( pnuicc_3cl))){
              pnuicc_2cl = self@database$pnuicc_2cl
            }else{
              pnuicc_2cl = ifelse(pnuicc_3cl<3,  pnuicc_3cl,2)
            }

            return(
              list(
                ptuicc_4cl = factor(ptuicc_4cl),
                ptuicc_3cl = factor(ptuicc_3cl),
                pnuicc_4cl = factor(pnuicc_4cl),
                pnuicc_3cl = factor(pnuicc_3cl),
                pnuicc_2cl = factor(pnuicc_2cl)
              ))
          }
)

setGeneric(name="preprocessing_tumor_char_neo",
           def=function(self)
           {
             standardGeneric("preprocessing_tumor_char_neo")
           }
)

setMethod(f="preprocessing_tumor_char_neo",
          signature="Preprocessing",
          definition=function(self)
          {

            if(one_missing(c("nbggpos_postneo"),self@database)){
              ypnuicc_4cl = self@database$ypnuicc_4cl
            }else{
              ypnuicc_4cl = cut (self@database$nbggpos_postneo, c(0,1,4,10,60), right=FALSE,labels = c(0,1,2,3))
            }

            if(one_missing(c(),self@database,list(ypnuicc_4cl))){
              ypnuicc_3cl = self@database$ypnuicc_3cl
            }else{
              ypnuicc_3cl = factor(ifelse(as.integer(as.character(ypnuicc_4cl)) < 3,  as.integer(as.character(ypnuicc_4cl)),2))
            }

            if(one_missing(c(),self@database,list(ypnuicc_3cl))){
              ypnuicc_2cl = self@database$ypnuicc_2cl
            }else{
              ypnuicc_2cl = factor(ifelse(as.integer(as.character(ypnuicc_3cl)) == 0,  0,1))
            }
            
            if(one_missing(c("breast_res_infiltr"),self@database,ypnuicc_2cl)){
              pcr <- self@database$pcr
            }else{
              pcr                                <- rep(NA,nrow(self@database))
              pcr[self@database$breast_res_infiltr == 0 & ypnuicc_2cl==0]  <- 1
              pcr[self@database$breast_res_infiltr == 1 | ypnuicc_2cl==1]  <- 0
            }
            
            if(one_missing(c("rcb_index"),self@database)){
              rcb_cl = self@database$rcb_cl
            }else{
              rcb_cl = cut (self@database$rcb_index, c(-1,0,1.36,3.28,Inf), right=TRUE,labels = c(0,1,2,3))
            }
            
            if(one_missing(c("mitotic_index_postneo"),self@database)){
              mitotic_index_cl_postneo = self@database$mitotic_index_cl_postneo
            }else{
              mitotic_index_cl_postneo = cut(self@database$mitotic_index_postneo, c(0,10,20,Inf), right = FALSE, labels = FALSE)
            }

            return(
              list(
                ypnuicc_4cl = factor(ypnuicc_4cl),
                ypnuicc_3cl = factor(ypnuicc_3cl),
                ypnuicc_2cl = factor(ypnuicc_2cl),
                pcr = factor(pcr),
                rcb_cl = factor(rcb_cl),
                mitotic_index_cl_postneo = factor(mitotic_index_cl_postneo)
              ))
          }
)

setGeneric(name="preprocessing_events_and_censor",
           def=function(self)
           {
             standardGeneric("preprocessing_events_and_censor")
           }
)

#Il sort d'où le dat censor???
setMethod(f="preprocessing_events_and_censor",
          signature="Preprocessing",
          definition=function(self)
          {

            if(one_missing(c("dat_last_news"),self@database)){
              year_last_news = self@database$year_last_news
            }else{
              year_last_news = as.integer(substr(self@database$dat_last_news,1,4))
            }

            if(one_missing(c("dat_last_news"),self@database)){
              dat_last_news_censor = self@database$dat_last_news_censor
            }else{
              dat_last_news_censor = self@database$dat_last_news
              dat_last_news_censor[which(self@database$dat_last_news > self@database$dat_censor_database )] <- self@database$dat_censor_database
            }
            
            if(one_missing(c("ev_prog_neo"),self@database)){
              ev_prog_neo_txt = self@database$ev_prog_neo_txt
            }else{
              ev_prog_neo_txt = factor(self@database$ev_prog_neo, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("ev_recloc"),self@database)){
              ev_recloc_txt = self@database$ev_recloc_txt
            }else{
              ev_recloc_txt = factor(self@database$ev_recloc, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("ev_recreg"),self@database)){
              ev_recreg_txt = self@database$ev_recreg_txt
            }else{
              ev_recreg_txt = factor(self@database$ev_recreg, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("ev_meta"),self@database)){
              ev_meta_txt = self@database$ev_meta_txt
            }else{
              ev_meta_txt = factor(self@database$ev_meta, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("ev_contro"),self@database)){
              ev_contro_txt = self@database$ev_contro_txt
            }else{
              ev_contro_txt = factor(self@database$ev_contro, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("ev_secondk"),self@database)){
              ev_secondk_txt = self@database$ev_secondk_txt
            }else{
              ev_secondk_txt = factor(self@database$ev_secondk, levels = c(0,1), labels = c("No","Yes"))
            }
            
            if(one_missing(c("status_vital"),self@database)){
              status_vital_txt = self@database$status_vital_txt
            }else{
              status_vital_txt = factor(self@database$status_vital, levels = c(0,1), labels = c("Alive","Dead"))
            }
            

            return(
              list(
                year_last_news = year_last_news,
                dat_last_news_censor = dat_last_news_censor,
                ev_prog_neo_txt = ev_prog_neo_txt,
                ev_recloc_txt = ev_recloc_txt,
                ev_recreg_txt = ev_recreg_txt,
                ev_meta_txt = ev_meta_txt,
                ev_contro_txt = ev_contro_txt,
                ev_secondk_txt = ev_secondk_txt,
                status_vital_txt = status_vital_txt
              ))
          }
)

setGeneric(name="preprocessing_settings_and_regimen",
           def=function(self)
           {
             standardGeneric("preprocessing_settings_and_regimen")
           }
)

setMethod(f="preprocessing_settings_and_regimen",
          signature="Preprocessing",
          definition=function(self)
          {
            if(one_missing(c("ct","neo_ct","adj_ct","breast_surgery"),self@database) & !one_missing(c("ct_setting_5cl"),self@database)){
              ct_setting_5cl <- self@database$ct_setting_5cl
            }else{
              ct_setting_5cl                                                                   <- rep(NA,nrow(self@database))
              ct_setting_5cl[which(self@database$ct == 0)]                                     <- 5
              ct_setting_5cl[which(self@database$neo_ct == 1)]                                 <- 1
              ct_setting_5cl[which(self@database$adj_ct == 1)]                                 <- 2
              ct_setting_5cl[intersect(which(self@database$neo_ct == 1), which(self@database$adj_ct == 1))]     <- 3
              ct_setting_5cl[intersect(which(self@database$ct == 1), which(self@database$breast_surgery == 0))] <- 4
            }

            if(one_missing(c("antiher2","neo_antiher2","adj_antiher2","breast_surgery"),self@database)){
              antiher2_setting_5cl <- self@database$antiher2_setting_5cl
            }else{
              antiher2_setting_5cl                                                                <- NA
              antiher2_setting_5cl[which(self@database$antiher2 == 0)]                            <- 5
              antiher2_setting_5cl[which(self@database$neo_antiher2 == 1)]                        <- 1
              antiher2_setting_5cl[which(self@database$adj_antiher2 == 1)]                        <- 2
              antiher2_setting_5cl[intersect(which(self@database$neo_antiher2 == 1), which(self@database$adj_antiher2 == 1))] <- 3
              antiher2_setting_5cl[intersect(which(self@database$antiher2 == 1), which(self@database$breast_surgery == 0))]   <- 4
            }

            return(
              list(
                ct_setting_5cl = factor(ct_setting_5cl),
                antiher2_setting_5cl = factor(antiher2_setting_5cl)
              ))
          }
)

setGeneric(name="preprocessing_pre_post_neo",
           def=function(self)
           {
             standardGeneric("preprocessing_pre_post_neo")
           }
)

setMethod(f="preprocessing_pre_post_neo",
          signature="Preprocessing",
          definition=function(self)
          {
            
            if(one_missing(c("mitotic_index_postneo","mitotic_index"),self@database)){
              mitotic_index_diff_post_pre_neo_abs = self@database$mitotic_index_diff_post_pre_neo_abs
              mitotic_index_diff_post_pre_neo_rel = self@database$mitotic_index_diff_post_pre_neo_rel
            }else{
              mitotic_index_diff_post_pre_neo_abs = as.integer(self@database$mitotic_index_postneo - self@database$mitotic_index)
              mitotic_index_diff_post_pre_neo_rel = round(as.numeric(100 * (self@database$mitotic_index_postneo - self@database$mitotic_index) / self@database$mitotic_index),1)
            }
            
            if(one_missing(c("str_til_perc_postneo","str_til_perc"),self@database)){
              str_til_diff_post_pre_neo_abs = self@database$str_til_diff_post_pre_neo_abs
              str_til_diff_post_pre_neo_rel = self@database$str_til_diff_post_pre_neo_rel
            }else{
              str_til_diff_post_pre_neo_abs = as.integer(self@database$str_til_perc_postneo - self@database$str_til_perc)
              str_til_diff_post_pre_neo_rel = round(as.numeric(100 * (self@database$str_til_perc_postneo - self@database$str_til_perc) / self@database$str_til_perc),1)
            }
            
            if(one_missing(c("it_til_perc_postneo","it_til_perc"),self@database)){
              it_til_diff_post_pre_neo_abs = self@database$it_til_diff_post_pre_neo_abs
              it_til_diff_post_pre_neo_rel = self@database$it_til_diff_post_pre_neo_rel
            }else{
              it_til_diff_post_pre_neo_abs = as.integer(self@database$it_til_perc_postneo - self@database$it_til_perc)
              it_til_diff_post_pre_neo_rel = round(as.numeric(100 * (self@database$it_til_perc_postneo - self@database$it_til_perc) / self@database$it_til_perc),1)
              
            }
            
            if(one_missing(c("tumor_cellularity_postneo","tumor_cellularity"),self@database)){
              tumor_cellularity_diff_post_pre_neo_abs = self@database$tumor_cellularity_diff_post_pre_neo_abs
              tumor_cellularity_diff_post_pre_neo_rel = self@database$tumor_cellularity_diff_post_pre_neo_rel
            }else{
              tumor_cellularity_diff_post_pre_neo_abs = as.integer(self@database$tumor_cellularity_postneo - self@database$tumor_cellularity)
              tumor_cellularity_diff_post_pre_neo_rel = round(as.numeric(100 * (self@database$tumor_cellularity_postneo - self@database$tumor_cellularity) / self@database$tumor_cellularity),1)
            }
            
            return(
              list(
                mitotic_index_diff_post_pre_neo_abs      = mitotic_index_diff_post_pre_neo_abs,
                str_til_diff_post_pre_neo_abs            = str_til_diff_post_pre_neo_abs,
                it_til_diff_post_pre_neo_abs             = it_til_diff_post_pre_neo_abs,
                tumor_cellularity_diff_post_pre_neo_abs  = tumor_cellularity_diff_post_pre_neo_abs,
                mitotic_index_diff_post_pre_neo_rel      = mitotic_index_diff_post_pre_neo_rel,
                str_til_diff_post_pre_neo_rel            = str_til_diff_post_pre_neo_rel,
                it_til_diff_post_pre_neo_rel             = it_til_diff_post_pre_neo_rel,
                tumor_cellularity_diff_post_pre_neo_rel  = tumor_cellularity_diff_post_pre_neo_rel
              ))
          }
)

setGeneric(name="preprocessing_delays_pathways",
           def=function(self)
           {
             standardGeneric("preprocessing_delays_pathways")
           }
)

setMethod(f="preprocessing_delays_pathways",
          signature="Preprocessing",
          definition=function(self)
          {

           if(one_missing(c("dat_first_cancer_surg","dat_bc_diagnosis"),self@database)){
              delay_diag_to_surg_day = self@database$delay_diag_to_surg_day
              delay_diag_to_surg_month = self@database$delay_diag_to_surg_month
            }else{
              delay_diag_to_surg_day = as.integer(as.Date(self@database$dat_first_cancer_surg) - as.Date(self@database$dat_bc_diagnosis))
              delay_diag_to_surg_month = round(as.numeric((as.Date(self@database$dat_first_cancer_surg) - as.Date(self@database$dat_bc_diagnosis)))/30.4375,1)
            }

            #In days
            if(one_missing(c("dat_first_neo_ct","dat_bc_diagnosis"),self@database)){
              delay_diag_to_neo_ct = self@database$delay_diag_to_neo_ct
            }else{
              delay_diag_to_neo_ct = as.integer(as.Date(self@database$dat_first_neo_ct) - as.Date(self@database$dat_bc_diagnosis))
            }
            
            #In days
            if(one_missing(c("dat_rando_inclusion","dat_bc_diagnosis"),self@database)){
              delay_diag_to_rando_inclusion = self@database$delay_diag_to_neo_ct
            }else{
              delay_diag_to_rando_inclusion = as.integer(as.Date(self@database$dat_rando_inclusion) - as.Date(self@database$dat_bc_diagnosis))
            }
            
            #In days
            if(one_missing(c("dat_end_neo_ct","dat_first_cancer_surg"),self@database)){
              delay_end_neo_ct_to_surg = self@database$delay_end_neo_ct_to_surg
            }else{
              delay_end_neo_ct_to_surg = as.integer(as.Date(self@database$dat_first_cancer_surg) - as.Date(self@database$dat_end_neo_ct))
            }

            #In days
            if(one_missing(c("dat_first_adj_ct","dat_first_cancer_surg"),self@database)){
              delay_surg_to_adj_ct = self@database$delay_surg_to_adj_ct
            }else{
              delay_surg_to_adj_ct = as.integer(as.Date(self@database$dat_first_adj_ct) - as.Date(self@database$dat_first_cancer_surg))
            }
            
            #In days
            if(one_missing(c("dat_end_first_ct","dat_first_rt"),self@database)){
              delay_end_first_ct_to_first_rt = self@database$delay_end_first_ct_to_first_rt
            }else{
              delay_end_first_ct_to_first_rt = as.integer(as.Date(self@database$dat_first_rt) - as.Date(self@database$dat_end_first_ct))
            }

            if(one_missing(c("dat_first_rt","dat_first_cancer_surg"),self@database)){
              delay_surg_to_first_rt_day = self@database$delay_surg_to_first_rt_day
              delay_surg_to_first_rt_month = self@database$delay_surg_to_first_rt_month
            }else{
              delay_surg_to_first_rt_day = as.integer(as.Date(self@database$dat_first_rt) - as.Date(self@database$dat_first_cancer_surg))
              delay_surg_to_first_rt_month = round(as.numeric((as.Date(self@database$dat_first_rt) - as.Date(self@database$dat_first_cancer_surg)))/30.4375,1)
            }
            
            #Delay diag to first ttt 
            #minimum entre diag_first_rt, diag_first_ct, diag_first_ht, diag_first_antiher2, dat_first_cancer_surg, dart_first_tc_other
            if(one_missing(c("dat_bc_diagnosis","dat_first_cancer_surg","dat_first_rt","dat_first_ct","dat_first_ht","dat_first_antiher2","dat_first_tc_other"),self@database)){
              delay_diag_to_first_ttt = self@database$delay_diag_first_ttt
            }else{
              dat_first_ttt = pmin(self@database$dat_first_cancer_surg, 
                                   self@database$dat_first_rt,
                                   self@database$dat_first_ct,
                                   self@database$dat_first_ht,
                                   self@database$dat_first_antiher2,
                                   self@database$dat_first_tc_other , na.rm = T)
              delay_diag_to_first_ttt = as.integer(as.Date(dat_first_ttt) - as.Date(self@database$dat_bc_diagnosis))
            }
            
            return(
              list(
                delay_diag_to_surg_day = delay_diag_to_surg_day,
                delay_diag_to_surg_month = delay_diag_to_surg_month,
                delay_diag_to_neo_ct = delay_diag_to_neo_ct,
                ndelay_diag_to_rando_inclusion = delay_diag_to_rando_inclusion,
                delay_end_neo_ct_to_surg = delay_end_neo_ct_to_surg,
                delay_surg_to_adj_ct = delay_surg_to_adj_ct,
                delay_end_first_ct_to_first_rt = delay_end_first_ct_to_first_rt,
                delay_surg_to_first_rt_day = delay_surg_to_first_rt_day,
                delay_surg_to_first_rt_month = delay_surg_to_first_rt_month,
                delay_diag_to_first_ttt = delay_diag_to_first_ttt
              ))
          }
)

setGeneric(name="preprocessing_evol",
           def=function(self)
           {
             standardGeneric("preprocessing_evol")
           }
)

setMethod(f="preprocessing_evol",
          signature="Preprocessing",
          definition=function(self)
          {
            ###########################################################
            ####################      EFS   ###########################
            ###########################################################
            #Status EFS diag
            if(one_missing(c("ev_prog_neo","ev_recloc","ev_recreg","ev_meta","status_vital"),self@database)){
              status_efs_diag <- self@database$status_efs_diag
            }else{
              status_efs_diag              <- rep(NA,nrow(self@database))
              status_efs_diag[unique(c(which(self@database$ev_prog_neo == 1),
                                  which(self@database$ev_recloc == 1),
                                  which(self@database$ev_recreg == 1),
                                  which(self@database$ev_meta == 1),
                                  which(self@database$status_vital == 1)))] <- 1
              status_efs_diag[Reduce(intersect,list(
                                          unique(c(which(self@database$ev_prog_neo == 0),which(is.na(self@database$ev_prog_neo)))),
                                          which(self@database$ev_recloc == 0),
                                          which(self@database$ev_recreg == 0),
                                          which(self@database$ev_meta == 0),
                                          unique(c(which(self@database$status_vital == 0),which(is.na(self@database$status_vital))))
                                          )
                                  )] <- 0
            }
            
            #Dat efs 
            if(one_missing(c("dat_prog_neo","dat_recloc","dat_recreg","dat_meta","dat_last_news_censor"),self@database)){
              dat_efs_util  <- pmin(self@database$dat_efs , self@database$dat_last_news_censor,na.rm = T)
              dat_efs       <- self@database$dat_efs
            }else{
              dat_efs_util  <- pmin (as.Date(self@database$dat_prog_neo), as.Date(self@database$dat_recloc),
                                     as.Date(self@database$dat_recreg) , as.Date(self@database$dat_meta),
                                     as.Date(self@database$dat_last_news_censor), na.rm=T) #Dat of last news censor for patients with no EFS event
              dat_efs       <- fifelse(status_efs_diag==1,dat_efs_util,as.Date(NA)) #NA for patients with no EFS event
            }
            
            #Delay efs diag
            if(one_missing(c(),self@database,list(dat_efs))){
              delay_efs_diag  = self@database$delay_efs_diag
            }else{
              delay_efs_diag  = round(as.numeric(dat_efs_util - as.Date(self@database$dat_bc_diagnosis))/30.4375,1)
            }
            


            ###########################################################
            ####################      RFS   ###########################
            ###########################################################
            #Status RFS diag
            if(one_missing(c("ev_recloc","ev_recreg","ev_meta","status_vital"),self@database)){
              status_rfs_diag <- self@database$status_rfs_diag
            }else{
              status_rfs_diag              <- rep(NA,nrow(self@database))
              status_rfs_diag[unique(c(which(self@database$ev_recloc == 1),
                                      which(self@database$ev_recreg == 1),
                                      which(self@database$ev_meta == 1),
                                      which(self@database$status_vital == 1)))] <- 1
              status_rfs_diag[Reduce(intersect,list(
                                          which(self@database$ev_recloc == 0),
                                          which(self@database$ev_recreg == 0),
                                          which(self@database$ev_meta == 0),
                                          unique(c(which(self@database$status_vital == 0),which(is.na(self@database$status_vital))))
                                          )
                                      )] <- 0
            }
            

            
            #Status RFS : seule difference avec status_rfs_diag : NA pour les patientes non opérées. 
            if(one_missing(c("breast_surgery"),self@database,list(status_rfs_diag))){
              status_rfs <- self@database$status_rfs
            }else{
              status_rfs <- status_rfs_diag
              status_rfs[which(self@database$breast_surgery == 0)] <- NA #NA if no surgery
            }

            
            #Dat rfs
            if(one_missing(c("dat_recloc","dat_recreg","dat_meta","dat_last_news_censor"),self@database)){
              dat_rfs_util  <- pmin(self@database$dat_rfs,self@database$dat_last_news_censor,na.rm = T)
              dat_rfs       <- self@database$dat_rfs
            }else{
              dat_rfs_util  <- pmin (as.Date(self@database$dat_recloc),
                                     as.Date(self@database$dat_recreg) ,
                                     as.Date(self@database$dat_meta),
                                     as.Date(self@database$dat_last_news_censor), na.rm=T)
              dat_rfs       <- fifelse(status_rfs_diag==1,dat_rfs_util,as.Date(NA))
            }
            
            #Delay rfs diag
            if(one_missing(c("dat_bc_diagnosis"),self@database,list(dat_rfs))){
              delay_rfs_diag  = self@database$delay_rfs_diag
            }else{
              delay_rfs_diag  = round(as.numeric(as.Date(dat_rfs_util) - as.Date(self@database$dat_bc_diagnosis))/30.4375,1)
            }
            
            #Delay rfs
            if(one_missing(c("dat_first_cancer_surg"),self@database,list(dat_rfs))){
              delay_rfs  = self@database$delay_rfs
            }else{
              delay_rfs  = round(as.numeric(as.Date(dat_rfs_util) - as.Date(self@database$dat_first_cancer_surg))/30.4375,1)
            }
            

            ###########################################################
            ####################     DRFS   ###########################
            ###########################################################
            #Status drfs diag
            if(one_missing(c("ev_meta","status_vital"),self@database)){
              status_drfs_diag <- self@database$status_drfs_diag
            }else{
              status_drfs_diag              <- rep(NA,nrow(self@database))
              status_drfs_diag[unique(c(which(self@database$ev_meta == 1),
                                  which(self@database$status_vital == 1)))] <- 1
              status_drfs_diag[Reduce(intersect,list(
                                          which(self@database$ev_meta == 0),
                                          unique(c(which(self@database$status_vital == 0),which(is.na(self@database$status_vital))))
                                          )
                                        )] <- 0
            }
            
            #Status drfs 
            if(one_missing(c("breast_surgery"),self@database,list(status_drfs_diag))){
              status_drfs <- self@database$status_drfs
            }else{
              status_drfs <- status_drfs_diag
              status_drfs[which(self@database$breast_surgery == 0)] <- NA 
            }
            
            #Delay drfs
            if(one_missing(c("dat_meta","dat_last_news_censor"),self@database)){
              dat_drfs_util  <- pmin(self@database$dat_drfs,self@database$dat_last_news_censor,na.rm=T)
              dat_drfs       <- self@database$dat_drfs
            }else{
              dat_drfs_util  <- pmin (as.Date(self@database$dat_meta),
                                      as.Date(self@database$dat_last_news_censor), na.rm=T)
              dat_drfs       <- fifelse(status_drfs_diag==1,dat_drfs_util,as.Date(NA))
            }
            
            if(one_missing(c("dat_bc_diagnosis"),self@database,list(dat_drfs))){
              delay_drfs_diag  = self@database$delay_drfs_diag
            }else{
              delay_drfs_diag  = round(as.numeric(as.Date(dat_drfs_util) - as.Date(self@database$dat_bc_diagnosis))/30.4375,1)
            }
            
            if(one_missing(c("dat_first_cancer_surg"),self@database,list(dat_drfs))){
              delay_drfs  = self@database$delay_drfs
            }else{
              delay_drfs  = round(as.numeric(as.Date(dat_drfs_util) - as.Date(self@database$dat_first_cancer_surg))/30.4375,1)
            }
            
            ###########################################################
            ####################     DFS   ###########################
            ###########################################################
            #Status dfs
            if(one_missing(c("ev_recloc","ev_recreg","ev_meta","ev_contro","ev_secondk","status_vital","breast_surgery"),self@database)){
              status_dfs <- self@database$status_dfs
            }else{
              status_dfs            <- rep(NA,nrow(self@database))
              status_dfs[unique(c(which(self@database$ev_recloc == 1),
                                  which(self@database$ev_recreg == 1),
                                  which(self@database$ev_meta == 1),
                                  which(self@database$ev_contro == 1),
                                  which(self@database$ev_secondk == 1),
                                  which(self@database$status_vital == 1)))] <- 1
              status_dfs[Reduce(intersect,list(
                  which(self@database$ev_recloc == 0),
                  which(self@database$ev_recreg == 0),
                  which(self@database$ev_meta == 0),
                  which(self@database$ev_contro == 0),
                  which(self@database$ev_secondk == 0),
                  unique(c(which(self@database$status_vital == 0),which(is.na(self@database$status_vital))))
              )
              )] <- 0
              status_dfs[which(self@database$breast_surgery == 0)] <- NA 
            }
            
            #Delay dfs
            if(one_missing(c("dat_recloc","dat_recreg","dat_meta","dat_contro","dat_secondk","dat_last_news_censor"),self@database)){
              dat_dfs_util  <- pmin(self@database$dat_dfs,self@database$dat_last_news_censor,na.rm=T)
              dat_dfs       <- self@database$dat_dfs
            }else{
              dat_dfs_util  <- pmin (as.Date(self@database$dat_recloc),
                                     as.Date(self@database$dat_recreg),
                                     as.Date(self@database$dat_meta),
                                     as.Date(self@database$dat_contro),
                                     as.Date(self@database$dat_secondk),
                                     as.Date(self@database$dat_last_news_censor), na.rm=T)
              dat_dfs       <- fifelse(status_dfs ==1,dat_dfs_util,as.Date(NA))
            }
            
            if(one_missing(c("dat_first_cancer_surg"),self@database,list(dat_dfs))){
              delay_dfs  = self@database$delay_dfs
            }else{
              delay_dfs  = round(as.numeric(as.Date(dat_dfs_util) - as.Date(self@database$dat_first_cancer_surg))/30.4375,1)
            }
            
            
            ###########################################################
            ####################     DSS   ############################
            ###########################################################
            
            #Status dss diag 
            if(one_missing(c("cause_death","status_vital"),self@database)){
              status_dss_diag <- self@database$status_dss_diag
            }else{
              status_dss_diag <- rep(NA,nrow(self@database))
              status_dss_diag[self@database$cause_death == 2 | self@database$status_vital == 0] <- 0
              status_dss_diag[self@database$cause_death == 1] <- 1
            }
            
            #Status dss manquant
            #TODO : dat_dss and delay_dss_diag manquant et delay_dss
            if(one_missing(c("breast_surgery"),self@database,list(status_dss_diag))){
              status_dss <- self@database$status_dss
            }else{
              status_dss <- status_dss_diag
              status_dss[which(self@database$breast_surgery == 0)] <- NA 
            }
            
            #Dat DSS
            if(one_missing(c("dat_last_news_censor"),self@database,list(status_dss_diag))){
              dat_dss_util  <- self@database$dat_dss 
              dat_dss       <- self@database$dat_dss
            }else{
              dat_dss_util  <- self@database$dat_last_news_censor
              dat_dss       <- fifelse(status_dss_diag==1,dat_dss_util,as.Date(NA))
            }
            
            #Delay DSS diag
            if(one_missing(c("dat_bc_diagnosis"),self@database,list(dat_dss))){
              delay_dss_diag  = self@database$delay_dss_diag
            }else{
              delay_dss_diag  = round(as.numeric(as.Date(dat_dss_util) - as.Date(self@database$dat_bc_diagnosis))/30.4375,1)
            }
            
            #Delay DSS
            if(one_missing(c("dat_first_cancer_surg"),self@database,list(dat_dss))){
              delay_dss  = self@database$delay_dss
            }else{
              delay_dss  = round(as.numeric(as.Date(dat_dss_util) - as.Date(self@database$dat_first_cancer_surg))/30.4375,1)
            }
            

            ###########################################################
            ####################     OS    ############################
            ###########################################################
            #Delay os 
            if(one_missing(c("dat_last_news_censor","status_vital"),self@database)){
              dat_os  = self@database$dat_os
            }else{
              dat_os = fifelse(self@database$status_vital==1,self@database$dat_last_news_censor,as.Date(NA))
            }
            
            if(one_missing(c("dat_last_news_censor","dat_bc_diagnosis"),self@database)){
              delay_os_diag  = self@database$delay_os_diag
            }else{
              delay_os_diag  = round(as.numeric(as.Date(self@database$dat_last_news_censor) - as.Date(self@database$dat_bc_diagnosis))/30.4375,1)
            }
            
            if(one_missing(c("dat_last_news_censor","dat_first_cancer_surg"),self@database)){
              delay_os = self@database$delay_os
            }else{
              delay_os = round(as.numeric(as.Date(self@database$dat_last_news_censor) - as.Date(self@database$dat_first_cancer_surg))/30.4375,1)
            }
            

            

            ###########################################################
            ####################  RETURN   ############################
            ###########################################################
            return(
              list(
                status_efs_diag = as.numeric(status_efs_diag),
                status_efs_diag_txt = factor(status_efs_diag, levels = c(0,1), labels = c("No","Yes")),
                dat_efs = dat_efs,
                delay_efs_diag = delay_efs_diag,
                
                status_rfs =  as.numeric(status_rfs),
                status_rfs_txt = factor(status_rfs, levels = c(0,1), labels = c("No","Yes")),
                status_rfs_diag =  as.numeric(status_rfs_diag),
                status_rfs_diag_txt = factor(status_rfs_diag, levels = c(0,1), labels = c("No","Yes")),
                dat_rfs = dat_rfs,
                delay_rfs = delay_rfs,
                delay_rfs_diag = delay_rfs_diag,
                
                status_dfs =  as.numeric(status_dfs),
                status_dfs_txt = factor(status_dfs, levels = c(0,1), labels = c("No","Yes")),
                dat_dfs = dat_dfs,
                delay_dfs = delay_dfs,
                
                status_drfs_diag = as.numeric(status_drfs_diag),
                status_drfs_diag_txt = factor(status_drfs_diag, levels = c(0,1), labels = c("No","Yes")),
                status_drfs =  as.numeric(status_drfs),
                status_drfs_txt = factor(status_drfs, levels = c(0,1), labels = c("No","Yes")),
                dat_drfs = dat_drfs,
                delay_drfs_diag = delay_drfs_diag,
                delay_drfs = delay_drfs,

                status_dss_diag =  as.numeric(status_dss_diag),
                status_dss_diag_txt = factor(status_dss_diag, levels = c(0,1), labels = c("No","Yes")),
                status_dss =  as.numeric(status_dss),
                status_dss_txt = factor(status_dss, levels = c(0,1), labels = c("No","Yes")),
                dat_dss = dat_dss,
                delay_dss_diag = delay_dss_diag,
                delay_dss = delay_dss,
               
                delay_os_diag = delay_os_diag,
                delay_os = delay_os,
                dat_os = dat_os
              ))
          }
)

setGeneric(name="preprocessing_fertility_pregnancy_diag",
           def=function(self)
           {
             standardGeneric("preprocessing_fertility_pregnancy_diag")
           }
)

setMethod(f="preprocessing_fertility_pregnancy_diag",
          signature="Preprocessing",
          definition=function(self)
          {
            
            if(one_missing(c("preg_dg","post_partum_dg"),self@database)){
              obst_cond_dg = self@database$obst_cond_dg
            }else{
              obst_cond_dg = ifelse(as.numeric(as.character(self@database$preg_dg)) == 1 | 
                                         as.numeric(as.character(self@database$post_partum_dg)) == 1,
                                         1,
                                         0)
            }
            
            return(
              list(
                obst_cond_dg = factor(obst_cond_dg)
              ))
          }
)

setGeneric(name="update",
           def=function(self,cols)
           {
             standardGeneric("update")
           }
)

setMethod(f="update",
          signature="Preprocessing",
          definition=function(self,cols)
          {
            for (i in 1:length(cols)){
              colnames(self@database)[which(colnames(self@database)==names(cols)[i])] <- paste(names(cols)[i],"old" , sep="_")
              self@database[,names(cols)[i]] <- cols[i]
            }
            return(self)
          }
)
