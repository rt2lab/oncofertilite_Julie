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



MappingCanto <- setClass(
  "MappingCanto",

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "Canto",
    database = read.csv('/Users/nadir/Desktop/projects/postdoc/DATABASES/databases/core/04_canto/data/preprocessing_before_comedications.csv')
  ),
  
  contains = "Mapping"
)


setMethod(f="mapping_patient_id",
          signature="MappingCanto",
          definition=function(self)
          {
            center = unlist(lapply(self@database$Center, function(x){if(is.na(x)) NA else{if(x=="INSTITUT CURIE SITE DE PARIS" | x=="INSTITUT CURIE HÔPITAL RENÉ HUGUENIN ") 1 else 2}}))
            
            return(list(
              database = rep(4,nrow(self@database)),
              refusal_data_use = rep(NA,nrow(self@database)),
              cletri = self@database$SUBJID,
              side  = factor(self@database$LOC, levels = c(1,2), labels = c(1,2)),
              dat_birth = as.Date(self@database$DT_Birth),
              dat_bc_diagnosis = as.Date(self@database$DT_Diagnosis),
              center = center
            ))
          }
)

setMethod(f="mapping_patient_char",
          signature="MappingCanto",
          definition=function(self)
          {
            fam_history1 = sapply(self@database$NB_Hist_Breast_Cancer1, function(x){if(is.na(x)) NA else{if(x==0) 0 else 1}})
            brca_mut = self@database$MUT1
            brca_mut[which(self@database$BRCA1 %in% c(1) | self@database$BRCA2 %in% c(1))] = 1
            brca_mut[which(self@database$MUT1 %in% c(1) & !self@database$BRCA1 %in% c(1) & !self@database$BRCA2 %in% c(1))] = 0 
            
            brca_1_2_mut = rep(NA, nrow(self@database))
            brca_1_2_mut[brca_mut %in% c(0)] = 0
            brca_1_2_mut[self@database$BRCA1 %in% c(1) & !self@database$BRCA2 %in% c(1) & !self@database$OTHMUT %in% c(1)] = 1
            brca_1_2_mut[self@database$BRCA2 %in% c(1) & !self@database$BRCA1 %in% c(1) & !self@database$OTHMUT %in% c(1)] = 2
            brca_1_2_mut[!self@database$BRCA1 %in% c(1) & !self@database$BRCA2 %in% c(1) & self@database$OTHMUT %in% c(1)] = 3
            
            smoking_3cl = factor(self@database$TABAC_BI, levels=c(1,2,3),labels = c(2,3,1))
            
            return(list(
              age_menarche       = as.numeric(self@database$AGEREGLE_BI),
              nb_preg            = as.numeric(self@database$NBGROSS),
              nb_child           = as.numeric(self@database$NBENFANT),
              breast_feed        = factor(self@database$ALLAIT, c(0,1), labels = c(0,1)),
              menop              = factor(self@database$HORMSTA,levels=c(1,2),labels = c(0,1)),
              age_menop          = as.numeric(self@database$AGEMENOP), 
              hrt                = factor(self@database$THS, levels=c(0,1),labels = c(0,1)),
              fam_history        = factor(fam_history1, levels=c(0,1),labels = c(0,1)),
              brca_screen        = factor(self@database$MUT1, levels = c(0,1), labels = c(0,1)),
              brca_mut           = brca_mut,
              brca_1_2_mut       = brca_1_2_mut,
              weight             = as.numeric(self@database$POIDS_BI),
              size               = as.numeric(self@database$TAILLE/100),
              smoking_3cl        = smoking_3cl,
              smoking            = factor(regroup(smoking_3cl, list(c(1,3), c(2)), c(0, 1))),
              drinking_alcohol   = factor(self@database$ALCOOL_BI, levels=c(0,1), labels = c(0,1)),
              charlson_indx      = as.numeric(self@database$CHARLSON),
              hosp_psy           = factor(self@database$PSYHOSP_BI, levels=c(0,1), labels = c(0,1))
            ))
          }
)

# DONE
setMethod(f="mapping_comedication",
          signature="MappingCanto",
          definition=function(self)
          {

            # one_comedic = unlist(lapply(rowSums(self@database[, paste("ATC", c("A","B","C","D","G","H","L","M","N","P","R","S","V"), sep = "_")]), function(x){if(is.na(x)) NA else{if(x==0) 0 else 1}}))
            # comedic_others = unlist(lapply(rowSums(self@database[, paste("ATC", c("B","D","G","L","M","P","R","S","V"), sep = "_")]), function(x){if(is.na(x)) NA else{if(x==0) 0 else 1}}))
            # 
            return(list(
              # comedic = factor(one_comedic),
              # comedic_n_nervous_system = factor(self@database$ATC_N),
              # comedic_c_cardiovascular = factor(self@database$ATC_C),
              # comedic_a_alimentary_metabo = factor(self@database$ATC_A),
              # comedic_h_hormonal_prep = factor(self@database$ATC_H),
              # comedic_others = comedic_others
            ))
          }
)

# TO DO, we do not have these variables in CANTO, but many others, mapping in CANTO still TO DO
setMethod(f="mapping_comorbidity",
          signature="MappingCanto",
          definition=function(self)
          {
#             
#             one_comedic = unlist(lapply(rowSums(data.work[, paste("ATC", c("A","B","C","D","G","H","L","M","N","P","R","S","V"), sep = "_")]), function(x){if(is.na(x)) NA else{if(x==0) 0 else 1}}))
#             
            return(list(
#               comorbidity_bin = ,
#               comor_hypertension_heart_disease = factor(self@database$HYPERTENS_M0),
#               comor_depression_anxiety = factor(self@database$),
#               comor_dyslipidemia = factor(self@database$),
#               comor_diabete = 
#               comor_ulcere_gastritis = 
            ))
          }
)

# inflammatory_BC, moddiag, er_intensity MISSING?
setMethod(f="mapping_bc_diagnosis",
          signature="MappingCanto",
          definition=function(self)
          {
 
           return(list(
              bilat_bc = factor(sapply(self@database$LOC, function(x){if(x==3) 1 else 0})),
              # moddiag           = factor(self@database$CIRCDIAG, levels = c("radiologique","clinique"),labels=c(0,1)),
              multifocality_clin  = factor(self@database$form, levels = c("unifocal","multifocal"),labels=c(0,1)),
              tclin               = as.numeric(self@database$tailleclin),
              ctuicc_5cl           = factor(self@database$ctuicc_5cl, c("T0","T1","T2","T3","T4"), c(0,1,2,3,4)), 
              cnuicc_4cl           = factor(self@database$cnuicc_4cl, c("N0","N1","N2","N3"), c(0,1,2,3)),
              muicc               = factor(rep(0,nrow(self@database)))
              # dat_first_biopsy    = NA
             
            
            ))
          }
)



setMethod(f="mapping_bc_biology",
          signature="MappingCanto",
          definition=function(self)
          {
            her2_status                                                                                                <- NA
            her2_status[self@database$HERIHC == "+++" | self@database$HERA == "oui"]                                   <- 1
            her2_status[self@database$HERIHC == "+" | self@database$HERIHC == "negatif" |self@database$HERA == "non"]  <- 0
            
            return(list(
              er_status           = factor(self@database$ER_status, levels = c("negative", "positive"), labels=c(0,1)),
              pr_status           = factor(self@database$PR_status, levels = c("negative", "positive"), labels=c(0,1)),
              er_intensity      = as.numeric(self@database$er_intensity),
              pr_intensity        = as.numeric(self@database$pr_intensity),
              er_percentage       = as.numeric(self@database$er_percentage),
              pr_percentage       = as.numeric(self@database$pr_percentage),
              her2_status         = factor(self@database$Her2_status, levels = c("negative", "positive"), labels=c(0,1)),
              # histo_5cl          = regroup(self@database$CIMO1,list(c("85003"),c("85203"),c("84803"),c("82113")),c(1,2,3,4),base=9), #???
              histo_3cl           = factor(self@database$histo_3cl_biop, levels = c("NST", "lobular", "others"), labels=c(1,2,9)),
              grade_3cl           = factor(self@database$grade_3cl_biop,c("Grade I","Grade II","Grade III"),c(1,2,3)), 
              ki67_perc           = as.integer(self@database$Ki67PERC),
              mitotic_index_cl = as.integer(self@database$index_mitotic_biop), # is it already in classes
              dcis_component      = factor(self@database$dcis_biop, levels = c("no","yes"), labels=c(0,1)),   ## CONTROLLER
              invasive_or_dcis    = factor(self@database$inv_or_insitu_biop, levels = c("invasive","dcis"), labels=c(1,2))
            ))
          }
)

# DONE
setMethod(f="mapping_surgery",
          signature="MappingCanto",
          definition=function(self)
          {
            
            breast_surgery_3cl <- rep(NA, nrow(self@database))
            breast_surgery_3cl[which(self@database$surgery == 'no')] = 9
            breast_surgery_3cl[which(self@database$breast_surgery_cl == 'lumpectomy')] = 1
            breast_surgery_3cl[which(self@database$breast_surgery_cl == 'mastectomy')] = 2
            
            axillary_surgery_4cl = factor(self@database$axillary_surgery, levels = c("SNB","AND", "BOTH", "NONE"), labels=c(1,2,3,9))

            return(list(
              breast_surgery        = factor(self@database$surgery, levels = c('no', 'yes'), labels = c(0,1)), 
              breast_surgery_3cl   = factor(breast_surgery_3cl),
              dat_first_breast_surg       = as.Date(self@database$date_surg), 
              axillary_surgery_4cl = axillary_surgery_4cl,
              dat_first_axillar_surg       = as.Date(self@database$date_first_axillar_surg), 
              comp_post_surg       = factor(self@database$comp_post_surg, levels = c("no","yes"), labels=c(0,1))
              # for reconstruction we only have immediate reconstruction
            ))
          }
)

# DONE
setMethod(f="mapping_treatments_binary",
          signature="MappingCanto",
          definition=function(self)
          {
            ht_type = rep(NA, nrow(self@database))
            ht_type[which(self@database$HORM1 %in% c(1))] = 1
            ht_type[which(self@database$HORM1 %in% c(1) & self@database$HORM2 %in% c(5))] = 3
            # ht_type[which(self@database$HORM1 %in% c(1) & (self@database$HORM2 %in% c(2) | self@database$HORM2 %in% c(3) | self@database$HORM2 %in% c(4)))] = 1
            
            
            return(list(
                ct               =  factor(self@database$CT, levels = c("no","yes"), labels=c(0,1)),
                dat_first_ct     =  pmin(as.Date(self@database$DTDEBCYCLE_ADJ),as.Date(self@database$DTDEBCYCLE_NEO), na.rm = T),
                dat_end_first_ct =  pmax(as.Date(self@database$DTFINCYCLE_ADJ),as.Date(self@database$DTFINCYCLE_NEO), na.rm = T),
                rt               =  factor(self@database$rt, levels = c("no","yes"), labels=c(0,1)),
                dat_first_rt     =  as.Date(self@database$dat_first_rt,format = "%Y-%m-%d"),
                ht               =  factor(self@database$ht, levels = c("no","yes"), labels=c(0,1)),
                dat_first_ht     =  as.Date(self@database$date_horm1,format = "%Y-%m-%d"),
                ht_type_5cl      =  factor(self@database$ht_type_5cl, levels = c('tamoxifen', 'aromatase inhibitor', 'tamoxifen + agonist', 'aromatase inhibitor + agonist', 'others'), labels = c(1,2,3,4,5)),
                antiher2         =  factor(self@database$trastu, levels = c("no","yes"), labels=c(0,1)),
                dat_first_antiher2 = as.Date(self@database$DTDEBHERCEP,format = "%Y-%m-%d"),
                tc_other         =  factor(self@database$other_target_therapy, levels = c("no","yes"), labels=c(0,1)),
                dat_first_tc_other = as.Date(self@database$DTDEBCIBLE1,format = "%Y-%m-%d")
             ))
          }
)


mapping_neoadj_or_not_f <- function(data){
  date_chir = as.Date(data$date_surg)
  neo_ht = factor(sapply(1:nrow(data), function(i){if(is.na(data$ht[i])){NA}else{if(data$ht[i]==0){0}else{if(is.na(date_chir[i]) | is.na(data$dat_first_ht[i])) NA else {if(data$dat_first_ht[i] < date_chir[i]) 1 else 0 } } } } ))
  neo_rt = factor(sapply(1:nrow(data), function(i){if(is.na(data$rt[i])){NA}else{if(data$rt[i]==0){0}else{if(is.na(date_chir[i]) | is.na(data$dat_first_rt[i])) NA else {if(data$dat_first_rt[i] < date_chir[i]) 1 else 0 } } } }))
  neo_antiher2 = factor(sapply(1:nrow(data), function(i){if(is.na(data$antiher2[i])){NA}else{if(data$antiher2[i]==0){0}else{if(is.na(date_chir[i]) | is.na(data$dat_first_antiher2[i])) NA else {if(data$dat_first_antiher2[i] < date_chir[i]) 1 else 0 }}  } }))
  neo_tc_other = factor(sapply(1:nrow(data), function(i){if(is.na(data$antiher2[i])){NA}else{if(data$antiher2[i]==0){0}else{if(is.na(date_chir[i]) | is.na(data$dat_first_tc_other[i])) NA else {if(data$dat_first_tc_other[i] < date_chir[i]) 1 else 0 }}  } }))
  dat_first_neo_ct = sapply(1:nrow(data), function(i){if(data$NAC_bin[i]=='no') NA else as.character(data$DTDEBCYCLE_NEO[i]) })
  dat_first_neo_ht = sapply(1:nrow(data), function(i){if(neo_ht[i]==0 | is.na(neo_ht[i])) NA else as.character(data$date_horm1[i]) })
  dat_first_neo_rt = sapply(1:nrow(data), function(i){if(neo_rt[i]==0 | is.na(neo_rt[i])) NA else as.character(data$dat_first_rt[i]) })
  dat_first_neo_antiher2 = sapply(1:nrow(data), function(i){if(neo_antiher2[i]==0 | is.na(neo_antiher2[i])) NA else as.character(data$DTDEBHERCEP[i]) })
  dat_first_neo_tc_other = sapply(1:nrow(data), function(i){if(neo_tc_other[i]==0 | is.na(neo_tc_other[i])) NA else as.character(data$DTDEBCIBLE1[i]) })
  
  return(list(
            neo_ct = factor(data$NAC_bin, levels = c("no","yes"), labels=c(0,1)),
            neo_ht = neo_ht, 
            neo_rt = neo_rt, 
            neo_antiher2 = neo_antiher2,
            neo_tc_other = neo_tc_other, 
            dat_first_neo_ct = as.Date(dat_first_neo_ct),
            dat_first_neo_ht = as.Date(dat_first_neo_ht),
            dat_first_neo_rt = as.Date(dat_first_neo_rt),
            dat_first_neo_antiher2= as.Date(dat_first_neo_antiher2), 
            dat_first_neo_tc_other= as.Date(dat_first_neo_tc_other)))
  
}


setMethod(f="mapping_neoadj_or_not",
          signature="MappingCanto",
          definition=function(self)
          {
            l = mapping_neoadj_or_not_f(self@database)

            return(
              l
            )
          }
)

# we do not have the number of cycles for anthra and taxanes separately
# target therapy is not expicitly stated, we even have things like "PERTUSUMAB OU SON PLACEBO "
setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingCanto",
          definition=function(self)
          {
            nb_cycles_neo_ct_taxanes = rep(0, nrow(self@database))
            nb_cycles_neo_ct_anthra = rep(0, nrow(self@database))
            
            vars1 = c('DROGUE1_NEO','DROGUE2_NEO','DROGUE3_NEO','DROGUE4_NEO','DROGUE5_NEO', 'DROGUE6_NEO')
            
         
            for(p in 1:nrow(self@database)){
              done = c()
              for(v in vars1){
                ch = substr(v, 7,7)
                
                if(!(self@database[p,v] %in% c(1,2,3) & "A" %in% done)){
                  for(i in 1:8){
                    st = paste0("DOSE", ch, i, "_NEO")
                  
                    if(self@database[p,v] %in% c(1,2,3)){
                      done = c("A")
                      if(!is.na(self@database[p,st]))
                        nb_cycles_neo_ct_anthra[p] = nb_cycles_neo_ct_anthra[p] + 1
                    }
                    if(self@database[p,v] %in% c(5)){
                      if(!is.na(self@database[p,st]))
                        nb_cycles_neo_ct_taxanes[p] = nb_cycles_neo_ct_taxanes[p] + 1
                    }
                    if(self@database[p,v]  %in% c(6) & self@database$OTHSEQ1_NEO[p] %in% c("TAXOL", "TAXOTERE + 1 TAXOL (PACLITAXEL)","TAXOL","TAXOL - CAPECITABINE",
                                                   "TAXOL HEBDO","TAXOL HEBDOMADAIRE","TAXOL SUITE INTOLERANCE TAXOTERE",
                                                   "TAXOTERE - TAXOL HEBDOMADAIRE","TAXOTERE PUIS TAXOL")){
                      if(!is.na(self@database[p,st]))
                        nb_cycles_neo_ct_taxanes[p] = nb_cycles_neo_ct_taxanes[p] + 1
                    }
                    if(self@database[p,v] %in% c(4)){
                      if(!is.na(self@database[p,st])){
                        nb_cycles_neo_ct_taxanes[p] = nb_cycles_neo_ct_taxanes[p] + 1
                        nb_cycles_neo_ct_anthra[p] = nb_cycles_neo_ct_anthra[p] + 1
                      }
                    }
                  }
                }
              }
            }
            
             return(list(
               neo_ct_regimen = factor(self@database$NAC_regimen, levels = c("anthra-taxans","anthra", "taxanes", "others"), labels = c(1,2,3,4)),
               nb_cycles_neo_ct = as.numeric(self@database$NBCYCLE_NEO),
               # nb_cycles_neo_ct_taxanes = ,
               # nb_cycles_neo_ct_anthra,
               dat_end_neo_ct = self@database$DTFINCYCLE_NEO,
               neo_ct_sequence = factor(self@database$SEQCHIMIO_NEO),
               neo_reduc_dos = factor(self@database$REDUCDOSE_NEO),
               neo_gcsf = factor(self@database$FACTCROISS_NEO)
               # regimen of target therapy is not mapped but present in CRF
            ))
          }
)


# we do not have the number of cycles for anthra and taxanes separately
# adj_antiher2 to do
setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingCanto",
          definition=function(self)
          {
            return(list(
              adj_ct = factor(self@database$CHIMIOADJ),
              adj_ct_regimen = factor(self@database$adj_regimen, levels = c("anthra-taxans","anthra", "taxanes", "others"), labels = c(1,2,3,4)),
              # nb_cycles_adj_ct_taxanes = 
              # nb_cycles_adj_ct_anthra = 
              adj_ct_sequence = factor(self@database$SEQCHIMIO_ADJ),
              nb_cycles_adj_ct = as.numeric(self@database$NBCYCLE_ADJ),
              dat_first_adj_ct = as.Date(self@database$DTDEBCYCLE_ADJ),
              dat_end_adj_ct = as.Date(self@database$DTFINCYCLE_ADJ),
              reduc_dos_adj = factor(self@database$REDUCDOSE_ADJ),
              gcsf_adj = factor(self@database$FACTCROISS_ADJ),
              adj_antiher2 = factor(self@database$trastu_adj, levels = c("no", "yes"), labels = c(0,1)),
              dat_first_adj_antiher2 = self@database$dat_first_adj_antiher2
            ))
          }
)

setMethod(f="mapping_treatments",
          signature="MappingCanto",
          definition=function(self)
          {
            
            return(list(
              reduc_dos_tz = factor(self@database$MODIFHERCEP),
              stop_tz = factor(self@database$INTHERCEP)
            ))
          }
)

# only for patients without NAC treatment
setMethod(f="mapping_tumor_char_surg",
          signature="MappingCanto",
          definition=function(self)
          {
            gang_pos_only_chir = rep(NA, nrow(self@database))
            gang_pos_only_chir[which(self@database$LOC %in% c(1))] <- self@database$ENVAHI_G_M0[which(self@database$LOC %in% c(1))]
            gang_pos_only_chir[which(self@database$LOC %in% c(2))] <- self@database$ENVAHI_D_M0[which(self@database$LOC %in% c(2))]
            gang_pos_only_chir[which(self@database$NAC_bin=="yes")] = NA
            
            n_gang_pres_only_chir = rep(NA, nrow(self@database))
            n_gang_pres_only_chir[which(self@database$LOC %in% c(1))] <- self@database$PRELEV_G_M0[which(self@database$LOC %in% c(1))]
            n_gang_pres_only_chir[which(self@database$LOC %in% c(2))] <- self@database$PRELEV_D_M0[which(self@database$LOC %in% c(2))]
            n_gang_pres_only_chir[which(self@database$NAC_bin=="yes")] = NA
            
            histo_size = as.integer(self@database$taillehisto)
            histo_size[which(self@database$NAC_bin=="yes")] = NA
            
            ptuicc_5cl = factor(self@database$pT)
            ptuicc_5cl[which(self@database$NAC_bin=="yes")] = NA
            #embols     = factor(self@database$EMBV,levels = c("non","oui"), labels = c(0,1))
            #embols[which(self@database$NAC_bin=="yes")] = NA
            multifocal_surg = factor(self@database$multifocal_surg, levels = c("no","yes"), labels = c(0,1))
            multifocal_surg[which(self@database$NAC_bin=="yes")] = NA
            
            return(list(
              nbggpos = gang_pos_only_chir,
              nbggprel = n_gang_pres_only_chir,
              histo_size = histo_size,
              ptuicc_5cl = ptuicc_5cl,
              multifocality_histo = multifocal_surg
            ))
          }
)

# lvi and TILs missing (not in CRF)
setMethod(f="mapping_tumor_char_neo",
          signature="MappingCanto",
          definition=function(self)
          {
            gang_pos_only_NAC = rep(NA, nrow(self@database))
            gang_pos_only_NAC[which(self@database$LOC %in% c(1))] <- self@database$ENVAHI_G_M0[which(self@database$LOC %in% c(1))]
            gang_pos_only_NAC[which(self@database$LOC %in% c(2))] <- self@database$ENVAHI_D_M0[which(self@database$LOC %in% c(2))]
            gang_pos_only_NAC[which(self@database$NAC_bin=="no")] = NA
            
            n_gang_pres_only_NAC = rep(NA, nrow(self@database))
            n_gang_pres_only_NAC[which(self@database$LOC %in% c(1))] <- self@database$PRELEV_G_M0[which(self@database$LOC %in% c(1))]
            n_gang_pres_only_NAC[which(self@database$LOC %in% c(2))] <- self@database$PRELEV_D_M0[which(self@database$LOC %in% c(2))]
            n_gang_pres_only_NAC[which(self@database$NAC_bin=="no")] = NA
            
            
            return(list(
              #breast_res_insitu    = factor(self@database$breast_res_insitu, levels = c("no","yes"), labels = c(0,1)),
              #breast_res_infiltr   = factor(self@database$breast_res_infiltr_tmp, levels = c("no","yes"), labels = c(0,1)),
              # ypnuicc_2cl         = factor(self@database$gang_post_chimio),
              pcr                   = factor(self@database$pCR, levels = c("no","yes"), labels = c(0,1)),
              nbggpos_postneo       = gang_pos_only_NAC,
              nbggprel_postneo      = n_gang_pres_only_NAC,
              mitotic_index_postneo = as.numeric(self@database$mitotic_index_postneo)
            ))
          }
)

setMethod(f="mapping_events_and_censor",
          signature="MappingCanto",
          definition=function(self)
          {
            # dat_censor_database = NA
            # dat_last_update = NA
            
          #   ev_prog_neo    <- 
          #   dat_prog_neo   <- 
             ev_recloc      <- factor(self@database$LOCRECUR)
             dat_recloc     <- as.Date(self@database$LOCRECURDATE)
             ev_recreg      <- factor(self@database$GANGRECUR)
             dat_recreg     <- as.Date(self@database$GANGRECURDATE)
             ev_meta        <- factor(self@database$METAST)
             dat_meta       <- as.Date(self@database$METASTAPPDATE)
             ev_contro      <- factor(self@database$CONTROLOC)
             dat_contro     <- as.Date(self@database$CONTROLOCAPPDATE)
             ev_secondk     <- factor(self@database$SECONDCANC)
             dat_secondk    <- as.Date(self@database$SECONDCANCDIAGDATE)
             status_vital   <- factor(self@database$DEAD, levels = c(0, 1), labels=c(0,1)) 
             # cause_death
             dat_last_news  <- as.Date(self@database$DTDERNNOUV)
             # ev_prog_neo[which(dat_prog_neo > dat_censor)] <- 0
             # ev_recloc[which(dat_recloc > dat_censor)]     <- 0
             # ev_recreg[which(dat_recreg > dat_censor)]     <- 0
             # ev_meta[which(dat_meta > dat_censor)]         <- 0
             # ev_contro[which(dat_contro > dat_censor)]     <- 0
             # ev_deuxk[which(dat_deuxk > dat_censor)]       <- 0
             # status_vital[which(dat_last_news > dat_censor & status_vital==1 )]       <- 0
             
             return(list(
             #   ev_prog_neo = ev_prog_neo,
             #   dat_prog_neo = dat_prog_neo,
                ev_recloc = ev_recloc,
                dat_recloc = dat_recloc,
                ev_recreg = ev_recreg,
                dat_recreg = dat_recreg,
                ev_meta = ev_meta,
                dat_meta = dat_meta,
                ev_contro = ev_contro,
                dat_contro = dat_contro,
                ev_secondk = ev_secondk,
                dat_secondk = dat_secondk,
                status_vital = status_vital,
                dat_last_news = dat_last_news
            ))
          }
)

setMethod(f="mapping_evol",
          signature="MappingCanto",
          definition=function(self)
          {

            return(list(

            ))
          }
)
