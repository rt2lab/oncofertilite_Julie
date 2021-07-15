#Util functions
#Two functions which may be useful in the mapping process

###################################################################
#Function n1 : na_factor
#Transform a column into a factor with specified NA values
#Input : col : a column, 
#        na_string : the list of strings to be transformed into NA
#        levels : the list of levels (optional)
#        labels : the list of labels (optional)
# Output : col : a factor column
#Example of use: 
#col_to_test = c(rep("Not specified",10),rep(5,15),rep(0,2),rep("NA",4))
#na_factor(col_to_test,c("Not specified","NA"))
#na_factor(col_to_test,c("Not specified","NA"),labels = c("Zero","Five"))
na_factor <- function(col,na_string,levels = NULL,labels = NULL){
  col[which(col %in% na_string)] <- NA
  print(col)
  levels = if (is.null(levels)) setdiff(unique(col),NA) else levels
  print(levels)
  labels = if (is.null(labels)) setdiff(unique(col),NA) else labels
  print(levels)
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
MappingTabacSurvival <- setClass( #TODO : change the name of the classe (e.g. MappingSein, MappingCanto...)
  "MappingTabacSurvival", #TODO : same

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "31_tabac_survival", #TODO : the name of your base
    #TODO : load your database here e.g.
    database = read.csv(file = file.path( 
        Sys.getenv("PROJECT_PATH"),
        "core/31_tabac_survival/data/Tabacreactualisation_DATA_2021-02-23_1030.csv"
    ), sep = ","),
    #TODO : the date to censor the data (default is no censor)
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
#Beware : you need to return the whole object of class MappingTabacSurvival (self)
setMethod(f="mapping_initial",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            #TODO : Modify self@database
            #Ex :   self@database <- self@database %>% select(-this_column)
            #On ne garde que les variables de survie
            #Survival variable
            self@database <- self@database %>% filter(questionnaire_tabac_complete==2)
            self@database$dat_recreg <- self@database$day_recreg
            surv_var <- c(data_dict$var[data_dict$family_var == "events_and_censor"], "record_id","date_consult", "date_chir")
            self@database <- self@database %>% dplyr::select(one_of(surv_var))
            return(self)
          }
)
###################################################################

###################################################################
#Step 3 : 
#Redefine the methods mapping_family_var

#Patient id
setMethod(f="mapping_patient_id",
          signature="MappingTabacSurvival", #Change here
          definition=function(self)
          {
            #You can right useful codes here
            #Ex : center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3))
            #Your need to return a named list of variables, comma separated
            #If nothing to return : either return an empty list
            # Or do not redefine the function at all. 
            numdos_curie = as.character(formatC(self@database$record_id,width = 7, flag = "0",format="d"))
            return(list(
              # database             = rep(7,nrow(self@database)),
              numdos_curie         = numdos_curie,
              cletri               = numdos_curie,
              # side                 = self@database$side                ,
              # dat_birth            = self@database$dat_birth           ,
              dat_bc_diagnosis     = as.Date(self@database$date_chir, format = "%Y-%m-%d")    
              # dat_rando_inclusion  = self@database$dat_rando_inclusion ,
              # center_curie         = self@database$center_curie        ,
              # center               = self@database$center   
            ))
          }
)

#Patient char
setMethod(f="mapping_patient_char",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # age_menarche        = self@database$age_menarche  ,
              # nb_preg             = self@database$nb_preg       ,
              # nb_child            = self@database$nb_child      ,
              # breast_feed         = self@database$breast_feed   ,
              # menop               = self@database$menop         ,
              # age_menop           = self@database$age_menop     ,
              # hrt                 = self@database$hrt           ,
              # fam_history         = self@database$fam_history   ,
              # brca_screen         = self@database$brca_screen   ,
              # brca_mut            = self@database$brca_mut      ,
              # brca_1_2_mut        = self@database$brca_1_2_mut  ,
              # weight              = self@database$weight        ,
              # size                = self@database$size          ,
              # smoking_3cl         = self@database$smoking_3cl   ,
              # smoking             = self@database$smoking       
            ))
          }
)

#Bc_diagnosis
setMethod(f="mapping_bc_diagnosis",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # bilat_bc            = self@database$bilat_bc           ,
              # inflammatory_BC     = self@database$inflammatory_BC    ,
              # moddiag             = self@database$moddiag            ,
              # multifocality_clin  = self@database$multifocality_clin ,
              # tclin               = self@database$tclin              ,
              # ctuicc_5cl          = self@database$ctuicc_5cl         ,
              # cnuicc_4cl          = self@database$cnuicc_4cl         ,
              # muicc               = self@database$muicc              ,
              # dat_first_biopsy    = self@database$dat_first_biopsy   
            ))
          }
)

#Bc_biology
setMethod(f="mapping_bc_biology",
          signature = "MappingTabacSurvival", #TODO : change to the name of your class
          definition = function(self)
          {
            return(list(
              # er_status            = self@database$er_status        ,
              # pr_status            = self@database$pr_status        ,
              # er_intensity         = self@database$er_intensity     ,
              # pr_intensity         = self@database$pr_intensity     ,
              # er_percentage        = self@database$er_percentage    ,
              # pr_percentage        = self@database$pr_percentage    ,
              # er_allred            = self@database$er_allred        ,
              # pr_allred            = self@database$pr_allred        ,
              # her2_status          = self@database$her2_status      ,
              # histo_5cl            = self@database$histo_5cl        ,
              # grade_3cl            = self@database$grade_3cl        ,
              # ki67_perc            = self@database$ki67_perc        ,
              # mitotic_index        = self@database$mitotic_index    ,
              # dcis_component       = self@database$dcis_component   ,
              # invasive_or_dcis     = self@database$invasive_or_dcis ,
              # p53                  = self@database$p53              ,
              # str_til_perc         = self@database$str_til_perc     ,
              # it_til_perc          = self@database$it_til_perc      ,
              # tumor_cellularity    = self@database$tumor_cellularity,
              # lvi_biop             = self@database$lvi_biop         
            ))
          }
)

#Surgery
setMethod(f="mapping_surgery",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              #breast_surgery_3cl   = self@database$breast_surgery_3cl  ,
              dat_first_breast_surg       = as.Date(self@database$date_chir, format = "%Y-%m-%d")      ,
              dat_first_axillar_surg     =  rep(as.Date(NA), nrow(self@database))
              # axillary_surgery_4cl = self@database$axillary_surgery_4cl,
              # comp_post_surg       = self@database$comp_post_surg      
            ))
          }
)

#Treatments binary
setMethod(f="mapping_treatments_binary",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # ct                   = self@database$ct                   ,
              # dat_first_ct         = self@database$dat_first_ct         ,
              # dat_end_first_ct     = self@database$dat_end_first_ct     ,
              # rt                   = self@database$rt                   ,
              # dat_first_rt         = self@database$dat_first_rt         ,
              # ht                   = self@database$ht                   ,
              # dat_first_ht         = self@database$dat_first_ht         ,
              # ht_type_5cl          = self@database$ht_type_5cl          ,
              # antiher2             = self@database$antiher2             ,
              # dat_first_antiher2   = self@database$dat_first_antiher2   ,
              # tc_other             = self@database$tc_other             ,
              # dat_first_tc_other   = self@database$dat_first_tc_other   
             ))
          }
)

#Neoadj or not
setMethod(f="mapping_neoadj_or_not",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # neo_ct                  = self@database$neo_ct                 ,
              # neo_ht                  = self@database$neo_ht                 ,
              # neo_rt                  = self@database$neo_rt                 ,
              # neo_antiher2            = self@database$neo_antiher2           ,
              # neo_tc_other            = self@database$neo_tc_other           ,
              # dat_first_neo_ct        = self@database$dat_first_neo_ct       ,
              # dat_first_neo_ht        = self@database$dat_first_neo_ht       ,
              # dat_first_neo_rt        = self@database$dat_first_neo_rt       ,
              # dat_first_neo_antiher2  = self@database$dat_first_neo_antiher2 ,
              # dat_first_neo_tc_other  = self@database$dat_first_neo_tc_other 
            ))
          }
)

#Neoadjuvant_ct_antiher2
setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # neo_ct_regimen         = self@database$neo_ct_regimen      ,
              # nb_cycles_neo_ct       = self@database$nb_cycles_neo_ct    ,
              # neo_ct_sequence        = self@database$neo_ct_sequence     ,
              # neo_antiher2_regimen   = self@database$neo_antiher2_regimen
               ))
          }
)

#Adjuvant ct antiher2
setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # adj_ct                   = self@database$adj_ct                  ,
              # adj_ct_regimen           = self@database$adj_ct_regimen          ,
              # nb_cycles_adj_ct_taxanes = self@database$nb_cycles_adj_ct_taxanes,
              # nb_cycles_adj_ct_anthra  = self@database$nb_cycles_adj_ct_anthra ,
              # adj_ct_sequence          = self@database$adj_ct_sequence         ,
              # nb_cycles_adj_ct         = self@database$nb_cycles_adj_ct        ,
              # dat_first_adj_ct         = self@database$dat_first_adj_ct        ,
              # dat_end_adj_ct           = self@database$dat_end_adj_ct          ,
              # reduc_dos_adj            = self@database$reduc_dos_adj           ,
              # gcsf_adj                 = self@database$gcsf_adj                ,
              # adj_antiher2             = self@database$adj_antiher2            ,
              # dat_first_adj_antiher2   = self@database$dat_first_adj_antiher2  
            ))
          }
)

#Tumor char surg
setMethod(f="mapping_tumor_char_surg",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # nbggpos                  = self@database$nbggpos            ,
              # histo_size               = self@database$histo_size         ,
              # ptuicc_5cl               = self@database$ptuicc_5cl         ,
              # lvi                      = self@database$lvi                ,
              # multifocality_histo      = self@database$multifocality_histo
            ))
          }
)

#Tumor char neo
setMethod(f="mapping_tumor_char_neo",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # breast_res_insitu         = self@database$breast_res_insitu        ,
              # breast_res_infiltr        = self@database$breast_res_infiltr       ,
              # nbggpos_postneo           = self@database$nbggpos_postneo          ,
              # lvi_postneo               = self@database$lvi_postneo              ,
              # rcb_index                 = self@database$rcb_index                ,
              # str_til_perc_postneo      = self@database$str_til_perc_postneo     ,
              # it_til_perc_postneo       = self@database$it_til_perc_postneo      ,
              # tumor_cellularity_postneo = self@database$tumor_cellularity_postneo,
              # mitotic_index_postneo     = self@database$mitotic_index_postneo    
            ))
          }
)

#Events and censor
setMethod(f="mapping_events_and_censor",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            
            #8 patientes avec date_consult manquante
            dat_last_update = pmax(as.Date(self@database$dat_last_update, format = "%Y-%m-%d"),
                                   as.Date(as.character(self@database$date_consult), format = "%Y-%m-%d"),
                                   na.rm = T)
            
            dat_last_news = pmax(as.Date(self@database$dat_last_news, format = "%Y-%m-%d"),
                                   as.Date(as.character(self@database$date_consult), format = "%Y-%m-%d"),
                                   na.rm = T)
            
            actu_eva <- self@database %>% 
              dplyr::select("status_vital",colnames(self@database)[grep("ev_", colnames(self@database))]) %>% 
              replace(is.na(.), 0)
            
             return(list(
               # dat_censor_database  = self@database$dat_censor_database,
               dat_last_update      = as.Date(dat_last_update)  ,
               # ev_prog_neo          = self@database$ev_prog_neo        ,
               # dat_prog_neo         = self@database$dat_prog_neo       ,
               ev_recloc            = actu_eva$ev_recloc       ,
               dat_recloc           = as.Date(self@database$dat_recloc, format = "%Y-%m-%d")         ,
               ev_recreg            = actu_eva$ev_recreg        ,
               dat_recreg           = as.Date(self@database$dat_recreg, format = "%Y-%m-%d")         ,
               ev_meta              = actu_eva$ev_meta              ,
               dat_meta             = as.Date(self@database$dat_meta, format = "%Y-%m-%d")           ,
               ev_contro            = actu_eva$ev_contro          ,
               dat_contro           = as.Date(self@database$dat_contro, format = "%Y-%m-%d")        ,
               ev_secondk           = actu_eva$ev_secondk           ,
               dat_secondk          = as.Date(self@database$dat_secondk, format = "%Y-%m-%d")        ,
               status_vital         = actu_eva$status_vital     ,
               cause_death          = self@database$cause_death        ,
               dat_last_news        = as.Date(dat_last_news)     
            ))
          }
)

#Comedication
setMethod(f="mapping_comedication",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Comorbidity
setMethod(f="mapping_comorbidity",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)


#fertility_pregnancy_diag
setMethod(f="mapping_fertility_pregnancy_diag",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              
            ))
          }
)


#fertility_preservation
setMethod(f="mapping_fertility_preservation",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # fertil_preserv              = self@database$fertil_preserv           ,
              # reason_no_PF                = self@database$reason_no_PF             ,
              # reason_no_PF_2              = self@database$reason_no_PF_2           ,
              # pf_discussion               = self@database$pf_discussion            ,
              # fpp_type                    = self@database$fpp_type                 ,
              # ivm                         = self@database$ivm                      ,
              # cos                         = self@database$cos                      ,
              # agonists_during_ct          = self@database$agonists_during_ct       ,
              # ovarian_cryopreservation    = self@database$ovarian_cryopreservation ,
              # oocyte_cryopreservation     = self@database$oocyte_cryopreservation  ,
              # embryo_cryopreservation     = self@database$embryo_cryopreservation  ,
              # frozen_oocytes              = self@database$frozen_oocytes           ,
              # frozen_embryos              = self@database$frozen_embryos           
            ))
          }
)


#fertility_after_cancer
setMethod(f="mapping_fertility_after_cancer",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            
            return(list(
              # return_center_pf                      = self@database$return_center_pf                   ,
              # mention_preg_desire                   = self@database$mention_preg_desire                ,
              # dat_preg_desire                       = self@database$dat_preg_desire                   ,
              # reuse_frozen_material                 = self@database$reuse_frozen_material              ,
              # reuse_frozen_cortex                   = self@database$reuse_frozen_cortex                ,
              # reuse_frozen_oocytes                  = self@database$reuse_frozen_oocytes               ,
              # reuse_frozen_embryo                   = self@database$reuse_frozen_embryo                ,
              # egg_donation                          = self@database$egg_donation                       ,
              # art_after_cancer                      = self@database$art_after_cancer                   ,
              # pregnancy_post_reuse_frozen_cortex    = self@database$pregnancy_post_reuse_frozen_cortex ,
              # pregnancy_post_reuse_frozen_oocytes   = self@database$pregnancy_post_reuse_frozen_oocytes,
              # pregnancy_post_reuse_frozen_embryo    = self@database$pregnancy_post_reuse_frozen_embryo ,
              # pregnancy_post_egg_donation           = self@database$pregnancy_post_egg_donation        ,
              # pregnancy_post_art_after_cancer       = self@database$pregnancy_post_art_after_cancer    
            ))
          }
)


# pregnancy_after_bc
setMethod(f="mapping_pregnancy_after_bc",
          signature="MappingTabacSurvival", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
              # pregnancy_post_k                 = self@database$pregnancy_post_k               ,
              # spontan_ART_preg_1               = self@database$spontan_ART_preg_1             ,
              # preg_outcome_preg_1              = self@database$preg_outcome_preg_1            ,
              # dat_start_preg_1                 = self@database$dat_start_preg_1               ,
              # comment_preg_1                   = self@database$comment_preg_1                 ,
              # spontan_ART_preg_2               = self@database$spontan_ART_preg_2             ,
              # preg_outcome_preg_2              = self@database$preg_outcome_preg_2            ,
              # dat_start_preg_2                 = self@database$dat_start_preg_2               ,
              # comment_preg_2                   = self@database$comment_preg_2                 ,
              # spontan_ART_preg_3               = self@database$spontan_ART_preg_3             ,
              # preg_outcome_preg_3              = self@database$preg_outcome_preg_3            ,
              # dat_start_preg_3                 = self@database$dat_start_preg_3               ,
              # comment_preg_3                   = self@database$comment_preg_3                 ,
              # comment_additional_pregnancies   = self@database$comment_additional_pregnancies 
            ))
          }
)

