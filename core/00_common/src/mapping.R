#Create class mapping
Mapping <- setClass(
  "Mapping",
  
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
    dat_censor = Sys.Date()
  )

)

setGeneric(name="mapping_initial",
           def=function(self)
           {
             standardGeneric("mapping_initial")
           }
)

setGeneric(name="mapping_patient_id",
           def=function(self)
           {
             standardGeneric("mapping_patient_id")
           }
)

setGeneric(name="mapping_patient_char",
           def=function(self)
           {
             standardGeneric("mapping_patient_char")
           }
)

setGeneric(name="mapping_bc_diagnosis",
           def=function(self)
           {
             standardGeneric("mapping_bc_diagnosis")
           }
)

setGeneric(name="mapping_bc_biology",
           def=function(self)
           {
             standardGeneric("mapping_bc_biology")
           }
)

setGeneric(name="mapping_surgery",
           def=function(self)
           {
             standardGeneric("mapping_surgery")
           }
)

setGeneric(name="mapping_treatments_binary",
           def=function(self)
           {
             standardGeneric("mapping_treatments_binary")
           }
)

setGeneric(name="mapping_neoadj_or_not",
           def=function(self)
           {
             standardGeneric("mapping_neoadj_or_not")
           }
)

setGeneric(name="mapping_neoadjuvant_ct_antiher2",
           def=function(self)
           {
             standardGeneric("mapping_neoadjuvant_ct_antiher2")
           }
)

setGeneric(name="mapping_adjuvant_ct_antiher2",
           def=function(self)
           {
             standardGeneric("mapping_adjuvant_ct_antiher2")
           }
)

setGeneric(name="mapping_treatments",
           def=function(self)
           {
             standardGeneric("mapping_treatments")
           }
)

setGeneric(name="mapping_tumor_char_surg",
           def=function(self)
           {
             standardGeneric("mapping_tumor_char_surg")
           }
)

setGeneric(name="mapping_tumor_char_neo",
           def=function(self)
           {
             standardGeneric("mapping_tumor_char_neo")
           }
)

setGeneric(name="mapping_events_and_censor",
           def=function(self)
           {
             standardGeneric("mapping_events_and_censor")
           }
)

setGeneric(name="mapping_evol",
           def=function(self)
           {
             standardGeneric("mapping_evol")
           }
)

setGeneric(name="mapping_comedication",
           def=function(self)
           {
             standardGeneric("mapping_comedication")
           }
)

setGeneric(name="mapping_comorbidity",
           def=function(self)
           {
             standardGeneric("mapping_comorbidity")
           }
)

setGeneric(name="mapping_fertility_pregnancy_diag",
           def=function(self)
           {
             standardGeneric("mapping_fertility_pregnancy_diag")
           }
)

setGeneric(name="mapping_fertility_preservation",
           def=function(self)
           {
             standardGeneric("mapping_fertility_preservation")
           }
)

setGeneric(name="mapping_fertility_after_cancer",
           def=function(self)
           {
             standardGeneric("mapping_fertility_after_cancer")
           }
)

setGeneric(name="mapping_pregnancy_after_bc",
           def=function(self)
           {
             standardGeneric("mapping_pregnancy_after_bc")
           }
)

setGeneric(name="update",
           def=function(self,cols)
           {
             standardGeneric("update")
           }
)

setMethod(f="update",
          signature="Mapping",
          definition=function(self,cols)
          {
            for (i in 1:length(cols)){ 
              colnames(self@database)[which(colnames(self@database)==names(cols)[i])] <- paste(names(cols)[i],"old" , sep="_")
              self@database[,names(cols)[i]] <- cols[i]
            }
            return(self)
          }
)
