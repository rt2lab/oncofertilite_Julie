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
MappingAp <- setClass( #TODO : change the name of the classe (e.g. MappingSein, MappingCanto...)
  "MappingAp", #TODO : same

  # Set the default values for the slots. (optional)
  prototype=list(
    name_database = "18_ap_saint_louis", #TODO : the name of your base
    #TODO : load your database here e.g.
    database = read.csv(file = file.path( 
        Sys.getenv("PROJECT_PATH"),
        "core/01_base_sein/data/base_sein.csv"
    ), row.names = 1),
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
#Beware : you need to return the whole object of class MappingMyBase (self)
setMethod(f="mapping_initial",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            #TODO : Modify self@database
            #Ex :   self@database <- self@database %>% select(-this_column)
            return(self)
          }
)
###################################################################

###################################################################
#Step 3 : 
#Redefine the methods mapping_family_var

#Patient id
setMethod(f="mapping_patient_id",
          signature="MappingMyBase", #Change here
          definition=function(self)
          {
            #You can right useful codes here
            #Ex : center_curie = factor(self@database$LIEUCHIRA, levels = c("I.C. Paris", "I.C. St Cloud","hors I.C."), labels = c(1,2,3))
            #Your need to return a named list of variables, comma separated
            #If nothing to return : either return an empty list
            # Or do not redefine the function at all. 
            return(list(
              #database = database,
              #numdos_curie = self@database$NUMDOS,
              #cletri = cletri
            ))
          }
)

#Patient char
setMethod(f="mapping_patient_char",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Bc_diagnosis
setMethod(f="mapping_bc_diagnosis",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Bc_biology
setMethod(f="mapping_bc_biology",
          signature = "MappingMyBase", #TODO : change to the name of your class
          definition = function(self)
          {
            return(list(
            ))
          }
)

#Surgery
setMethod(f="mapping_surgery",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Treatments binary
setMethod(f="mapping_treatments_binary",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
             ))
          }
)

#Neoadj or not
setMethod(f="mapping_neoadj_or_not",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Neoadjuvant_ct_antiher2
setMethod(f="mapping_neoadjuvant_ct_antiher2",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
               ))
          }
)

#Adjuvant ct antiher2
setMethod(f="mapping_adjuvant_ct_antiher2",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Tumor char surg
setMethod(f="mapping_tumor_char_surg",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Tumor char neo
setMethod(f="mapping_tumor_char_neo",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Events and censor
setMethod(f="mapping_events_and_censor",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
             return(list(
            ))
          }
)

#Comedication
setMethod(f="mapping_comedication",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)

#Comorbidity
setMethod(f="mapping_comorbidity",
          signature="MappingMyBase", #TODO : change to the name of your class
          definition=function(self)
          {
            return(list(
            ))
          }
)
