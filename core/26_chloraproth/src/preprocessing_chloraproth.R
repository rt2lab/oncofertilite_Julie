library(dplyr)
library(stringr)
library(tableone)
library(readxl)

load("/Users/ahamypet/RT2Lab/databases/core/26_chloraproth/data/raw/chloraproth.RData")
chloraproth$numdos_curie		<- formatC(chloraproth$numdos_curie,width = 7, flag = "0")

# Remove data use refusal
chloraproth <- chloraproth %>% filter(refusal_data_use___1 == 0,
                                      is.na(exclusion_clear)) %>% 
                                      select(-refusal_data_use___1 )
head(chloraproth)
nrow(chloraproth) # 1237

# Select colnames()
colnames_factor                 <- colnames(chloraproth)[grep(".factor",colnames(chloraproth)) %>% unique()]
colnames_no_factor              <- gsub(".factor","",colnames_factor) %>% unique()
colnames_integer_or_numeric_tmp <- setdiff(colnames(chloraproth),colnames_no_factor) 
colnames_final                  <- c(colnames_integer_or_numeric_tmp)
chloraproth               <- chloraproth[,colnames_final]
colnames(chloraproth)     <- gsub(".factor","",colnames(chloraproth)) 
colnames(chloraproth)     <- gsub("___1","",colnames(chloraproth)) 

chloraproth              <- chloraproth %>% mutate_all(funs(str_replace(., "Unchecked", "NA"))) %>% 
                                            mutate_all(funs(str_replace(., "Checked"  , "Yes"))) %>% 
                                            mutate(implant_volume         = as.integer(implant_volume),
                                                   age_reconstruc_surgery = as.integer(age_reconstruc_surgery),
                                                   year = as.character(substr(dat_reconstruc_surgery,1,4)))
chloraproth$infection     <-  ifelse(chloraproth$ssi == "NA","No","Yes") 
head(chloraproth)

chloraproth$axillary_surgery <- ifelse(chloraproth$axillary_surgery_4cl == "No Axillary Surgery", "No","Yes" )
chloraproth$axillary_surgery_4cl <- ifelse(chloraproth$axillary_surgery_4cl == "Both", "Axillary Node Dissection",chloraproth$axillary_surgery_4cl )
chloraproth %>% group_by(axillary_surgery_4cl,axillary_surgery) %>% count()

# Map names
extract_good_file_names = str_extract(list.files(path = file.path(Sys.getenv(),"RT2Lab","databases","core","00_common","docs")),
                                      "datadict_RT2_v[:digit:]?[:digit:].xlsx") #Get all version names
last_version_data_dict <- names(which.max(sapply(extract_good_file_names,function(x) as.integer(stringr::str_sub(x, 15,-6))))) 
data_dict = read_excel(paste0("~/RT2Lab/databases/core/00_common/docs/", last_version_data_dict), 1)
# Add a datadict_comp
datadict_comp            <- read_excel("~/RT2Lab/databases/core/26_chloraproth/doc/datadict_chloraproth_comp.xlsx")
new_datadict             <- bind_rows (data_dict, datadict_comp %>% filter( ! var %in% data_dict$var))# %>% tail()

chloraproth %>% filter(ssi == "Yes")

colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi")]           <- "Staphylococcus"
colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi___2")]       <- "Streptococcus"
colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi___3")]       <- "Proteus"
colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi___4")]       <- "Citrobacter"
colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi___5")]       <- "Other"
colnames(chloraproth)[which(colnames(chloraproth)=="bacterio_ssi___6")]       <- "None"

colnames(chloraproth)[which(colnames(chloraproth)=="ssi_management")]           <- "Implant_retrieved"
colnames(chloraproth)[which(colnames(chloraproth)=="ssi_management___2")]       <- "Change_of_implant"
colnames(chloraproth)[which(colnames(chloraproth)=="ssi_management___3")]       <- "Use_of_antibiotics"
colnames(chloraproth)[which(colnames(chloraproth)=="ssi_management___4")]       <- "Cleaning"

# Preliminary analyses
var_selected <- chloraproth %>% select(year,age_reconstruc_surgery,asa_score,
                                       implant_volume,center_curie,
                                       comor_diabete,smoking_3cl,comor_hypertension,
                                       ct,rt,
                                       rmi_or_rms_or_change,high_familial_risk,
                                       symetrisation_implant,axillary_surgery_4cl,
                                       dermal_matrix , implant_brand , skin_spare , 
                                       nipple_spare, reconstruct_incision_3cl, implant_position,
                                       infection,delay_surg_ssi,
                                       Staphylococcus,Streptococcus,Proteus,Citrobacter,Other,None,
                                       Implant_retrieved,Change_of_implant,Use_of_antibiotics,Cleaning,
                                       implant_saved
                                       ) %>% colnames()

names_var_selected	<-  new_datadict[match(var_selected,new_datadict$var),"names_var"] %>% as.matrix() %>% as.character()
matching_name_file  <- data.frame(variable = var_selected, name_variable = names_var_selected)

matching_name_file    <- data.frame(variable      = var_selected, 
                                    name_variable = names_var_selected)
mydataset <- chloraproth
head(chloraproth)
# chloraproth_marcel <- chloraproth[,c(var_selected,"infection")]
chloraproth_marcel <- chloraproth[,c(var_selected)]

save(chloraproth_marcel, file = "/Users/ahamypet/RT2Lab/databases/core/26_chloraproth/data/processed/chloraproth_marcel.RData")
save(matching_name_file, file = "/Users/ahamypet/RT2Lab/databases/core/26_chloraproth/data/processed/matching_name_file.RData")

chloraproth_marcel$year
chloraproth_marcel$infection

Table1            <- CreateTableOne(var_selected, "infection",data = chloraproth) 
table1_preformat  <- print(Table1, quote=TRUE, noSpaces=TRUE,showAllLevels = TRUE,)
source('~/RT2Lab/databases/core/00_common/src/R_functions_ASHP/format_tableone_v2.R', local = TRUE)

Table1_WP <- Table1_format
Table1_WP
write_excel_csv2( Table1_WP, "/Users/ahamypet/RT2Lab/databases/core/26_chloraproth/data/Table1_WP.csv") 

chloraproth %>% group_by(ssi___1) %>% count()
chloraproth %>% group_by(ssi___1.factor) %>% count()
 chloraproth <- chloraproth %>% mutate(infection = ssi___1.factor)
#                                       year_diag = substr())
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(dermal_matrix.factor)) %>%
#   group_by(dermal_matrix.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(dermal_matrix.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(center_curie.factor)) %>%
#   group_by(center_curie.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(center_curie.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(comor_hypertension.factor)) %>%
#   group_by(comor_hypertension.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(comor_hypertension.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(ct.factor)) %>%
#   group_by(ct.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(ct.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(smoking_3cl.factor)) %>%
#   group_by(smoking_3cl.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(smoking_3cl.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)
# 
# Dat.label <-  chloraproth %>% 
#   filter(!is.na(comor_diabete.factor)) %>%
#   group_by(comor_diabete.factor,infection  ) %>% 
#   dplyr::summarise(count=n()) %>%  
#   group_by(comor_diabete.factor) %>% 
#   mutate(ypos = cumsum(count) - 0.5*count) %>% 
#   mutate(percent_full = count/sum(count)) %>% 
#   mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
#   mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
#   ungroup() ; head(Dat.label)

head(chloraproth)

# Run logistic regression

chloraproth$infection_bin <- ifelse(chloraproth$infection == "Yes",1,0)

mod <-  glm(infection_bin ~ NULL ,data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 
mod <-  glm(infection_bin ~ center_curie ,data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 
mod <-  glm(infection_bin ~ center_curie + implant_volume ,data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 
mod <-  glm(infection_bin ~ center_curie + implant_volume + implant_position,data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 
mod <-  glm(infection_bin ~ center_curie + implant_volume + implant_position + smoking_3cl,
            data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 
mod <-  glm(infection_bin ~ center_curie + implant_volume + implant_position + smoking_3cl +reconstruct_incision_3cl,
            data = chloraproth , family="binomial") ; summary(mod)								 																
add1(mod,~ implant_volume+asa_score+year+
       center_curie+smoking_3cl+
       comor_hypertension+nipple_spare+reconstruct_incision_3cl+implant_position, test="Chisq") 

