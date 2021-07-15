require(tidyverse)
require(broom)
require(survival)
library(tibble)
options(scipen=999)


# dataf          <- neorep_TILs
# var_to_test    <- c("perc_stromal_lymphocytes","str_TILS_changes_abs_val","perc_stromal_lymphocyte2")                    
# names_variable_to_test  <- c("Pre-NAC TILs","TILs changes","Post-NAC TILs")
# var_to_nest_by <- "subtype.f"
# del            <- "delDFS2.m"
# ev             <- "DFS"

dataf  			        <- neorep_TILs[neorep_TILs$subtype.f=="HER2",] ; print(dim(dataf))
quali 			        <-  c("str_TILS_10_perc")							
nomquali		        <- 	c("TILs levels (by 10%)")
var_to_explain	    <- "RCH4.f"
level_to_import	    <- "pCR"
i=1

extract_glm_coef(dataf,quali,nomquali,var_to_explain,level_to_import)

extract_glm_coef <- function(dataf,quali,nomquali,var_to_explain,level_to_import ) {

  tmp_mod    					<- glm(dataf[,var_to_explain] ~  dataf[,quali[i]]    , family="binomial")  
  summary(tmp_mod)
  tmp_tableau				<- table(dataf[,quali[i]],dataf[,var_to_explain])
  variable				  <- rep(quali[i],nrow(tmp_tableau))
  name_variable			<- variable
  modalities				<- rownames(tmp_tableau)
  n_tot					    <- unname(rowSums(tmp_tableau))		
  nb_var_to_explain		<- unname(tmp_tableau[modalities,level_to_import])		
  perc_var_to_explain		<- round(nb_var_to_explain/n_tot*100,1)

  # tidy_tmp_mod_no_exp <- tidy(tmp_mod)
  tidy_tmp_mod_exp    <- tidy(tmp_mod,exponentiate=TRUE)
#   tidy_tmp_mod_exp$term
# grep()  
#   class(  tidy_tmp_mod_exp$term)
  library(stringr)
  tidy_tmp_mod_exp <- tidy_tmp_mod_exp %>% mutate(
                                                  # non_null_levels = str_detect(tidy_tmp_mod_exp$term,"dataf[, quali[i]]"),
                                                  IConfbas  = round(estimate-1.96*std.error,2),
                                                  IConfhaut = round(estimate+1.96*std.error,2),
                                                  OR      = round(estimate,2),
                                                  IC95    = paste0("[",IConfbas," - ",IConfhaut,"]"),
                                                  ptmp    = round(p.value,3)  ,
                                                  p       = ifelse(ptmp==0.000,"<0.001",ptmp)   )
                  #                                 variable_raw      = variable,
                  #                                 variable_clean    = rep(nomquali[i],nrow(tidy_tmp_mod_exp) ),
                  # variable_clean_unique =   ifelse(!duplicated(variable_clean), variable_clean, ""),
                  #                                 levels            = modalities,
                  #                                 ntot    = n_tot,
                  #                                 npCR    = nb_var_to_explain,
                  #                                 perc_pCR = perc_var_to_explain)
  tab_results <- tidy_tmp_mod_exp   %>%     
    # select(variable_raw,variable_clean,variable_clean_unique,levels,ntot,npCR,perc_pCR,OR,IC95,p)
  select(term,OR,IC95,p)
  
  return(tab_results)
                                                                                        }
    

# Ready to use in an article table  

# nest_clean  <-  ifelse(!duplicated(tab_results$var_to_nest_by), as.character(tab_results$var_to_nest_by), "")
# tab_results <-  add_column(tab_results,nest_clean,.after = "var_to_nest_by")
# 
# variable_clean     <- names_variable_to_test[match(tab_results$variable, var_to_test)]
# tab_results        <-  add_column(tab_results,variable_clean,.after = "variable")
# tab_results$term   <- gsub("value","",tab_results$term)
# 
# colnames(tab_results)  <- c("var_to_nest_by","group","term","variable_raw","variable","HR","95%CI","p") 
# tab_results            <- tab_results[,c("var_to_nest_by","group","variable_raw","variable","term","HR","95%CI","p") ]
# 
# # Only remains to add the reference class ...
#           # Help BS or CL !!
#           
#           # tab_results_tmp  <-  tab_results %>% 
#           #                       select(variable_raw)
#           #                       mutate(tmp_levels= levels( neorep_TILs[,as.character(variable_raw) ] )  )
#           tmp_col     <- tab_results$variable_raw[1]
#           tmp_levels  <- levels( dataf[,as.character(tmp_col) ]     ) 
#           # tmp_col_tab_results <- paste0( tmp_levels[],  )
# 
# # and could capture the effectives? from log rank
# 
# return(tab_results)
# 
# # Help : pourquoi mon  return tab_results marche pas?
# }  
# 
