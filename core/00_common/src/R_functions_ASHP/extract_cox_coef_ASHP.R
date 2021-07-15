require(tidyverse)
require(broom)
require(survival)
library(tibble)

# dataf          <- neorep_TILs
# var_to_test    <- c("perc_stromal_lymphocytes","str_TILS_changes_abs_val","perc_stromal_lymphocyte2")                    
# names_variable_to_test  <- c("Pre-NAC TILs","TILs changes","Post-NAC TILs")
# var_to_nest_by <- "subtype.f"
# del            <- "delDFS2.m"
# ev             <- "DFS"


extract_cox_coef <- function(dataf,var_to_test,var_to_nest_by,del,ev ) {
  
  if(var_to_nest_by != "none") {
# Data in long format to have all the TILs in the same column
df <- melt(dataf[,c(var_to_test,del,ev,var_to_nest_by) ],c(del,ev,var_to_nest_by))
                              }
  
if(var_to_nest_by == "none") {
  # Data in long format to have all the TILs in the same column
  df                <- melt(dataf[,c(var_to_test,del,ev) ],c(del,ev))
  df$var_to_nest_by <- "tmp"
}

colnames(df)[which(colnames(df) ==var_to_nest_by)]   <- "var_to_nest_by"
colnames(df)[which(colnames(df) ==del)]   <- "del"
colnames(df)[which(colnames(df) ==ev)]   <- "ev"
# levels(df$value)
# df$value <- factor(df$value, levels=c("<= 10%","[11-59%]",">= 60%"))
                   
#                                          "1st quart","2nd quart","3rd quart","4th quart"))

# Double nesting
        ## First nest : generate all the submatrix / grouped by subtype annd TILs
df2 <- df %>% group_by(var_to_nest_by) %>% nest(.key=data1)  %>% # nest by subtypes
          mutate(data1 = map(data1, ~.x %>% group_by(variable) %>% nest)) # in the nested data frames we nest by TILs

df2.models <- df2 %>% 
  mutate(data1=map(data1, ~.x %>%  # x pour j'utilise toute la matrix (BS est pas sur)
                     mutate(models= map(data,~{tidy(      coxph( Surv(del,ev)~value,data=.)  ,   exponentiate=TRUE   ) %>%  
                         select(term,estimate,p.value,conf.low,conf.high)}))))


# Unnest to have a clean matrix    
tab_results <-  df2.models %>% unnest %>% select(var_to_nest_by,variable,models) %>% unnest %>% 
                      mutate_each(funs(round(.,3)),estimate,p.value,conf.low,conf.high) %>%
                      mutate(HR=estimate,
                      CI= paste0("[", conf.low,"-",conf.high,"]") ,
                      pval=p.value ) %>%
                      select(var_to_nest_by,term,variable,HR,CI,pval) %>% as.data.frame()

# Ready to use in an article table  

nest_clean  <-  ifelse(!duplicated(tab_results$var_to_nest_by), as.character(tab_results$var_to_nest_by), "")
tab_results <-  add_column(tab_results,nest_clean,.after = "var_to_nest_by")

variable_clean     <- names_variable_to_test[match(tab_results$variable, var_to_test)]
tab_results        <-  add_column(tab_results,variable_clean,.after = "variable")
tab_results$term   <- gsub("value","",tab_results$term)

colnames(tab_results)  <- c("var_to_nest_by","group","term","variable_raw","variable","HR","95%CI","p") 
tab_results            <- tab_results[,c("var_to_nest_by","group","variable_raw","variable","term","HR","95%CI","p") ]

# Only remains to add the reference class ...
          # Help BS or CL !!
          
          # tab_results_tmp  <-  tab_results %>% 
          #                       select(variable_raw)
          #                       mutate(tmp_levels= levels( neorep_TILs[,as.character(variable_raw) ] )  )
          tmp_col     <- tab_results$variable_raw[1]
          tmp_levels  <- levels( dataf[,as.character(tmp_col) ]     ) 
          # tmp_col_tab_results <- paste0( tmp_levels[],  )

# and could capture the effectives? from log rank

return(tab_results)

# Help : pourquoi mon  return tab_results marche pas?
}  

