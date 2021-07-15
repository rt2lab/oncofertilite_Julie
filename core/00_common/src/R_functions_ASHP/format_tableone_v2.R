# 4 Inputs : 
      ## mydataset (dataset to perform tableone)
      ## var_selected (variables to put into tableone)
      ## table1_preformat # from createTableone
      ## one file to match names : matching_name_file 
            # With 2 columns : variable and name_variable
# 2 Outputs of the function :
      ## Table1_format : ready to save as csv for tableone of any medical article
      ## vector_missing_data : to use as legend for table one (clean summary of missing data).

library(stringr)
library(tableone)
library(tidyverse)

variable_clean  <- rownames(table1_preformat)

table1_preformat_no_rownames <- table1_preformat %>% as_tibble() %>% as.data.frame()
variable_clean <- stringr::str_replace_all(variable_clean,'\\\"',"")
variable_clean <- stringr::str_replace(variable_clean," \\(mean \\(SD\\)\\)","") 
variable_clean <- stringr::str_replace(variable_clean," \\(median \\[IQR\\]\\)","") 
variable_clean <- stringr::str_replace(variable_clean," \\(%\\)","") 
variable_clean <- variable_clean %>% as.data.frame()

table1_preformat_df <- bind_cols(variable_clean,table1_preformat_no_rownames) %>% as.data.frame()
colnames(table1_preformat_df)[1] <- "variable"

colnames(table1_preformat_df) <- gsub('"',"",colnames(table1_preformat_df))
colnames(table1_preformat_df) <- gsub('X.',"",colnames(table1_preformat_df))
colnames(table1_preformat_df) <- gsub('\\.',"",colnames(table1_preformat_df))

# Add names
class(table1_preformat_df)
table1_preformat_df <- table1_preformat_df %>% mutate(names_variable_clean=NA)
table1_preformat_df[,"names_variable_clean"] <- matching_name_file[match(table1_preformat_df$variable,matching_name_file$variable ),"name_variable"]
table1_preformat_df[which(is.na(table1_preformat_df[,"names_variable_clean"])),"names_variable_clean"] <- ""  # Remove NA

# Order
Table1_format            <- table1_preformat_df %>% select(variable, names_variable_clean,level,everything()) %>% 
                                                    select(-variable) 
Table1_format[1,"level"] <- 'n'

# Generate legend with number of missing data.
missing_data  <- sapply(mydataset[,var_selected], function(x) sum(is.na(x)))
missing_data  <- missing_data[missing_data!=0] %>% as.data.frame() 
colnames(missing_data) <- "missing"

missing_data  <- missing_data %>% 
                  rownames_to_column(., var = "variable") %>% 
                  mutate(name_var = matching_name_file[match(variable,matching_name_file$variable),"name_variable"]) %>%
                  mutate(concat = paste0(name_var, ", n=",missing ) )
vector_missing_data <- missing_data %>% select(concat) %>% as.matrix() %>% as.character() %>% paste0(.,collapse = "; ") %>% 
                       paste0("Missing data: ",.)

MyGenericFootnote    <- "The “n” denotes the number of patients. In case of categorical variables, percentages are expressed between brackets. In case of continuous variables, mean value is reported, with standard deviation between brackets. In case of nonnormal continuous variables, median value is reported, with interquartile range between brackets." 


# P-values are from the Fisher exact test and Kruskal-Wallis test when comparing categorical and continuous variables against two categories BMI, respectively. 
# IQR: interquartile Range, SD: standard deviation."
