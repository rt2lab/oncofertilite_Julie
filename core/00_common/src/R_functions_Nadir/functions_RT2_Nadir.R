##########################################################################################################################################################
###                                                                                                                                                    ###
###                                                 FUNCTIONS FOR PLOTS                                                                                ###
###                                               AND STATISTICAL ANALYSIS                                                                             ###
###                                                                                                                                                    ###
###  printTables:                       Print the tableone with a customized layout                                                                    ###    
###  barPlotRT2:                        plots the distribution of a categorical variables in the horizontal form, as preferred by the RT2 Lab          ### 
###  logisticRegressionTable:           univariable and multivariable table for logistic regression                                                    ###
###  preformatTable1:                   create a table summarizing baseline variables                                                                  ###
###  preformatTable1MultipleVariables:  create a table summarizing baseline variables for multiple stratification variables                            ###
###  cox_univariable_multivariable:     run the coz model for univariable and multivariable analysis                                                   ###
###                                                                                                                                                    ###
##########################################################################################################################################################



###################################################################################
### INSTALL DOCSTRING: install.packages('docstring')                            ###
###                                                                             ###
### To see the documentation for a function, use  docstring(function_name)      ###
###                                                                             ###
###################################################################################

############################################################################  DO NOT CALL
############################ INTERNAL FUNCTIONS ############################  THESE FUNCTIONS
############################################################################  DIRECTLY


UpperCaseParenthesis <- function(x) {
  pos =  gregexpr(pattern ='\\(',x)[[1]][1]
  if(pos > 0)
    x <- paste0(substr(x, 1, (pos-1)) ,toupper(substr(x, pos, pos+1)), substr(x, pos+2, nchar(x)))
  return(x)
}

firstUp <- function(v) {
  v = as.character(v)
  for(i in 1:length(v)){
    x = v[i]
    if(identical(x, character(0))){
      return("")
    } else {
      if(!is.na(x)){
        if(nchar(x)>0){
          x = paste0(toupper(substr(x, 1, 1)),  tolower(substr(x, 2, nchar(x))))
          v[i] = x#UpperCaseParenthesis(x)
        }
      }
    }
  }
  if(length(v)==1)return(v[1])
  else return(v)
}

## Do not call this function, it is an internal function
preformatTable1_subfunct = function(table1, var_selected, names_var_selected, var_non_normal=NA, mydataset, n_digits = 1){
  
  matching_name_file    <- data.frame(variable = var_selected, name_variable = names_var_selected)
  sink("/dev/null") 
  ## WINDOWS: sink("nul") 
  if(is.na(var_non_normal[1]))
    table1_preformat      <- print(table1, quote=F, noSpaces=TRUE, showAllLevels = TRUE, pDigits=3, contDigits=1, catDigits=n_digits, smd=F )
  else
    table1_preformat      <- print(table1, quote=F, noSpaces=TRUE, showAllLevels = TRUE, pDigits=3, contDigits=1, catDigits=n_digits, smd=F,  nonnormal = var_non_normal)
  sink() 
  variable_clean  <- rownames(table1_preformat)
  
  table1_preformat_no_rownames <- table1_preformat %>% as_tibble() %>% as.data.frame()
  variable_clean <- stringr::str_replace_all(variable_clean,'\\\"',"")
  variable_clean <- stringr::str_replace(variable_clean," \\(median \\[IQR\\]\\)","") 
  variable_clean <- stringr::str_replace(variable_clean," \\(mean \\(SD\\)\\)","") 
  
  variable_clean <- stringr::str_replace(variable_clean," \\(%\\)","") 
  variable_clean <- variable_clean %>% as.data.frame()
  
  table1_preformat_df <- bind_cols(variable_clean,table1_preformat_no_rownames) %>% as.data.frame()
  colnames(table1_preformat_df)[1] <- "variable"
  
  colnames(table1_preformat_df) <- gsub('"',"",colnames(table1_preformat_df))
  colnames(table1_preformat_df) <- gsub('X.',"",colnames(table1_preformat_df))
  colnames(table1_preformat_df) <- gsub('\\.',"",colnames(table1_preformat_df))
  
  # Add names
  table1_preformat_df <- table1_preformat_df %>% mutate(names_variable_clean=NA)
  table1_preformat_df[,"names_variable_clean"] <- matching_name_file[match(table1_preformat_df$variable,matching_name_file$variable ),"name_variable"]
  table1_preformat_df$names_variable_clean = as.character(table1_preformat_df$names_variable_clean)
  table1_preformat_df[which(is.na(table1_preformat_df[,"names_variable_clean"])),"names_variable_clean"] <- ""  # Remove NA
  
  # Order
  Table1_format            <- dplyr::select(table1_preformat_df, variable, names_variable_clean,level,everything()) %>% dplyr::select(-variable)
  
  colnames(Table1_format)[which(colnames(Table1_format) == "names_variable_clean")] = "Variable name"
  
  Table1_format[1,"level"] <- 'n'
  
  # Generate legend with number of missing data.
  missing_data  <- sapply(mydataset[,var_selected], function(x) sum(is.na(x)))
  missing_data  <- missing_data[missing_data!=0] %>% as.data.frame() %>% dplyr::rename("missing" = 1) %>%
    rownames_to_column(., var = "variable") %>% 
    dplyr::mutate(name_var = matching_name_file[match(variable,matching_name_file$variable),"name_variable"]) %>%
    dplyr::mutate(concat = paste0(name_var, ", n=",missing ) )
  string_missing_data <- missing_data %>% dplyr::select(concat) %>% as.matrix() %>% as.character() %>% paste0(.,collapse = "; ") %>% 
    paste0("Missing data: ",.)
  
  return(list(Table1_format, string_missing_data))
}

kernel_density <- function(vect, Y_label, min_occurency=1, Y_lab_size = 13, axis_title_size = 16, lbsize=NA, lbsizePrerc = NA){
  
  ggplot(as.data.frame(vect), aes(x=vect)) + 
    geom_histogram(binwidth=3, colour="black", fill="steelblue") +
    theme_bw()+ 
    theme(legend.position="top", axis.text=element_text(size=Y_lab_size), panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),axis.line.x=element_blank(),
          axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
  
}



logisticRegressionTableUnivariareGroups = function(data,  data_imputed = NA, explanatory, nom_explanatory, var_to_explain, level_to_import, classes, perform_imputation = F, 
                                                   variable_stratify = NA, variables_use_multivariable = NA, alpha_cut_multivariable = 1, all_multivariable_values = F){
  dataOrig = data
  data_tmp = dataOrig[which(dataOrig[,variable_stratify]==classes[1]),]
  data_imputed_tmp = data_imputed
  
  if(length(data_imputed)>0){
    long1 <- complete(data_imputed, action='long', include=TRUE)
    long2 <- long1[which(long1[,variable_stratify]==classes[1]),]
    data_imputed_tmp <- as.mids(long2)
    
  }
  # first class
  lr_table = logisticRegressionTable(data = data_tmp, data_imputed = data_imputed_tmp, explanatory = explanatory, 
                                     nom_explanatory = nom_explanatory, var_to_explain = var_to_explain, level_to_import = level_to_import, perform_imputation = perform_imputation,
                                     variables_use_multivariable = variables_use_multivariable, alpha_cut_multivariable = alpha_cut_multivariable, all_multivariable_values = all_multivariable_values)
  lr_table = rbind(c('','', classes[1], rep('', 8)), lr_table)
  
  #other classes
  for(var in classes[2:length(classes)]){
    data_tmp = dataOrig[which(dataOrig[,variable_stratify]==var),]
    
    if(length(data_imputed)>0){
      long1 <- complete(data_imputed, action='long', include=TRUE)
      long2 <- long1[which(long1[,variable_stratify]==var),]
      data_imputed_tmp <- as.mids(long2)
      
    }
    
    lr_table_tmp = logisticRegressionTable(data = data_tmp, data_imputed = data_imputed_tmp, explanatory = explanatory, 
                                           nom_explanatory = nom_explanatory, var_to_explain = var_to_explain, level_to_import = level_to_import, perform_imputation = perform_imputation,
                                           variables_use_multivariable = variables_use_multivariable, alpha_cut_multivariable = alpha_cut_multivariable, all_multivariable_values = all_multivariable_values)
    lr_table_tmp = rbind(c('','', var, rep('', 8)), lr_table_tmp)
    
    lr_table = cbind(lr_table, lr_table_tmp[,3:11])
  }
  
  
  return(lr_table)
}



#################### END INTERNAL FUNCTIONS


get_pretty_colnames = function(vec, path_data_dictionary_folder){
  #' Get pretty names of data dictionary on a vector of baroables
  #'
  #' @param vec vector of variables
  #' @param path_data_dictionary_folder path of data dictionary
  #' 
  if(!exists("data_dict")){
    f2 = getMaximumDataDictFile(path_data_dictionary_folder)
    data_dict <<- read.xlsx(f2, sheetName = 'datadict', as.data.frame = T)
  }
  v = c()
  for(elem in vec){
    if(elem %in% data_dict$var){
      v = c(v, as.character(data_dict$names_var[which(data_dict$var == elem)]))
    } else{
      v = c(v, elem)
    }
  }
  return(v)
}



printTables = function(obj){
  
  #' Print the tableone or the logistic regression table with a customized layout
  #'
  #' @param obj object returned by function preformatTable1 or preformatTable1MultipleVariables
  #' @return No returned object
  
  library(kableExtra)
  
  if(is.list(obj)){
    df_table1 = obj[[1]]
    str = obj[[2]]
  } else {
    df_table1 = obj
  }
  
  if('Variable name' %in% colnames(df_table1)){
    varsCols = which(df_table1$`Variable name` != '')
  } else {
    varsCols = which(df_table1[3:nrow(df_table1),1] != '') + 2
  }
  column_to_color = c()
  to_color = T
  pos_last_color = varsCols[1]
  for(i in varsCols[1]:nrow(df_table1)){
    if(to_color){
      column_to_color = c(column_to_color,i)
    }
    
    if((i + 1) %in% varsCols){
      to_color = !to_color
    }
  }
  
  if('p' %in% colnames(df_table1)){
    pvals_cols = which(colnames(df_table1) == 'p')
    v = df_table1$p
    v[which(v=='<0.001')] = 0.0001
    v[which(v=='')] = 1
    v[which(v=='NaN')] = 1
    v=as.numeric(v)
    
    df_table1$p = cell_spec(df_table1$p, color = ifelse(v <0.05, "red", "black"))
  } else {
    pvals_cols = which(df_table1[1,] == 'p')
    for(pval_col in pvals_cols){
      v = as.character(df_table1[,pval_col])
      v[which(v=='<0.001')] = 0.0001
      v[which(v=='')] = 1
      v[which(v=='p')] = 1
      v[which(v=='NaN')] = 1
      v=as.numeric(v)
      df_table1[,pval_col] = cell_spec(df_table1[,pval_col], color = ifelse(v < 0.05, "red", "black"))
    }
  }
  
  if(is.list(obj)){
    print(str)
  }
  
  kbl(df_table1, escape = F) %>%
    kable_styling() %>%
    # column_spec(pvals_cols, bold = T) %>%
    row_spec(column_to_color, color = "black", background = "#F0F0F0")
  
  
}


barPlotRT2 <- function(vect, Y_label, min_occurency=1, Y_lab_size = 13, axis_title_size = 16, lbsize=NA, lbsizePrerc = NA, nCharGoNewLine = 30, sort =T, pecrThresholdLabel = 10){
  
  #' Plot the distribution of a categorical variable
  #'
  #' Make a ggplot figure of the distrinution of a categorical variable with horizontal bars, as preferred in the RT2 Lab
  #'
  #' @param vect the vector to use for the distriution
  #' @param Y_label label for the Y axis
  #' @param min_occurency cut threshold for including a level in the plot: you can show only levels that appears at least  'min_occurency' times
  #' @param Y_lab_size labels size in the Y axis
  
  if(sort)
    lvls=sapply(names(sort(table(vect), decreasing=T)), function(x){firstUp(x)})
  else
    lvls=sapply(names(table(vect)), function(x){firstUp(x)})
  
  lvls = unlist(sapply(lvls, function(x){paste(strwrap(x, width = nCharGoNewLine), collapse = "\n")}))
  
  if(sort)
    df <- data.frame(x=lvls, counts=as.numeric(sort(table(vect), decreasing=T)))
  else
    df <- data.frame(x=lvls, counts=as.numeric(table(vect)))
  df <- df %>% dplyr::filter(counts >= min_occurency)
  
  m = max(df$count) *1.2
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if(tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  if(is.na(lbsize)){
    lbsize = Y_lab_size-6
    if(lbsize>5)
      lbsize=5
    if(lbsize<1)
      lbsize=1
    
  }
  if(is.na(lbsizePrerc)){
    lbsizePrerc = 6
  }
  
  perc = round(df$counts/sum(df$counts)*100,1)
  perc = ifelse(perc > pecrThresholdLabel, perc, '' )
  perc = ifelse(perc != '', paste0(perc, "%"), '' )

  plt <- ggplot(df) +
    aes(x=factor(x, levels = rev(lvls)), y=counts) +
    geom_bar(position = 'dodge', stat="identity", fill="steelblue") +
    geom_text(aes(label=counts), position=position_dodge(width=1), hjust=-.25, size=lbsize) +
    geom_text(aes(y = max(counts)*0.02, label=perc), colour = 'white', 
              vjust = .4,  hjust=-.0, nudge_y = .2, size=lbsizePrerc) +
    xlab("") +
    coord_flip()  +
    scale_y_continuous(limits=c(0, m), breaks=t) +
    xlab(Y_label) +
    theme_bw() + theme(legend.position="top", axis.text=element_text(size=Y_lab_size), panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                       axis.ticks.x=element_blank(), axis.title.y = element_text(size = axis_title_size), axis.text.x=element_blank(),axis.line.x=element_blank(),
                       axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
  return(plt)
}


barPlotRT2_vertical <- function(vect, Y_label, min_occurency=1, Y_lab_size = 13, axis_title_size = 16, lbsize=NA, lbsizePrerc = NA){
  
  #' Plot the distribution of a categorical variable
  #'
  #' Make a ggplot figure of the distrinution of a categorical variable with horizontal bars, as preferred in the RT2 Lab
  #'
  #' @param vect the vector to use for the distriution
  #' @param Y_label label for the Y axis
  #' @param min_occurency cut threshold for including a level in the plot: you can show only levels that appears at least  'min_occurency' times
  #' @param Y_lab_size labels size in the Y axis
  
  
  lvls=sapply(names(table(vect)), function(x){firstUp(x)})
  
  lvls = unlist(sapply(lvls, function(x){paste(strwrap(x, width = 50), collapse = "\n")}))
  
  df <- data.frame(x=lvls, counts=as.numeric(table(vect)))
  
  df <- df %>% dplyr::filter(counts >= min_occurency)
  
  m = max(df$count) *1.2
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if(tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  if(is.na(lbsize)){
    lbsize = Y_lab_size-6
    if(lbsize>5)
      lbsize=5
    if(lbsize<1)
      lbsize=1
    
  }
  if(is.na(lbsizePrerc)){
    lbsizePrerc = 6
  }
  
  plt <- ggplot(df) +
    aes(x=factor(x, levels = (lvls)), y=counts) +
    geom_bar(position = 'dodge', stat="identity", fill="steelblue") +
    geom_text(aes(label=counts), position=position_dodge(width=1), vjust=-.2, size=lbsize) +
    geom_text(aes(y = max(counts)*0.02, label=paste0(round(counts/sum(counts)*100,1), "%")), colour = 'white', 
              vjust = .7, nudge_y = .2, size=lbsizePrerc) +
    xlab("") +
    scale_y_continuous(limits=c(0, m), breaks=t) +
    xlab(Y_label) +
    theme_bw() + theme(legend.position="top", axis.text=element_text(size=Y_lab_size), panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.title.x=element_blank(),
                       axis.ticks.x=element_blank(), axis.title.y = element_text(size = axis_title_size), axis.text.y=element_blank(),axis.line.x=element_blank(),
                       axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
  return(plt)
}



logisticRegressionTable = function(data, data_imputed = NA, explanatory, nom_explanatory, var_to_explain, level_to_import,
                                   variables_use_multivariable = NA, variables_keep_multivariable = NA, 
                                   perform_imputation = F, alpha_cut_multivariable = 0.2, alpha_show_multivariable = 0.1, all_multivariable_values = T, step_AIC = T){
  #' Univariable and multivariable table for logistic regression
  #'
  #' Make the Univariable and multivariable table for logistic regression to show on a paper. 
  #' For Univariable and multivariable analysis, only rows with no NAs will be used for analysis.
  #'
  #' @param data your data frame
  #' @param data_imputed your data frame with multiple imputed datasets without NAs. In case this set of datasets is given, analysis are done on the imputed datas
  #' @param explanatory vector of names of variables to include in the analysis
  #' @param nom_explanatory vector of pretty names corresponding to the variables you included in the analysis, in the same order of the explanatory vector
  #' @param var_to_explain name of the outcome variable, e.g. "pcr"
  #' @param level_to_import string corresponding to the level of the variable 'var_to_explain' that should be reported in the table
  #' @param perform_imputation if data imputation has to be done. In these case analysis are averaged over 10 imputations
  #' @param variables_use_multivariable vector of names of variables to use in the multivariable analysis, use the name provided in the 'explanatory' variable, default NA
  #' @param variables_keep_multivariable vector of names of variables to use in the multivariable analysis, along with variables passing the p-value test in univariable analysis,
  #'                                    use the name provided in the 'explanatory' variable, default NA
  #' @param alpha_cut_multivariable value used to filter variables for the multivariable analysis. Only variables associated to the outcome variables with a 
  #'                               p-value lower than 'alpha_cut_multivariable' will be used for multivariable analysis, default 1
  #'                             
  #'                             WARNNG: this feature selection technique has been proved to provide poor results. Use instead methods like FORWARD, BACKWARD, STEPWISE or other like LASSO or RIDGE regression
  #'                             to filter variables to use in the logistic regression model
  #' @param all_multivariable_values logic value, for multiple logistic regression write all values also the ones not significative, default FALSE
  #' 
  #' @return A data frame with the logistic regression table                            
  
  library(finalfit)
  library(dplyr)
  library(mice)
  
  # check target variable
  if(is.character(data[, var_to_explain])){
    stop('Variable to explain must be numeric (0,1) or a factor with two levels. In this case the first level will take value 0, the second level value 1')
  }
  if(is.factor(data[, var_to_explain])){
    if(length(levels(data[, var_to_explain])) != 2){
      stop('Variable to explain must have only two levels!')
    }
    warning(paste0('Variable to explain is a factor. The first level (',levels(data[, var_to_explain])[1] ,') will take value 0, the second level (', levels(data[, var_to_explain])[2] ,') value 1'))
  }
  
  ## check target variable for imputed data
  if(!is.na(data_imputed)){
    if(is.character(data_imputed[[1]][, var_to_explain])){
      stop('Variable to explain must be numeric (0,1) or a factor.')
    }
    if(is.factor(data_imputed[[1]][, var_to_explain])){
      if(length(levels(data_imputed[[1]][, var_to_explain])) != 2){
        stop('Variable to explain must have only two levels!')
      }
      warning('Variable to explain is a factor. First level will take value 0, second value 1')
    }
  }

  
  tab 											  <- NULL
  tmp_tab 										<- NULL
  variables_to_keep_in_the_multivariable_analysis	      <- NULL
  
  ## if perform_imputation = T, make 10 imputations with mice
  if(perform_imputation){
    colnames(data) = make.names(colnames(data))
    explanatory = make.names(explanatory)
    
    predM = data %>% 
      select(var_to_explain, explanatory) %>% 
      missing_predictorMatrix(
        drop_from_imputed = c(var_to_explain)
      )
    
    data_imputed = data %>% 
      select(var_to_explain, explanatory) %>%
      mice(m = 10, predictorMatrix = predM)  
  }

  for (i in 1:length(explanatory)){
    #print(explanatory[i])
    dataNotNA = data[which(!is.na(data[,var_to_explain]) & !is.na(data[,explanatory[i]])),]
    tmp_tableau							<- table(dataNotNA[,explanatory[i]],dataNotNA[,var_to_explain])
    
    
    # 1. class numeric
    if(is.numeric(data[,explanatory[i]])) 
    {
      if(!is.na(data_imputed)){
        fits = data_imputed %>% with(glm(formula(ff_formula(var_to_explain, explanatory[i])), family="binomial"))
        fits_pool = fits %>% pool()
        fit_imputed = fits_pool %>% fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)
        s = as.character(fit_imputed[1,2])
        s = gsub("\\(", "", s)
        s = gsub("\\)", "", s)
        s = gsub("p=", "", s)
        s = gsub(",", "", s)
        list_res = strsplit(s, "\\, |\\,| ")[[1]]
        OR 									<- list_res[1]
        IC 			           	<- paste0("[",list_res[2],"]")
        pval	 							<- list_res[3]
      } else {
        tmp_mod    					<- glm(dataNotNA[,var_to_explain] ~  dataNotNA[,explanatory[i]]    , family="binomial")  
        
        OR 									<- round(exp(coef(summary(tmp_mod))[,1]),2)[2] # On remplace le coefficient de l'intercept par l'OR de référence=1
        pval	 							<- coef(summary(tmp_mod))[,4][2]
        tab 		           	<- round(exp(confint(tmp_mod)),2)
        IClow 	           	<- tab[,1]
        ICup 		           	<- tab[,2]
        tmp_IC	           	<- rbind(IClow,ICup)
        IC 			           	<- paste0("[",IClow," - ",ICup,"]")
        IC[1]		           	<- c("")
        IC  <- IC[2]
        
      }
      nb_var_to_explain <- dataNotNA[which(dataNotNA[,var_to_explain] == level_to_import),var_to_explain,drop=FALSE] %>% nrow()  
      
      # create vector of variables for multivariable analysis
      if(	pval < alpha_cut_multivariable)			
      {
        tmp_variables_to_keep								              <- 	explanatory[i]
        variables_to_keep_in_the_multivariable_analysis		<- c(variables_to_keep_in_the_multivariable_analysis,tmp_variables_to_keep)							         							         
      }
      
      pval <- ifelse(pval < 0.001,"<0.001", round(as.numeric(pval), 3))
      # suppress intercept
     

      # Export final table 
      tab									<- cbind(explanatory[i],nom_explanatory[i],"",length(dataNotNA[,  explanatory[i]]),"",nb_var_to_explain, paste(round(nb_var_to_explain/nrow(data)*100,1), "%"), OR,IC,pval)
      colnames(tab)				<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")
      tab                     <- as.data.frame(tab)
      tab$name2           <- tab[,"name"]
      tab[-1,"name2"]     <- "" 
      tmp_tab							<- rbind(tmp_tab,tab)							
      #print("boucle1")
    } else {
      # factor
      # 2. Un des >=4 groupes vides => 	# Exporter les effectifs et % sans les OR 
      if(any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1)		
      {
        variable				  <- rep(explanatory[i],nrow(tmp_tableau))
        modalities				<- rownames(tmp_tableau)
        name_variable			<- rep(nom_explanatory[i],length(modalities))						
        n_tot					    <- unname(rowSums(tmp_tableau))		
        n_in_model				<- rep("NA",nrow(tmp_tableau))		
        nb_var_to_explain		<- unname(tmp_tableau[modalities,level_to_import])		
        perc_var_to_explain		<- round(nb_var_to_explain/n_tot*100,1)
        OR						<- rep("",nrow(tmp_tableau))
        IC						<- rep("",nrow(tmp_tableau))
        pval					<- rep("",nrow(tmp_tableau))
        
        # Exporter final table
        tab									<- cbind(variable,name_variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
        colnames(tab)				<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")
        tab                     <- as.data.frame(tab)
        tab$name2           <- tab[,"name"]
        tab[-1,"name2"]     <- "" 
        tmp_tab							<- rbind(tmp_tab,tab)							
      }
      
      # 3. There in no empty group
      if( (!any(tmp_tableau==0)  &  nrow(tmp_tableau) > 1)  )		
      {
        modalities 							<-levels(as.factor(dataNotNA[,explanatory[i]]))
        variable 							  <-rep(explanatory[i],length(modalities))
        name_variable						<-rep(nom_explanatory[i],length(modalities))	
        
        n_tot								    <-  table(dataNotNA[,  explanatory[i]])
        n_in_model							<-	table(dataNotNA[which(!is.na(dataNotNA[,var_to_explain])),  explanatory[i]])
        nb_var_to_explain				<-  table(dataNotNA[, var_to_explain],dataNotNA[, explanatory[i]])
        
        tmp 							    	<- as.data.frame(		table(dataNotNA[, var_to_explain],dataNotNA[, explanatory[i]])		)
        nb_var_to_explain				<-	tmp[which(tmp$Var1 == level_to_import),"Freq"]
        perc_var_to_explain	<- round(nb_var_to_explain*100/n_in_model,1)
        
        
        if(!is.na(data_imputed)){
          fits = data_imputed %>% with(glm(formula(ff_formula(var_to_explain, explanatory[i])), family="binomial"))
          fits_pool = fits %>% pool()
          fit_imputed = fits_pool %>% fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)
          OR = c("")
          IC = c("")
          pval = c("")
          for(k in 1:nrow(fit_imputed)){
            s = as.character(fit_imputed[k,2])
            s = gsub("\\(", "", s)
            s = gsub("\\)", "", s)
            s = gsub("p=", "", s)
            s = gsub(",", "", s)
            list_res = strsplit(s, "\\, |\\,| ")[[1]]
            OR 									<- c(OR, list_res[1])
            IC 			           	<- c(IC, paste0("[",list_res[2],"]"))
            pval	 							<- c(pval, gsub('p', '',list_res[3]))
          }
        } else {
          tmp_mod    							<-  glm(dataNotNA[,var_to_explain] ~  dataNotNA[,explanatory[i]]    , family="binomial")  
          tmp_summary  						<- summary(tmp_mod)
          OR 									    <- round(exp(coef(summary(tmp_mod))[,1]),2) # replace the coefficient of intercept with  the OR of reference=1
          OR[1]								    <- "1"
          pval	 							    <- coef(summary(tmp_mod))[,4]
        
          
          # Extraction of pvalues, OR, IC
          pval	 							<- round(coef(summary(tmp_mod))[,4],3)
          pval[1] 						<- ""
          # Round
          # for (k in 2:length(modalities))	{ pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3)) }		
          
          tab 		           	<- round(exp(confint(tmp_mod)),2)
          IClow 	           	<- tab[,1]
          ICup 		           	<- tab[,2]
          tmp_IC	           	<- rbind(IClow,ICup)
          IC 			           	<- paste0("[",IClow," - ",ICup,"]")
          IC[1]		           	<- c("")
          
          if(length(OR) != length(variable) ) 
          {
            OR						<- c(OR, rep("NA",  (length(variable)-length(OR) )   )    )
            IC						<- c(IC, rep("NA",  (length(variable)-length(IC) )   )    )
            pval						<- c(pval, rep("",  (length(variable)-length(pval) )   )    )
          }
        }
        
        # Selection of variables to keep for multivariable analysis
        if(	length(as.numeric(pval[-1])[which(as.numeric(pval[-1]) < alpha_cut_multivariable)	]) > 0 | any(pval[-1] == '<0.001')) 		
        {
          tmp_variables_to_keep								              <- 				unique(variable)
          variables_to_keep_in_the_multivariable_analysis	<- c(variables_to_keep_in_the_multivariable_analysis,tmp_variables_to_keep)							         							         
        }
        
        # Export the final table
        tab									    <- cbind(variable,name_variable,modalities,n_tot,n_in_model,nb_var_to_explain,paste(perc_var_to_explain,"%"),OR,IC,pval)
        colnames(tab)						<- c("var","name","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain),"OR","95%CI","pval")
        tab                     <- as.data.frame(tab)
        tab$name2               <- ifelse( tab[,"pval"]==""  ,tab[,"name"] , "")
        tmp_tab								  <- rbind(tmp_tab,tab)							
      }
    }
  }
  
  # set the variables to use in multivariable analysis
  if(!is.na(variables_use_multivariable[1])){
    if(all(variables_use_multivariable %in% colnames(data)))
      variables_to_keep_in_the_multivariable_analysis = variables_use_multivariable
    else
      stop('ERROR! The variables you provided in parameter "variables_use_multivariable" are not included in the data matrix')
  } else {
    if(!is.na(variables_keep_multivariable)){
      if(all(variables_keep_multivariable %in% colnames(data))){
        variables_to_keep_in_the_multivariable_analysis = unique(c(variables_keep_multivariable, variables_to_keep_in_the_multivariable_analysis))
      }else{
        stop('ERROR! The variables you provided in parameter "variables_keep_multivariable" are not included in the data matrix')
      }
    }
  }
  
  print(paste0('Covariates for multivariable analysis are: ', paste(unlist(sapply(variables_to_keep_in_the_multivariable_analysis, function(x){
                                                  nom_explanatory[which(explanatory==x)]
                                                })), collapse = ', ')))
  
  # select variables to keep
  tmp_tab <- tmp_tab[ ,c("var","name","name2","levels","n total","n in model",paste0("nb ",var_to_explain),paste0("%",var_to_explain), "OR","95%CI","pval")]        
  
  
  #### glm for multivariable analysis
  if(!is.na(data_imputed)){
    fits = data_imputed %>% with(glm(formula(ff_formula(var_to_explain, variables_to_keep_in_the_multivariable_analysis)), family="binomial"))
    fits_pool = fits %>% pool()
    fit_imputed = fits_pool %>% fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)
    OR = c()
    IC = c()
    pval = c()
    for(k in 1:nrow(fit_imputed)){
      s = as.character(fit_imputed[k,2])
      s = gsub("\\(", "", s)
      s = gsub("\\)", "", s)
      s = gsub("p=", "", s)
      s = gsub(",", "", s)
      list_res = strsplit(s, "\\, |\\,| ")[[1]]
      OR 									<- c(OR, list_res[1])
      IC 			           	<- c(IC, paste0("[",list_res[2],"]"))
      pval	 							<- c(pval, gsub('p', '',list_res[3]))
    }
    tab2 = data.frame(OR = OR, IC_en_forme = IC, pval = pval)
    rownames(tab2) = fit_imputed$explanatory
  } else{
  
  
    dataNotNA = na.omit(data[, c(variables_to_keep_in_the_multivariable_analysis, var_to_explain)])
    fla <- paste(var_to_explain, ' ~ ', paste( paste0('`',variables_to_keep_in_the_multivariable_analysis,'`'), collapse="+"))
    mod3 <-  glm(	fla, data = dataNotNA, family="binomial") ;
    
    
    sum_mod_def 		<- summary(mod3)
    IC					<- round(exp(confint(mod3)),2)
    OR					<- round(exp(coef(mod3)),2)
    IC_en_forme 		<- paste("[",IC[,1],"-", IC[,2],"]")
    pval 				<- round(coef(sum_mod_def)[,4],3)
    
    # for (k in 2:length(pval))		{
    #   pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3))
    # }
    
    tmp_tab2				<-rbind(OR,IC_en_forme,pval)
    tab2 				<- t(tmp_tab2)
  }
    
  tmp_tab$OR_multiv = rep('', nrow(tmp_tab))
  tmp_tab$OR_95_multiv = rep('', nrow(tmp_tab))
  tmp_tab$p_val_multiv = rep('', nrow(tmp_tab))
  
  tab2 = as.data.frame(tab2)
  names = rownames(tab2)
  tab2 = as.data.frame(apply(tab2, 2, as.character), stringsAsFactors = F, row.names = unlist(sapply(names, function(x){gsub('`', '', x)})))
  
  tmp_tab = as.data.frame(apply(tmp_tab, 2, as.character), stringsAsFactors = F, row.names = unlist(sapply(names, function(x){gsub('`', '', x)})))

  for(i in 1:nrow(tmp_tab)){
    look = paste0(tmp_tab$var[i], tmp_tab$levels[i])
    if(look %in% rownames(tab2) ){
      # print(i)
      # print(as.numeric(tmp_tab$pval[i]))
      if(!is.na(tmp_tab$pval[i])){
        pos = which(rownames(tab2) == look)
        if(tab2$pval[pos] == '<0.001' | as.numeric(tab2$pval[pos]) < alpha_show_multivariable | all_multivariable_values == T){
          tmp_tab$OR_multiv[i] = as.character(tab2$OR[pos])
          tmp_tab$OR_95_multiv[i] = tab2$IC_en_forme[pos]
          if(is.na(data_imputed))
            tmp_tab$p_val_multiv[i] = ifelse(tab2$pval[pos] <0.001,"<0.001", round(as.numeric(tab2$pval[pos]), 3)) 
          else
            tmp_tab$p_val_multiv[i] = tab2$pval[pos]
        }
      }
    }
  }
  
  if(is.na(data_imputed))
    tmp_tab$pval = unlist(sapply(tmp_tab$pval, function(x){if(x!=''){if(x == "<0.001") "<0.001" else {if(as.numeric(x) < 0.001) "<0.001" else round(as.numeric(x),3) }} else ''}))
  
  
  
  ## multivariable logistic regression choosing variables thanks to AIC step
  if(is.na(variables_use_multivariable[1])){
    if(step_AIC == TRUE){
      dataNotNA = na.omit(data[, c(explanatory, var_to_explain)])
      fla <- paste(var_to_explain, ' ~ ', paste( paste0('`',explanatory,'`'), collapse="+"))
      mod3 <-  glm(	fla, data = dataNotNA, family="binomial") ;
      sink("/dev/null") 
      res = step(mod3, trace=0)
      sink()
      variables_to_keep_in_the_multivariable_analysis = colnames(res$model)[-which(colnames(res$model)==var_to_explain)]
    
  
      print(paste0('Covariates for multivariable analysis after stepAIC are: ', paste(unlist(sapply(variables_to_keep_in_the_multivariable_analysis, function(x){
        nom_explanatory[which(explanatory==x)]
      })), collapse = ', ')))
      
      
      ## run again multivariable
      
      if(!is.na(data_imputed)){
        fits = data_imputed %>% with(glm(formula(ff_formula(var_to_explain, variables_to_keep_in_the_multivariable_analysis)), family="binomial"))
        fits_pool = fits %>% pool()
        fit_imputed = fits_pool %>% fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)
        OR = c()
        IC = c()
        pval = c()
        for(k in 1:nrow(fit_imputed)){
          s = as.character(fit_imputed[k,2])
          s = gsub("\\(", "", s)
          s = gsub("\\)", "", s)
          s = gsub("p=", "", s)
          s = gsub(",", "", s)
          list_res = strsplit(s, "\\, |\\,| ")[[1]]
          OR 									<- c(OR, list_res[1])
          IC 			           	<- c(IC, paste0("[",list_res[2],"]"))
          pval	 							<- c(pval, gsub('p', '',list_res[3]))
        }
        tab2 = data.frame(OR = OR, IC_en_forme = IC, pval = pval)
        rownames(tab2) = fit_imputed$explanatory
      } else{
        
        
        dataNotNA = na.omit(data[, c(variables_to_keep_in_the_multivariable_analysis, var_to_explain)])
        fla <- paste(var_to_explain, ' ~ ', paste( paste0('`',variables_to_keep_in_the_multivariable_analysis,'`'), collapse="+"))
        mod3 <-  glm(	fla, data = dataNotNA, family="binomial") ;
        
        
        sum_mod_def 		<- summary(mod3)
        IC					<- round(exp(confint(mod3)),2)
        OR					<- round(exp(coef(mod3)),2)
        IC_en_forme 		<- paste("[",IC[,1],"-", IC[,2],"]")
        pval 				<- round(coef(sum_mod_def)[,4],3)
        
        # for (k in 2:length(pval))		{
        #   pval[k] <- ifelse(pval[k]<0.001,"<0.001", round(as.numeric(pval[k]), 3))
        # }
        
        tmp_tab2				<-rbind(OR,IC_en_forme,pval)
        tab2 				<- t(tmp_tab2)
      }
      
      tmp_tab$OR_multiv_AIC = rep('', nrow(tmp_tab))
      tmp_tab$OR_95_multiv_AIC = rep('', nrow(tmp_tab))
      tmp_tab$p_val_multiv_AIC = rep('', nrow(tmp_tab))
      
      tab2 = as.data.frame(tab2)
      names = rownames(tab2)
      tab2 = as.data.frame(apply(tab2, 2, as.character), stringsAsFactors = F, row.names = unlist(sapply(names, function(x){gsub('`', '', x)})))
      
      tmp_tab = as.data.frame(apply(tmp_tab, 2, as.character), stringsAsFactors = F, row.names = unlist(sapply(names, function(x){gsub('`', '', x)})))
      
      for(i in 1:nrow(tmp_tab)){
        look = paste0(tmp_tab$var[i], tmp_tab$levels[i])
        if(look %in% rownames(tab2) ){
          # print(i)
          # print(as.numeric(tmp_tab$pval[i]))
          if(!is.na(tmp_tab$pval[i])){
            pos = which(rownames(tab2) == look)
            if(tab2$pval[pos] == '<0.001' | as.numeric(tab2$pval[pos]) < alpha_show_multivariable | all_multivariable_values == T){
              tmp_tab$OR_multiv_AIC[i] = as.character(tab2$OR[pos])
              tmp_tab$OR_95_multiv_AIC[i] = tab2$IC_en_forme[pos]
              if(is.na(data_imputed))
                tmp_tab$p_val_multiv_AIC[i] = ifelse(tab2$pval[pos] <0.001,"<0.001", round(as.numeric(tab2$pval[pos]), 3)) 
              else
                tmp_tab$p_val_multiv_AIC[i] = tab2$pval[pos]
            }
          }
        }
      }
    }
  }
  
  
  rownames(tmp_tab) = NULL
  
  
  #remove first column
  tmp_tab2 = tmp_tab %>% dplyr::select(-c(var, name2, 'n in model'))
  
  tmp_tab2 = apply(tmp_tab2, 2, as.character)
  
  tmp_tab2 = rbind(colnames(tmp_tab2),tmp_tab2)
  
  if(step_AIC)
    tmp_tab2[1,] = c('Variable name', 'Level', 'N', var_to_explain, paste0('% ',var_to_explain), 'OR', 'OR(95%)', 'p', 'OR', 'OR(95%)', 'p', 'OR', 'OR(95%)', 'p' )
  else
    tmp_tab2[1,] = c('Variable name', 'Level', 'N', var_to_explain, paste0('% ',var_to_explain), 'OR', 'OR(95%)', 'p', 'OR', 'OR(95%)', 'p' )
  
  
  # remove duplicated names in first column
  rem = which(duplicated(tmp_tab2[,'name']))
  tmp_tab2[rem,'name'] = ""
  
  if(step_AIC)
    colnames(tmp_tab2) = c('', '',  'Population', '', '','Univariable', '', '', 'Multivariable', '', '', 'Multivariable (step AIC)', '', '')
  else
    colnames(tmp_tab2) = c('', '',  'Population', '', '','Univariable', '', '', 'Multivariable', '', '')
    
  return(tmp_tab2)
}



preformatTable1 <- function(stratif, stratif_order = NA, stratif2=NA, stratif2_order=NA, 
                            var_selected, names_var_selected, mydataset, missing = F, perc_by_column = F, n_digits = 1, drop_levels = T){
  
  #' Create a table summarizing baseline variables
  #'
  #' Create a table summarizing all baseline variables (both continuous and categorical) optionally stratifying by one or more variables and performing statistical tests. 
  #'
  #' @param stratif name of the variable to stratify e.g. "pcr". If NA only the whole population will be analyzed
  #' @param stratif_order vector of strings corresponding to the order of levels for the variable 'stratif'
  #' @param stratif2 name of the variable on which we want to stratify the 'stratif' variable. e.g. cancer subtype
  #' @param stratif2_order vector of strings corresponding to the order of levels for the variables 'stratif2'
  #' @param var_selected vector of names of variables to analyse in the table
  #' @param names_var_selected vector of pretty names for variables 'var_selected'
  #' @param mydataset data frame containing all variables to use
  #' @param missing logical, if NAs should be included in the levels for categorical variables 
  #' @param perc_by_column logical, if percentage should be evaluated by column intead of the default one by row  
  #' @param n_digits number of digits in p-value evaluation  
  #' @param drop_levels logical, remove unused levels                          
  #'
  #' @return A list containing the data frame with the result of table 1 and a string containing the count for missing data         
  
  library(stringr)
  library(tableone)
  library(tidyverse)
  library(Matrix)
  library(dplyr)
  
  cols = c(var_selected)
  
  if(!is.na(stratif))
    cols = c(cols, stratif)
  
  if(!is.na(stratif2))
    cols = c(cols, stratif2)
  
  mydataset = dplyr::select(mydataset, cols)
  
  if(drop_levels){
    for(c in 1:ncol(mydataset)){
      if(class(mydataset[,c]) == 'factor')
        mydataset[,c] = factor(droplevels( mydataset[,c]))
      
    }
   
  }
  
  # missing values
  if(missing == TRUE){
    for(c in 1:ncol(mydataset)){
      if(is.na(stratif) | colnames(mydataset)[c]!=stratif){
        if(!class(mydataset[,c])=="numeric" & !class(mydataset[,c])=="integer"){
          if(length(which(is.na(mydataset[,c]))) > 0){
            if(class(mydataset[,c])=="factor"){
              levels(mydataset[,c]) = c(levels(mydataset[,c]), "Missing")
            }
            mydataset[which(is.na(mydataset[,c])),c] = "Missing"
          }
        }
      }
    }
  }
  
  ## look for non normal variables, apply Shapiro-Wilk normality test with a strong p-value 1e-10 to find non normal variables
  var_non_normal = c()
  for(v in var_selected){
    if(class(mydataset[,v])=='numeric'){
      if(length(na.omit(mydataset[,v])) < 5000)
        s = shapiro.test(na.omit(mydataset[,v]))
      else
        s = shapiro.test(na.omit(mydataset[,v])[1:5000])
      if(s$p.value < 1e-10 ){
        # variable is not normal
        var_non_normal = c(var_non_normal, v)
      }
    }
  }
  
  if(length(var_non_normal) == 0)
    var_non_normal = NA
  
  # stratify on chosen variable
  if(!is.na(stratif)){
    if(length(stratif_order) < 1){
      stratif_order = names(table(mydataset[,which(colnames(mydataset)==stratif)]))
    }
    
    # 1. Table 1 in subcategories
    table1  <- CreateTableOne(vars = var_selected, strata = stratif, data = mydataset) 
    res = preformatTable1_subfunct (table1, var_selected, names_var_selected, var_non_normal, mydataset, n_digits)
    tbl = res[[1]]
    
    # 2. Table 1 in whole population
    table2 <- CreateTableOne(vars = var_selected, data=mydataset) 
    res2 = preformatTable1_subfunct (table2, var_selected, names_var_selected, var_non_normal, mydataset, n_digits)
    tbl2 = res2[[1]]
    
    # 3. Compil both tables 
    Overall=tbl2$Overall
    table1_global <- cbind(tbl,Overall)
    table1_global<-table1_global[,c(1:2, ncol(table1_global), c(match(stratif_order,colnames(table1_global))), (ncol(table1_global)-2):(ncol(table1_global)-1))]
    # table1_global = table1_global %>% dplyr::select(-test)
    
    # if the percentage is to find by column
    if(perc_by_column == T){
      
      # remove lines that are non factors
      test = 2:nrow(table1_global)
      to_remove = c()
      for(t in test){
        if(tolower(table1_global[t,1]) %in% tolower(names_var_selected))
          if(table1_global[t+1,1]!=''){
            to_remove = c(to_remove, t)
          }
      }
      if(length(to_remove) > 0)
        test = test[-which(test %in% to_remove)]
      
      if('Overall' %in% colnames(table1_global)){
        start_col = 4
      } else {
        start_col = 3
      }
      
      # remove percentage part from strings
      for(col in start_col:(ncol(table1_global)-2)){
        table1_global[,col] = unlist(lapply(1:nrow(table1_global), function(i){if(i %in% test)gsub("\\s*\\([^\\)]+\\)","",as.character(table1_global[i,col])) else table1_global[i,col]}))
      }
      
      
      for(i in test){
        row = table1_global[i,]
        for(col in start_col:(ncol(table1_global)-2)){
          table1_global[i,col] = paste0(row[col], " (",round(as.numeric(row[col])/sum(as.numeric(row[4:(ncol(table1_global)-2)]))*100, digits=n_digits), ")")
        }
      }
    }
    
    # stratify by super category if provided
    if(!is.na(stratif2)){
      test = table1_global[,ncol(table1_global)]
      table1_global = table1_global[, 1:(ncol(table1_global)-1)]
      for(order in stratif2_order){
        mydatasetPart = mydataset %>% dplyr::filter(mydataset[,which(colnames(mydataset)==stratif2)] == order)
        
        # 1. Table 1 in subcategories
        table1  <- CreateTableOne(vars = var_selected, strata = stratif, data = mydatasetPart) 
        res = preformatTable1_subfunct (table1, var_selected, names_var_selected, var_non_normal, mydatasetPart, n_digits)
        tbl = res[[1]]
        
        # 2. Table 1 in whole population
        table2 <- CreateTableOne(vars = var_selected, data=mydatasetPart) 
        res2 = preformatTable1_subfunct (table2, var_selected, names_var_selected, var_non_normal, mydatasetPart, n_digits)
        tbl2 = res2[[1]]
        
        # 3. Compil both tables 
        Overall=tbl2$Overall
        table1_global2 <- cbind(tbl,Overall)
        table1_global2<-table1_global2[,c(1:2, ncol(table1_global2), c(match(stratif_order,colnames(table1_global2))), (ncol(table1_global2)-2):(ncol(table1_global2)-1))]
        
        table1_global2 = table1_global2[, 1:(ncol(table1_global2)-1)]
        
        if(perc_by_column == T){
          for(col in 3:(ncol(table1_global2)-1)){
            table1_global2[,col] = unlist(lapply(1:nrow(table1_global2), function(i){if(i %in% test)gsub("\\s*\\([^\\)]+\\)","",table1_global2[i,col]) else table1_global2[i,col]}))
          }
          # make attentnion: percentage is not to evaluate for continuous variables
          
          for(i in test){
            row = table1_global2[i,]
            for(col in 4:(ncol(table1_global2)-1)){
              table1_global2[i,col] = paste0(row[col], " (",round(as.numeric(row[col])/sum(as.numeric(row[4:(ncol(table1_global2)-1)]))*100, digits=1), ")")
            }
          }
        }
        
        table1_global = cbind(table1_global, table1_global2[3:(3+length(stratif_order)+1)])
      }
      table1_global = cbind(table1_global, test=test)
      table1_global = apply(table1_global, 2, as.character)
      
      table1_global = rbind(colnames(table1_global), table1_global)
      names = c("","")
      stratif2_order2 = c('Overall', stratif2_order)
      for(order in stratif2_order2){
        names = c(names, order, rep("", (length(stratif_order)+1)))
      }
      names = c(names, 'test')
      colnames(table1_global) = names 
    }
    
    return(list(table1_global, res2[[2]]))
    
  } else {
    
    # 2. Table 1 in whole population
    table2 <- CreateTableOne(vars = var_selected, data=mydataset) 
    res2 = preformatTable1_subfunct (table1=table2, var_selected=var_selected, names_var_selected=names_var_selected, mydataset=mydataset)
    
    return(res2)
  }
}



preformatTable1MultipleVariables <- function(stratif_vector, stratif_order_list = NA, var_selected, names_var_selected, mydataset, missing = F, perc_by_column = F, overall = TRUE){
  #' Create a table summarizing baseline variables based on multiple variables (all at the same level, not conditional stratification)
  #'
  #' Create a table 1 for multiple variables. Each variable use all the dataset, this is not a conditional stratification but simply a repetition of table 1 on multiple variables using all data.
  #'
  #' @param stratif_vector vector of variables on which you want to stratify e.g. medication A, medication B, etc.
  #' @param stratif_order_list list with orders of levels for the variables used to stratify the variable 'stratif_vector'
  #' @param var_selected variables to analyse in the table
  #' @param names_var_selected pretty names for variables
  #' @param mydataset data
  #' @param missing if missing data analysis for categorical variables should be included as a levels
  #' @param perc_by_column if percentage should be evaluated by column intead of by row
  #' @overall if the overall population has to be reported
  #'                        
  #' @return A list containing the data frame with the result of table 1 and a string containing the count for missing data         
  #' 
  #' 
  tblAll = preformatTable1(stratif = stratif_vector[1], stratif_order = stratif_order_list[[1]], var_selected = var_selected, names_var_selected = names_var_selected, 
                           mydataset =  mydataset, missing = missing, perc_by_column = perc_by_column)
  tbl = tblAll[[1]][,1:(ncol(tblAll[[1]])-1)]
  if(overall == F)
    tbl = tbl[,-3]
  for(i in 2:length(stratif_vector)){
    tbl1 = preformatTable1(stratif = stratif_vector[i], stratif_order = stratif_order_list[[i]], var_selected = var_selected, names_var_selected = names_var_selected, 
                           mydataset =  mydataset, missing = missing, perc_by_column = perc_by_column)[[1]]
    
    if(overall)
      start = 3
    else
      start = 4
    tbl = cbind(tbl[,1:(ncol(tbl))], tbl1[,start:(ncol(tbl1)-1)])
  }
  
  tbl = cbind(tbl,test = tbl1[,ncol(tbl1)])
  
  tbl = apply(tbl, 2, as.character)
  tbl = rbind(colnames(tbl), tbl )
  
  i = 1
  names = c("","")
  for(var in stratif_vector){
    if(overall)
      names = c(names, var, rep("", (length(stratif_order_list[[i]])+1)))
    else
      names = c(names, var, rep("", (length(stratif_order_list[[i]]))))
    i = i + 1
  }
  names = c(names, 'test')
  
  colnames(tbl) = names 
  
  tbl[1,] = gsub("\\..*","",tbl[1,])
  
  
  return((tbl))
}






############### COX MODEL

############### COX MODEL

cox_univariable_multivariable <- function(ev, del, explanatory, nom_explanatory, mydataset, variables_use_multivariable = NA, alpha_cut_multivariable = 0.2, 
                                          alpha_cut_show_multivariable = 0.05){
  #' Run the cox model on the outcome variable with some predictor variables and return a table summarizing 
  #' hazard ratios, p-values and confidence intervals for the cox model. The code runs also the evaluation of the
  #' proportional hazard function by running a statistcal test. Schoenfeld residuals are plotted against time and 
  #' returned as an object. 
  #' 
  #' REMEMBER: the Cox model relies on the proportional hazards (PH) assumption, implying that the factors investigated have a constant impact on the hazard - or risk - over time.
  #'
  #' @param ev evenement variable (failure variable: 0 (censored) or 1 (event))
  #' @param del time variable corresponding to the evenement
  #' @param explanatory variables to analyse in the table
  #' @param nom_explanatory pretty names for variables
  #' @param mydataset data
  #' @param variables_use_multivariable vector of variables to use in the multivariable analysis, default NA
  #' @param alpha_cut_multivariable value used to filter variables for the multivariable analysis. Only variables associated to the outcome variables with a p-value lower than 'alpha_cut_multivariable'
  #'                             will be used for multivariable analysis, default 1
  #' @param alpha_cut_show_multivariable p-value under which values are shown in the multivariable analysis, default 0.05                       
  #' 
  #' @return A list containing the data frame with the result of table 1 and the plot of the martingale residuals, to analyse in case of violation of proportional hazard assumption
  #' 
  #' 
  
  schoenfeld_plots = list()
  
  # remove spaces and other non default characters from column names
  dataf_clean = mydataset
  explanatory = make.names(explanatory)
  colnames(dataf_clean) = make.names(colnames(dataf_clean))
  
  pvalues <- RR <- IClow <- ICup <- IC <- tab <-  tabf <- NULL
  variables_to_keep_in_the_multivariable_analysis = c()
  
  for (i in 1:length(explanatory)) {
    # violate the ph assumption?
    violated = FALSE
    
    if(!class(dataf_clean[,explanatory[i]]) %in% c("numeric","integer") ) {
      dataf_clean[,explanatory[i]] <- droplevels(as.factor(dataf_clean[,explanatory[i]]))
      tmp_tab 		    <- table(dataf_clean[,explanatory[i]])
      tmp_tab_NA 		  <- table(dataf_clean[,explanatory[i]],exclude=NULL)		 # remove if the only observation is NA for the outcome
      table_events 		<- table(dataf_clean[, ev],dataf_clean[,explanatory[i]],exclude=NULL)
      table_events_no_NA 		<- table(dataf_clean[, ev],dataf_clean[,explanatory[i]])
      tmp_ncol	      <- ncol(table_events)-1
      
      # not NA events in both variables
      if(length(which(is.na(rownames(table_events))))>0){
        titi	<- table_events[which(is.na(rownames(table_events))),-(tmp_ncol+1)] 
        tmp_tab_no_NA_for_variable		<- tmp_tab	-  titi 
      } else{
        tmp_tab_no_NA_for_variable		<- tmp_tab	
      }
      
      # if we have more than one level
      # if(	! any(tmp_tab_no_NA_for_variable==0)	) {
      
      if(  (length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&    ! any(tmp_tab==0)	&	!any(rowSums(table_events_no_NA)==0)	)	{
        #COX
        tmp_cox 			      <- coxph( formula = as.formula(paste0("Surv(",del, ",", ev, ") ~ ", explanatory[i]) ), data = dataf_clean)
        
      }
    } else {
      #tmp_cox 			      <- coxph(Surv(dataf_clean[, del], dataf_clean[, ev]) ~ dataf_clean[,explanatory[i]])
      tmp_cox 			      <- coxph( formula = as.formula(paste0("Surv(",del, ",", ev, ") ~ ", explanatory[i]) ), data = dataf_clean)
    }
    
    # p value for residuals
    test.ph =  cox.zph(tmp_cox)
    p_cox_assumption = test.ph$table[1,'p']
    if(p_cox_assumption < 0.05)
      violated = TRUE
    
    p_val_KM = NA
    # get p-value of kaplan meier in univariable 
    if(!class(dataf_clean[,explanatory[i]]) %in% c("numeric","integer") ){
      KM_fit = survdiff(formula = as.formula(paste0("Surv(I(",del, "/365.25),", ev, ") ~ ", explanatory[i]) ), data = dataf_clean)
      p_val_KM = 1 - pchisq(KM_fit$chisq, length(KM_fit$n) - 1)
      p_val_KM = ifelse(p_val_KM < 0.01, "<0.01", round(p_val_KM, 2))
    }
    
    p_cox_assumption = ifelse(p_cox_assumption < 0.01, "<0.01", round(p_cox_assumption, 2))	   
    
    # plot schoenfeld residuals. In principle, the Schoenfeld residuals are independent of time. A plot that shows a non-random pattern against time is evidence of violation of the PH assumption.
    # In this figure, the solid line is a smoothing spline fit of
    # the scaled Schoenfeld residuals against the transformed time and the dashed lines indicate ±2 standard
    # errors. A systematic deviation from a straight horizontal line would indicate a violation of the PH
    # assumption because, for a valid assumption, the coefficient(s) do not vary over time.
    plot.new()
    par(mar=c(5,5,5,2), xpd=F)
    plot(test.ph, ylab = paste0('Beta(t) for ', nom_explanatory[i]))
    abline(0,0,lty=3, col=c("red"))
    leg2=c()
    for(k in 1:length(tmp_cox$coefficients)){
      logHR = round(log(exp(tmp_cox$coefficients)[k]),2)
      abline(logHR,0,lty=4, col=c("blue"))
      leg2 = c(leg2, paste0("log(HR)=", logHR, ' (Conventional Cox)'))
    }
    par(xpd=T)
    legend(x=mean(par('usr')[c(1,2)]), # average of range of x-axis
           y=par('usr')[4]+ ((par('usr')[4] - par('usr')[3]) * 0.1), # top of the y axis with additional shift
           legend=c("log(HR)=0 (Null effect)", leg2),
           col = c('red', rep('blue', length(tmp_cox$coefficients))), lty=c(3,rep(4, length(tmp_cox$coefficients))), cex=1,
           xjust = 0.5, # centers legend at x coordinate
           yjust = 0.5) # centers legend at y coordinate)
    schoenfeld_plots[[explanatory[i]]] = recordPlot()
    dev.off()
    
    
    
    p_lr_test 		      <- summary(tmp_cox)$sctest["pvalue"]                                  # le p obtenu par le score du logrank
    p_Wald 		          <- summary(tmp_cox)$waldtest["pvalue"]                                 # le p obtenu par le score de Wald
    p_RdV 		          <- summary(tmp_cox)$logtest["pvalue"]                                 # le p obtenu par le score du RDV
    
    lowest_pval        <- min(p_lr_test,p_Wald, p_RdV,na.rm=TRUE)
    
    p_lr_test2 	 <- ifelse(p_lr_test < 0.01, "<0.01", round(p_lr_test, 2))	        
    p_lr_test3 	 <- ifelse(p_lr_test < 0.001, "<0.001", round(p_lr_test, 3))	     
    
    p_Wald2 	   <- ifelse(p_Wald < 0.01, "<0.01", round(p_Wald, 2))	         
    p_Wald3 	   <- ifelse(p_Wald < 0.001, "<0.001", round(p_Wald, 3))	     
    
    p_RdV2 	     <- ifelse(p_RdV < 0.01, "<0.01", round(p_RdV, 2))	        
    p_RdV3 	     <- ifelse(p_RdV < 0.001, "<0.001", round(p_RdV, 3))	      
    
    # Si parmi 3 tests ci dessus est signif, on a le droit de faire les test individuellement 
    #   if(lowest_pval<=alpha_cut_multivariable){
    p_indiv 				<- summary(tmp_cox)$coeff[,'Pr(>|z|)']																			# Wald test
    p_indiv2 			<- ifelse(p_indiv < 0.01,"<0.01"   , round(p_indiv, 2))                       # 
    p_indiv3 			<- ifelse(p_indiv < 0.001,"<0.001" , round(p_indiv, 3))
    #  } 
    
    
    # Extract variables to keep in multivariable analysis
    #----------------------------------------------------------------
    if(	lowest_pval < alpha_cut_multivariable)		{
      tmp_variables_to_keep								              <- 				explanatory[i]
      variables_to_keep_in_the_multivariable_analysis		<- c(variables_to_keep_in_the_multivariable_analysis,tmp_variables_to_keep)							         							         
    }
    #----------------------------------------------------------------
    if(!class(dataf_clean[,explanatory[i]]) %in% c("numeric","integer") ) {
      # Build log rank model for extracting effectives
      lr 				    <- survdiff(Surv(dataf_clean[, del], dataf_clean[, ev])~dataf_clean[ ,explanatory[i]], na.action=na.omit) #  data=dataf_clean,
      tmp_events		<- lr$obs
      tmp_effectifs	<- unlist(lr$n)
      tmp_effectifs	<- unname(tmp_effectifs)
    }
    
    RR 				    <-  round(summary(tmp_cox)$conf.int[,1],2)
    IClow 			  <- round(summary(tmp_cox)$conf.int[,3],2)
    ICup 			    <- round(summary(tmp_cox)$conf.int[,4],2)
    IC 				    <- paste0("[",round(IClow,2)," - ",round(ICup,2),"]")
    
    # if(lowest_pval >= alpha_cut_show_multivariable ){
    #   RR <- IC		<- ""
    # }
    
    if(!class(dataf_clean[,explanatory[i]]) %in% c("numeric","integer") ) {
      tab1 			    <- data.frame( variable_name=explanatory[i],Variable = nom_explanatory[i], "Class"=levels(as.factor(dataf_clean[,explanatory[i]]))[1], "HR"=1, "CI"=NA, "p_indiv2"=NA,"p_indiv3"=NA,  # Test indiv
                                 "p_log_rank2"=p_lr_test2, "p_log_rank3"=p_lr_test3,
                                 "p_Wald2"=p_Wald2, "p_Wald3"=p_Wald3, p_cox_assumption = p_cox_assumption, p_KM = p_val_KM) 
      
      tab2 			    <- data.frame(variable_name = explanatory[i],Variable = nom_explanatory[i], 	"Class"=levels(as.factor(dataf_clean[,explanatory[i]]))[-1], "HR"=RR, "CI"=IC, "p_indiv2"=p_indiv2,"p_indiv3"=p_indiv3 ,
                                "p_log_rank2"=NA, "p_log_rank3"=NA,
                                "p_Wald2"=NA, "p_Wald3"=NA, p_cox_assumption = NA, p_KM = NA)
      
      tab 			    <- rbind(tab1, tab2)
      # Add numbers from log rank model
      #print(tmp_effectifs)
      tab$Number[1:length(tmp_effectifs)] 		<- as.matrix(tmp_effectifs)
      tab$Events[1:length(tmp_effectifs)] 		<- as.matrix(tmp_events)
      tab[] 			    <- lapply(tab, as.character)
      tab$Var_unique <- c(nom_explanatory[i],  rep(NA, (length(levels(as.factor(dataf_clean[,explanatory[i]])))  -1)      )     )
      
    } else {
      tab 			    <- data.frame( variable_name=explanatory[i],Variable = nom_explanatory[i], "Class"= '', "HR"= RR, "CI"=IC, "p_indiv2"=p_indiv2,"p_indiv3"=p_indiv3,  # Test indiv
                                "p_log_rank2"=p_lr_test2, "p_log_rank3"=p_lr_test3,
                                "p_Wald2"=p_Wald2, "p_Wald3"=p_Wald3, p_cox_assumption = p_cox_assumption, p_KM = NA) 
      
      tab$Var_unique <- c(nom_explanatory[i])
      tab$Number <- length(which(!is.na(dataf_clean[,explanatory[i]])))
      tab$Events <- length(which(!is.na(dataf_clean[,explanatory[i]]) & dataf_clean[,ev]==1))
    }
    
    
    tab	<- tab[,c("variable_name","Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "p_indiv3", "p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2", 
                  "p_cox_assumption", "p_KM")] 			
    
    tabf 			<- rbind(tabf, tab)        
  } 
  # else {
  #     ## numeric variable
  #     if(class(dataf_clean[,explanatory[i]]) %in% c("numeric","integer")){
  #       tmp_cox 			      <- coxph(Surv(dataf_clean[, del], dataf_clean[, ev]) ~ dataf_clean[,explanatory[i]])
  #     
  #     } else if(	( any(tmp_tab_no_NA_for_variable==0))| (!(length(which(tmp_tab == 0)) != (length(tmp_tab)-1)  )		&
  #                                                        ! any(tmp_tab==0)	) | 	any(rowSums(table_events_no_NA)==0)   ) {
  #       print('aaaaaa')                              
  #       tmp_levels  <- levels(as.factor(dataf_clean[,explanatory[i]    ]))
  #       tab         <- matrix("NA",ncol=12,nrow=(length(tmp_levels))   )        
  #       colnames(tab) <-  c("variable_name","Variable","Var_unique", "Class", "Number","Events", "HR", "CI", "p_indiv3", "p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2") 			
  #       tabf 			<- rbind(tabf, tab)        
  #     }
  #   }
  # }
  
  # Mise en page tableau
  rownames(tabf) <- NULL
  
  tabf           <- tabf[,c("variable_name", "Variable","Var_unique", "Class", "Number","Events", "HR", "CI",  "p_indiv3","p_Wald3","p_log_rank3","p_indiv2","p_Wald2","p_log_rank2", 
                            'p_cox_assumption', 'p_KM')]   	
  mat_tab_f      <- as.matrix(tabf)
  mat_tab_f[is.na(mat_tab_f)]               <- ""
  mat_tab_f[which(mat_tab_f[,4]=="1.00"),4] <-"1"
  tabf		                                  <- mat_tab_f
  
  # ASHP : grosso modo, il faudra toujours virer dans les tableaux sauf p et p*
  colnames(tabf)  <-  c("Variable_name","Variable_trash","Variable", "Category", "n","ev", "HR", "95%CI", "p*", "p_Wald3","p","p_indiv2","p_Wald2","p_log_rank2", 
                        'p_cox_assumption', 'p_KM') 			
  
  # Remove those with variable continue; 
  
  tabf_short      <- tabf[,c("Variable_name", "Variable","Category","n","ev","HR","95%CI","p_log_rank2","p_indiv2", 'p_cox_assumption', 'p_KM') ]
  
  
  # set the variables to use in multivariable analysis
  if(!is.na(variables_use_multivariable[1])){
    if(variables_use_multivariable %in% explanatory)
      variables_to_keep_in_the_multivariable_analysis = variables_use_multivariable
    else
      stop('ERROR! The varuiables you provided in parameter "variables_use_multivariable" are not included in the "explanatory" vector')
  }
  
  
  
  ### MULIVARIATE ANALYSIS
  tmp_var			  <- variables_to_keep_in_the_multivariable_analysis
  missing_data  <- sapply(dataf_clean[,tmp_var], function(x) sum(is.na(x)))
  missing_data  <- missing_data[missing_data!=0] ; sort(missing_data)
  too_many_NA   <- 0# c("RCB_class","lvi_postneo","pCR")
  tmp_var			  <- setdiff(variables_to_keep_in_the_multivariable_analysis,too_many_NA)
  combined_mat2 <- na.omit(dataf_clean[,c(tmp_var,c(del,ev))]) 
  #print(dim(combined_mat2)); print(dim(dataf_clean))
  # Writing multivariable in the loop
  
  
  cox 			<- coxph(formula = as.formula(paste0('Surv(', del, ',', ev, ')~1+', paste(tmp_var, collapse = '+'))), data=combined_mat2, method="breslow")
  tmp_tab 		<- NULL
  tmp_tab_1 		<- NULL
  tmp_tab_2 		<- NULL
  tmp_tab_3 		<- NULL
  tmp_tab_4 		<- NULL
  tmp_summary		<- summary(cox)
  tmp_levels		<-unlist(cox$xlevels)							
  tmp_coef_et_IC_all 					<- tmp_summary$conf.int
  
  tmp_pvals_all 		<- round(coef(tmp_summary)[,5],3)
  
  for (k in 1:length(tmp_var)) {	
    tmp_vect_1		<- c("1","-","-")
    
    if(!class(dataf_clean[,tmp_var[k]]) %in% c("numeric","integer") ) {
      tmp_levels_var_k					<- cox$xlevels[[tmp_var[k]]]
      
      tmp_pos_levels						<- grep(tmp_var[k],names(tmp_levels))
      tmp_names_levels					<- tmp_levels[tmp_pos_levels]
      
      tmp_levels_2 							<- c(unname(tmp_names_levels))
      
      
      tmp_coef_et_IC_all_var_k			<- tmp_coef_et_IC_all[grep(paste0("^", tmp_var[k]),names(cox$coefficients)),]
      
      #class(tmp_coef_et_IC_all_var_k)
      if(length(tmp_levels_var_k)==2) {
        tmp_coef_et_IC_all_var_k <- t(as.matrix(tmp_coef_et_IC_all_var_k))                      
      }
      
      variable		<- rep(tmp_var[k],length(tmp_levels_var_k))
      HR				<- round(tmp_coef_et_IC_all_var_k[,1],2)
      IC 				<- paste("[", round(tmp_coef_et_IC_all_var_k[,3],2),"-",round(tmp_coef_et_IC_all_var_k[,4],2),"]")
      
      tmp_pvals_var_k		<- tmp_pvals_all[grep(paste0("^", tmp_var[k]),names(cox$coefficients))]
      lowest_pval = min(as.numeric(tmp_pvals_var_k))
      if(is.na(lowest_pval)) lowest_pval = 1
      tmp_pvals_var_k 		<- ifelse(tmp_pvals_var_k < 0.001, "<0.001", round(tmp_pvals_var_k, 3))	
      if(!lowest_pval<=alpha_cut_show_multivariable ){
        HR <- IC	<- tmp_pvals_var_k	<- rep("", length(tmp_pvals_var_k))
        tmp_vect_1 = c('','','')
      }
      
      tmp_tab_1				<- cbind(HR,IC,tmp_pvals_var_k)
      tmp_tab_2				<- rbind( tmp_vect_1,tmp_tab_1)
      # 									rownames(tmp_tab_2)[1] 	<- c(unname(tmp_names_levels[1]))
      
      tmp_tab_3				<- cbind(variable,tmp_levels_2)
      tmp_tab_4				<- cbind(tmp_tab_3,tmp_tab_2)
      tmp_tab					<- rbind(tmp_tab,tmp_tab_4)
    } else {
      pos = which(names(tmp_coef_et_IC_all[,1]) == tmp_var[k])
      variable = tmp_var[k]
      tmp_levels_2 = ''
      p = tmp_pvals_all[which(names(tmp_pvals_all)==tmp_var[k])]
      HR				<- round(tmp_coef_et_IC_all[pos,1],2)
      IC 				<- paste("[", round(tmp_coef_et_IC_all[pos,3],2),"-",round(tmp_coef_et_IC_all[pos,4],2),"]")
      lowest_pval = min(as.numeric(p))
      if(is.na(lowest_pval)) lowest_pval = 1
      p 		<- ifelse(p < 0.001, "<0.001", round(p, 3))	
      if(!lowest_pval<=alpha_cut_show_multivariable ){
        HR <- IC	<- p	<- ""
        tmp_vect_1 = c('','','')
      }
      tmp_tab_4 = cbind(variable, tmp_levels_2,HR,IC,p)
      tmp_tab					<- rbind(tmp_tab,tmp_tab_4)
    }
    
    
    
  }	
  
  tbl_univ_multiv = dplyr::left_join(as.data.frame(tabf_short), as.data.frame(tmp_tab), by=c('Variable_name'='variable', 'Category'='tmp_levels_2'))
  tbl_univ_multiv = dplyr::select(tbl_univ_multiv, -Variable_name)
  tbl_univ_multiv = apply(tbl_univ_multiv, 2, as.character)
  tbl_univ_multiv = rbind(colnames(tbl_univ_multiv), tbl_univ_multiv)
  colnames(tbl_univ_multiv) = c('', '', 'Population', '',    'univariable', '', '', '', '',  '', 'multivariable', '', '')
  tbl_univ_multiv[1,] = c("Characteristics" , "Class", "N", "Ev", "HR", "95%CI", "p*", "p", 'p_cox_assumption', 'p_KM', "HR", "95%CI", "p")
  
  tbl_univ_multiv[is.na(tbl_univ_multiv)] = ""
  
  return(list(tbl_univ_multiv, schoenfeld_plots))
}            




interactionTests = function(mydataset, variables_to_test, names_variables_to_test, variable_to_predict, variable_to_test_with){
  
  #' Run the interaction test for multiple variables with respect to a third variable
  #' e.g. For all variables run:
  #'      model1 = variable_to_predict ~ variables_to_test[1] + variable_to_test_with + variable_to_test_with * ariables_to_test[1]
  #'      model2 = variable_to_predict ~ variables_to_test[1] + variable_to_test_with
  #'      anova(model1, model2)
  #' 
  #' Return the p-value for the anova
  #'
  #' @param mydataset dataframe containing all the variables to test
  #' @param variables_to_test vector of names of variables to test
  #' @param names_variables_to_test vector of pretty names for variables to test
  #' @param variable_to_predict name of the target variable 
  #' @param variable_to_test_with name of the variable we want to include in the interaction
  #' 
  #' @return A data frame with p-values for the anova
  
  library(tidyverse)
  
  #######################################################################################################
  #Loop on all variables to test to test interactions
  #######################################################################################################
  list_interactions_glm            <- list()
  AIC_gain            <- matrix(NA,nrow = 0, ncol = 3 )
  colnames(AIC_gain) = c('AIC with interaction', 'AIC without interaction', 'AIC_gain')
  
  for (i in 1:length(variables_to_test )) {
    
    tmp_var                        <- variables_to_test[i]

    dataf   <- mydataset[,c(as.character(tmp_var),variable_to_test_with,variable_to_predict)]  
    colnames(dataf)    <-c('tmp_var', 'variable_to_test_with', 'variable_to_predict')
    head(dataf)
    
    #With interaction
    mod1      <-  glm(variable_to_predict ~ tmp_var + variable_to_test_with + tmp_var * variable_to_test_with, data=dataf , family="binomial")
    #Without interaction
    mod2      <-  glm(variable_to_predict ~ tmp_var + variable_to_test_with,data=dataf , family="binomial")
    
    
    tmp_anova <-anova(mod1,mod2, test="Chisq")
    tmp_interaction <- round(tmp_anova[,"Pr(>Chi)"][2],3)
    list_interactions_glm [[i]]  <- tmp_interaction
    
    AIC_gain = rbind(AIC_gain, c(mod1$aic, mod2$aic, mod2$aic - mod1$aic))
  }
  
  names(list_interactions_glm)  <- names_variables_to_test
  df_p_interactions_glm         <- unlist(list_interactions_glm)
  df_p_interactions_glm[which(df_p_interactions_glm <0.15)]
  
  
  df_p_interactions_glm       <- sort(df_p_interactions_glm)
  df_p_interactions_glm       <- as.data.frame(df_p_interactions_glm)
  df_p_interactions_glm       <- df_p_interactions_glm %>% rownames_to_column(var = "name")
  colnames(df_p_interactions_glm) = c('Name', 'p interaction')
  df_p_interactions_glm = cbind(df_p_interactions_glm, AIC_gain)
  
  return(df_p_interactions_glm)
}
