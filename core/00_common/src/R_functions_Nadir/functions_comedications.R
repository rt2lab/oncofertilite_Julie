
################################################       PLOT FUNCTIONS FOR MEDICATIONS

### CLEANING
# removeNonChronicMedications()                         -> Remove non systemic medications present in the data dictionary

### PLOTS
# plotMedicationTable()                                 -> Create the table of all medications by the 5 ATC levels. Each level has the number of medications belonging to that level.
# plotMedicationFirstSecondLevel()                      -> sunburst plot of first and second level
# plotLevel_n_MultipleSheets()                          -> Create a pdf with all medications at level 4 or 5 grouped by ATC level 1. Each class is plot on a different page.
# plotLevelNSameSheet()                                 -> Create a plot with the top medications for the choosen classes (classes_keep) at ATC level 1.
# plotLevel1()                                          -> Create plot for ATC level 1
# plotLevel2()                                          -> Create plot for ATC level 2
# plotLevel3()                                          -> Create plot for ATC level 3
# plotLevel4()                                          -> Create plot for ATC level 4
# plotLevel5()                                          -> Create plot for ATC level 5
# plotLevel5_with_ATC()                                 -> Create plot for ATC level 5 grouped by ATC level 2
# plotAllLevels()                                       -> Create plot for all ATC levels
# plotLevels1_2_5()                                     -> Create plot for levels 1, 2 and 5
# plotMedicationDistribution_NumberOfComedications()    -> Plot the distribution of medications with respect to the number of comedications

### CAUSAL ANALYSIS ON MEDICAIONS
# createClassesCausalAnalysis()                         -> Create variables for statistical analysis. Variables are added to the returned data frame (named ATC_.. )
# createDatasetImputation()                             -> Create dataset with imputed data (no NAs for variables in covariates_imputation)
# runTmle()                                             -> Run TMLE (Targeted Maximum Likelihood Estimation)
# comedicationEffectPlot()                              -> Plot the causal effect with OR and CIs


library(ggplot2)

############################################################################  DO NOT CALL
############################ INTERNAL FUNCTIONS ############################  THESE FUNCTIONS
############################################################################  DIRECTLY
mytheme <-  ggplot2::theme(
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.title.y=element_blank(),
  # axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)


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
          v[i] = UpperCaseParenthesis(x)
        }
      }
    }
  }
  if(length(v)==1)return(v[1])
  else return(v)
}

## split string in multiple lines such that each line has less than 25 characters
addNwelineMiddle <- function(x){
  if(length(x > 5)){
    x = gsub('(.{1,25})(\\s|$)', '\\1\n', x)
    x = gsub("[\r\n]", "", x)
  }
  return(x)
  
}

evaluateTableComedicationGrouped = function(ttConcTable, database_preprocessed_labels){
  
  if(!"subtype.f" %in% colnames(ttConcTable))
    ttConcTable$subtype.f = unlist(lapply(ttConcTable$base_cletri, function(x) {return(database_preprocessed_labels$subtype[which(database_preprocessed_labels$base_cletri==x)])}))
  
  matrix_comedic <- dplyr::select(ttConcTable, numdos7 = base_cletri, subtype.f,
                                  atc_level5_cod,atc_level5_lib,
                                  atc_level1_cod,atc_level1_lib,
                                  atc_level2_cod,atc_level2_lib,
                                  atc_level3_cod,atc_level3_lib,
                                  atc_level4_cod,atc_level4_lib,
                                  atc_level1_lib)
  
  tab_summary_global <-  matrix_comedic %>% dplyr::group_by(atc_level1_lib,atc_level1_cod,
                                                            atc_level2_lib,atc_level2_cod,
                                                            atc_level3_lib,atc_level3_cod,
                                                            atc_level4_lib,atc_level4_cod,
                                                            atc_level5_lib,atc_level5_cod) %>% 
    dplyr::summarise(count=n()) %>% 
    dplyr:: group_by(atc_level4_lib) %>% 
    dplyr:: mutate(sum_level4 = sum(count)) %>%
    dplyr:: group_by(atc_level3_lib) %>% 
    dplyr:: mutate(sum_level3 = sum(count)) %>%
    dplyr:: group_by(atc_level2_lib) %>% 
    dplyr:: mutate(sum_level2 = sum(count)) %>%
    dplyr:: group_by(atc_level1_lib) %>%
    dplyr:: mutate(sum_level1 = sum(count))  %>% 
    dplyr::select (atc_level1_lib,atc_level1_cod,sum_level1,
                   atc_level2_lib,atc_level2_cod,sum_level2,
                   atc_level3_lib,atc_level3_cod,sum_level3,
                   atc_level4_lib,atc_level4_cod, sum_level4,
                   atc_level5_lib,atc_level5_cod,count) %>% as.data.frame()
  
  tab_summary_global <-  tab_summary_global[rev(order( tab_summary_global$sum_level1, tab_summary_global$sum_level2,
                                                       tab_summary_global$sum_level3,tab_summary_global$sum_level4 )),]
  
  head(tab_summary_global) ;dim(tab_summary_global) # 202 15
  
  # tab_summary_global$atc_level5_lib = paste0(tab_summary_global$atc_level5_lib,"__",tab_summary_global$atc_level5_cod)
  # tab_summary_global$atc_level4_lib = paste0(tab_summary_global$atc_level4_lib,"__",tab_summary_global$atc_level4_cod)
  
  return(tab_summary_global)
}


evaluateTabSummaryComed = function(tab_summary_gl){
  tab_summary_comed <- tab_summary_gl %>% dplyr::filter(atc_level1_cod !=0,
                                                        count > 0) %>% 
    dplyr::mutate(level_5_and_letter_2lines = NA) %>%
    dplyr::arrange(.,count)
  head(tab_summary_comed)
  
  for (i  in 1 : length(tab_summary_comed$atc_level5_cod) ) {
    # i=1
    tab_summary_comed$level_5_and_letter_2lines[i] <- paste(strwrap(tab_summary_comed$atc_level5_lib[i], 
                                                                    width = 50), collapse = "\n") 
    tab_summary_comed$level_4_and_letter_2lines[i] <- paste(strwrap(tab_summary_comed$atc_level4_lib[i], 
                                                                    width = 50), collapse = "\n") 
  }
  
  tab_summary_comed$level_5_and_letter_2lines <- as.character(tab_summary_comed$level_5_and_letter_2lines)
  tab_summary_comed$level_5_and_letter_2lines <- factor(tab_summary_comed$level_5_and_letter_2lines, levels=unique(tab_summary_comed$level_5_and_letter_2lines))
  
  tab_summary_comed$level_4_and_letter_2lines <- as.character(tab_summary_comed$level_4_and_letter_2lines)
  tab_summary_comed$level_4_and_letter_2lines <- factor(tab_summary_comed$level_4_and_letter_2lines, levels=unique(tab_summary_comed$level_4_and_letter_2lines))
  # levels(tab_summary_comed$level_5_and_letter_2lines) =  gsub('\n', '', levels(tab_summary_comed$level_5_and_letter_2lines))
  return(tab_summary_comed)
}


color_anat          <- c("Alimentary tract and metabolism"="#0DB14B",
                         "Cardiovascular system"= "#CC004C", #B70F1D",#
                         "Systemic hormonal preparations, excl. sex hormones and insulins"="#F37021",
                         "Systemic hormonal preparations, excl. Sex hormones and insulins"="#F37021",
                         "Nervous system"="#0089D0",
                         "Others"="#868686",
                         "Antiinfectives for systemic use"="#A186B5", 
                         "Dermatologicals"="#DD86B5",
                         "Blood and blood forming organs"= "#CC004C",
                         "Musculo-skeletal system"="#FCB711",
                         "Respiratory system" = "#6460AA",
                         "Antiparasitic products, insecticides and repellents" = "#FF6633",
                         "Sensory organs" = "#FFCCCC",
                         "Antineoplastic and immunomodulating agents"="#9999CC",
                         "Genito urinary system and sex hormones"="#004BE1",
                         "Various"="#fc5b11")


shortNameMedication   <- list("Alimentary tract and metabolism"="Alimentary",
                              "Cardiovascular system"="Cardiovascular \nsystem",
                              "Systemic hormonal preparations, excl. sex hormones and insulins"="Hormonal",
                              "Nervous system"="Nervous \nsystem",
                              "Others"="#facb66",
                              "Antiinfectives for systemic use"="Antiinfectives", 
                              "Dermatologicals"="Dermato",
                              "Blood and blood forming organs"= "Blood  ",
                              "Musculo-skeletal system"="Muscular",
                              "Respiratory system" = "Respiratory",
                              "Antiparasitic products, insecticides and repellents" = "Antiparasitic products",
                              "Sensory organs" = "Sensory organs",
                              "Antineoplastic and immunomodulating agents"="Antineoplastic and immunomodulating agents",
                              "Genito urinary system and sex hormones"="Genito urinary system",
                              "Various"="Various",
                              "NA"="NA")

getMaximumDataDictFile = function(path_data_dictionary_folder){
  # load maximum date
  files = list.files(path_data_dictionary_folder)
  files = files[which(grepl('datadict_RT2', files))]
  if(any(grepl('~', files)))
    files = files[-which(grepl('~', files))]
  version = max(as.numeric(substr(files, 15, 16)))
  fileLoad = paste0(path_data_dictionary_folder, "/datadict_RT2_v",version, '.xlsx')
  return(fileLoad)
}

# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)




##############################################################################
########################## END INTERNAL FUNCTIONS ############################
##############################################################################


removeNonChronicMedications = function(data_medication_long, path_data_dictionary_folder, project){
  #' Remove non systemic medications present in the data dictionary
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param path_data_dictionary_folder path of the folder where the data dictionary is stored. NOTE: the path is the one of the folder, NOT the one of the file
  #' @param project name of the column in the data dictionary corresponding to our project e.g. 'class_to_discard_CANTO'
  #' @return list of 2 data frames: one with the chronic medications, one with the removed ones
  #' 
  library(xlsx)
  f2 = getMaximumDataDictFile(path_data_dictionary_folder)
  data_dict = read.xlsx(f2, sheetName = 'comedic_atc_remove', as.data.frame = T)
  ATCtoRemove = as.character(data_dict$ATC_cod)
  ATCtoRemove = ATCtoRemove[which(trim(as.character(data_dict[,project])) == 'Yes')]
  removedNonSystemic = c()
  
  for(ATC_remove in ATCtoRemove){
    removedNonSystemic = c(removedNonSystemic, grep(paste0('^',ATC_remove), data_medication_long$atc_cod))
  }
  
  data_medication_long_chronic = data_medication_long[-removedNonSystemic,]
  data_medication_long_removed = data_medication_long[which(!data_medication_long$atc_cod %in% data_medication_long_chronic$atc_cod),]
  
  return(list(data_medication_long_chronic,data_medication_long_removed))
}




plotMedicationTable = function(data_medication_long){
  #' Create the table of all medications by the 5 ATC levels. Each level has the number of medications belonging to that level.
  #'
  #' @param data_medication_long frame of medications, long version
  #' @return the table with all medications by all 5 ATC levels
  #' 
  
  library(dplyr)
  tbl = as.data.frame(matrix(NA, nrow = 100000, ncol = 10), stringsAsFactors = F)
  colnames(tbl) = c("ATC level 1", "n", "ATC level 2", "n", "ATC level 3", "n", "ATC level 4", "n", "ATC level 5", "n")
  
  # level 1
  med1 = sort(unique(data_medication_long$atc_level1_cod))
  tmp = 1 
  for(m in med1){
    size=    nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level2_lib!=atc_level1_lib & atc_level1_cod==m & nchar(atc_level5_cod)==7), atc_level5_cod)%>% dplyr::summarise())
    tbl[tmp, 1] = paste(firstUp(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==m)[1]]), paste0('(', toupper(m), ')'))
    tbl[tmp, 2] = length(which(data_medication_long$atc_level1_cod==m))
    
    # level2
    med2 = as.data.frame(dplyr::group_by(data_medication_long %>% dplyr::filter( atc_level1_cod==m), firstUp(atc_level2_cod)) %>% dplyr::summarise())[,1]
    med2 = med2[which(nchar(med2) == 3)]
    tmp2 = tmp
    for(m2 in med2){
      tbl[tmp2, 3] = paste(firstUp(data_medication_long$atc_level2_lib[which(data_medication_long$atc_level2_cod==m2)[1]]), paste0('(', toupper(m2), ')'))
      tbl[tmp2, 4] = length(which(data_medication_long$atc_level2_cod==m2))
      size2 = nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level2_lib!=atc_level1_lib & atc_level2_cod==m2 & nchar(atc_level5_cod)==7), atc_level5_cod)%>% dplyr::summarise())
      
      # level3
      med3 = toupper(as.data.frame(dplyr::group_by(data_medication_long %>% dplyr::filter( atc_level2_cod==m2), firstUp(atc_level3_cod)) %>% dplyr::summarise())[,1])
      med3 = med3[which(nchar(med3) == 4)]
      tmp3 = tmp2
      for(m3 in med3){
        tbl[tmp3, 5] = paste(firstUp(data_medication_long$atc_level3_lib[which(data_medication_long$atc_level3_cod==m3)[1]]), paste0('(', toupper(m3), ')'))
        tbl[tmp3, 6] = length(which(data_medication_long$atc_level3_cod==m3))
        size3 = nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level2_lib!=atc_level1_lib & atc_level3_cod==m3 & nchar(atc_level5_cod)==7), atc_level5_cod)%>% dplyr::summarise())
        
        # level4
        med4 = toupper(as.data.frame(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level3_cod==m3), firstUp(atc_level4_cod)) %>% dplyr::summarise())[,1])
        med4 = med4[which(nchar(med4) == 5)]
        tmp4 = tmp3
        for(m4 in med4){
          tbl[tmp4, 7] = paste(firstUp(data_medication_long$atc_level4_lib[which(data_medication_long$atc_level4_cod==m4)[1]]), paste0('(', toupper(m4), ')'))
          tbl[tmp4, 8] = length(which(data_medication_long$atc_level4_cod==m4))
          # size4= max(nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level5_lib!=atc_level4_lib & atc_level4_cod==m4), firstUp(atc_level5_lib))%>% dplyr::summarise()),
          #            nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level4_lib!=atc_level3_lib & atc_level4_cod==m4), firstUp(atc_level5_lib))%>% dplyr::summarise()),
          #            nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level3_lib!=atc_level2_lib & atc_level4_cod==m4), firstUp(atc_level5_lib))%>% dplyr::summarise()),
          #            nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level2_lib!=atc_level1_lib & atc_level4_cod==m4), firstUp(atc_level5_lib))%>% dplyr::summarise()))
          
          
          # level5
          med5 = toupper(as.data.frame(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level4_cod==m4), firstUp(atc_level5_cod)) %>% dplyr::summarise())[,1])
          med5 = med5[which(nchar(med5) == 7)]
          tmp5 = tmp4
          for(m5 in med5){
            tbl[tmp5, 9] = paste(firstUp(data_medication_long$atc_level5_lib[which(data_medication_long$atc_level5_cod==m5)[1]]), paste0('(', toupper(m5), ')'))
            tbl[tmp5, 10] = length(which(data_medication_long$atc_level5_cod==m5))
            #size5= nrow(dplyr::group_by(data_medication_long %>% dplyr::filter(atc_level4_lib!=atc_level5_lib & atc_level5_cod==m5), firstUp(atc_level5_lib))%>% dplyr::summarise())
            
            tmp5 = tmp5 + 1
          }
          
          tmp4 = tmp4 + length(med5)
        }
        
        tmp3 = tmp3 + size3
      }
      
      tmp2 = tmp2 + size2
    }
  
    tmp = tmp + size
  }
  
  ind <- apply(tbl, 1, function(x) all(is.na(x)))
  tbl <- tbl[ !ind, ]
  
  tbl[is.na(tbl)] = ''
  
  
  return(tbl)
}



plotMedicationFirstSecondLevel = function(data_medication_long, file=NA, remove_duplicated_medication_for_one_patient = F, 
                                          legend_size_squares = NA, label_size_legend = NA, label_size_plot = 20, threshold_ratio = 0.05, percentage = F, plot_type = 'sunburst'){
  #' 
  #' Create the sunburst plot with first and second ATC levels
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param file where to save the plot, not necessary 
  #' @param remove_duplicated_medication_for_one_patient if TRUE, the function removes duplicated medications (ATC level 2) for the same patient
  #' @param legend_size_squares size of the legend squares, if NA it will be evaluated automatically
  #' @param label_size_legend size of labels for legend
  #' @param label_size_plot size of labels for plot
  #' @param threshold_ratio threshold ratio for writing the number of medication belonging to that level (avoid to write on very small regions)
  #' @param percentage boolean for writiting the percentage of this medication with respect to the whole number of medications
  #' @return the table with all medications by all 5 ATC levels
  #' 
  #' 
  library(plotly)
  library(dplyr)
  library(gridExtra)
  library(ggpubr)
  library(cowplot)
  
  if(!plot_type %in% c('sunburst', 'treemap'))
    stop('Plot type not supported!')
  
  data_cpy = data_medication_long
  
  if(remove_duplicated_medication_for_one_patient){
    data_cpy = data_cpy[-which(duplicated(data_cpy[, c('base_cletri', 'atc_level1_cod')])), ]
    data_medication_long = data_medication_long[-which(duplicated(data_medication_long[, c('base_cletri', 'atc_level2_cod')])), ]
  }
  
  firstOrder = as.data.frame(data_medication_long %>% group_by(atc_level1_cod, atc_level1_lib) %>% dplyr::summarize(count=n()) %>% arrange(desc(count)))
  firstOrder$atc_level1_lib =  unlist(lapply(firstOrder$atc_level1_lib, function(x){firstUp(x)}))
  firstOrder$shortName =  unlist(lapply(1:nrow(firstOrder), function(i){if(firstOrder$count[i] / nrow(data_medication_long) > threshold_ratio){
      if(percentage){
        paste0(shortNameMedication[firstOrder$atc_level1_lib[i]], "\n n=", firstOrder$count[i], "(", round(firstOrder$count[i] / nrow(data_medication_long) * 100,1),"%)")
      } else {
        paste0(shortNameMedication[firstOrder$atc_level1_lib[i]], "\n n=", firstOrder$count[i])
      }
    } else {shortNameMedication[firstOrder$atc_level1_lib[i]]}}))
    
  
  firstOrderTmp = as.data.frame(data_cpy %>% group_by(atc_level1_lib_cod, atc_level1_lib) %>% dplyr::summarize(count=n()) %>% arrange(desc(count)))
  firstOrderTmp$atc_level1_lib =  unlist(lapply(firstOrderTmp$atc_level1_lib, function(x){firstUp(x)}))
  firstOrderTmp$shortName =  unlist(lapply(1:nrow(firstOrderTmp), function(i){if(firstOrderTmp$count[i] / nrow(data_cpy) > threshold_ratio) {
    if(percentage){
      paste0(shortNameMedication[firstOrderTmp$atc_level1_lib[i]], "\n n=", firstOrderTmp$count[i],"(", round(firstOrderTmp$count[i] / nrow(data_medication_long) * 100,1),"%)")
    } else {
      paste0(shortNameMedication[firstOrderTmp$atc_level1_lib[i]], "\n n=", firstOrderTmp$count[i])
      
    }
  } else {shortNameMedication[firstOrderTmp$atc_level1_lib[i]]}}))
  
  firstOrderTmp = firstOrderTmp[order(match(firstOrderTmp[,1],firstOrder[,1])),]
  
  firstOrder$correct = firstOrderTmp$shortName
  
  secondOrder = data_medication_long %>% group_by(atc_level2_cod, atc_level2_lib, atc_level1_lib, atc_level1_cod) %>% dplyr::summarize(count=n()) 
  secondOrder1 = secondOrder %>% group_by(atc_level1_cod) %>% dplyr::summarise(sum = sum(count)) %>% mutate( sum = sum )
  secondOrder2 = inner_join(secondOrder, secondOrder1)  
  secondOrder2$atc_level1_lib =  unlist(lapply(secondOrder2$atc_level1_lib, function(x){firstUp(x)}))
  secondOrder2$shortName =  unlist(lapply(1:nrow(secondOrder2), function(i){if(secondOrder2$sum[i] / nrow(data_medication_long) > threshold_ratio){
    if(percentage){
      paste0(shortNameMedication[secondOrder2$atc_level1_lib[i]], "\n n=", secondOrder2$sum[i],"(", round(secondOrder2$sum[i] / nrow(data_medication_long) * 100,1),"%)")
    } else {
      paste0(shortNameMedication[secondOrder2$atc_level1_lib[i]], "\n n=", secondOrder2$sum[i])
    }
  } else {shortNameMedication[secondOrder2$atc_level1_lib[i]]}}))
  
  secondOrder2$shortNameCorrection = unlist(lapply(secondOrder2$shortName, function(x){if(!x %in% firstOrder$shortName) x else{ firstOrder$correct[which(firstOrder$shortName==x)]} } ))

  
  secondOrder3 = secondOrder2 %>% arrange(desc(sum),atc_level1_cod,desc(count))
  
  secondOrder3$atc_level1_lib =  unlist(lapply(secondOrder3$atc_level1_lib, function(x){firstUp(x)}))
  colors1=unlist(lapply(firstOrder$atc_level1_lib, function(x){color_anat[x]}))
  colors2=c()
  pos = 1
  for(c in colors1){
    colfunc <- colorRampPalette(c(c, "white"))
    colors = colfunc(length(which(substr(secondOrder3$atc_level2_cod,1,1)==firstOrder$atc_level1_cod[pos]))+2)
    colors2 = c(colors2, colors[2:(length(colors)-1)])
    pos = pos + 1
  }
  names(colors2) = secondOrder3$atc_level2_cod
  
  colors=c("#FFFFFF",colors1,colors2)
  colorsList = list(colors=colors)
  name = paste0("All\n comedications\n n=", nrow(data_medication_long))
  
  
  lb2 = unlist(lapply(1:nrow(secondOrder3), function(i){if(secondOrder3$count[which(secondOrder3$atc_level2_cod==secondOrder3$atc_level2_cod[i])] / nrow(data_medication_long) > threshold_ratio){
    paste0(secondOrder3$atc_level2_cod[i],"\n n=", secondOrder3$count[i])
  }else{secondOrder3$atc_level2_cod[i]} }))
  
  lb2_short = unlist(lapply(1:nrow(secondOrder3), function(i){secondOrder3$atc_level2_cod[i] }))
  
  labels = c(name, firstOrder$correct, lb2)
  
  ##LEGEND START
  # store vectors for clors and labels for the legend
  # n=gsub("\\n","",as.character(shortNameMedication[firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==substr(labels[2],1,1) )]))]))
  n=gsub("\\n","",as.character(shortNameMedication[firstUp(as.character(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==substr(labels[2],1,1) )][1]))]))
  labels2=c(bquote(bold(.(n))))
  colors2=c("white")
  borderColor = c("white")
  count = 1
  for(i in (length(firstOrder$shortName)+2):length(labels)){
    #labels2 = c(labels2, paste0(substr(labels[i],1,3), ": ", firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==substr(labels[i],1,3) )]))))
    labels2 = c(labels2, paste0(substr(labels[i],1,3), ": ", firstUp(as.character(data_medication_long$atc_level2_lib[which(data_medication_long$atc_level2_cod==substr(labels[i],1,3) )][1]))))
    colors2 = c(colors2, colors[i])
    borderColor = c(borderColor, "black")
    if(i+1 <= length(labels)){
      if(substr(labels[i],1,1) != substr(labels[i+1],1,1) ){
        #n = gsub("\\n","",as.character(shortNameMedication[firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==substr(labels[i+1],1,1) )]))]))
        n = gsub("\\n","",as.character(shortNameMedication[firstUp(as.character(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==substr(labels[i+1],1,1) )]))]))
        labels2 = c(labels2, bquote(bold(.(n))))
        colors2 = c(colors2, "white")
        borderColor = c(borderColor, "white")
        count = count + 1
      }
    }
  }
  
  # fake data_medication_long to plot
  df <- data.frame(value = runif(length(labels2), 0, 1),
                   x = rep(1:length(labels2), each = 1),
                   group1 = rep(1:length(labels2)),
                   group2 = rep(c("fake"), each = length(labels2)))
  
  if(is.na(legend_size_squares))
  {
    legend_size_squares = 0.45
    label_size_legend = 8
    if( nrow(secondOrder3) > 40){
      legend_size_squares = 0.4
      label_size_legend = 8
    }
    if( nrow(secondOrder3) > 60){
      legend_size_squares = 0.4
      label_size_legend = 8
    }
    if( nrow(secondOrder3) > 70){
      legend_size_squares = 0.3
      label_size_legend = 7
    }
  }
  
  # plot for legend
  p2 <- ggplot(df, aes(x = x, y = value,
                       fill = interaction(group2, group1),
                       color = interaction(group2, group1))) +
    geom_col(position = "dodge") +
    scale_fill_manual("",values = colors2, labels=labels2)  +
    scale_color_manual("",values = borderColor, labels=labels2) +
    guides(fill=guide_legend(ncol=2)) +
    theme(legend.text=element_text(size=label_size_legend), legend.key.size = unit(legend_size_squares, "cm"))
  
  
  legend <- cowplot::get_legend(p2)
  legend_plot_ttt <- as_ggplot(legend) + theme(plot.margin = unit(c(1,2,1,1), "cm"))
  ##LEGEND END 
  
  
  if(plot_type == 'sunburst'){
    p= plot_ly(
      labels = labels,
      parents = c("",rep(name, nrow(firstOrder)),                    secondOrder3$shortNameCorrection),
      values = c(sum(firstOrder$count),firstOrder$count,secondOrder3$count),
      type = 'sunburst',
      branchvalues = 'total',
      leaf=list(opacity = 1),
      marker = colorsList,
      size=I(label_size_plot))
    filename="sunburstComeds.png"
    
    # save the plot
    if (!require("processx")) install.packages("processx")
    orca(p, filename, width = 800, height = 800) ##orca is the replace of export function
  } else {
    p= plot_ly(
      labels = unlist(lapply(labels, function(x){gsub('\n','',x)})),
      parents = unlist(lapply(c("",rep(name, nrow(firstOrder)),                    secondOrder3$shortNameCorrection), function(x){gsub('\n','',x)})),
      values = c(sum(firstOrder$count),firstOrder$count,secondOrder3$count),
      type = 'treemap',
      branchvalues = 'total',
      marker = colorsList,
      size=I(label_size_plot)) 
    filename="treemap.png"
    
    # save the plot
    if (!require("processx")) install.packages("processx")
    orca(p, filename, width = 700, height = 700) ##orca is the replace of export function
  }
  


  if(plot_type == 'sunburst'){
    plot_1<- ggdraw(clip="on") + draw_image(filename,scale = 0.9)
    plot_1<- plot_1 + theme(plot.margin = unit(c(-0.2,-1,-0.5,0), "cm"))
  } else {
    plot_1<- ggdraw(clip="on") + draw_image(filename,scale = 0.9)
    plot_1<- plot_1 + theme(plot.margin = unit(c(-0.2,-1,-0.5,0), "cm"))
    plot_1
  }



  #pl <- grid.arrange(p,legend_plot_ttt,ncol = 2,top="Main Title")
  g <- grid.arrange( grobs = list(plot_1,legend_plot_ttt), ncol = 2, widths=c(4,5))
 
  g2 <- cowplot::ggdraw(g) + 
  #   # same plot.background should be in the theme of p1 and p2 as mentioned above
  theme(plot.background = element_rect(fill="white", color = NA))
  
  
  if(!is.na(file)){
    ggsave(file, width = 14, height = 8, dpi = 200, units = "in", device='png')
  }
  return(p)
} 






plotLevel_n_MultipleSheets = function(data_medication_long, database_preprocessed_labels, filename=NA, lev=5){
  #' Create a pdf with all medications at level 4 or 5 grouped by ATC level 1. Each class is plot on a different page.
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels mapped dataset with all variables for patients
  #' @param filename file where to store plots
  #' @param lev do you want level 4 or 5 of the ATC?
  #' 
  #' @return no returned object
  #' 
   
  tab_summary_global = evaluateTableComedicationGrouped(data_medication_long, database_preprocessed_labels)
  tab_summary_comed = evaluateTabSummaryComed(tab_summary_global)
  
  tbl = table(tab_summary_comed$atc_level1_lib)
  p_level_5=list()
  if(!is.na(filename))
    pdf(filename)
  
  
  for(t in names(tbl)){
    if(lev == 5){
      tb <- filter(tab_summary_comed, atc_level1_lib==t)
      tb$level1_regrouped_fU = sapply(tb$atc_level1_lib, firstUp)
      print(ggplot(tb,aes(x=level_5_and_letter_2lines,y=count, fill=level1_regrouped_fU))+ 
              geom_histogram(stat="identity")+coord_flip() + 
              scale_fill_manual(name = "Anatomical class", values= color_anat  )  +  
              xlab("")+ylab("")+ ggtitle (t)+
              #facet_wrap(~atc_level1_lib, scales = "free", ncol = 1, strip.position = 'top') +
              geom_text(aes(label=count), position=position_dodge(width=1), hjust=-.15, size=1.5) +
              ylim(0,(max(tb$count)+100))+
              theme_minimal() +
              theme(axis.text=element_text(size=4), text = element_text(size = 5), legend.position="none", plot.title = element_text(face="plain"),
              plot.background = element_rect(fill="white", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),)) +
              guides(fill=guide_legend(nrow=2,byrow=TRUE))
    } else {
      if(lev == 4){
        
        
        tab_summary_comed_4 <- tab_summary_global %>% dplyr::filter(!is.na(atc_level4_lib)) %>% 
          dplyr::select(atc_level1_lib,atc_level4_lib,sum_level4,atc_level4_cod)%>%  unique() %>%
          dplyr::mutate(level_4_and_letter = paste0(atc_level4_lib," (",atc_level4_cod,")"),
                        level_4_and_letter_2lines = NA) %>%
          dplyr::arrange(.,sum_level4)

        tb <- filter(tab_summary_comed_4, atc_level1_lib==t)
        
        tb$level_4_and_letter <- as.character(tb$level_4_and_letter)
        tb$level_4_and_letter <- factor(tb$level_4_and_letter,
                                                                levels=unique(tb$level_4_and_letter))
        
        tb$level1_regrouped_fU = sapply(tb$atc_level1_lib, firstUp)
        print(ggplot(tb,aes(x=level_4_and_letter,y=sum_level4, fill=level1_regrouped_fU))+ 
                geom_histogram(stat="identity")+coord_flip() + 
                scale_fill_manual(name = "Anatomical class", values= color_anat  )  +  
                xlab("")+ylab("")+ ggtitle (t)+
                #facet_wrap(~atc_level1_lib, scales = "free", ncol = 1, strip.position = 'top') +
                geom_text(aes(label=sum_level4), position=position_dodge(width=1), hjust=-.15, size=1.5) +
                ylim(0,(max(tb$sum_level4)+100))+
                theme_minimal() +
                theme(axis.text=element_text(size=4), text = element_text(size = 5), legend.position="none", plot.title = element_text(face="plain"),
                      plot.background = element_rect(fill="white", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(),))
      }
    }
    
  }
  
  library(gridExtra)
  
  if(!is.na(filename))
    dev.off()
}


plotLevelNSameSheet = function(data_medication_long, database_preprocessed_labels, filename=NA, classes_keep = c('N', 'C', 'A'),
                               lev=5, label_axis_size = 10, names_size = 12, legend_size = 15, legend = T, min_occurency = 1){
  #' Create a plot with the top medications for the choosen classes (classes_keep) at ATC level 1.
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param classes_keep classes on ATC level 1 to be put n the plot
  #' @param lev level 4 or level 5 of the ATC, used to list active principles
  #' @param label_axis_size plot 
  #' @param names_size size for names of active principles
  #' @param legend_size size for legend
  #' @param legend should the legend be put on the plot? 
  #' @param min_occurency minimun number of times a principe active must be present to be put on the plot
  #' 
  #' @return no returned object
  #' 
  
  require(gridExtra)
  
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels)
  tab_summary_comed = evaluateTabSummaryComed(tab_summary_global)
  
  if(!is.na(filename))
    pdf(filename)
  
  #namesKeep = sapply(classes_keep, function(x){firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==x)]))})
  namesKeep = unique(unlist(sapply(classes_keep, function(x){firstUp(as.character(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==x)]))})))
  tab_summary_comed$atc_level1_lib[which(!tab_summary_comed$atc_level1_lib %in% namesKeep)] = 'Others'
  # tab_summary_comed_keep = tab_summary_comed[which(tab_summary_comed$atc_level1_lib %in% namesKeep),]
  tab_summary_comed$atc_level1_lib = as.character(sapply(tab_summary_comed$atc_level1_lib, firstUp))
  
  tab_summary_comed = dplyr::filter(tab_summary_comed, count >= min_occurency & nchar(atc_level5_cod)>6)
  tab_summary_comed$level_5_and_letter_2lines = as.character(tab_summary_comed$level_5_and_letter_2lines)
  tab_summary_comed$level_5_and_letter_2lines = factor(tab_summary_comed$level_5_and_letter_2lines, levels =  tab_summary_comed$level_5_and_letter_2lines, labels =  as.character(sapply(as.character(tab_summary_comed$level_5_and_letter_2lines), firstUp)))
  
  dd = data.frame(name = paste0(firstUp(tab_summary_comed$atc_level2_lib), " (", tab_summary_comed$atc_level2_cod, ")"), ATC = tab_summary_comed$atc_level2_cod)
  
  dd2 = dd[with(dd, order(ATC)), ]
  
  
  tab_summary_comed$level_2_name_and_ATC = factor(x = dd$name, levels = unique(dd2$name))
  
  v = c("Nervous system",  "Cardiovascular system" , "Alimentary tract and metabolism", "Others")
  
  
  if(legend)
    position = 'top'
  else
    position = "none"
    
  if(lev == 5){
    pl = list()
    for(family in v){
     # if()
      tab_summary_comedFamily = tab_summary_comed %>% filter(atc_level1_lib==family)

      pl[[family]] = ggplot(tab_summary_comedFamily,aes(x=level_5_and_letter_2lines, y=count, fill=atc_level1_lib))+ 
              geom_histogram(stat="identity")+coord_flip() + 
              scale_fill_manual(values= color_anat  )  +  
              xlab("")+ylab("") +
              ggforce::facet_col(vars(level_2_name_and_ATC), scales = "free_y", space = "free") +
              #facet_wrap(~atc_level2_lib, scales = "free_y", ncol = , strip.position = 'top') +
              geom_text(aes(label=count), position=position_dodge(width=1), hjust=-.18, size=4,) +
              ylim(0,(max(tab_summary_comedFamily$count)*1.2))+
              ggtitle(family) +
              theme(plot.title = element_text(hjust = 0.5, size=19), 
                    strip.text.x = element_text(size = 17),
                    axis.text=element_text(size=label_axis_size), text = element_text(size = 14), legend.position=position,
                    plot.background = element_rect(fill="white", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), legend.text = element_text(size = legend_size))
      
    }
    do.call("grid.arrange", c(pl, ncol=2))
      
      
  } else {
    if(lev == 4){
      tab_summary_comed_4 <- tab_summary_global %>% dplyr::filter(!is.na(atc_level4_lib)) %>% 
        dplyr::select(atc_level1_lib,atc_level4_lib,sum_level4,atc_level4_cod)%>%  unique() %>%
        dplyr::mutate(level_4_and_letter = paste0(atc_level4_lib," (",atc_level4_cod,")"),
                      level_4_and_letter_2lines = NA) %>%
        dplyr::arrange(.,sum_level4)
      
      tb <- filter(tab_summary_comed_4, atc_level1_lib==t)
      
      tb$level_4_and_letter <- as.character(tb$level_4_and_letter)
      tb$level_4_and_letter <- factor(tb$level_4_and_letter,
                                      levels=unique(tb$level_4_and_letter))
      
      tb$level1_regrouped_fU = sapply(tb$atc_level1_lib, firstUp)
      pl = ggplot(tb,aes(x=level_4_and_letter,y=sum_level4, fill=level1_regrouped_fU))+ 
              geom_histogram(stat="identity")+coord_flip() + 
              scale_fill_manual(name = "Anatomical class", values= color_anat  )  +  
              xlab("")+ylab("")+ ggtitle (t)+
              #facet_wrap(~atc_level1_lib, scales = "free", ncol = 1, strip.position = 'top') +
              geom_text(aes(label=sum_level4), position=position_dodge(width=1), hjust=-.1, size=1.3) +
              ylim(0,(max(tb$sum_level4)+100))+
              ggtitle('') +
              theme_minimal() +
              theme(axis.text=element_text(size=4), text = element_text(size = 5), legend.position="none", plot.title = element_text(face="plain"),
                    strip.text.x = element_text(size = 12),
                    plot.background = element_rect(fill="white", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank())
      }
    }
  
  if(!is.na(filename))
    dev.off()
}



plotLevel1 <- function(data_medication_long, database_preprocessed_labels, ncols_legend = 2, nrows_legend = 3, percentage=T, remove_duplicated_medication_for_one_patient = F, 
                       numbers_size = 10, legend = T, file = NA, title = "Level 1"){
  #' Create plot for ATC level 1
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param ncols_legend number of columns for legend
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param remove_duplicated_medication_for_one_patient remove medcation if present twice for the same patient
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param legendshould legend be put on the plot?
  #' @param file file where to save the plot
  #' @param title title of the plot
  #' 
  #' @return plot of ATC level 1
  #' 
  
  nbpatients = nrow(database_preprocessed_labels)
  
  if(!"subtype.f" %in% colnames(data_medication_long))
    data_medication_long$subtype.f = unlist(lapply(data_medication_long$base_cletri, function(x) {return(database_preprocessed_labels$subtype[which(database_preprocessed_labels$base_cletri==x)])}))
  
  data_cpy = data_medication_long
  
  if(remove_duplicated_medication_for_one_patient){
    data_cpy = data_cpy[-which(duplicated(data_cpy[, c('base_cletri', 'atc_level1_cod')])), ]
    data_medication_long = data_medication_long[-which(duplicated(data_medication_long[, c('base_cletri', 'atc_level1_cod')])), ]
  }
  
  
  matrix_comedic <- dplyr::select(data_medication_long, numdos7 = base_cletri, subtype.f,
                                  atc_level5_cod,atc_level5_lib,
                                  atc_level1_cod,atc_level1_lib,
                                  atc_level2_cod,atc_level2_lib,
                                  atc_level3_cod,atc_level3_lib,
                                  atc_level4_cod,atc_level4_lib,
                                  atc_level1_lib)
  
 # matrix_comedic = matrix_comedic[which(!is.na(matrix_comedic$subtype.f)),]
  
  Dat.label <-  matrix_comedic %>% filter(!is.na(atc_level1_lib)) %>% 
    group_by(atc_level1_lib) %>% 
    dplyr::summarise(count=n()) %>%
    mutate(level1_regrouped2=factor(atc_level1_lib,
                                    levels=rev(levels(as.factor(matrix_comedic$atc_level1_lib)) )  )) %>% 
    mutate(ypos = cumsum(count) - 0.5*count) %>% 
    mutate(percent_full = count/sum(count)*100) %>% 
    mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
    mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
    arrange(count)

  ## To keep same order          
  Dat.label$level1_regrouped2 <- as.character(Dat.label$level1_regrouped2)
  Dat.label$level1_regrouped2 = sapply(as.character(Dat.label$level1_regrouped2), firstUp)
  Dat.label$level1_regrouped2 <- factor(Dat.label$level1_regrouped2, levels=unique(Dat.label$level1_regrouped2))
  
  Dat.label$tmp <- "All"
  Dat.label$tmp <- factor(Dat.label$tmp, levels=unique(Dat.label$tmp))
  
  m = max(Dat.label$count) *1.2
  tick = m / 10
  #if(tick > 100 & tick < 1000)
    t = seq(0,m,tick)
  #if(tick > 10 & tick < 100)
  #  t = seq(0,m,100)
  
  
  ## Plot atc_level1_lib WP
  p_level_1_wp <- ggplot(Dat.label, aes(x=tmp, y=count, fill=level1_regrouped2  )) +
    geom_bar(stat="identity",  width=0.8, position = position_dodge(1)) +  guides(colour=FALSE)+
    coord_flip(y=c(0,m))+
    facet_grid(tmp~., scales = "free")+
    scale_y_continuous(limits=c(0, m), breaks=t) +
    geom_text(aes(label=count), position=position_dodge(width=1), hjust=-.15, size=numbers_size-5.5) +
    ggtitle(title)+ xlab ("") + ylab("")+
    scale_fill_manual(name = "", values= color_anat  )+ guides(fill=guide_legend(ncol = ncols_legend,byrow=TRUE)) +
    theme(  axis.ticks.x = element_blank(), line = element_blank(), axis.text.x = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y=element_blank(),legend.text=element_text(size=14), 
          plot.title = element_text(face="plain", size=12), legend.position = "top",
          strip.text = element_text(face="plain", size=11), plot.margin = unit(c(0, 0, 0, 0.0), "cm"))
  p_level_1_wp <- p_level_1_wp + guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE)) + mytheme
  
  if(percentage){
    v = round(Dat.label$count/nbpatients*100,1)
    l = sapply(v, function(x){if(x >10)paste0(x, "%")else ""})
    p_level_1_wp = p_level_1_wp + geom_text(aes(label=l,y=3.5,x=tmp), 
                                            position=position_dodge(width=1),  hjust=-0.1, color = "white", size=numbers_size-7) 
  }
  
  pleg <- get_legend(p_level_1_wp)
  
  # remove legend from here
  p_level_1_wp <- p_level_1_wp + theme(legend.position="none")

  matrix_comedic$subtype.f=as.character(matrix_comedic$subtype.f)
  matrix_comedic$subtype.f[which(is.na(matrix_comedic$subtype.f))] = "NA"
  # By subtype
  Dat.label <-  matrix_comedic %>% filter(!is.na(atc_level1_lib)) %>% 
    group_by( subtype.f ,atc_level1_lib) %>% 
    dplyr::summarise(count=n()) %>%
    mutate(level1_regrouped2=factor(atc_level1_lib,levels=rev(levels(as.factor(matrix_comedic$atc_level1_lib)) )  )) %>% 
    mutate(ypos = cumsum(count) - 0.5*count) %>% 
    mutate(percent_full = count/sum(count)*100) %>% 
    mutate(percent_format = paste0(round(count/sum(count)*100), '%')) %>% 
    mutate(ypos_percent = cumsum(percent_full) - 0.5*percent_full)  %>%
    group_by(subtype.f) %>%
    arrange((count),subtype.f)
  
  
  Dat.label$subtype.f <- as.character(Dat.label$subtype.f)
  Dat.label$subtype.f <- factor(Dat.label$subtype.f, levels=c("Luminal","TNBC","HER2+", "NA"))
  
  Dat.label$level1_regrouped2 <- as.character(Dat.label$level1_regrouped2)
  Dat.label$level1_regrouped2 = sapply(as.character(Dat.label$level1_regrouped2), firstUp)

  Dat.label$level1_regrouped2 <- factor(Dat.label$level1_regrouped2, levels=unique(Dat.label$level1_regrouped2))
  
  m1 = max(Dat.label$count) *1.2
  tick = m1 / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  
  p_level_1_subtype <- ggplot(Dat.label, aes(subtype.f, y=count,fill=level1_regrouped2  )) +
    geom_bar(stat="identity",  width=0.8,position= position_dodge(1)) +  guides(colour=FALSE)+
    theme(  legend.position = "none", axis.ticks.x = element_blank() ,
            axis.ticks.y = element_blank() ,axis.text.y = element_blank(),axis.title.x=element_blank(),
            axis.text.x=element_blank(),axis.line.x=element_blank(),
            strip.text = element_text(face="plain", size=9), plot.margin = unit(c(0, 0, 0, 0), "cm"))   +
    coord_flip(y=c(0,m) ) +
    facet_grid(subtype.f~., scales = "free")+
    ylab("") + xlab ("") +
    geom_text(aes(label=count), position=position_dodge(width=1), hjust=-.15, size=numbers_size-5.5) +
    scale_fill_manual(name = "Anatomical class", values= color_anat  )  
  
  p_level_1_subtype <- p_level_1_subtype + mytheme
  
  p_level_1_compil <- plot_grid(p_level_1_wp,p_level_1_subtype,
                                labels = c(""), rel_heights = c(3,7),
                                nrow = 2, ncol = 1, align="hv")
  
  p_level_1_final = p_level_1_compil
  if(legend)
    p_level_1_final <- plot_grid(pleg, p_level_1_compil, nrow = 2,
                               rel_heights = c(1,3))
  
  if(!is.na(file))              
    save_plot(file, p_level_1_final,  base_height=12, base_width=4)
  
  return(p_level_1_final )
}



plotLevel2 <- function(data_medication_long, database_preprocessed_labels, nrows_legend=2, percentage = T, remove_duplicated_medication_for_one_patient = F, 
                       names_size = 11, numbers_size = NA, min_occurency = 1, legend=T, legend_size=12, file=NA, marginX=1.2, title = "Level 2"){
  #' Create plot for ATC level 2
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param remove_duplicated_medication_for_one_patient remove medication if present twice for the same patient
  #' @param names_size size for names of medications
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param legend should legend be put on the plot?
  #' @param legend_size size of the legend
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' @param title title of the plot

  #' @return plot of ATC level 2
  #' 
  
  nbpatients = nrow(database_preprocessed_labels)
  
  if(!"subtype.f" %in% colnames(data_medication_long))
    data_medication_long$subtype.f = unlist(lapply(data_medication_long$base_cletri, function(x) {return(database_preprocessed_labels$subtype[which(database_preprocessed_labels$base_cletri==x)])}))
   
  data_cpy = data_medication_long
  
  if(remove_duplicated_medication_for_one_patient){
    data_medication_long = data_medication_long[-which(duplicated(data_medication_long[, c('base_cletri', 'atc_level2_cod')])), ]
  }
  
  
  data_medication_long = filter(data_medication_long, nchar(atc_level2_cod) == 3)
  
  if(is.na(numbers_size))
    numbers_size = names_size - 7
  w = 60
  # if legend is false, we are in the picture for all levels, so we reduce the number of characters to go newline
  if(legend==F)
    w=40
  library(cowplot)
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels)
  
  tab_summary_comed_2 <- tab_summary_global %>% dplyr::filter(!is.na(atc_level2_lib), sum_level2 >= min_occurency) %>% 
    dplyr::select(atc_level1_lib,atc_level2_lib,sum_level2,atc_level2_cod)%>%  unique() %>%
    dplyr::mutate(level_2_and_letter = paste0(atc_level2_lib," (",atc_level2_cod,")"),
                  level_2_and_letter_2lines = NA) %>%
    dplyr::arrange(.,sum_level2)
  head(tab_summary_comed_2)
  
  tab_summary_comed_2$atc_level1_lib = sapply(as.character(tab_summary_comed_2$atc_level1_lib), firstUp)
  
  for (i  in 1 : length(tab_summary_comed_2$level_2_and_letter) ) {
    tab_summary_comed_2$level_2_and_letter_2lines[i] <- paste(strwrap(tab_summary_comed_2$level_2_and_letter[i],
                                                                      width = w), collapse = "\n") # ligne a adapter en fonction des datas
  }
  tab_summary_comed_2$level_2_and_letter_2lines = sapply(as.character(tab_summary_comed_2$level_2_and_letter_2lines), firstUp)
  # tab_summary_comed_2$level_2_and_letter_2lines = sapply(as.character(tab_summary_comed_2$level_2_and_letter_2lines), addNwelineMiddle)
  
  tab_summary_comed_2$level_2_and_letter_2lines <- as.character(tab_summary_comed_2$level_2_and_letter_2lines)
  tab_summary_comed_2$level_2_and_letter_2lines <- factor(tab_summary_comed_2$level_2_and_letter_2lines,  
                                                          levels  =unique(tab_summary_comed_2$level_2_and_letter_2lines))
  

  m = max(tab_summary_comed_2$sum_level2) *marginX
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if( tick > 1 & tick < 10)
    t = seq(0,m,10)

  p_level_2 <-  ggplot(data =tab_summary_comed_2,aes(x=level_2_and_letter_2lines,y=sum_level2,   
                                                    fill=atc_level1_lib))+ 
    geom_col()+coord_flip() + 
    scale_fill_manual(name = " ", values= color_anat  ) +
    geom_text(aes(label=sum_level2), position=position_dodge(width=1), hjust=-.15, size=numbers_size) +
    xlab("")+ylab("")+ ggtitle(title)+ 
    scale_y_continuous(limits=c(0, m), breaks=t) +
    theme( axis.text=element_text(size=names_size), legend.text=element_text(size=legend_size), 
           legend.position="top", legend.justification='center', plot.title = element_text(face="plain", size=12, hjust = 0, vjust=0), plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  if(percentage){
    v = round(tab_summary_comed_2$sum_level2/nbpatients*100,1)
    l = sapply(v, function(x){if(x >1)paste0(x, "%")else ""})
    p_level_2= p_level_2 + geom_text(aes(label=l,y=3.5,x=level_2_and_letter_2lines), position=position_dodge(width=1),  
              hjust=-0.01, color = "white", size=numbers_size) 
  }
  # pleg <- get_legend(p_level_2)
  
  # p2 without legend
  if(legend==F)
    p_level_2 <- p_level_2 + theme(legend.position="none")
  
  if(!is.na(file))
    save_plot(p_level_2,file=file,base_height=15, base_width=25)      
  
  return(p_level_2+guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE)) + mytheme)
}






plotLevel3 <- function(data_medication_long, database_preprocessed_labels,  nrows_legend=2, percentage = T, remove_duplicated_medication_for_one_patient = F, 
                        names_size =11, numbers_size=NA,  min_occurency = 1, legend = T, legend_size=12, file=NA, marginX=1.2){
  
  #' Create plot for ATC level 3
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param remove_duplicated_medication_for_one_patient remove medcation if present twice for the same patient
  #' @param names_size size for names of medications
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param legend should legend be put on the plot?
  #' @param legend_size size of the legend
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' 
  #' @return plot of ATC level 3
  #' 
  nbpatients = nrow(database_preprocessed_labels)
  
  data_cpy = data_medication_long
  
  if(remove_duplicated_medication_for_one_patient){
    data_medication_long = data_medication_long[-which(duplicated(data_medication_long[, c('base_cletri', 'atc_level3_cod')])), ]
  }
  
  if(is.na(numbers_size))
    numbers_size = names_size - 7
  
  data_medication_long = filter(data_medication_long, nchar(atc_level3_cod) == 4)
  
  w = 60
  # if legend is false, we are in the picture for all levels, so we reduce the number of characters to go newline
  if(legend==F)
    w=40
  
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels)
  
  tab_summary_comed_3 <- tab_summary_global %>% dplyr::filter(!is.na(atc_level3_lib), sum_level3 >= min_occurency) %>% 
    dplyr::select(atc_level1_lib,atc_level3_lib,sum_level3,atc_level3_cod)%>%  unique() %>%
    dplyr::mutate(level_3_and_letter = paste0(atc_level3_lib," (",atc_level3_cod,")"),
                  level_3_and_letter_2lines = NA) %>%
    dplyr::arrange(.,sum_level3)
  
  for (i  in 1 : length(tab_summary_comed_3$level_3_and_letter) ) {
    tab_summary_comed_3$level_3_and_letter_2lines[i] <- paste(strwrap(tab_summary_comed_3$level_3_and_letter[i], 
                                                                      width = w), collapse = "\n") 
  }
  
  tab_summary_comed_3$level_3_and_letter_2lines = sapply(as.character(tab_summary_comed_3$level_3_and_letter_2lines), firstUp)
  
  tab_summary_comed_3$level_3_and_letter_2lines <- as.character(tab_summary_comed_3$level_3_and_letter_2lines)
  tab_summary_comed_3$level_3_and_letter_2lines <- factor(tab_summary_comed_3$level_3_and_letter_2lines,
                                                          levels=unique(tab_summary_comed_3$level_3_and_letter_2lines))
  
 # levels(tab_summary_comed_3$level_3_and_letter_2lines) =  gsub('\n', '', levels(tab_summary_comed_3$level_3_and_letter_2lines))
  
  m = max(tab_summary_comed_3$sum_level3) *marginX
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if( tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  tab_summary_comed_3$atc_level1_lib = sapply(as.character(tab_summary_comed_3$atc_level1_lib), firstUp)
  
  p_level_3 <-  ggplot(data=tab_summary_comed_3,aes(x=level_3_and_letter_2lines, y=sum_level3,    
                                                    fill=atc_level1_lib))+ 
    geom_histogram(stat="identity")+coord_flip() + 
    scale_fill_manual(name = " ", values= color_anat  ) +
    geom_text(aes(label=sum_level3), position=position_dodge(width=1), hjust=-.15, size=numbers_size) +
    xlab("")+ylab("")+ ggtitle("Level 3")+ 
    scale_y_continuous(limits=c(0, m), breaks=t) +
    theme( axis.text=element_text(size=names_size),
           legend.position="top", plot.title = element_text(face="plain", size=8, hjust = 0, vjust=0),axis.title.x=element_blank(),legend.text=element_text(size=legend_size),
           axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), plot.margin = unit(c(0, 0, 0, -0.0), "cm"))
  
  if(percentage)
    p_level_3 = p_level_3 + geom_text(aes(label=paste0(round(sum_level3/nbpatients*100,1), "%"),y=3.5,x=level_3_and_letter_2lines), position=position_dodge(width=1),  
                                      hjust=-0.1, color = "white", size=numbers_size)
    
  pleg <- get_legend(p_level_3)
  
  if(legend == FALSE)
    p_level_3 <- p_level_3 + theme(legend.position="none")
  
  if(!is.na(file))
    save_plot(p_level_3,file=file,base_height=40, base_width=45)   
  return(p_level_3 + guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE)) + mytheme)
}




plotLevel4 <- function(data_medication_long, database_preprocessed_labels, nrows_legend=2, percentage = T, remove_duplicated_medication_for_one_patient = F, 
                       names_size=11, numbers_size=NA, min_occurency = 1, legend = T, legend_size = 12, file=NA, marginX=1.2){
  
  #' Create plot for ATC level 4
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param remove_duplicated_medication_for_one_patient remove medcation if present twice for the same patient
  #' @param names_size size for names of medications
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param legend should legend be put on the plot?
  #' @param legend_size size of the legend
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' 
  #' @return plot of ATC level 4
   
  nbpatients = nrow(database_preprocessed_labels)
  
  data_cpy = data_medication_long
  
  if(remove_duplicated_medication_for_one_patient){
    data_medication_long = data_medication_long[-which(duplicated(data_medication_long[, c('base_cletri', 'atc_level3_cod')])), ]
  }
  
  if(is.na(numbers_size))
    numbers_size = names_size - 7
  
  data_medication_long = filter(data_medication_long, nchar(atc_level4_cod) == 5)
  
  w = 60
  # if legend is false, we are in the picture for all levels, so we reduce the number of characters to go newline
  if(legend==F)
    w=40
  
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels)
  
  tab_summary_comed_4 <- tab_summary_global %>% dplyr::filter(!is.na(atc_level4_lib), sum_level4 >= min_occurency) %>% 
    dplyr::select(atc_level1_lib,atc_level4_lib,sum_level4,atc_level4_cod)%>%  unique() %>%
    dplyr::mutate(level_4_and_letter = paste0(atc_level4_lib," (",atc_level4_cod,")"),
                  level_4_and_letter_2lines = NA) %>%
    dplyr::arrange(.,sum_level4)
  
  for (i  in 1 : length(tab_summary_comed_4$level_4_and_letter) ) {
    tab_summary_comed_4$level_4_and_letter_2lines[i] <- paste(strwrap(tab_summary_comed_4$level_4_and_letter[i], 
                                                                      width = w), collapse = "\n") 
  }
  
 # tab_summary_comed_4$level_4_and_letter_2lines = sapply(as.character(tab_summary_comed_4$level_4_and_letter_2lines), firstUp)
  
  tab_summary_comed_4$level_4_and_letter_2lines <- as.character(tab_summary_comed_4$level_4_and_letter_2lines)
  tab_summary_comed_4$level_4_and_letter_2lines <- factor(tab_summary_comed_4$level_4_and_letter_2lines,
                                                          levels=unique(tab_summary_comed_4$level_4_and_letter_2lines))
  
 # levels(tab_summary_comed_4$level_4_and_letter_2lines) =  gsub('\n', '', levels(tab_summary_comed_4$level_4_and_letter_2lines))
  
  m = max(tab_summary_comed_4$sum_level4) *marginX
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if( tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  tab_summary_comed_4$atc_level1_lib = sapply(as.character(tab_summary_comed_4$atc_level1_lib), firstUp)
  
  p_level_4 <-  ggplot(data=tab_summary_comed_4,aes(x=level_4_and_letter_2lines, y=sum_level4,    
                                                    fill=atc_level1_lib))+ 
    geom_histogram(stat="identity")+coord_flip() + 
    scale_fill_manual(name = " ", values= color_anat  ) +
    geom_text(aes(label=sum_level4), position=position_dodge(width=1), hjust=-.15, size=numbers_size) +
    xlab("")+ylab("")+ ggtitle("Level 4")+ 
    scale_y_continuous(limits=c(0, m), breaks=t) +
    theme( axis.text=element_text(size=names_size), legend.text=element_text(size=legend_size),
           legend.position="top", plot.title = element_text(face="plain", size=8, hjust = 0, vjust=0),axis.title.x=element_blank(),
           axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), plot.margin = unit(c(0, 0, 0, -0.0), "cm"))
  
  if(percentage)
    p_level_4 = p_level_4 + geom_text(aes(label=paste0(round(sum_level4/nbpatients*100,1), "%"),y=3.5,x=level_4_and_letter_2lines), position=position_dodge(width=1),  
                                      hjust=-0.1, color = "white", size=numbers_size)
    
  pleg <- get_legend(p_level_4)
  
  if(legend == FALSE)
    p_level_4 <- p_level_4 + theme(legend.position="none")
  
  if(!is.na(file))
    save_plot(p_level_4,file=file,base_height=40, base_width=45)   
  return(p_level_4 + guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE))  + mytheme)
}



plotLevel5 <- function(data_medication_long, database_preprocessed_labels, nrows_legend=2, percentage = T, 
                       names_size=11, numbers_size=NA, min_occurency = 1, legend=T, legend_size=12, file=NA, marginX=1.2, title = 'Level 5'){
  #' Create plot for ATC level 5
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param names_size size for names of medications
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param legend should legend be put on the plot?
  #' @param legend_size size of the legend
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' @param title title of the plot
  #' 
  #' @return plot of ATC level 5
  #'
  
  nbpatients = nrow(database_preprocessed_labels)
  
  if(is.na(numbers_size))
    numbers_size = names_size - 7
  
  data_medication_long = filter(data_medication_long, nchar(atc_level5_cod) >= 7)
  
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels)%>% 
    dplyr::filter(!is.na(atc_level5_lib), count >= min_occurency)
  tab_summary_comed = evaluateTabSummaryComed(tab_summary_global)
  
  # tab_summary_comed$level_5_and_letter_2lines = as.character(tab_summary_comed$level_5_and_letter_2lines)
  # tab_summary_comed$level_5_and_letter_2lines = as.character(sapply(as.character(tab_summary_comed$level_5_and_letter_2lines), firstUp))
  # tab_summary_comed$level_5_and_letter_2lines = paste0(gsub("__", " (", tab_summary_comed$level_5_and_letter_2lines), ")")
  # pos = gregexpr(pattern = "\\(", text =  tab_summary_comed$level_5_and_letter_2lines)
  # tab_summary_comed$level_5_and_letter_2lines = sapply(1:length(pos), function(i){paste0(substr(tab_summary_comed$level_5_and_letter_2lines[i],1,pos[[i]][[1]]), 
  #                         toupper(substr(tab_summary_comed$level_5_and_letter_2lines[i],(pos[[i]][[1]]+1),nchar(tab_summary_comed$level_5_and_letter_2lines[i]))))})
  tab_summary_comed$atc_level1_lib = sapply(as.character(tab_summary_comed$atc_level1_lib), firstUp)

  tab_summary_comed$level_5_and_letter_2lines = unlist(lapply(1:nrow(tab_summary_comed), function(i){firstUp(as.character(tab_summary_comed$level_5_and_letter_2lines[i]))}))
  
  tab_summary_comed$level_5_and_letter_2lines <- factor(tab_summary_comed$level_5_and_letter_2lines,  
                                                          levels  =unique(tab_summary_comed$level_5_and_letter_2lines))
  
  m = max(tab_summary_comed$count) * marginX
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if( tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  p_level_5 <-  ggplot(data=tab_summary_comed,aes(x=level_5_and_letter_2lines,y=count, fill=atc_level1_lib))+ 
    geom_histogram(stat="identity")+coord_flip() + 
    scale_fill_manual(name = "", values= color_anat  )  +  
    xlab("")+ylab("")+ ggtitle (title)+
    geom_text(aes(label=tab_summary_comed$count), position=position_dodge(width=1), hjust=-.15, size=numbers_size) +
    ylim(0,m)+
    theme( axis.text=element_text(size=names_size), legend.text=element_text(size=legend_size),
           legend.position="top", legend.justification='left', plot.title = element_text(face="plain", size=12),axis.title.x=element_blank(),
           axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), plot.margin = unit(c(0, 0, 0, 0.0), "cm"))
  
  if(percentage)
    p_level_5 = p_level_5 + geom_text(aes(label=paste0(round(count/nbpatients*100,1), "%"),y=1,x=level_5_and_letter_2lines), position=position_dodge(width=1),  
                                      hjust=-0.1, color = "white", size=numbers_size)
    
  if(!is.na(file))
    save_plot(p_level_5,file=file,base_height=49, base_width=25)
  
  if(legend == FALSE)
    p_level_5 <- p_level_5 + theme(legend.position="none")
  
  return(p_level_5+guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE)) + mytheme)
  
}


plotLevel5_with_ATC <- function(data_medication_long, database_preprocessed_labels, nrows_legend=2, percentage = T, 
                                names_size=11, numbers_size=NA,  min_occurency = 1, legend=T, legend_size=12, file=NA, marginX=1.2, title = 'Level 5'){
  
  #' Create plot for ATC level 5 grouped by ATC level 2
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param percentage write the percentage of
  #' @param names_size size for names of medications
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param legend should legend be put on the plot?
  #' @param legend_size size of the legend
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' @param title title of the plot
  #' 
  #' @return plot of ATC level 5 grouped by ATC level 2
  #'
  
  nbpatients = nrow(database_preprocessed_labels)
  
  if(is.na(numbers_size))
    numbers_size = names_size - 7
  
  data_medication_long = filter(data_medication_long, nchar(atc_level5_cod) >= 7)
  
  tab_summary_global = evaluateTableComedicationGrouped(ttConcTable = data_medication_long, database_preprocessed_labels = database_preprocessed_labels) %>% 
    dplyr::filter(!is.na(atc_level5_lib), count >= min_occurency)
  tab_summary_comed = evaluateTabSummaryComed(tab_summary_global)
  
  # tab_summary_comed$level_5_and_letter_2lines = as.character(tab_summary_comed$level_5_and_letter_2lines)
  # tab_summary_comed$level_5_and_letter_2lines = as.character(sapply(as.character(tab_summary_comed$level_5_and_letter_2lines), firstUp))
  # tab_summary_comed$level_5_and_letter_2lines = paste0(gsub("__", " (", tab_summary_comed$level_5_and_letter_2lines), ")")
  # pos = gregexpr(pattern = "\\(", text =  tab_summary_comed$level_5_and_letter_2lines)
  # tab_summary_comed$level_5_and_letter_2lines = sapply(1:length(pos), function(i){paste0(substr(tab_summary_comed$level_5_and_letter_2lines[i],1,pos[[i]][[1]]), 
  #                         toupper(substr(tab_summary_comed$level_5_and_letter_2lines[i],(pos[[i]][[1]]+1),nchar(tab_summary_comed$level_5_and_letter_2lines[i]))))})
  tab_summary_comed$atc_level1_lib = sapply(as.character(tab_summary_comed$atc_level1_lib), firstUp)
  
  tab_summary_comed$level_5_and_letter_2lines = unlist(lapply(1:nrow(tab_summary_comed), function(i){paste0(firstUp(as.character(tab_summary_comed$level_5_and_letter_2lines[i])), " (", tab_summary_comed$atc_level5_cod[i], ')')}))
  
  tab_summary_comed$level_5_and_letter_2lines <- factor(tab_summary_comed$level_5_and_letter_2lines,  
                                                        levels  =unique(tab_summary_comed$level_5_and_letter_2lines))
  
  m = max(tab_summary_comed$count) * marginX
  tick = m / 10
  if(tick > 100 & tick < 1000)
    t = seq(0,m,1000)
  if(tick > 10 & tick < 100)
    t = seq(0,m,100)
  if( tick > 1 & tick < 10)
    t = seq(0,m,10)
  
  p_level_5 <-  ggplot(data=tab_summary_comed,aes(x=level_5_and_letter_2lines,y=count, fill=atc_level1_lib))+ 
    geom_histogram(stat="identity")+coord_flip() + 
    scale_fill_manual(name = "", values= color_anat  )  +  
    xlab("")+ylab("")+ ggtitle (title)+
    geom_text(aes(label=tab_summary_comed$count), position=position_dodge(width=1), hjust=-.15, size=numbers_size) +
    ylim(0,m)+
    ggforce::facet_col(vars(atc_level2_cod), scales = "free", space = "free", strip.position = 'right') +
    # facet_grid(atc_level2_cod~., scales = "free")+
    theme( axis.text=element_text(size=names_size), legend.text=element_text(size=legend_size),
           legend.position="top", legend.justification='left', plot.title = element_text(face="plain", size=9),axis.title.x=element_blank(),
           axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), plot.margin = unit(c(0, 0, 0, 0.0), "cm"))
  
  if(percentage)
    p_level_5 = p_level_5 + geom_text(aes(label=paste0(round(count/nbpatients*100,1), "%"),y=1,x=level_5_and_letter_2lines), position=position_dodge(width=1),  
                                      hjust=-0.1, color = "white", size=numbers_size)
  
  if(!is.na(file))
    save_plot(p_level_5,file=file,base_height=49, base_width=25)
  
  if(legend == FALSE)
    p_level_5 <- p_level_5 + theme(legend.position="none")
  
  return(p_level_5+guides(fill=guide_legend(nrow=nrows_legend,byrow=TRUE)) + mytheme)
  
}


plotAllLevels <- function(data_medication_long, database_preprocessed_labels, nrows_legend=3, 
                          remove_duplicated_medication_for_one_patient = F, percentage = F, names_size_level_1 = 9, names_size_other_levels = 6, numbers_size = 2.5,
                          min_occurency=10,  min_occurency_5 = 10, file=NA, marginX = 1.2, legend_size = 9, title = 'All levels'){
  
  #' Create plot for all ATC levels
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param remove_duplicated_medication_for_one_patient remove medcation if present twice for the same patient
  #' @param percentage write the percentage of
  #' @param names_size_level_1 size for names of medications in level 1
  #' @param names_size_other_levels size for names of medications in other levels
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency minimum number of occurrences for a medication to be put on the plot 
  #' @param min_occurency_5 minimum number of occurrences for a medication to be put on the plot for level 5 
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' @param legend_size size of the legend
  #' @param title title of the plot
  #' 
  #' @return plot for all ATC levels
  #'
  #'
  #'

  p2 = plotLevel2(data_medication_long = data_medication_long, database_preprocessed_labels=database_preprocessed_labels, 
                  min_occurency = 0, legend = T, legend_size=7, nrows_legend = nrows_legend)
  
  pintermediaire <- plot_grid(plotLevel1(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                        remove_duplicated_medication_for_one_patient = remove_duplicated_medication_for_one_patient,
                                         legend = F, percentage = percentage, ncols_legend = 7), NULL, 
                              
                              plotLevel2(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels,
                                         remove_duplicated_medication_for_one_patient = remove_duplicated_medication_for_one_patient,
                                         min_occurency = min_occurency, percentage = percentage, legend = F, legend_size=legend_size, nrows_legend = 7, 
                                         names_size = names_size_other_levels, numbers_size = numbers_size, marginX = marginX), NULL, 
                              
                              plotLevel3(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                         remove_duplicated_medication_for_one_patient = remove_duplicated_medication_for_one_patient,
                                         min_occurency = min_occurency, percentage = percentage, legend = F, legend_size=legend_size, nrows_legend = 7, 
                                         names_size = names_size_other_levels, numbers_size = numbers_size, marginX = marginX), NULL, 
                              
                              plotLevel4(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                         remove_duplicated_medication_for_one_patient = remove_duplicated_medication_for_one_patient,
                                         min_occurency = min_occurency, percentage = percentage, legend = F, legend_size=legend_size, nrows_legend = 7, 
                                         names_size = names_size_other_levels, numbers_size = numbers_size, marginX = (marginX + 0.1)), NULL, 
                              
                              plotLevel5(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels,
                                         min_occurency = min_occurency_5, percentage = F, legend = F, legend_size=legend_size, nrows_legend = 7, 
                                         names_size = 6, numbers_size = 2.5, marginX = (marginX + 0.1)),
                              
                              rel_widths=c(4,0.2,5,-0.1,5,-0.1,5,-0.1,5),nrow = 1)
  
  # pintermediaire <- plot_grid(p_level_1_compil,
  #                             p_level_2,
  #                             rel_widths=c(5,5),ncol=2)
  
  pleg <- get_legend(p2)
  
  plot_all_levels <- plot_grid(pleg, pintermediaire, nrow = 2,
                               rel_heights = c(1,5))
  plot_all_levels
  if(!is.na(file))
    save_plot(plot_all_levels, file=file,base_height=15, base_width=25)
  
  return(plot_all_levels)
}


plotLevels1_2_5 <- function(data_medication_long, database_preprocessed_labels, nrows_legend=2, 
                            remove_duplicated_medication_for_one_patient = F, percentage=F, names_size = 9, numbers_size = 3,
                             min_occurency_2=10, min_occurency_5=10, file=NA, marginX = 1.2, legend_size = 10, title = 'All levels'){
  #' Create plot for levels 1, 2 and 5
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param nrows_legend number of rows for legend
  #' @param remove_duplicated_medication_for_one_patient remove medcation if present twice for the same patient
  #' @param percentage write the percentage of
  #' @param names_size size for names of medications in level 1
  #' @param numbers_size size for number of medications, the number is positioned on the right of bars
  #' @param min_occurency_2 minimum number of occurrences for a medication to be put on the plot for ATC level 2
  #' @param min_occurency_5 minimum number of occurrences for a medication to be put on the plot for ATC level 5
  #' @param file file where to save the plot
  #' @param marginX margin on the right of numbers
  #' @param legend_size size of the legend
  #' @param title title of the plot
  #' 
  #' @return plot for levels 1, 2 and 5
  #'
  #'
  #'
  
  p2 = plotLevel2(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, min_occurency = 0, 
                  legend = T, legend_size=legend_size, nrows_legend = 3)
  
  pintermediaire <- plot_grid(plotLevel1(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                         remove_duplicated_medication_for_one_patient = remove_duplicated_medication_for_one_patient, legend = F, percentage = percentage, 
                                         ncols_legend = 7), NULL, 
                              plotLevel2(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                         remove_duplicated_medication_for_one_patient= remove_duplicated_medication_for_one_patient, min_occurency = min_occurency_2, 
                                         percentage = percentage, legend = F, legend_size=legend_size, nrows_legend = 7, names_size = names_size, numbers_size = numbers_size, marginX = marginX), NULL, 
                              plotLevel5(data_medication_long = data_medication_long, database_preprocessed_labels = database_preprocessed_labels, 
                                         min_occurency = min_occurency_5, percentage = percentage, legend = F, legend_size=legend_size, nrows_legend = 7, names_size = names_size, 
                                         numbers_size = numbers_size, marginX = marginX),
                              
                              rel_widths=c(4,0.2,5,-0.1,5,-0.1,5,-0.1,5),nrow = 1)
  
  # pintermediaire <- plot_grid(p_level_1_compil,
  #                             p_level_2,
  #                             rel_widths=c(5,5),ncol=2)
  
  pleg <- get_legend(p2)
  
  plot_all_levels <- plot_grid(pleg, pintermediaire, nrow = 2,
                               rel_heights = c(1,5))
  plot_all_levels
  if(!is.na(file))
    save_plot(plot_all_levels, file=file,base_height=15, base_width=25)
  
  return(plot_all_levels)
}

plotMedicationDistribution_NumberOfComedications <- function(data_medication_long, cutThresholdOthers = 5){
  #' Plot the distribution of medications with respect to the number of comedications
  #'
  #' @param data_medication_long dataframe of medications, long version
  #' @param cutThresholdOthers threshold from which comedications number must be grouped together in the group cutThresholdOthers and more
  #' 
  #' @return plot of the distribution for every medication number 
  #'

  data_medication_long[ , c("atc_cod")][
    sapply(data_medication_long[ , c("atc_cod")],
           function(x) x == "")] <- NA
  
  if(!'base_cletri' %in% colnames(data_medication_long))
    data_medication_long$base_cletri = data_medication_long$base_cletri
  data_medication_long$base_cletri = as.character(data_medication_long$base_cletri)
  table_comed <-
    by(data_medication_long[ , c("atc_cod")],
       data_medication_long$base_cletri,
       function(x) {
         a <- table(as.matrix(x), useNA = "always")
         return(list(a))
       })
  
  recap_comed <-
    sapply(table_comed,
           function(x) by(c(x > 0),
                          names(c(x > 0)),
                          function(x) sum(x) == 1))
  
  level1_fullPCR <- 
    sapply(recap_comed,
           function(x) substr(names(which(x == T)), 1, 1))
  level2_fullPCR <- 
    sapply(recap_comed,
           function(x) substr(names(which(x == T)), 1, 3))
  level5_fullPCR <- 
    sapply(recap_comed,
           function(x) substr(names(which(x == T)), 1, 8))
  
  ## we need to tranform the data as in the example
  # N MED
  # 1 A
  # 2 B
  # 2 C
  # ...
  
  dataMedication <- data.frame(matrix(vector(),ncol=2))
  colnames(dataMedication) <-c("Med","N")
  for(i in 1:length(level5_fullPCR)){
    med_patient = level5_fullPCR[[i]]
    for(j in 1:length(med_patient)){
      #print(c(substr(med_patient[j],1,1), length(med_patient)))
      dataMedication[nrow(dataMedication)+1,] =  c(substr(med_patient[j],1,1), length(med_patient))
    }
  }
  
  dataMedication$N <- factor(as.numeric(dataMedication$N), levels  = sort(unique(as.numeric(substr(dataMedication$N,1,2)))))
  
  # dataMedication$Med = sapply(dataMedication$Med, function(x){firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==x)]))})
  dataMedication$Med = as.character(unlist(sapply(dataMedication$Med, function(x){(as.character(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==x)[1]]))})))
  
  dataMedication$N = as.numeric(dataMedication$N)
  

  dataMedication$N[which(dataMedication$N >= cutThresholdOthers)] = cutThresholdOthers
  
  distrib = aggregate(cbind(count = Med) ~ N, 
                      data = dataMedication, 
                      FUN = function(x){NROW(x)})
  
  
  # test
  d2 <- dataMedication %>% 
    dplyr::group_by(N, Med) %>% 
    dplyr::summarise(count=n()) %>% 
    dplyr::mutate(perc=count/sum(count)) %>% 
    dplyr::arrange(N,count)
  
  d3 <- dataMedication %>% 
    dplyr::group_by(N) %>% 
    dplyr::summarise(count=n()) 
  d3$N=as.numeric(as.character(d3$N))
  #d3 = rbind(c(0,length(which(apply(comed_type1ChronicPCRspecified,1,function(x){length(which(x=="Yes"))})==0))), d3)
  
  l = round(d2$perc*100)
  l[which(l < 5)] = ""
  l = as.character(sapply(l, function(x){if(nchar(x)>0){paste0(x,"%")}else x}))
  
  d2$Med
  
  p1 = ggplot(d2, aes(x = factor(N), y = perc*100, fill = factor(Med, levels = unique(d2$Med)))) +
    geom_bar(stat="identity") +
    scale_fill_manual(name = "", values= color_anat   )  + 
    labs(x = "Number of comedications", y = "percent", fill = "Med") +
    geom_text(aes(label=l),stat="identity",position=position_stack(0.5), size=8)+
    theme(text = element_text(size=22), legend.text=element_text(size=17), axis.title.x=element_blank(), axis.title.y = element_text(size = 17),
          axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), legend.position="top", legend.justification='left', panel.border = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
  p1=p1+guides(fill=guide_legend(ncol = 2,byrow=TRUE))
  
  if(max(d3$N) == cutThresholdOthers)
    d3$N[nrow(d3)] = paste0(cutThresholdOthers , ' and more')
  p2 = ggplot(d3, aes(x = factor(N), y=count)) + geom_bar(stat="identity") +
    geom_text(aes(label=count), vjust=-.3, size=8) +
    ylim(0, (max(d3$count)*1.4)) +
    theme(text = element_text(size=23), axis.title.y = element_text(size = 17 ) , panel.border = element_blank(), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
    labs(x = "Number of comedications per patient")
  
  library(grid)
  library("gridExtra")
  # grid.newpage()
  pl = arrangeGrob(p1, p2, ncol = 1, heights = c(4, 1))
  return(pl)
} 






# 
# plotMedicationDistribution_NumberOfComedications <- function(datattConc, cutThresholdOthers = 10){
#   datattConc[ , c("ATC")][
#     sapply(datattConc[ , c("ATC")],
#            function(x) x == "")] <- NA
#   
#   if(!'base_cletri' %in% colnames(datattConc))
#     datattConc$base_cletri = datattConc$base_cletri
#   datattConc$base_cletri = as.character(datattConc$base_cletri)
#   table_comed <-
#     by(datattConc[ , c("ATC")],
#        datattConc$base_cletri,
#        function(x) {
#          a <- table(as.matrix(x), useNA = "always")
#          return(list(a))
#        })
#   
#   recap_comed <-
#     sapply(table_comed,
#            function(x) by(c(x > 0),
#                           names(c(x > 0)),
#                           function(x) sum(x) == 1))
#   
#   level1_fullPCR <- 
#     sapply(recap_comed,
#            function(x) substr(names(which(x == T)), 1, 1))
#   level2_fullPCR <- 
#     sapply(recap_comed,
#            function(x) substr(names(which(x == T)), 1, 3))
#   level5_fullPCR <- 
#     sapply(recap_comed,
#            function(x) substr(names(which(x == T)), 1, 8))
#   
#   ## we need to tranform the data as in the example
#   # N MED
#   # 1 A
#   # 2 B
#   # 2 C
#   # ...
#   
#   dataMedication <- data.frame(matrix(vector(),ncol=2))
#   colnames(dataMedication) <-c("Med","N")
#   for(i in 1:length(level5_fullPCR)){
#     med_patient = level5_fullPCR[[i]]
#     for(j in 1:length(med_patient)){
#       #print(c(substr(med_patient[j],1,1), length(med_patient)))
#       dataMedication[nrow(dataMedication)+1,] =  c(substr(med_patient[j],1,1), length(med_patient))
#     }
#   }
#   
#   dataMedication$N <- factor(as.numeric(dataMedication$N), levels  = sort(unique(as.numeric(substr(dataMedication$N,1,2)))))
#   
#   dataMedication$Med = sapply(dataMedication$Med, function(x){firstUp(as.character(ATC$ATC.level.name[which(ATC$ATC.code==x)]))})
#   
#   dataMedication$N = as.numeric(dataMedication$N)
#   
#   
#   dataMedication$N[which(dataMedication$N >= cutThresholdOthers)] = cutThresholdOthers
#   
#   distrib = aggregate(cbind(count = Med) ~ N, 
#                       data = dataMedication, 
#                       FUN = function(x){NROW(x)})
#   
#   
#   # ggplot(dataMedication, aes(x=Med,fill=Med))+
#   #   geom_bar(stat='count')+
#   #   facet_wrap(~N, scales="free_y", ncol=1)
#   
#   
#   # test
#   d2 <- dataMedication %>% 
#     dplyr::group_by(N, Med) %>% 
#     dplyr::summarise(count=n()) %>% 
#     dplyr::mutate(perc=count/sum(count)) %>% 
#     dplyr::arrange(N,count)
#   
#   d3 <- dataMedication %>% 
#     dplyr::group_by(N) %>% 
#     dplyr::summarise(count=n()) 
#   d3$N=as.numeric(as.character(d3$N))
#   #d3 = rbind(c(0,length(which(apply(comed_type1ChronicPCRspecified,1,function(x){length(which(x=="Yes"))})==0))), d3)
#   
#   l = round(d2$perc*100)
#   l[which(l < 5)] = ""
#   l = as.character(sapply(l, function(x){if(nchar(x)>0){paste0(x,"%")}else x}))
#   
#   d2$Med
#   
#   p1 = ggplot(d2, aes(x = factor(N), y = perc*100, fill = factor(Med, levels = unique(d2$Med)))) +
#     geom_bar(stat="identity") +
#     scale_fill_manual(name = "", values= color_anat   )  + 
#     labs(x = "Number of comedications", y = "percent", fill = "Med") +
#     geom_text(aes(label=l),stat="identity",position=position_stack(0.5), size=8)+
#     theme(text = element_text(size=22), legend.text=element_text(size=17), axis.title.x=element_blank(), axis.title.y = element_text(size = 17),
#           axis.text.x=element_blank(),axis.line.x=element_blank(), axis.ticks.x=element_blank(), legend.position="top", legend.justification='left', panel.border = element_blank(), 
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
#   p1=p1+guides(fill=guide_legend(ncol = 2,byrow=TRUE))
#   
#   if(max(d3$N) == cutThresholdOthers)
#     d3$N[nrow(d3)] = paste0(cutThresholdOthers , ' and more')
#   p2 = ggplot(d3, aes(x = factor(N), y=count)) + geom_bar(stat="identity") +
#     geom_text(aes(label=count), vjust=-.3, size=8) +
#     ylim(0, (max(d3$count)*1.4)) +
#     theme(text = element_text(size=23), axis.title.y = element_text(size = 17 ) , panel.border = element_blank(), 
#           panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
#     labs(x = "Number of comedications per patient")
#   
#   library(grid)
#   library("gridExtra")
#   # grid.newpage()
#   pl = arrangeGrob(p1, p2, ncol = 1, heights = c(4, 1))
#   return(pl)
# } 
# 

clusterPAtients <- function(data_medication_long){
  variablesAll = c('ATC_A', 'ATC_C', 'ATC_N', 'ATC_other', 'age', 'bmi', 'pathcr')
  variables = c('ATC_A', 'ATC_C', 'ATC_N', 'ATC_other', 'age', 'bmi')
  
  dataAllVars = database_pcr_elig[, variablesAll]
  dataAllVars = dataAllVars[complete.cases(dataAllVars),]
  
  data_medication_long = database_pcr_elig[, -which(!colnames(data_medication_long) %in% variables)]

  
  # PCA
  pca = prcomp(data_medication_long, center = T, scale. = T)
  
  summary(pca)
  
  library(ggbiplot)
  ggbiplot(pca, groups = factor(dataAllVars$pathcr))
  
  
  # fill NA gaps
  
  
  data_medication_long = imp[[1]]
  library(FactoMineR)

  data_medication_long=data_medication_long[,c("pCR", "age", "bmi", "htgrad", "tumstat", "p53","nuicc_4cl", "menop", "subtype3")]
  
  resFM = MFA(data_medication_long, group = c(1,2,2,3,1,1),
              type = c("n","c","c","n","n","n"), ncp = 5,
              name.group = c("pCR", "Age-BMI", "Clinical size - ki67", "Cancer caracteristics", "Menopause", "Chemo type" ),
              num.group.sup = 1,graph = F)
  
  
}


plotGLM = function (data_medication_long, dependent, explanatory, random_effect = NULL, 
                    factorlist = NULL, glmfit = NULL, confint_type = NULL, remove_ref = FALSE, 
                    breaks = NULL, column_space = c(-0.5, -0.1, 0.1, 0.25, 0.4, 0.7, 0.82), dependent_label = NULL, 
                    prefix = "", suffix = ": OR (95% CI, p-value)", table_text_size = 5, 
                    title_text_size = 18, plot_opts = NULL, table_opts = NULL) 
{
  library(forestmodel)
  library(survival)
  library(dplyr)
  library(scales)
  library(finalfit)
  
  #requireNamespace("ggplot2")
  #requireNamespace("scales")
  if (!is.null(factorlist)) {
    if (is.null(factorlist$Total)) 
      stop("summary_factorlist function must include total_col=TRUE")
    if (is.null(factorlist$fit_id)) 
      stop("summary_factorlist function must include fit_id=TRUE")
  }
  if (is.null(factorlist)) {
    factorlist = summary_factorlist(data_medication_long, dependent, explanatory, 
                                    total_col = TRUE, fit_id = TRUE)
  }
  if (remove_ref) {
    factorlist = factorlist %>% dplyr::mutate(label = ifelse(label == 
                                                               "", NA, label)) %>% tidyr::fill(label) %>% dplyr::group_by(label) %>% 
      dplyr::filter(dplyr::row_number() != 1 | dplyr::n() > 
                      2) %>% rm_duplicate_labels()
  }
  if (is.null(breaks)) {
    breaks = scales::pretty_breaks()
  }
  if (is.null(confint_type) && is.null(random_effect)) {
    confint_type = "profile"
  }else if (is.null(confint_type) && (!is.null(random_effect) | class(glmfit) == "glmerMod")) {
    confint_type = "default"
  }
  if (is.null(glmfit) && is.null(random_effect)) {
    glmfit = glmmulti(data_medication_long, dependent, explanatory)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type)
  } else if (is.null(glmfit) && !is.null(random_effect)) {
    glmfit = glmmixed(.data_medication_long, dependent, explanatory, random_effect)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)", 
                         confint_type = confint_type, ...)
  }
  if (!is.null(glmfit) && is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type, estimate_name = "OR", 
                         exp = TRUE)
  }else if (!is.null(glmfit) && !is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)", 
                         confint_type = confint_type, estimate_name = "OR", 
                         exp = TRUE, ...)
  }
  glmfit_df = fit2df(glmfit, condense = FALSE, confint_type = confint_type, 
                     estimate_name = "OR", exp = TRUE)
  df.out = finalfit_merge(factorlist, glmfit_df_c)
  df.out = finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
  # df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>% 
  #  as.numeric()
  # df.out$Total[which(df.out$levels %in% c("Mean (SD)", "Median (IQR)"))] = dim(data_medication_long)[1]
  df.out$levels[which(df.out$levels %in% c("Mean (SD)", "Median (IQR)"))] = "-"
  if (any(is.na(df.out$label))) {
    remove_rows = which(is.na(df.out$label))
    df.out = df.out[-remove_rows, ]
  } 
  
  df.out$p = unlist(lapply(df.out$`OR (multivariable)`, function(x){if(grepl('p',x)){substr(x, (gregexpr('p',x)[[1]][1]+2), (nchar(x)-1))} else ""}))
  #df.out$p = unlist(lapply(df.out$p, function(x){if(substr(x,1,1)=="=")substr(x,2,nchar(x))else x}))
  #df.out$`OR (multivariable)`[seq(2,nrow(df.out),2)] = paste0(substr(df.out$`OR (multivariable)`[seq(2,nrow(df.out),2)],1,5), "[", substr(df.out$`OR (multivariable)`[seq(2,nrow(df.out),2)],7,15),"]")
  df.out$`OR (multivariable)` =  unlist(lapply(df.out$`OR (multivariable)`, function(x){if(grepl('\\(',x)){paste0(substr(x, (gregexpr('\\(',x)[[1]][1]), (gregexpr(',',x)[[1]][1] - 1)), ")")} else ""}))
  fillColor = sapply((1:nrow(df.out)), function(i){if(is.na(df.out$U95[i])){"black"}else{if(df.out$U95[i] < 1){"red"}else{if(df.out$L95[i] > 1) "green" else "black"}}})
  
  df.out$levels = as.character(df.out$levels)
  df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])
  g1 = ggplot(df.out, aes(x = as.numeric(OR), xmin = as.numeric(L95), 
                          xmax = as.numeric(U95), y = fit_id)) + geom_point(size = 2.5, shape=22, fill = fillColor) + geom_errorbarh(height = 0.2) + 
    geom_vline(xintercept = 1, linetype = "longdash", colour = "black") + 
    scale_x_continuous(trans="pseudo_log", breaks = pretty_breaks()) + 
    xlab("Odds ratio (95% CI, log scale)") + theme_classic(14) + 
    theme(axis.title.x = element_text(), axis.title.y = element_blank(), 
          axis.text.y = element_blank(), axis.line.y = element_blank(), 
          axis.ticks.y = element_blank(), legend.position = "none",
          plot.margin=unit(c(-1.2,1,1,-1), "cm"))
  
  t0 = ggplot(df.out[1,], aes(x = 1, y = 1)) + 
    annotate("text", x = column_space[1], y = 1, 
             label = colnames(df.out)[2], hjust = 0, size = table_text_size, fontface =2) + 
    annotate("text", x = column_space[2], y = 1, 
             label = colnames(df.out)[3], hjust = 1, size = table_text_size, fontface =2) + 
    annotate("text", x = column_space[3], y = 1, 
             label = paste(dependent, "=", colnames(df.out)[4]), hjust = 1, size = table_text_size, fontface =2) + 
    annotate("text", x = column_space[4], y = 1, 
             label = paste(dependent, "=", colnames(df.out)[5]), hjust = 1, size = table_text_size, fontface =2) +
    annotate("text", x = column_space[5], y = 1, 
             label = colnames(df.out)[6], hjust = 1, size = table_text_size, fontface =2) +
    annotate("text", x = column_space[6], y = 1, 
             label = colnames(df.out)[8], hjust = 1, size = table_text_size, fontface =2) + 
    annotate("text", x = column_space[7], y = 1, 
             label = colnames(df.out)[12], hjust = 1, size = table_text_size, fontface =2) + 
    theme_classic(14) + theme(
      axis.title.x = element_text(colour = "white"), 
      axis.text.x = element_text(colour = "white"), axis.title.y = element_blank(), 
      axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
      line = element_blank(),
      plot.margin=unit(c(0.5,0.2,-0.7,1), "cm"))
  
  t0Invisible = ggplot(df.out[1,], aes(x = 1, y = 1)) + 
    annotate("text", x = column_space[1], y = 1, 
             label = colnames(df.out)[2], hjust = 0, size = table_text_size, fontface =2, colour = "white") + 
    theme_classic(14) + theme(
      axis.title.x = element_text(colour = "white"), 
      axis.text.x = element_text(colour = "white"), axis.title.y = element_blank(), 
      axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
      line = element_blank(),
      plot.margin=unit(c(1,1,-0.7,-1), "cm"))
  
  t1 = ggplot(df.out, aes(x = as.numeric(OR), y = fit_id)) + 
    annotate("text", x = column_space[1], y = df.out$fit_id, 
             label = df.out[, 2], hjust = 0, size = table_text_size) + 
    annotate("text", x = column_space[2], y = df.out$fit_id, 
             label = df.out[, 3], hjust = 1, size = table_text_size) + 
    annotate("text", x = column_space[3], y = df.out$fit_id, 
             label = df.out[, 4], hjust = 1, size = table_text_size) + 
    annotate("text", x = column_space[4], y = df.out$fit_id, 
             label = df.out[, 5], hjust = 1, size = table_text_size) +
    annotate("text", x = column_space[5], y = df.out$fit_id, 
             label = df.out[, 6], hjust = 1, size = table_text_size) +
    annotate("text", x = column_space[6], y = df.out$fit_id, 
             label = df.out[, 8], hjust = 1, size = table_text_size) + 
    annotate("text", x = column_space[7], y = df.out$fit_id, 
             label = df.out[, 12], hjust = 1, size = table_text_size) + 
    theme_classic(14) + theme(axis.title.x = element_text(colour = "white"), 
                              axis.text.x = element_text(colour = "white"), axis.title.y = element_blank(), 
                              axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                              line = element_blank(),
                              plot.margin=unit(c(-1.2,0.2,1,1), "cm"))
  g1 = g1 + plot_opts 
  t1 = t1 + table_opts
  t2 = gridExtra::grid.arrange(t0, t1, nrow=2, ncol=1,heights = c(2, 8))
  g2 = gridExtra::grid.arrange(t0Invisible, g1, nrow=2, ncol=1,heights = c(2, 8))
  title = plot_title(data_medication_long, dependent, dependent_label = dependent_label, 
                     prefix = prefix, suffix = suffix)
  gridExtra::grid.arrange(t2, g2, ncol = 2, widths = c(5, 1), 
                          top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size), 
                                               just = "left"))
  return(df.out)
}



plot_KM_comeds <- function(tt_event, event, name_y_lab, name_x_lab, vec, ncols, nrows_legend, d, ATC = NA){
  pl = list()
  for(v in vec){
    print('aaa')
    print(v)
    print(as.formula(paste0("Surv(I(",tt_event, "/365.25),", event, ") ~ ",v)))
    sfit_subtype3_trt_arm_wholepop <-  survfit(formula = as.formula(paste0("Surv(I(",tt_event, "/365.25),", event, ") ~ ", v) ), data = d)
    tit = paste(strwrap(gsub("\\.", " ", v), width = 20), collapse = "\n")
    if(is.data.frame(ATC)){
     
      ATC_cp = ATC
      ATC_cp$ATC.level.name = make.names(firstUp(ATC_cp$ATC.level.name), unique = T)
      if(v %in% ATC_cp$ATC.level.name & v!= 'Others')
        tit = paste0(tit, " (ATC ", ATC_cp$ATC.code[which(ATC_cp$ATC.level.name==v)], ")")
    }
    print(sfit_subtype3_trt_arm_wholepop)
    pl[[v]] <- ggsurvplot(sfit_subtype3_trt_arm_wholepop,
               data = d,
               palette = "lancet", 
               fontsize = 2.5, # size of the numbers in the risk table
               risk.table = F,
               conf.int = F,
               pval = TRUE,
               ggtheme = theme_minimal(),
               ylab = name_y_lab,
               xlab = name_x_lab,
               legend = "top",
               legend.labs = c("No", "Yes"),
               legend.title = "",
               pval.size = 3.5,
               title = tit,
               font.x = 10, font.y = 10, font.legend = 10, font.title = 10,
               censor.size = 6, # numveric value specifying the point size of censors. Default is 4.5
               tables.theme = theme_cleantable(),
               size = 2)
  }
  arrange_ggsurvplots(pl, ncol = 3, nrow = 3)
  
}


### Causal analysis

createClassesCausalAnalysis = function(data_medication_very_wide, data_medication_long, database_preprocessed_labels, analysed_classes = c('A', 'C', 'N', 'M', 'H', 'B', 'R'),
                                       threshold_ATC1 = 20, threshold_ATC2 = 20, threshold_ATC5 = 10){
  
  #' Create variables for statistical analysis. Variables are added to the returned data frame (named ATC_.. )
  #'
  #' @param data_medication_very_wide dataframe of medications, very wide version
  #' @param data_medication_long dataframe of medications, long version
  #' @param database_preprocessed_labels database with patient information
  #' @param analysed_classes ATC level 1 classes that have to be analysed.
  #' 
  #' @return data frame with assigned variables for causal analysis
  #' 
   
  analysed_classes <<- analysed_classes
  threshold_ATC1 <<- threshold_ATC1
  threshold_ATC2 <<- threshold_ATC2
  threshold_ATC5 <<- threshold_ATC5

  data_medication_very_wide = data_medication_very_wide[match(database_preprocessed_labels$base_cletri, data_medication_very_wide$base_cletri),]
  
  database_preprocessed_labels = dplyr::left_join(database_preprocessed_labels, data_medication_very_wide, by='base_cletri')
  
  # level 1
  base = data.frame(atc = data_medication_long$atc_level1_cod, base_cletri = data_medication_long$base_cletri)
  atc = unique(base$atc)
  ATC_level1tot <- unlist(sapply(atc, function(x){ length(unique((dplyr::filter(base, atc == x) %>% dplyr::select(base_cletri))[,1]) ) }))
  names(ATC_level1tot) = atc
  ATC_level1tot = sort(ATC_level1tot, decreasing = T)
  ATC_level1 <<- ATC_level1tot[which(ATC_level1tot >= threshold_ATC1 & names(ATC_level1tot) %in% analysed_classes)]	
  ATC_level1	
  
  # level 2
  base = data.frame(atc2 = data_medication_long$atc_level2_cod, base_cletri = data_medication_long$base_cletri)
  atc2 = unique(base$atc2)
  ATC_level2tot <- unlist(sapply(atc2, function(x){ length(unique((dplyr::filter(base, atc2 == x) %>% dplyr::select(base_cletri))[,1]) ) }))
  names(ATC_level2tot) = atc2
  ATC_level2 = sort(ATC_level2tot, decreasing = T)
  ATC_level2 <<- ATC_level2[which(ATC_level2 >= threshold_ATC2 )] #& substr(names(ATC_level2),1,1) %in% analysed_classes)]	
  ATC_level2	
  
  # level 5
  base = data.frame(atc5 = data_medication_long$atc_level5_cod, base_cletri = data_medication_long$base_cletri)
  base = base[which(!is.na(base$atc5)),]
  atc5 = unique(base$atc5)
  ATC_level5tot <- unlist(sapply(atc5, function(x){ length(unique((dplyr::filter(base, atc5 == x) %>% dplyr::select(base_cletri))[,1]) ) }))
  names(ATC_level5tot) = atc5
  ATC_level5 = sort(ATC_level5tot, decreasing = T)
  ATC_level5 <<- ATC_level5[which(ATC_level5 >= threshold_ATC5 )] #& substr(names(ATC_level5),1,1) %in% analysed_classes)]	
  ATC_level5
  

  # level 1
  database_preprocessed_labels$ATC_other <- 	database_preprocessed_labels[	, paste("ATC", analysed_classes, sep = "_")] <- 0	
  
  for(cl in analysed_classes){
    if(paste0(cl, '_bin') %in% colnames(database_preprocessed_labels))
      database_preprocessed_labels[,paste0("ATC_", cl)] <- 	as.numeric(factor(database_preprocessed_labels[,paste0(cl, '_bin')], labels = c(0,1), levels = c('No', 'Yes')))-1
  }
  
  for (b in database_preprocessed_labels$base_cletri){	
    if (any (! (dplyr::filter(data_medication_long, base_cletri==b) %>% dplyr::select(atc_level1_cod))[,1] %in% analysed_classes)) 	
      database_preprocessed_labels[database_preprocessed_labels$base_cletri == b, "ATC_other"] <- 1	
  }
  
  #level2
  ATC_2_analysis_classes = names(ATC_level2)
  ATC_2_analysis_classes = sort(ATC_2_analysis_classes)
  ATC_2_analysis_classes_present = c()
  
  ATC_2_analysis_names = paste0('ATC_', ATC_2_analysis_classes)
  
  
  ATC_2_except_analysis_names <<- unlist(sapply(ATC_2_analysis_classes, function(x){
    if(substr(x,1,1) %in% analysed_classes) paste0("ATC_", substr(x,1,1), '_except_', x) 
    else paste0("ATC_others_except", x)}))
  
  
  # level 1 except level 2 
  database_preprocessed_labels[	 , ATC_2_except_analysis_names] <- 0	
  
  for(cl in ATC_2_analysis_classes){
    if(paste0(cl, '_bin') %in% colnames(database_preprocessed_labels)){
      database_preprocessed_labels[,paste0("ATC_", cl)] <- 	as.numeric(factor(database_preprocessed_labels[,paste0(cl, '_bin')], labels = c(0,1), levels = c('No', 'Yes')))-1
      ATC_2_analysis_classes_present = c(ATC_2_analysis_classes_present, paste0("ATC_", cl))
    }
  }
  
  ATC_2_analysis_classes_present <<- ATC_2_analysis_classes_present
  
  for(b in database_preprocessed_labels$base_cletri){
    medicaments_lev2 = unique((dplyr::filter(data_medication_long, base_cletri==b) %>% dplyr::select(atc_level2_cod))[,1])
    medicaments_lev1 = substr(medicaments_lev2,1,1)
    for(med2 in names(ATC_2_except_analysis_names)){
      med1 = substr(med2,1,1)
      if(med1 %in% analysed_classes){
        if(any(medicaments_lev2[which(medicaments_lev1 == med1)] != med2)){
          database_preprocessed_labels[which(database_preprocessed_labels$base_cletri == b), ATC_2_except_analysis_names[which(names(ATC_2_except_analysis_names) == med2)]] <- 1
        }
      } else {
        if(length(medicaments_lev2[which(medicaments_lev2 != med2 & !substr(medicaments_lev2,1,1) %in% analysed_classes)]) > 0)
          database_preprocessed_labels[which(database_preprocessed_labels$base_cletri == b), ATC_2_except_analysis_names[which(names(ATC_2_except_analysis_names) == med2)]] <- 1
      }
    }
  }
  
  
  # ATC level 5
  ATC_5_analysis_classes = names(ATC_level5)
  ATC_5_analysis_classes_present = c()
  for(cl in ATC_5_analysis_classes){
    if(paste0(cl, '_bin') %in% colnames(database_preprocessed_labels)){
      ATC_5_analysis_classes_present = c(ATC_5_analysis_classes_present, cl)
      
      database_preprocessed_labels[,paste0("ATC_", cl)] <- 	as.numeric(factor(database_preprocessed_labels[,paste0(cl, '_bin')], labels = c(0,1), levels = c('No', 'Yes')))-1
      #  database_preprocessed_labels[,paste0("ATC_", substr(cl,1,1), "_except_", cl)] <- 	
      #  as.numeric(factor(database_preprocessed_labels[,paste0(substr(cl,1,1), "_except_", cl, '_bin')],labels = c(0,1), levels = c('No', 'Yes')))-1
    }
  }
  ATC_5_analysis_classes_present <<- ATC_5_analysis_classes_present
  
  ATC_5_except_analysis_names <<- unlist(sapply(ATC_5_analysis_classes_present, function(x){
    if(substr(x,1,1) %in% analysed_classes) paste0("ATC_", substr(x,1,1), '_except_', x)
    else paste0("ATC_others_except", x)}))
  
  # level 1 except level 2 
  database_preprocessed_labels[	 , ATC_5_except_analysis_names] <- 0	
  
  # level 1 except level 5
  for (b in database_preprocessed_labels$base_cletri){	
    medicaments_lev5 = as.character(na.omit(unique((dplyr::filter(data_medication_long, base_cletri==b) %>% dplyr::select(atc_cod))[,1])))
    medicaments_lev1 = substr(medicaments_lev5,1,1)
    for(med5 in names(ATC_5_except_analysis_names)){
      med1 = substr(med5,1,1)
      if(med1 %in% analysed_classes){
        if(any(medicaments_lev5[which(medicaments_lev1 == med1)] != med5)){
          database_preprocessed_labels[which(database_preprocessed_labels$base_cletri == b), ATC_5_except_analysis_names[which(names(ATC_5_except_analysis_names) == med5)]] <- 1
        }
      } else {
        if(length(medicaments_lev5[which(medicaments_lev5 != med5 & !substr(medicaments_lev5,1,1) %in% analysed_classes)]) > 0)
          database_preprocessed_labels[which(database_preprocessed_labels$base_cletri == b), ATC_5_except_analysis_names[which(names(ATC_5_except_analysis_names) == med5)]] <- 1
      }
    }
  }
  
  lev5_not_present = ATC_5_analysis_classes[which(!ATC_5_analysis_classes %in% ATC_5_analysis_classes_present)]
  c = unlist(sapply(lev5_not_present, function(x){unique(data_medication_long$atc_level5_lib[which(data_medication_long$atc_level5_cod==x)[1]])}))
  
  lev5_not_present_df = ATC_level5[which(names(ATC_level5) %in% names(unlist(sapply(lev5_not_present, function(x){unique(data_medication_long$atc_level5_lib[which(data_medication_long$atc_level5_cod==x)[1]])}))))]
  lev5_not_present_df <<- data.frame(ATC = names(lev5_not_present_df), count = lev5_not_present_df, name = c)
  
  # lev5_present = ATC_5_analysis_classes[which(paste0("ATC_",ATC_5_analysis_classes) %in% ATC_5_analysis_classes_present)]
  # unlist(sapply(lev5_present, function(x){unique(data_medication_long$atc_level5_lib[which(data_medication_long$atc_level5_cod==x)[1]])}))
  return(database_preprocessed_labels)
}


createDatasetImputation = function(database_for_imputation, covariates_imputation, variable_to_predict, n_imputations = 10){
  #' Create dataset with imputed data (no NAs for variables in covariates_imputation)
  #'
  #' @param database_for_imputation data frame of clinical variables, usually named database_preprocessed_labels
  #' @param covariates_imputation vector of covariates that have to be used in causal analysis
  #' @param variable_to_predict binary predictor use in causal analysis, e.g. 'pcr'
  #' 
  #' @return data frame with imputed datasets
  #' 
  #' 
  library(mice)
  
  covariates_imputation <<- covariates_imputation
  
  d = dplyr::select(database_for_imputation,
                    covariates_imputation)
  
  for(c in 1:ncol(d)){
    if(length(which(is.na(d[,c]))) > 0.5 * nrow(d)){
      stop(paste0('Variable ', colnames(d)[c], ' has too many NAs (> 50%). It cannot be used as a covariate. Remove it from the covariates vector and try again.'))
    }
  }
  
  imp0 <- mice(dplyr::select(database_for_imputation,
                             covariates_imputation),
                             maxit = 1, m = 1, seed = 1234)
  
  pred <- imp0$predictorMatrix
  
  imp5 <-
    mice(dplyr::select(database_for_imputation,
                       covariates_imputation),
         predictorMatrix = pred, 
         maxit = 5, m = n_imputations, seed = 1234)
  
  data_imputed <- list()
  
  for(i in 1:imp5$m){
    data_imputed[[i]] <- cbind(database_for_imputation[, -which(names(database_for_imputation) %in%	covariates_imputation)],	complete(imp5, i))	
    data_imputed[[i]]$subtype <- database_for_imputation$subtype
  }
  
  warning = 0 
  for(i in 1:imp5$m){
    data_imputed[[i]] <- cbind(database_for_imputation[ , -which(names(database_for_imputation) %in%	covariates_imputation)],	complete(imp5, i))	
    
    if(is.factor(data_imputed[[i]][,variable_to_predict])){
      data_imputed[[i]][,variable_to_predict] = as.numeric(data_imputed[[i]][,variable_to_predict])
    
      if(unique(data_imputed[[i]][,variable_to_predict] %in% c(1,2)))
        data_imputed[[i]][,variable_to_predict] = data_imputed[[i]][,variable_to_predict] - 1
      
      if(warning == 0){
        warning(paste0('Variable to explain is a factor. The first level (',levels(data_imputed[[i]][,variable_to_predict])[1] ,') will take value 0, the second level (', levels(data_imputed[[i]][,variable_to_predict])[2] ,') value 1'))
        warning = 1
      }
    } else {
      if(!unique(data_imputed[[i]][,variable_to_predict] %in% c(0,1))){
        stop('Variable to explain is numeric but not in range (0,1).')
      }
    }
  }
  
  return(data_imputed)
}



runTmle = function(dataAnalysis, imputated_list, var_to_explain, z_var = NULL, n_iterations = 10, n_cores = 10, statification_variable = NA, level2_analysis = F, 
                   level5_analysis = F, specific_treatment_run = NA, subtypes_analysis = F){
  #' Run TMLE (Targeted Maximum Likelihood Estimation)
  #'
  #' @param dataAnalysis data frame of clinical variables, usually named database_preprocessed_labels
  #' @param imputated_list list of imputed data frames (object returned by createDatasetImputation())
  #' @param var_to_explain binary target variable
  #' @param z_var possible binary mediator 
  #' @param n_iterations number of interactions for muliple TMLE analysis on multiple imputed datasets. This value should correspond to the length of imputated_list 
  #' @param n_cores number of CPUs to use 
  #' @param statification_variable if stratification should be performed   
  #' @param level2_analysis if analysis on ATC level 2 should be performed
  #' @param level5_analysis if analysis on ATC level 5 should be performed
  #' @param specific_treatment_run run TMLE on a specific treatment binary variable
  #' 
  #' @return data frame with imputed datasets
  #' 
  #'  
  
  
  
  if(!length(which(unique(imputated_list[[1]][,var_to_explain]) %in% c(0,1)) == T) == 2){
    stop(paste0('Value in variable ', var_to_explain, ' contains values different from 1 and 0'))
  }

  library(tmle)	
  library(parallel)

  # Specify the given library for the superLearner in the following:	
  # 	
  SL.library <- c("SL.glm", "SL.step", "SL.stepAIC",	
                  "SL.glmnet",	
                  "SL.glm.interaction", "SL.gam",	
                  "SL.randomForest", "SL.rpart")	
  
  # library(randomForest)
  
  ATC_lev1 <<- c(paste("ATC", analysed_classes, sep = "_"), 'ATC_other')
  
  
  function_lev1_2_5 = function(xx){
    x = xx[[1]]
    atc_class = xx[[2]]
    zz = xx[[3]]
    
    covariates_TMLE_run = covariates_imputation
    
    if(length(xx) == 5){
      stratification_variable = xx[[4]]
      stratification = xx[[5]]
      x = dplyr::filter(x, stratification_variable == stratification)
      if(stratification_variable %in% covariates_TMLE_run)
      covariates_TMLE_run = covariates_TMLE_run[-which(covariates_TMLE_run == stratification_variable)]
    }
    
    if(!is.null(zz))
      zz = x[, "trt_arm"]
    
    
    if(atc_class %in% ATC_lev1){
      return(tmle(Y = x[,var_to_explain],
                  A = x[, atc_class],	
                  W = x[,	c(ATC_lev1[-which(ATC_lev1 == atc_class)],
                            covariates_TMLE_run)],	
                  Z = zz,	
                  family = "binomial",	
                  g.SL.library = SL.library,	
                  Q.SL.library = SL.library,	
                  V = 10,	
                  verbose = TRUE))	
    } 
    
    if(atc_class %in% ATC_2_analysis_classes_present){
      corresponding_ATC_lev1 = substr(atc_class, 1, 5)
      
      if(corresponding_ATC_lev1 %in% ATC_lev1){
        vec = c(ATC_lev1[-which(ATC_lev1 == corresponding_ATC_lev1)], ATC_2_except_analysis_names[which(names(ATC_2_except_analysis_names) == gsub('ATC_','',atc_class))])
      } else{
        corresponding_ATC_lev1 = 'ATC_other'
        vec = c(ATC_lev1[-which(ATC_lev1 == corresponding_ATC_lev1)], ATC_2_except_analysis_names[which(names(ATC_2_except_analysis_names) == gsub('ATC_','',atc_class))])
      }
      
      return(tmle(Y = x[, var_to_explain],	
                  A = x[, atc_class],	
                  W = x[,	c(vec,
                            covariates_TMLE_run)],	
                  Z = zz,	
                  family = "binomial",	
                  g.SL.library = SL.library,	
                  Q.SL.library = SL.library,	
                  V = 10,	
                  verbose = TRUE))
    }
    
    if(atc_class %in% paste0("ATC_", ATC_5_analysis_classes_present)){
      corresponding_ATC_lev1 = substr(atc_class, 1, 5)
      
      if(corresponding_ATC_lev1 %in% ATC_lev1){
        vec = c(ATC_lev1[-which(ATC_lev1 == corresponding_ATC_lev1)], ATC_5_except_analysis_names[which(names(ATC_5_except_analysis_names) == gsub('ATC_','',atc_class))])
      } else{
        corresponding_ATC_lev1 = 'ATC_other'
        vec = c(ATC_lev1[-which(ATC_lev1 == corresponding_ATC_lev1)], ATC_5_except_analysis_names[which(names(ATC_5_except_analysis_names) == gsub('ATC_','',atc_class))])
      }
      
      x = x[complete.cases(x[, c(var_to_explain,atc_class,c(vec,covariates_TMLE_run) )]), ]
      
      return(tmle(Y = x[,var_to_explain],	
                  A = x[, atc_class],	
                  W = x[,	c(vec,
                            covariates_TMLE_run)],	
                  Z = zz,	
                  family = "binomial",	
                  g.SL.library = SL.library,	
                  Q.SL.library = SL.library,	
                  V = 10,	
                  verbose = TRUE))
    }
    
    
    
    
  }
  
  
  assign_results = function(treatment, subtypes = NULL, z_var = NULL ){
    my_list = list()
    
    for(i in 1:n_iterations){
      my_list[[i]] = list(imputated_list[[i]], treatment, z_var)
    }
    
    if(n_cores > 1)
      result_ATC_full = mclapply(my_list, function_lev1_2_5, mc.cores = n_cores)
    else
      result_ATC_full = lapply(my_list, function_lev1_2_5)
      
    if(is.null(z_var)){
      mean_result_ATC_full <-	
        base::mean(sapply(result_ATC_full, function(x) x$estimates$OR$psi))	
      vartot_result_ATC_full <- 	
        base::mean(sapply(result_ATC_full,	
                    function(x) x$estimates$OR$var.log.psi)) + 	
        (1 + 1/length(result_ATC_full)) *	
        var(sapply(result_ATC_full,	
                   function(x) x$estimates$OR$log.psi))	
      CI_result_ATC_full <- 	
        exp(base::mean(sapply(result_ATC_full,	
                        function(x) x$estimates$OR$log.psi)) + 	
              qt(c(.025,.975),	
                 df = (length(result_ATC_full) - 1) *	
                   vartot_result_ATC_full / 	
                   ((1 + 1/length(result_ATC_full)) * 	
                      var(sapply(result_ATC_full,	
                                 function(x) x$estimates$OR$log.psi)))) *	
              sqrt(vartot_result_ATC_full))	
      
      # assign values 	
      assign(paste0("mean_result_", treatment, "_full"), mean_result_ATC_full, envir = .GlobalEnv)
      assign(paste0("CI_result_", treatment, "_full"), CI_result_ATC_full, envir = .GlobalEnv)
    }
    
    
    # analysis on stratification
    if(!is.na(statification_variable)){
      for(stratification in statification_variable){
        print(stratification)
        my_list = list()
        for(i in 1:n_iterations){
          my_list[[i]] = list(imputated_list[[i]], atc_class, z_var, statification_variable, stratification)
        }
        # with no Z
        result_ATC_full_subtype = mclapply(my_list, function_lev1_2_5, mc.cores = n_cores)
        
        if(is.null(z_var)){
          mean_result_ATC_full <-	
            base::mean(sapply(result_ATC_full_subtype, function(x) x$estimates$OR$psi))	
          vartot_result_ATC_full <- 	
            base::mean(sapply(result_ATC_full_subtype,	
                        function(x) x$estimates$OR$var.log.psi)) + 	
            (1 + 1/length(result_ATC_full_subtype)) *	
            var(sapply(result_ATC_full_subtype,	
                       function(x) x$estimates$OR$log.psi))	
          CI_result_ATC_full <- 	
            exp(base::mean(sapply(result_ATC_full_subtype,	
                            function(x) x$estimates$OR$log.psi)) + 	
                  qt(c(.025,.975),	
                     df = (length(result_ATC_full_subtype) - 1) *	
                       vartot_result_ATC_full / 	
                       ((1 + 1/length(result_ATC_full_subtype)) * 	
                          var(sapply(result_ATC_full_subtype,	
                                     function(x) x$estimates$OR$log.psi)))) *	
                  sqrt(vartot_result_ATC_full))	
          
          # assign values 	
          assign(paste0("mean_result_", treatment, stratification, "_full"), mean_result_ATC_full, envir = .GlobalEnv)
          assign(paste0("CI_result_", treatment, stratification, "_full"), CI_result_ATC_full, envir = .GlobalEnv)
        }
        
      }
    }
  }
  
  if(is.na(specific_treatment_run)){
    for(atc_class in ATC_lev1){
      if(length(which(dataAnalysis[,atc_class]==1)) > threshold_ATC1){
        print(atc_class)
        if(subtypes_analysis == T)
          assign_results(treatment = atc_class, z_var = z_var, subtypes = unique(as.character(na.omit(dataAnalysis$subtype))))
        else 
          assign_results(treatment = atc_class, z_var = z_var)
      }
    }
    
    if(level2_analysis == T){
      ATC_2_analysis_names_kept = c()
      for(atc_class in ATC_2_analysis_classes_present){
        if(length(which(dataAnalysis[,atc_class]==1)) > threshold_ATC2){
          print(atc_class)
          ATC_2_analysis_names_kept = c(ATC_2_analysis_names_kept, atc_class)
          assign_results(treatment = atc_class, z_var = z_var)
        }
      }
      ATC_2_analysis_names_kept <<- ATC_2_analysis_names_kept
    }
    
    if(level5_analysis == T){
      ATC_5_analysis_names_kept = c()
      for(atc_class in ATC_5_analysis_classes_present){
        atc_class = paste0('ATC_',atc_class)
        if(length(which(dataAnalysis[,atc_class]==1)) > threshold_ATC5){
          if(length(which(is.na(dataAnalysis[,atc_class]))) < 0.05 * nrow(dataAnalysis)){
            print(atc_class)
            ATC_5_analysis_names_kept = c(ATC_5_analysis_names_kept, atc_class)
            assign_results(treatment = atc_class)
          }
        }
      }
      ATC_5_analysis_names_kept <<- ATC_5_analysis_names_kept
    }
  } else {
    if(specific_treatment_run %in% colnames(dataAnalysis)){
      if(length(which(is.na(dataAnalysis[,specific_treatment_run]))) < 0.1 * nrow(dataAnalysis)){
        assign_results(treatment = specific_treatment_run, z_var = z_var)
      } else {
        stop('Too many NAs on the treatment variable (>10%). Check your data first and filter NAs.')
      }
    }
    else
      stop('The variable is not included the input data frame.')
  }
}




clusterisePatientsComedications = function(data_medication_long, database_preprocessed_labels, variables_plot = NA, level = 1){
  #' Plot the heatmap of medications with dendogram based on correlation
  #'
  #' @param data_medication_long data frame of comedications, long form
  #' @param database_preprocessed_labels database with patient information 
  #' @param variables_plot variables to be put in the plot (e.g. age, bmi, etc..)
  #' @param level atc level to plot (1, 2 or 5)
  #' 
  #' library(dplyr)
  #' 
  #' library(gplots)
  library(dendextend)
  
  if(level == 1)
    var = 'atc_level1_cod'
  if(level == 2)
    var = 'atc_level2_cod'
  if(level == 5)
    var = 'atc_level5_cod'
  
  level_unique = unique(data_medication_long[,var])
  
  data_level = as.data.frame(matrix(0, nrow = nrow(database_preprocessed_labels), ncol = length(level_unique)))
  colnames(data_level) = level_unique
  rownames(data_level) = as.character(database_preprocessed_labels$base_cletri)
  for(i in 1:nrow(data_medication_long)){
    code = data_medication_long[i, var]
    base_cletri = as.character(data_medication_long$base_cletri[i])
    data_level[base_cletri,code] = 1
  }
  
  # assign comedication of level 2 with less than 10 cases to class others
  freq = apply(data_level, 2, function(x){length(which(x==1))})
  vars_assign_others = names(freq[which(freq < 10)])
  if(length(vars_assign_others) > 0){
    data_level$Others = unlist(lapply(1:nrow(data_level), function(i){if(sum(data_level[i,vars_assign_others]) > 0) 1 else 0}))
    data_level = data_level[,-which(colnames(data_level) %in% vars_assign_others)]
  }
  
  keep = 1:nrow(data_level)
  
  # remove patients with no comedications
  # keep = as.numeric(which(rowSums(data_level) != 0))
  data_level_al_least_one_medic = t(data_level)
  # data_level_al_least_one_medic = t(data_level[which(rowSums(data_level) != 0),])
  
  row_labels = unlist(lapply(rownames(data_level_al_least_one_medic), function(x){
    if(x %in% data_medication_long[,var]){color_anat[[data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod == substr(x,1,1))[1]] ]]} else 'black'
  }))
  
  rownames(data_level_al_least_one_medic) = unlist(lapply(rownames(data_level_al_least_one_medic), function(x){
    if(x %in% data_medication_long[,var]){paste0(data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod == substr(x,1,1))[1]], " (",x ,")")} else x
  }))
  
  # # hierarchical clustering
  # dist_mat <<- dist(data_level2_al_least_one_medic, method = 'binary')
  # hclust_1 <<- hclust(dist_mat, method = 'ward.D')
  # 
  # # plot(hclust_1)
  # dist_mat_t <<- dist(t(data_level2_al_least_one_medic), method = 'binary')
  # hclust_1_t <<- hclust(dist_mat_t, method = 'ward.D')
  
  # library(RColorBrewer)
  # n <- 10
  # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  # cols_branches = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))[1:n_clust]
  # 
  # dend1 <- as.dendrogram(hclust_1_t)
  # dend1 <- color_branches(dend1, k = n_clust, col = cols_branches)
  # 
  # dend1_t <- as.dendrogram(hclust_1)
  
  # source("https://raw.githubusercontent.com/talgalili/dendextend/master/R/attr_access.R")
  # col_labels <- get_leaves_branches_col(dend1)
  # col_labels <- col_labels[order(order.dendrogram(dend1))]
  
  
  # h = heatmap.2(as.matrix(data_level2_al_least_one_medic),
  #               main = paste( ""),
  #               trace="none",
  #               margins =c(5,15),
  #               col = c("#EBF5FB", "#52B3D5"),
  #               #breaks=col_breaks,
  #               dendrogram="both",
  #               Rowv = dend1_t,
  #               Colv = dend1,
  #               #key.xlab = "",
  #               key=FALSE,
  #               cexRow = x_label_size,
  #               cexCol = 0.8,
  #               na.rm = TRUE,
  #               ColSideColors = col_labels,# to add nice colored strips
  #               RowSideColors = row_labels,# to add nice colored strips
  #               colCol = col_labels,# to add nice colored labels - only for qplots 2.17.0 and higher   
  #               
  # )
  
  ## prepare colors
  annotation_colors = row_labels
  names(annotation_colors) = rownames(data_level_al_least_one_medic)
  annotation_col_list = list(Medications = annotation_colors, subtype = c('Luminal' = '#b8cce4', 'TNBC' = '#e6b8b7', 'HER2+' = '#d8e4bc'), 
                             pcr = c('No' = '#DC2010', 'Yes' = '#59E90C'))
  
  annotation_r = data.frame(Medications= rownames(data_level_al_least_one_medic))
  rownames(annotation_r) = rownames(data_level_al_least_one_medic)
  annotation_r$Medications = as.character(annotation_r$Medications)
  
  annotation_c = NULL
  if(!is.na(variables_plot[1])){
    # library(mice)
    
    annotation_c= database_preprocessed_labels[keep, variables_plot]
    rownames(annotation_c) = colnames(data_level_al_least_one_medic)
    
    # imp0 <- mice(annotation_c,
    #              maxit = 1, m = 1, seed = 1234)
    # 
    # pred <- imp0$predictorMatrix
    # 
    # imp5 <-
    #   mice(annotation_c,
    #        predictorMatrix = pred, 
    #        maxit = 5, m = 1, seed = 1234)
    # 
    # annotation_c_complete = complete(imp5, 1)
    # rownames(annotation_c_complete) = colnames(data_level2_al_least_one_medic)
  }
  
  library(pheatmap)
  pheatmap(data_level_al_least_one_medic, 
           col = c("#EBF5FB", "#52B3D5"),
           show_rownames=T, cluster_cols=T, cluster_rows=T, show_colnames = F, 
           clustering_distance_rows="binary", cex=1,
           clustering_distance_cols="binary", clustering_method="complete", border_color=F,
           annotation_row   = annotation_r,
           annotation_col   = annotation_c,
           annotation_colors  = annotation_col_list, legend = F, legend_labels = F, annotation_legend = T, treeheight_row = 100, treeheight_col = 100)
  
  library(grid)
  sink("/dev/null") 
  gg = grid.ls(grid.force())
  sink()
  
  grid.gedit(gg$name[length(gg$name)], gp = gpar(col="white"))
  grid.gedit(gg$name[length(gg$name)-1], gp = gpar(fill="white"))
  grid.gedit(gg$name[length(gg$name)-2], gp = gpar(col="white"))
 
  
  grid.gedit(gg$name[grep('row_annotation_names',gg$name )], gp = gpar(col="white"))
  
  
  # cut_avg <- cutree(hclust_1_t, k = n_clust)
  # 
  # # assign patients to each cluster
  # database_preprocessed_labels$cluster =sapply(1:nrow(database_preprocessed_labels), function(i){
  #   if(database_preprocessed_labels$base_cletri[i] %in% names(cut_avg)){cut_avg[which(names(cut_avg)==database_preprocessed_labels$base_cletri[i])]} else 0})
  # 
  # data_medication_long$cluster = sapply(1:nrow(data_medication_long), function(i){
  #   if(data_medication_long$base_cletri[i] %in% names(cut_avg)){cut_avg[which(names(cut_avg)==data_medication_long$base_cletri[i])]} else 0})
  # 
  # return(data_medication_long)
  
}


comedicationEffectPlot <- function(dataAnalysis, data_medication_long, axis_title_size = 10, axis_text_size = 10, title_size = 10, newline_every_n_char = 40) {
  #' Plot the causal effect with OR and CIs
  #'
  #' @param dataAnalysis data frame with patient information 
  #' @param data_medication_long data frame of comedications, long form 
  #' @param axis_title_size size of titles in axis
  #' @param axis_text_size size of text in axis
  #' @param title_size size of title
  #' @param newline_every_n_char go newline every n characters in medication names
   
  boxLabels <- list()
  v1 = c()
  ATC_2_analysis_names_kept_greater_n = c()

  ATC_list_lev2 = ls(.GlobalEnv)[grep('mean_result_', ls(.GlobalEnv))]
  ATC_list_lev2 = substr(ATC_list_lev2, 13, 19)
  if(length(which(grepl('_f',ATC_list_lev2))) > 0)
    ATC_list_lev2 = ATC_list_lev2[-which(grepl('_f',ATC_list_lev2))]
  if(length(which(grepl('_oth',ATC_list_lev2))) > 0)
    ATC_list_lev2 = ATC_list_lev2[-which(grepl('_oth',ATC_list_lev2))]
  
  
  ATC_list_lev1 = ls(.GlobalEnv)[grep('mean_result_', ls(.GlobalEnv))]
  ATC_list_lev1 = unique(substr(ATC_list_lev1, 13, 17))
  if(length(which(grepl('_o',ATC_list_lev1))) > 0){
    ATC_list_lev1 = ATC_list_lev1[-which(grepl('_o',ATC_list_lev1))]
    ATC_list_lev1 = c(ATC_list_lev1, 'ATC_other')
  }
    
  for(atc_class in ATC_list_lev2){
    
    atc_lev1_cl = substr(atc_class,1,5)
    if(length(v1) > 0){
      if(v1[length(v1)] != atc_lev1_cl)
        v1 = c(v1, atc_lev1_cl)
    } else {
      v1 = c(v1, atc_lev1_cl)
    }
    
    boxLabels[[atc_lev1_cl]] = paste0(atc_lev1_cl, " - ", data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==substr(atc_lev1_cl,5,5))[1]])
    
    
    if(length(which(dataAnalysis[,atc_class]==1)) > threshold_ATC2){
      ATC_2_analysis_names_kept_greater_n = c(ATC_2_analysis_names_kept, atc_class)
      boxLabels[[atc_class]] = paste0(atc_class, " - ", data_medication_long$atc_level2_lib[which(data_medication_long$atc_level2_cod==substr(atc_class,5,7))[1]])
      
    }
  }
  
  for(atc_lev1_cl in ATC_list_lev1){
    if(atc_lev1_cl != 'ATC_other')
      boxLabels[[atc_lev1_cl]] = paste0(atc_lev1_cl, " - ", data_medication_long$atc_level1_lib[which(data_medication_long$atc_level1_cod==substr(atc_lev1_cl,5,5))[1]])
    else
      boxLabels[[atc_lev1_cl]] = atc_lev1_cl
  }
  
  # boxLabels[["ATC_other"]] = "Others"
  
  ## All
  v = names(boxLabels)
  
  ## ALL
  df_full_ATC_level1 = data.frame(matrix(NA, nrow = 0,ncol = 3 ))
  for(val in v){
    df_full_ATC_level1 = rbind(df_full_ATC_level1, c(as.numeric(unlist(c(mget(ls(.GlobalEnv, pattern = paste0('mean_result_',val, "_full")), envir = .GlobalEnv)))), as.numeric(unlist(mget(ls(.GlobalEnv, pattern = paste0('CI_result_',val, "_full")), envir = .GlobalEnv)))))
  }
  colnames(df_full_ATC_level1) = c('boxOdds', 'boxCILow', 'boxCIHigh')
  rownames(df_full_ATC_level1) = v
  
  df1_ <- dplyr::select(dataAnalysis, v)
  df1_ <- apply(df1_,2,as.numeric)
  df2_ <- dplyr::filter(dataAnalysis, pcr=='Yes') %>% dplyr::select(v)
  df2_ <- apply(df2_,2,as.numeric)
  
  
  df_ATC_level1_all <-
    data.frame("Eff" = c(colSums(
      df1_)),
      "Eff_pCR" = c(colSums(
        df2_))
    )
  
  df_full_ATC_level1 = cbind(df_full_ATC_level1, df_ATC_level1_all)

  
  keep = 1:nrow(df_full_ATC_level1)
  #keep = c(1,4,6,9)
  #keep = c(2,3,5,7,8)
  
  data = df_full_ATC_level1
  data$type = rep(v[keep],1)
  data$ATC = rep(v[keep],1)
  data$ATC1 = substr(data$ATC,1,5)
  if(length(which(data$ATC1 == 'ATC_o')))
    data$ATC1[which(data$ATC1 == 'ATC_o')] = 'ATC_other'
  data$names = c(rep("ALL" , length(unique(data$type))))
  data$names = factor(data$names, levels = c('ALL'))
  rownames(data) = 1:nrow(data)
  
  data$type = factor(data$type, levels = unique(data$type), labels = unlist(lapply(unique(data$type), function(x){boxLabels[[x]]})))
  
  
  
  
  require(gridExtra)
  df=data
  
  n_cols = max(as.data.frame(dplyr::group_by(df, ATC1) %>% dplyr::summarize(count=n()))$count)
  n_rows = length(unique(df$ATC1))
  
  emptyIterator = 1
  
  df=as.data.frame(df)
  
  if(any(duplicated(paste(df$names, " (n (A=1) : ", df$Eff, '; n (A=1, pCR=1) :', df$Eff_pCR, ")", sep="")))){
    lab = paste(rownames(df), " (n (A=1) : ", df$Eff, '; n (A=1, pCR=1) :', df$Eff_pCR, ")", sep="")
  } else {
    lab = paste(df$names, " (n (A=1) : ", df$Eff, '; n (A=1, pCR=1) :', df$Eff_pCR, ")", sep="")
  }
  
  pl = list()
  iterInData = 1
  posInGrid = 1
  for(cl in unique(df$ATC1)){
    
    df_part = df %>% filter(ATC1==cl)
    
    lab = unique(df_part$names)
    
    
    for(cl2 in unique(df_part$ATC)){
      tit = ''
      if(posInGrid %% n_rows == 1){
        tit = trim(substr(boxLabels[[cl]], gregexpr('-', boxLabels[[cl]])[[1]][1] + 1, nchar(boxLabels[[cl]])))
        tit = paste(strwrap(tit, width = newline_every_n_char), collapse = "\n")
        if(!grepl('\n', tit)){
          tit = paste(tit, '\n')
        }
      } 
      
      df_part2 = df_part %>% filter(ATC==cl2) 
      df_part2$type = paste(strwrap(df_part2$type[1], width = 30), collapse = "\n") 
      
      if(!grepl('\\n', df_part2$type))
        df_part2$type = paste0(df_part2$type, '\n')
      
      col = unlist(lapply(rownames(df_part2), function(x){ if(df_part2[x,"boxCILow"] < 1 & df_part2[x,"boxCIHigh"] < 1 ) "red" 
        else{
          if (df_part2[x,"boxCILow"] > 1 & df_part2[x,"boxCIHigh"] > 1 ) "chartreuse4" 
          else "gray50"
        }
      }))
      
      df_part2$All = ifelse(df_part2$names=='ALL', 'All', 'by arm')
      
      pl[[paste0(cl,'-',cl2)]] = 
        ggplot(df_part2,
               aes(x = boxOdds,
                   y = factor(lab, levels = lab))) +
        geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
        geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow),
                       size = .5, height = .2, color = 'gray50' ) +
        geom_point(size = 3.5, color = col) +
        theme_bw() +
        # scale_x_continuous(name = "Odds ratio for pCR",
        #                    breaks = seq(0, ceiling(max(df$boxCIHigh)), .5),
        #                    labels = seq(0, ceiling(max(df$boxCIHigh)), by = .5),
        #                    limits = c(0.01, ceiling(max(df$boxCIHigh)))) +
        facet_grid(All~type, scales = "free", space = 'free') +
        xlab('') +
        theme(panel.grid.minor = element_blank(),
              axis.title.x = element_text(margin = margin(t = -0.5, r = -0.3, b = 0, l = 0,unit = "cm")),
              axis.text=element_text(size=axis_text_size),
              axis.title=element_text(size=axis_title_size,face="bold"),
              plot.title = element_text(size = title_size, face = "bold", hjust = 0.5),
              strip.text = element_text(size=9),
              strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))) +
        ylab('') +
        xlim(0, ceiling(max(df$boxCIHigh))) +
        ggtitle(tit) +
        labs(tag = toupper(letters[iterInData]))
      
      # pl[[paste0(cl,'-',cl2)]] = arrangeGrob(pl[[paste0(cl,'-',cl2)]], top = textGrob(toupper(letters[iter]), 
      #                                     x = unit(0, "npc"),
      #                                     y   = unit(0, "npc"), 
      #                                     just=c("left","top"),
      #                                     gp=gpar(col="black", fontsize=18, fontfamily="Times Roman")))
      iterInData = iterInData + 1
      posInGrid = posInGrid + 1
    }
    
    posInSubclassPlot = length(unique(df_part$ATC))
    while(posInSubclassPlot < n_rows){
      pl[[paste0(emptyIterator)]] = ggplot() + theme_void()
      emptyIterator = emptyIterator + 1
      posInSubclassPlot  = posInSubclassPlot + 1
      posInGrid = posInGrid + 1
    }
    
  }
  do.call("grid.arrange", c(pl, ncol=n_rows, as.table = FALSE))
}
