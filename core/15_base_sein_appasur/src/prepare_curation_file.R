prepare_cleaned_records <- function(file_name){
  #Load last update Eva/Aullene database. 
  if(grepl(".RData",file_name)){
    env <- new.env()
    name_file <- load(paste(Sys.getenv("PROJECT_PATH"),'core/', opt$db_name,'/data',file_name, sep="/"),
               env)[1]
    cleaned_records1 <- as.data.frame(env[[name_file]])
    rm(env)
  }else if(grepl(".csv",file_name)){
    cleaned_records1 <- read.csv(paste(Sys.getenv("PROJECT_PATH"),'core/', opt$db_name,'/data',file_name, sep="/"), 
                                 sep = ';')
  }else{
    print("The input should be a csv or RData file")
    return(NULL)
  }
  
  cleaned_records1$numdos_curie <- as.character(formatC(as.integer(as.character(cleaned_records1$numdos_curie)),
                                                        width = 7, flag = "0",format="d"))

  
  #Il y avait un doublon sur la patient 668
  #Doublon retiré par Aullène donc ne plus faire tourner la ligne suivante.  
  #cleaned_records1 <- cleaned_records1 %>% filter(record_id != 668)
  
  #27 Novembre : Doublon sur la patiente : 989000 #6901 6902
  
  #Test sur 8901809	ok, 1001575 ok, 0990759	ok 
  
  ####Beware : after mid-October : there was a change in the RedCap form
  #### Meaning that you can have dat_xx_actu not NA but ev_xx_actu NA
  #### As a consequence : we reconstruct all columns ev_xx_actu as ifelse(is.na(dat_xx_actu))
  #### True for recloc recreg meta only (not 2K and contro).
  cleaned_records1 <- cleaned_records1 %>%
    mutate(
      ev_recloc_actu2 = ifelse(dat_recloc_actu == "", 0,1),
      ev_recreg_actu2 = ifelse(dat_recreg_actu == "", 0,1),
      ev_meta_actu2 = ifelse(dat_meta_actu == "", 0,1)
    )
  
  #Recloc
  cleaned_records1$ev_recloc_cleaned <- pmax(cleaned_records1$ev_recloc,cleaned_records1$ev_recloc_actu2, na.rm =T)
  cleaned_records1$dat_recloc_cleaned <- pmin(as.Date(cleaned_records1$dat_recloc, format = "%Y-%m-%d"),
                                              as.Date(cleaned_records1$dat_recloc_actu, format = "%Y-%m-%d"),
                                              na.rm =T)
  
  #Recreg
  cleaned_records1$ev_recreg_cleaned <- pmax(cleaned_records1$ev_recreg,cleaned_records1$ev_recreg_actu2, na.rm =T)
  cleaned_records1$dat_recreg_cleaned <- pmin(as.Date(cleaned_records1$dat_recreg, format = "%Y-%m-%d"),
                                              as.Date(cleaned_records1$dat_recreg_actu, format = "%Y-%m-%d"),
                                              na.rm =T)
  
  #Meta
  cleaned_records1$ev_meta_cleaned <- pmax(cleaned_records1$ev_meta,cleaned_records1$ev_meta_actu2, na.rm =T)
  cleaned_records1$dat_meta_cleaned <- pmin(as.Date(cleaned_records1$dat_meta, format = "%Y-%m-%d"),
                                              as.Date(cleaned_records1$dat_meta_actu, format = "%Y-%m-%d"),
                                              na.rm =T)
  
  #Contro
  cleaned_records1$ev_contro_cleaned <- pmax(cleaned_records1$ev_contro,cleaned_records1$ev_contro_actu, na.rm =T)
  cleaned_records1$dat_contro_cleaned <- pmin(as.Date(cleaned_records1$dat_contro, format = "%Y-%m-%d"),
                                            as.Date(cleaned_records1$dat_contro_actu, format = "%Y-%m-%d"),
                                            na.rm =T)
  
  #Secondk
  cleaned_records1$ev_secondk_cleaned <- pmax(cleaned_records1$ev_secondk,cleaned_records1$ev_secondk_actu, na.rm =T)
  cleaned_records1$dat_secondk_cleaned <- pmin(as.Date(cleaned_records1$dat_secondk, format = "%Y-%m-%d"),
                                              as.Date(cleaned_records1$dat_secondk_actu, format = "%Y-%m-%d"),
                                              na.rm =T)
  
  #Status vital
  cleaned_records1$status_vital_cleaned <- pmax(cleaned_records1$status_vital,cleaned_records1$status_vital_actu, na.rm =T)
  
  #Dat last news
  cleaned_records1$dat_last_news_cleaned <- pmax(as.Date(cleaned_records1$dat_last_news, format = "%Y-%m-%d"),
                                               as.Date(cleaned_records1$dat_last_news_actu, format = "%Y-%m-%d"),
                                               na.rm =T)
  #Patiente 8835 (8504310) 8914 (8908257) dat_last_news_actu = "9999-12-31". Pourquoi ? TODO : check avec Aullene. 
  #cleaned_records1$dat_last_news_cleaned[which(cleaned_records1$dat_last_news_cleaned == "9999-12-31")] <- NA
  #Patiente 1106923 (3576) et 0710245 (9772) : dat last news est apres la date actuelle (faute de frappe?? A corriger).
  
  #Dat last update
  cleaned_records1$dat_last_update_cleaned <- pmax(as.Date(cleaned_records1$dat_last_update, format = "%Y-%m-%d"),
                                                 as.Date(cleaned_records1$dat_last_update_actu, format = "%Y-%m-%d"),
                                                 na.rm =T)
  
  #Select all cleaned variables
  cleaned_records1_to_join <- cleaned_records1 %>% select(numdos_curie,
                                                          ev_recloc_cleaned, dat_recloc_cleaned,
                                                          ev_recreg_cleaned, dat_recreg_cleaned,
                                                          ev_meta_cleaned, dat_meta_cleaned,
                                                          ev_contro_cleaned,dat_contro_cleaned,
                                                          ev_secondk_cleaned,dat_secondk_cleaned,
                                                          status_vital_cleaned,
                                                          dat_last_news_cleaned,
                                                          dat_last_update_cleaned)
  
  return(cleaned_records1_to_join)
}

