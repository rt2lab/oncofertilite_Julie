#Load ATC cod
atc <- read.csv(file.path(Sys.getenv("PROJECT_PATH"),"core/00_common/docs/ATC_WHO.csv"), sep = ";") %>% 
  dplyr::select(atc_level_cod = ATC.code, atc_level_lib =ATC.level.name) %>%
  mutate(atc_level_cod = as.character(atc_level_cod)) %>%
  mutate(atc_level_lib = stringr::str_to_sentence(atc_level_lib)) %>%
  mutate(level = case_when(nchar(atc_level_cod) == 1 ~ 1,
                           nchar(atc_level_cod) == 3 ~ 2,
                           nchar(atc_level_cod) == 4 ~ 3,
                           nchar(atc_level_cod) == 5 ~ 4,
                           nchar(atc_level_cod) == 7 ~ 5
  )) 

#Prepare 1 database per ATC cod level (unique car parfois quelques répétitions)
atc_level1 <- atc %>% filter(level == 1) %>% rename(atc_level1_cod = atc_level_cod, atc_level1_lib = atc_level_lib) %>% select(-level) %>% unique()
atc_level2 <- atc %>% filter(level == 2) %>% rename(atc_level2_cod = atc_level_cod, atc_level2_lib = atc_level_lib) %>% select(-level) %>% unique()
atc_level3 <- atc %>% filter(level == 3) %>% rename(atc_level3_cod = atc_level_cod, atc_level3_lib = atc_level_lib) %>% select(-level) %>% unique()
atc_level4 <- atc %>% filter(level == 4) %>% rename(atc_level4_cod = atc_level_cod, atc_level4_lib = atc_level_lib) %>% select(-level) %>% unique()
atc_level5 <- atc %>% filter(level == 5) %>% rename(atc_level5_cod = atc_level_cod, atc_level5_lib = atc_level_lib) %>% select(-level) %>% unique()

#Mutate to get atc code level 1,2,3,4
db <- db %>% 
  mutate(atc_cod = as.character(atc_cod)) %>%
  mutate(atc_level1_cod = ifelse(nchar(atc_cod) > 0,substr(atc_cod, 1,1), NA),
         atc_level2_cod = ifelse(nchar(atc_cod) > 2,substr(atc_cod, 1,3), NA),
         atc_level3_cod = ifelse(nchar(atc_cod) > 3,substr(atc_cod, 1,4), NA),
         atc_level4_cod = ifelse(nchar(atc_cod) > 4,substr(atc_cod, 1,5), NA),
         atc_level5_cod = ifelse(nchar(atc_cod) > 6,substr(atc_cod, 1,7), NA),
  )

#Left join to get libs
db <- db %>% 
  left_join(atc_level1) %>%
  left_join(atc_level2) %>%
  left_join(atc_level3) %>%
  left_join(atc_level4) %>%
  left_join(atc_level5)

#Create cod_lib variable
db <- db %>%
  mutate(
    atc_level1_lib_cod = ifelse(is.na(atc_level1_lib), NA,paste0(atc_level1_lib, " (",atc_level1_cod,")")),
    atc_level2_lib_cod = ifelse(is.na(atc_level2_lib), NA,paste0(atc_level2_lib, " (",atc_level2_cod,")")),
    atc_level3_lib_cod = ifelse(is.na(atc_level3_lib), NA,paste0(atc_level3_lib, " (",atc_level3_cod,")")),
    atc_level4_lib_cod = ifelse(is.na(atc_level4_lib), NA,paste0(atc_level4_lib, " (",atc_level4_cod,")")),
    atc_level5_lib_cod = ifelse(is.na(atc_level5_lib), NA,paste0(atc_level5_lib, " (",atc_level5_cod,")"))
  )

#Create variable 7cl
tokeep_atc1 <- c("A","C","H","M","N","R")
db <- db %>%
  mutate(
    atc_level1_7cl_lib = ifelse(atc_level1_cod %in% tokeep_atc1, atc_level1_cod,"Others"),
    atc_level1_7cl_lib_cod = ifelse(atc_level1_cod %in% tokeep_atc1, atc_level1_lib_cod,"Others"),
    atc_level1_7cl_cod = ifelse(atc_level1_cod %in% tokeep_atc1, atc_level1_cod,"Others")
  )