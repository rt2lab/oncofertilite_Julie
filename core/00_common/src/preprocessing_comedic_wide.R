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

#Load top 60 ATC level 2.
top_atc_level2 <- read_excel(file.path(Sys.getenv("PROJECT_PATH"),"core/42_transla_comedic/docs/top_atc2_snds_raw.xlsx")) #%>% filter(in_top_60  == "Yes")
#Load top 100 ATC level 5.
top_atc_level5 <- read_excel(file.path(Sys.getenv("PROJECT_PATH"),"core/42_transla_comedic/docs/top_atc5_snds_mapped.xlsx")) %>% filter(in_top_150  == "Yes")

#On fait un unique de la base de données : car les count sont à l'échelle des molécules (unique).
db <- unique(db)

#Step 1 : atc level 1, wide dataset
db_wide1 <- db %>% 
  #transform(atc_level1_7cl_cod = plyr::revalue(atc_level1_7cl_cod, c("Others" = "Others_level1"))) %>%
  group_by(base_cletri,atc_level1_cod) %>%  #On laisse tous les levels 1 ATC pour l'instant car on en a besoin pur db_very_wide
  summarise(bin = "Yes", count = length(unique(atc_cod))) %>%
  tidyr::pivot_wider(id_cols = c("base_cletri"),
              names_from = atc_level1_cod,
              values_from = c("bin","count"),
              names_glue = "{atc_level1_cod}_{.value}") %>%
  mutate_at(vars(ends_with("bin")), replace_na,replace ="No") %>%
  mutate_at(vars(ends_with("count")), replace_na,replace =0)

#Attention: i lfaut la colonne même si personne ne prend de ce medicament dans la base de données. 
atc1_missing <- setdiff(atc$atc_level_cod[atc$level==1],sub("_bin|_count","",colnames(db_wide1)))
for (cod in atc1_missing){
  db_wide1 <- db_wide1 %>% mutate(!!paste0(cod,"_bin") := "No")
  db_wide1 <- db_wide1 %>% mutate(!!paste0(cod,"_count") := 0)
}

#Step 2 : atc level 2 : db_wide
#We need to expand the data frame to get all possible values to deal with NA values
atc_to_expand <- atc %>% filter(level == 2) %>% select(atc_level2_cod = atc_level_cod) %>% mutate(atc_level1_cod = substr(atc_level2_cod,1,1))
db_expand <- db %>% select(base_cletri) %>% unique() %>% left_join(atc_to_expand, by = character()) 

db_wide2 <-db %>% 
  mutate(atc_level2_cod = replace_na(atc_level2_cod, replace ="UNKNOWN")) %>%
  mutate(bin = "Yes") %>%
  full_join(db_expand) %>%
  group_by(base_cletri, atc_level1_cod) %>%
  mutate(bin = case_when(
    bin == "Yes" & atc_level2_cod != "UNKNOWN" ~ "Yes",
    is.na(bin) & any(atc_level2_cod == "UNKNOWN") ~ NA_character_,
    TRUE ~  "No"
  )) %>%
  mutate(count = ifelse(all(atc_level2_cod != "UNKNOWN"), 1,NA)) %>%
  filter(atc_level2_cod != "UNKNOWN") %>% 
  mutate(atc_level2_top_cod = ifelse(atc_level2_cod %in% top_atc_level2$code_atc2,atc_level2_cod,"Others_level2")) %>%
  group_by(base_cletri,atc_level2_top_cod) %>% 
  summarise(bin =  case_when(
    any(bin == "Yes") ~ "Yes",
    any(is.na(bin)) ~NA_character_,
    TRUE ~ "No"
  ), 
  count = ifelse(bin == "No", 0, sum(count))) %>%
  pivot_wider(id_cols = c("base_cletri"),
              names_from = atc_level2_top_cod,
              values_from = c("bin","count"),
              names_glue = "{atc_level2_top_cod}_{.value}")

#Step 3 : create variable for very wide level 2
db_very_wide2 <- full_join(db_wide1,db_wide2)
for (atc2 in top_atc_level2$code_atc2){
  #print(atc2)
  atc1 = substr(atc2,1,1)
  db_very_wide2 <- db_very_wide2 %>% 
    mutate("{atc1}_except_{atc2}_bin" := ifelse(!!sym(paste0(atc1,"_count")) - !!sym(paste0(atc2,"_count")) > 0,"Yes","No"))
}
db_very_wide2 <- db_very_wide2 %>% select(base_cletri, contains("except"))

#Step 4 : level5
#We need to expand the data frame to get all possible values to deal with NA values
atc_to_expand <- atc %>% filter(level == 5) %>% select(atc_level5_cod = atc_level_cod)
atc_to_expand <- atc_to_expand %>% 
  mutate(atc_level1_cod = substr(atc_level5_cod,1,1),
         atc_level2_cod = substr(atc_level5_cod,1,3),
         atc_level3_cod = substr(atc_level5_cod,1,4),
         atc_level4_cod = substr(atc_level5_cod,1,5))
db_expand <- db %>% select(base_cletri) %>% unique() %>% left_join(atc_to_expand, by = character()) 

#Level 5
db_wide5 <-db %>% 
  mutate(atc_level5_cod = replace_na(atc_level5_cod, replace ="UNKNOWN")) %>%
  mutate(bin = "Yes") %>%
  full_join(db_expand) %>%
  group_by(base_cletri) %>%
  mutate(bin = case_when(
    bin == "Yes" & atc_level5_cod != "UNKNOWN" ~ "Yes",
    is.na(bin) & any(atc_level5_cod == "UNKNOWN") & grepl(paste0(paste0("^",atc_cod[atc_level5_cod == "UNKNOWN"]), collapse = "|") , atc_level5_cod) ~ NA_character_,
    TRUE ~  "No"
  )) %>%
  filter(atc_level5_cod != "UNKNOWN") %>% 
  mutate(atc_level5_top_cod = ifelse(atc_level5_cod %in% top_atc_level5$code_atc5,atc_level5_cod,"Others_level5")) %>%
  group_by(base_cletri,atc_level5_top_cod) %>% 
  summarise(bin =  case_when(
    any(bin == "Yes") ~ "Yes",
    any(is.na(bin)) ~NA_character_,
    TRUE ~ "No"
  )) %>%
  pivot_wider(id_cols = c("base_cletri"),
              names_from = atc_level5_top_cod,
              values_from = c("bin"),
              names_glue = "{atc_level5_top_cod}_{.value}")

#Step 3 : create variable for very wide level 2
db_very_wide5 <- full_join(db_wide1,db_wide5)
for (atc5 in top_atc_level5$code_atc5){
  #print(atc5)
  atc1 = substr(atc5,1,1)
  db_very_wide5 <- db_very_wide5 %>% 
    mutate("{atc1}_except_{atc5}_bin" := ifelse(!!sym(paste0(atc1,"_count")) - ifelse(!!sym(paste0(atc5,"_bin")) == "Yes" | is.na(!!sym(paste0(atc5,"_bin"))), 1, 0 ) > 0,"Yes","No"))
}
#Elise : we put NA into 1 in order to count as "Yes" if there is for sure another comedication.  
db_very_wide5 <- db_very_wide5 %>% select(base_cletri, contains("except"))

#Ici on modifie et on crée Others_level1
others_bin <- paste0(setdiff((atc %>% filter(level ==1))$atc_level_cod,c("A","B","C","H","M","N","R")), "_bin")
others_count <- paste0(setdiff((atc %>% filter(level ==1))$atc_level_cod,c("A","B","C","H","M","N","R")), "_count")
db_wide1 <- db_wide1 %>% 
  unite(Others_level1_bin, contains(others_bin)) %>%
  mutate(Others_level1_bin = ifelse(grepl("Yes",Others_level1_bin), "Yes","No")) %>%
  mutate(Others_level1_count = rowSums(across(contains(others_count)))) %>%
  select(-contains(others_count))

#On ordonne les colonnes  par ordre alphabetique (sauf other à la fin)
db_wide1 <- db_wide1 %>%
  select(sort(tidyselect::peek_vars())) %>%
  relocate(base_cletri) %>%
  relocate("Others_level1_bin", .after = last_col()) %>%
  relocate("Others_level1_count", .after = last_col())

#level2, ordre alphabetique On ordonne les colonnes  par ordre alphabetique (sauf other à la fin)
db_wide2 <- db_wide2 %>%
  select(sort(tidyselect::peek_vars())) %>%
  relocate(base_cletri) %>%
  relocate("Others_level2_bin", .after = last_col()) %>%
  relocate("Others_level2_count", .after = last_col())

#On ordonne les colonnes  par ordre alphabetique (sauf other à la fin)
db_wide5 <- db_wide5 %>%
  select(sort(tidyselect::peek_vars())) %>%
  relocate(base_cletri) %>%
  relocate("Others_level5_bin", .after = last_col())

#Add variable nb_comedic et comedic_bin
db_total <- db %>%
  group_by(base_cletri) %>% 
  summarise(comedic_bin = "Yes", comedic_count = length(unique(atc_cod)))

#Left join all data frames in good order
db_wide <- db_total %>%
  left_join(db_wide1) %>%
  left_join(db_wide2) %>%
  left_join(db_wide5)

#Finally : add patients with no comedic at all 
id_no_comed <- setdiff(id$base_cletri, db_wide$base_cletri)
db_wide_no_comed <- cbind(as.data.frame(id_no_comed), as.data.frame(matrix(NA, ncol = ncol(db_wide) -1, nrow = length(id_no_comed)))) 
colnames(db_wide_no_comed) <- colnames(db_wide)
db_wide_no_comed <- db_wide_no_comed %>%
  mutate_at(vars(ends_with("bin")), replace_na,replace ="No") %>%
  mutate_at(vars(ends_with("count")), replace_na,replace =0)

db_wide <-rbind(db_wide,db_wide_no_comed)

#Very wide
db_very_wide <- db_total %>%
  left_join(db_wide1) %>%
  left_join(db_wide2) %>%
  left_join(db_very_wide2) %>%
  left_join(db_wide5) %>%
  left_join(db_very_wide5)
db_very_wide_no_comed <- cbind(as.data.frame(id_no_comed), as.data.frame(matrix(NA, ncol = ncol(db_very_wide) -1, nrow = length(id_no_comed)))) 
colnames(db_very_wide_no_comed) <- colnames(db_very_wide)
db_very_wide_no_comed <- db_very_wide_no_comed %>%
  mutate_at(vars(ends_with("bin")), replace_na,replace ="No") %>%
  mutate_at(vars(ends_with("count")), replace_na,replace =0)
db_very_wide <-rbind(db_very_wide,db_very_wide_no_comed)



#Sunburst 

library(ggplot2)

# make some fake data
df <- data.frame(
  'level1'=c('a', 'a', 'a', 'a', 'b', 'b', 'c', 'c', 'c'), 
  'level2'=c('a1', 'a2', 'a3', 'a4', 'b1', 'b2', 'c1', 'c2', 'c3'), 
  'value'=c(.025, .05, .027, .005, .012, .014, .1, .03, .18))

# sunburst plot
ggplot(df, aes(y=value)) +
  geom_bar(aes(fill=level1, x=0), width=.5, stat='identity') + 
  geom_bar(aes(fill=level2, x=.25), width=.25, stat='identity') + 
  coord_polar(theta='y')
