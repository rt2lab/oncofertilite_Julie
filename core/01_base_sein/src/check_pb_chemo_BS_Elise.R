load("/Users/ahamypet/RT2Lab/databases/core/02_neorep/data/neorep_before_mapping.RData")
head(neorep_before_mapping)
neorep_before_mapping_tmp <-  neorep_before_mapping %>% 
                            rename(numdos_curie = numdos7,
                                   neo_ct_regimen_ASHP = neo_ct_regimen ) %>%
  select(numdos_curie, neo_ct_regimen_ASHP) 
head(neorep_before_mapping_tmp)
str(neorep_before_mapping_tmp)
table(neorep_before_mapping_tmp$neo_ct_regimen_ASHP, exclude=NULL)

# Import data with PB elise
load("/Users/ahamypet/RT2Lab/databases/core/01_base_sein/data/neo_regimen_to_check.RData")
str(neo_regimen_to_check)
neo_regimen_to_check$neo_ct_regimen <- as.character(neo_regimen_to_check$neo_ct_regimen) 
nac_check <- neo_regimen_to_check %>% rename(neo_ct_regimen_elise = neo_ct_regimen) %>%  
  left_join(.,neorep_before_mapping_tmp ) %>% filter(!is.na(neo_ct_regimen_ASHP) )

head(nac_check)
nac_check %>% group_by(neo_ct_regimen_elise,neo_ct_regimen_ASHP)%>% count()
# neo_ct_regimen_elise neo_ct_regimen_ASHP     n
# <chr>                              <dbl> <int>
# 1 1                                      1   416
# 2 1                                      2     1
# 3 1                                      3     1
# 4 1                                      4    12
# 5 2                                      1     1
# 6 2                                      2     6
# 7 3                                      1     3
# 8 3                                      3    17
# 9 3                                      4     5
