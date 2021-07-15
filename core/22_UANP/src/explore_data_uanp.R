library(dplyr)

load("/Users/ahamypet/RT2Lab/databases/core/22_UANP/data/raw/uamp_f.RData")
uamp_f %>% as.data.frame() -> uanp
head(uamp_f)
uanp %>% group_by(Motif.TEXTE.LIBRE) %>% count()
head(uanp)

uanp$Accueil...Origine
uanp %>% filter(consult_sur_place == "yes") %>% 
  # select(DATE,year,IPP,text_new,hospi_simpl) %>% 
  View()

uanp_short_file_passages_2012_sept_2020 <- uanp %>% filter(consult_sur_place == "yes") %>% 
                                           select(DATE,year,IPP,text_new,hospi_simpl) 

save(uanp_short_file_passages_2012_sept_2020,file = "/Users/ahamypet/RT2Lab/databases/core/22_UANP/data/raw/uanp_short_file_passages_2012_sept_2020.RData")

uanp %>% filter(consult_sur_place == "yes") %>% group_by(year,mois) %>% count() %>% as.data.frame()

