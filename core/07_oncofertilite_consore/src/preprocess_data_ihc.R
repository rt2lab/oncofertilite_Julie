
  
library(readxl)
resultats_ER_PR_HER_export_data <- read_excel("databases/core/07_oncofertilite_consore/data/raw/resultats_ER_PR_HER_export_data.xlsx")
head(resultats_ER_PR_HER_export_data)
resultats_ER_PR_HER_export_data$numdos_curie		<- formatC(as.integer(resultats_ER_PR_HER_export_data$patient_ipp),width = 7, flag = "0")

resultats_ER_PR_HER_export_data %>% mutate(er_status = case_when(ro == "-" ~ 0,
                                                                 ro == "+" ~ 1),
                                           pr_status = case_when(rp == "-" ~ 0,
                                                                 rp == "+" ~ 1),
                                          her2_status = case_when(her2 == "-" ~ 0,
                                                                  her2 == "+" ~ 1)) %>% select(numdos_curie, er_status, pr_status, her2_status) -> oncofert_ER_PR_HER2

save(oncofert_ER_PR_HER2, file = "databases/core/07_oncofertilite_consore/data/raw/oncofert_ER_PR_HER2.RData")
nrow(oncofert_ER_PR_HER2)

