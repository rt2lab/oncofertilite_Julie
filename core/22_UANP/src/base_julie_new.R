self@database$dat_first_biopsy
self@database$dat_first_breast_surg
self@database$dat_first_axillar_surg

self@database$
load("/Users/ahamypet/RT2Lab/databases/core/07_oncofertilite_consore/data/07_oncofertilite_consore_preprocessed_labels.RData")
head(database_preprocessed_labels)
typ_chir_missing_oncofertilite <- database_preprocessed_labels %>% select(numdos_curie,dat_first_cancer_surg) %>% mutate(tumorectomy_or_mastectomy = "")
save(typ_chir_missing_oncofertilite, file = "/Users/ahamypet/RT2Lab/oncofertilite/discuss_or_not_discuss/data/typ_chir_missing_oncofertilite.RData")

database_preprocessed_labels %>% select(neo_ct,adj_ct,contains("delay")) %>% head()
database_preprocessed_labels$dat_first_neo_ct
database_preprocessed_labels$dat_first_ct

# delay_diag_to_surg_day delay_diag_to_surg_month delay_diag_to_neo_ct delay_surg_to_adj_ct delay_rfs_diag delay_drfs_diag delay_dss_diag delay_os_diag delay_rfs
# 1                    -14                     -0.5                   NA                   NA           86.3            86.3           86.3          86.3      86.8
# 2                     15                      0.5                   NA                   NA          101.7           101.7          101.7         101.7     101.2

database_preprocessed_labels$dat_first_biopsy
database_preprocessed_labels$dat_bc_diagnosis
database_preprocessed_labels$dat_first_surg

database_preprocessed_labels$dat_first_neo_ct
database_preprocessed_labels$dat_first_ct
database_preprocessed_labels$dat_first_ct

database_preprocessed_labels$delay_diag_to_first_ttt

class(database_preprocessed_labels$dat_first_biopsy)
class(database_preprocessed_labels$dat_bc_diagnosis)
class(database_preprocessed_labels$dat_first_surg)

database_preprocessed_labels$