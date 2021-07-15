#Load preprocessed dataset
load(file.path(
  Sys.getenv("PROJECT_PATH"),
  "core",
  "05_feeric",
  "data",
  sprintf("%s_preprocessed.RData", "05_feeric")))

dim(database_preprocessed)

#Remove patients with incoherent dates
solution_incoherence <- read_excel("/Users/elisedumas/Code/databases/core/05_feeric/data/incoherences_dates_solution.xlsx",col_types="text")
remove_from_sunburst <- solution_incoherence$cletri[solution_incoherence$Solution =="remove pour sunburst"]
database_preprocessed <- database_preprocessed %>% filter(! cletri %in% remove_from_sunburst)

#NA for adj herceptin when error
na_herceptin <- solution_incoherence$cletri[solution_incoherence$Solution == "remove_adj_herceptin"]
database_preprocessed$dat_first_adj_antiher2[database_preprocessed$cletri %in% na_herceptin] <- NA

#NA for adj CT when error
na_ct <- solution_incoherence$cletri[solution_incoherence$Solution == "remove_adj_ct"]
database_preprocessed$dat_first_adj_ct[database_preprocessed$cletri %in% na_ct] <- NA

#NA for neo RT when error
na_neo_rt <- solution_incoherence$cletri[solution_incoherence$Solution == "remove_neo_rt"]
database_preprocessed$dat_first_neo_rt[database_preprocessed$cletri %in% na_neo_rt] <- NA

#Save dataset for sunburst
save(
  database_preprocessed,
  file=file.path(output_folder, "base_preprocessed_remove_sunburst.RData")
)
