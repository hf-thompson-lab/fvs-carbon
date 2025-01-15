library(targets)
library(tarchetypes)
tar_source()
tar_option_set(
  packages = c(
    "tidyverse",
    "RSQLite",
    "measurements",
    "reshape2",
    "maps",
    "pbapply"
  )
)
list(
  tar_target(fiadb, "data/raw/SQLite_FIADB_ENTIRE.db", format = "file"),
  tar_target(nk_table1_csv, "data/raw/NK_Table_1.csv", format = "file"),

  tar_target(nk_table1, read_nk_table1(nk_table1_csv)),
  tar_target(nk_table1_expanded, compute_nk_table1_expanded(nk_table1)),
  tar_target(nk_matching_cond, load_nk_matching_cond(fiadb, nk_table1_expanded)),
  tar_target(nk_all_cond, compute_nk_all_cond(nk_table1_expanded, nk_matching_cond)),
  tar_target(nk_translated_stands, nk_transalte_stand_ids(nk_all_cond)),
  tar_target(nk_to_fvs, compute_nk_to_fvs(fiadb, nk_translated_stands))
)
