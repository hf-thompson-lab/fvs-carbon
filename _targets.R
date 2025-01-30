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

  # 01_IdentifyingStands.Rmd
  tar_target(nk_table1, nk_read_table1(nk_table1_csv)),
  tar_target(nk_table1_expanded, nk_extract_cols_from_plot_code(nk_table1)),
  tar_target(nk_all_plot, nk_load_plot_all_invyr(nk_table1_expanded, fiadb)),
  tar_target(nk_matching_plot, nk_match_plots(nk_table1_expanded, nk_all_plot)),
  tar_target(nk_to_fia, nk_transalte_to_fia(nk_matching_plot)),
  tar_target(nk_to_fvs, nk_translate_to_fvs(nk_to_fia, fiadb)),
  # tar_render(identifying_stands, "01_IdentifyingStands.Rmd")

  # 02_NKProjectedCarbon.Rmd
  tar_target(nk_fig2_dir, "data/raw/nk_fig2/", format = "file"),
  tar_target(nk_fig2, nk_read_fig2(nk_fig2_dir))
)
