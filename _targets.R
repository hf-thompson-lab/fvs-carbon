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
  tar_target(nk_table_1_csv, "data/raw/NK_Table_1.csv", format = "file"),

  # Species Crosswalk
  tar_target(fvsne_table_3_2_1_csv, "data/raw/FVSne_Overview_Table_3.2.1.csv", format = "file"),
  tar_target(fvsne_table_3_2_1, fvsne_read_table_3_2_1(fvsne_table_3_2_1_csv)),
  tar_target(fia_table_11_5_17_csv, "data/raw/FIADB_11.5.17_JENKINS_SPGRPCD.csv", format = "file"),
  tar_target(fia_table_11_5_17, fia_read_table_11_5_17(fia_table_11_5_17_csv)),
  tar_target(
    species_crosswalk,
    generate_species_crosswalk(fiadb, fvsne_table_3_2_1, fia_table_11_5_17)
  ),

  # 01_IdentifyingStands.Rmd
  tar_target(nk_table_1, nk_read_table_1(nk_table_1_csv)),
  tar_target(nk_table_1_expanded, nk_extract_cols_from_plot_code(nk_table_1)),
  tar_target(nk_all_plot, nk_load_plot_all_invyr(nk_table_1_expanded, fiadb)),
  tar_target(nk_matching_plot, nk_match_plots(nk_table_1_expanded, nk_all_plot)),
  tar_target(nk_to_fia, nk_transalte_to_fia(fiadb, nk_matching_plot)),
  tar_target(nk_to_fvs, nk_translate_to_fvs(nk_to_fia, fiadb)),
  tar_render(identifying_stands, "01_IdentifyingStands.Rmd", output_dir = "rendered/"),

  # 02_NKProjectedCarbon.Rmd
  tar_target(nk_fig_2_dir, "data/raw/nk_fig2/", format = "file"),
  tar_target(nk_fig_2, nk_read_fig_2(nk_fig_2_dir)),
  tar_render(nk_projected_carbon, "02_NKProjectedCarbon.Rmd", output_dir = "rendered/"),
  
  # 03_NKNoManagement.Rmd
  tar_target(nk_table_4_csv, "data/raw/NK_Table_4.csv", format = "file"),
  tar_target(nk_table_4, nk_read_table_4(nk_table_4_csv)),
  tar_target(nk_regen, nk_generate_regen(nk_table_4, species_crosswalk))
)
