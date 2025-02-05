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
  # TODO nik: do we want tar_download() to manage this database?
  #           potentially interesting WIP: sqltargets https://github.com/daranzolin/sqltargets
  tar_target(fiadb, "data/raw/SQLite_FIADB_ENTIRE.db", format = "file"),
  # TODO nik: all the file / read pairs can be replaced with tarchetypes::tar_file_read
  # see https://docs.ropensci.org/tarchetypes/reference/tar_file_read.html
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
  #tar_render(nk_identifying_stands, "01_IdentifyingStands.Rmd", output_dir = "rendered/"),

  # 02_NKProjectedCarbon.Rmd
  tar_target(nk_fig_2_dir, "data/raw/nk_fig2/", format = "file"),
  tar_target(nk_fig_2, nk_read_fig_2(nk_fig_2_dir)),
  #tar_render(nk_projected_carbon, "02_NKProjectedCarbon.Rmd", output_dir = "rendered/"),
  
  # 03_NKNoManagement.Rmd
  tar_target(nk_table_4_csv, "data/raw/NK_Table_4.csv", format = "file"),
  tar_target(nk_table_4, nk_read_table_4(nk_table_4_csv)),
  tar_target(nk_regen, nk_generate_regen(nk_table_4, species_crosswalk))
  # Two things:
  # 1. Run FVS at all.
  # 1.a. factor out fvs_keywords(...)
  #      it wants stands, title, mgmtid, regen, etc.
  #      probably many of these are tables, e.g. regen is <standid, year, species, density>
  #      time can be <standid, cycle, cycle length>, where we also need <standid, startyear>
  # 1.b. factor out fvs input data gathering
  # 1.c. Actually run FVS. At least on windows, run it directly; elsewhere, maybe
  #      prompt the user till done, or poll for output data.
  # 1.c.1. targets wants tar_rep() to return a slice of a data frame; probably this is
  #        FVS_Summary2_East
  # 1.c.2. processx::process$new() is probably the method to use to run FVSne
  # 1.c.3. unclear how to have a targets function that produces multiple output -
  #        maybe a "file" target that is a folder that contains the output?
  # 1.d. factor out fvs error parsing
  # 1.e. factor out fvs result gathering
  # 1.e.1. lapply(dbListTables(part), \(table) {dbWriteTable(dest, table, tbl(part, table) |> collect(), append = TRUE)
  # 2. Run FVS in parallel
  # 2.a. it's probably important to have workers manage their own data storage:
  #      https://books.ropensci.org/targets/performance.html#worker-storage
  #      and/or to be deliberate about which targets run in the controller node:
  #      https://books.ropensci.org/targets/performance.html#local-targets
)
