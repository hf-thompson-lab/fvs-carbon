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
  tar_file_read(
    nk_table_1,
    "data/raw/NK_Table_1.csv",
    read_csv(!!.x, col_types = cols(`FIA plot code` = col_character()))
  ),

  # Species Crosswalk
  tar_file_read(
    fvsne_table_3_2_1,
    "data/raw/FVSne_Overview_Table_3.2.1.csv",
    read_csv(!!.x, col_types = "iiciccc")
  ),
  tar_file_read(
    fia_table_11_5_17,
    "data/raw/FIADB_11.5.17_JENKINS_SPGRPCD.csv",
    read_csv(!!.x, col_types = cols(JENKINS_SPGRPCD = col_integer()))
  ),
  tar_target(
    species_crosswalk,
    generate_species_crosswalk(fiadb, fvsne_table_3_2_1, fia_table_11_5_17)
  ),

  # 01_IdentifyingStands.Rmd
  tar_target(nk_table_1_expanded, nk_extract_cols_from_plot_code(nk_table_1)),
  tar_target(nk_all_plot, nk_load_plot_all_invyr(nk_table_1_expanded, fiadb)),
  tar_target(nk_matching_plot, nk_match_plots(nk_table_1_expanded, nk_all_plot)),
  tar_target(nk_to_fia, nk_transalte_to_fia(fiadb, nk_matching_plot)),
  tar_target(nk_to_fvs, nk_translate_to_fvs(nk_to_fia, fiadb)),
  tar_render(nk_identifying_stands, "01_IdentifyingStands.Rmd", output_dir = "rendered/"),

  # 02_NKProjectedCarbon.Rmd
  tar_file_read(
    nk_fig_2,
    "data/raw/nk_fig2/",
    nk_read_fig_2(!!.x)
  ),
  tar_render(nk_projected_carbon, "02_NKProjectedCarbon.Rmd", output_dir = "rendered/"),
  
  # 03_NKNoManagement.Rmd
  tar_file_read(
    nk_table_4,
    "data/raw/NK_Table_4.csv",
    read_csv(!!.x, col_types = cols(`Management scenario` = col_character(), .default = col_number()))
  ),
  tar_target(nk_regen, nk_generate_regen(nk_table_4, species_crosswalk)),
  ## Run FVS - note that format = "file" because it produces files outside targets' control
  tar_target(nk_grow_plot, nk_project_grow(fiadb, "plot", nk_to_fia, nk_regen), format = "file"),
  tar_target(nk_grow_plot_carbon, fvs_read_output(nk_grow_plot[3], "FVS_Carbon")),
  tar_target(nk_grow_plot_summary, fvs_read_output(nk_grow_plot[3], "FVS_Summary2_East")),
  # By Subplot
  # TODO: figure out why re-running by subplot and condition doesn't work
  tar_file_read(
    nk_grow_subplot_carbon,
    "data/fvs/FVS_NKBySubplot_NONE_Carbon.csv",
    read_csv(!!.x, col_types = cols(StandID = col_character()))
  ),
  # By Condition
  # TODO: figure out why re-running this doesn't work
  tar_file_read(
    nk_grow_cond_carbon,
    "data/fvs/FVS_NKByCondition_NONE_Carbon.csv",
    read_csv(!!.x, col_types = cols(StandID = col_character()))
  )
  #tar_render(nk_no_management, "03_NKNoManagement.Rmd", output_dir = "rendered/")

  # Two things:
  # x. Run FVS at all.
  # x.a. factor out fvs_keywords(...)
  #      it wants stands, title, mgmtid, regen, etc.
  #      probably many of these are tables, e.g. regen is <standid, year, species, density>
  #      time can be <standid, cycle, cycle length>, where we also need <standid, startyear>
  # x.b. factor out fvs input data gathering
  # x.c. Actually run FVS. At least on windows, run it directly; elsewhere, maybe
  #      prompt the user till done, or poll for output data.
  # x.c.1. targets wants tar_rep() to return a slice of a data frame; probably this is
  #        FVS_Summary2_East
  # x.c.2. processx::process$new() is probably the method to use to run FVSne
  # x.c.3. unclear how to have a targets function that produces multiple output -
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
