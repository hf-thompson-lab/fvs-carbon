# format = "file" because it produces files outside targets' control
tar_target(
  nk_byplot_none,
  {
    stand_type <- "plot"
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- paste0("NKBy", stringr::str_to_title(stand_type))
    mgmt_id <- "NONE"
  
    # We communicate with FVS through files. FVSOnline shows a model in which
    # a "project" (the inputs and outputs of a single FVS run) live in a
    # single directory; we follow that model.
    project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
    }

    nk_background_regen <- nk_extract_regen(nk_regen, "Background")

    nk_grow_only_stands <- nk_plot_crosswalk |>
      rename(
        STAND_ID = `FIA plot code`,
        FIRST_YEAR = MEASYEAR
      ) |>
      mutate(LAST_YEAR = 2165) |>
      select(STAND_ID, STAND_CN, FIRST_YEAR, LAST_YEAR)

    fvs_run(
      fvsbin_dir,
      fvs_variant,
      project_dir,
      fiadb,
      stand_type,
      title,
      mgmt_id,
      nk_grow_only_stands,
      nk_background_regen
    )
  },
  format = "file"
)
