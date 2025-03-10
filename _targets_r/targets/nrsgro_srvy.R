tar_target(
  nrsgro_srvy,
  {
    plots_for_fvs <- nrsgro_plot |>
      rename(
        STAND_CN = CN,
        FIRST_YEAR = MEASYEAR
      ) |>
      mutate(
        LAST_YEAR = FIRST_YEAR,
        # STAND_ID matches STAND_ID for nrsgro GROW, but this isn't
        # actually necessary. We'll line things up later using CN.
        STAND_ID = sprintf("%04d%03d%05d", STATECD, COUNTYCD, PLOT)
      )
    
    # No establishment for survey runs
    
    # We're running 0-year timesteps, but FVS requires us to run at least one
    # year, so we run 0-year timesteps for 1 year.
    timestep <- 1
    
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "NRSGrowOnly"
    mgmt_id <- "SRVY"

    # We communicate with FVS through files. FVSOnline shows a model in which
    # a "project" (the inputs and outputs of a single FVS run) live in a
    # single directory; we follow that model.
    project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
    }
    
    fvs_run(
      fvsbin_dir = fvsbin_dir,
      fvs_variant = fvs_variant,
      project_dir = project_dir,
      fiadb = fiadb,
      title = title,
      mgmt_id = mgmt_id,
      stands = plots_for_fvs,
      num_partitions = fvs_num_partitions,
      partition = fvs_partition
    )
  },
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here:
  pattern = map(fvs_partition)
)

