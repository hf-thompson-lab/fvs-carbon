tar_target(
  nrshrv_srvy,
  {
    # fvs_run wants a table of the form:
    # STAND_ID - arbitrary identifier for a stand
    # STAND_CN - CN for the plot
    # FIRST_YEAR - start of the projection; if this doesn't align with the actual
    #   year (MEASYEAR) of first survey, the stand will be projected from the
    #   survey year to the start of the projection.
    # LAST_YEAR - end of the projection
    timestep <- 10 # years; determined by FVSne variant
    
    plots_for_fvs <- nrshrv_plot |>
      left_join(
        nrshrv_plot_stats |> distinct(STATECD, COUNTYCD, PLOT, STAND_ID),
        by = join_by(STATECD, COUNTYCD, PLOT)
      ) |>
      rename(
        STAND_CN = CN,
        FIRST_YEAR = MEASYEAR
      ) |>
      mutate(
        LAST_YEAR = FIRST_YEAR
      )
    
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "NRSHarvest"
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
      regen = NULL, # No regen for survey
      harvest = NULL, # No harvest for survey
      num_partitions = fvs_num_partitions,
      partition = fvs_partition
    )
  },
  # iteration = "vector" branches execution for each partition value (see below)
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here.
  # cross() ensures that every combination of values for its arguments is processed
  # map() distributes each value of its argument to a separate sub-target (branch)
  # so cross(randseed, map(partition)) will run each partition in a separate branch,
  # and each branch will run with each value of randseed
  pattern = map(fvs_partition)
)
