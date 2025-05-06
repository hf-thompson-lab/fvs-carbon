tar_target(
  nrsgro_ca10,
  {
    # fvs_run wants a table of the form:
    # STAND_ID - arbitrary identifier for a stand
    # STAND_CN - CN for the plot
    # FIRST_YEAR - start of the projection; if this doesn't align with the actual
    #   year (MEASYEAR) of first survey, the stand will be projected from the
    #   survey year to the start of the projection.
    # LAST_YEAR - end of the projection
    timestep <- 10 # years; determined by FVSne variant
    
    plots_for_fvs <- nrsgro_plot |>
      group_by(STATECD, COUNTYCD, PLOT) |>
      arrange(INVYR) |>
      mutate(
        STAND_CN = if_else(row_number() == 1, CN, NA)
      ) |>
      summarize(
        STAND_CN = min(STAND_CN, na.rm = TRUE), # only one will not be NA
        FIRST_YEAR = min(MEASYEAR, na.rm = TRUE),
        LAST_YEAR = max(MEASYEAR, na.rm = TRUE),
        .groups = "keep"
      ) |>
      ungroup() |>
      filter_add_stand_id()
    

    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "NRSGrowOnly"
    mgmt_id <- "CA10"
    
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
      calibration = nrsgro_ca10_calibration,
      calib_mort = nrsgro_ca10_mortality,
      calib_years = 10,
      regen = nrsgro_ca10_regen,
      num_partitions = fvs_num_partitions,
      partition = fvs_partition,
      random_seed = fvs_randseed
    )
  },
  # iteration = "vector" branches execution for each partition value (see pattern, below)
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here.
  # cross() ensures that every combination of values for its arguments is processed
  # map() distributes each value of its argument to a separate sub-target (branch)
  # so cross(randseed, map(partition)) will run each partition in a separate branch,
  # and each branch will run with each value of randseed
  pattern = cross(fvs_randseed, map(fvs_partition))
)
