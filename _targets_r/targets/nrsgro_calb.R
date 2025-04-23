tar_target(
  nrsgro_calb,
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
      mutate(
        # STAND_ID won't match FVS_PLOTINIT_PLOT.STAND_ID; that's OK
        STAND_ID = sprintf("%04d%03d%05d", STATECD, COUNTYCD, PLOT)
      )
    
    # fvs_run wants establishment in the form:
    # STAND_CN
    # SPECIES (FVS_SPCD)
    # DENSITY (TPA)
    # HEIGHT (FT)
    estab_for_fvs <- nrsgro_estab_rate |>
      mutate(
        STAND_ID = sprintf("%04d%03d%05d", STATECD, COUNTYCD, PLOT)
      ) |>
      left_join(
        plots_for_fvs |> select(STAND_ID, STAND_CN),
        by = join_by(STAND_ID)
      ) |>
      select(STAND_CN, STATECD, COUNTYCD, PLOT, SPCD, RATE_PER_ACRE) |>
      mutate(RATE_PER_ACRE = floor(RATE_PER_ACRE * timestep)) |>
      rename(DENSITY = RATE_PER_ACRE) |>
      left_join(
        nrsgro_estab_height |>
          select(STATECD, COUNTYCD, PLOT, SPCD, HT),
        by = join_by(STATECD, COUNTYCD, PLOT, SPCD)
      ) |>
      mutate(HT = floor(HT)) |>
      rename(HEIGHT = HT) |>
      left_join(
        species_crosswalk |>
          select(SPCD, FVS_SPCD),
        by = join_by(SPCD)
      ) |>
      rename(SPECIES = FVS_SPCD) |>
      filter(!is.na(SPECIES) & !is.na(DENSITY) & !is.na(HEIGHT)) |>
      select(STAND_CN, SPECIES, DENSITY, HEIGHT)
      
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "NRSGrowOnly"
    mgmt_id <- "CALB"
    
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
      calibration = nrsgro_calb_calibration,
      calib_mort = nrsgro_calb_mortality,
      regen = estab_for_fvs,
      num_partitions = fvs_num_partitions,
      partition = fvs_partition,
      random_seed = fvs_randseed
    )
  },
  # iteration = "vector" branches execution for each partition value (see below)
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here.
  # cross() ensures that every combination of values for its arguments is processed
  # map() distributes each value of its argument to a separate sub-target (branch)
  # so cross(randseed, map(partition)) will run each partition in a separate branch,
  # and each branch will run with each value of randseed
  pattern = cross(fvs_randseed, map(fvs_partition))
)
