tar_target(
  cficop_none,
  {
    timestep <- 10 # years; determined by FVSne variant
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "CFICarbonOffsetProgram"
    mgmt_id <- "NONE"
    project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
    }
    
    fvs_run(
      fvsbin_dir = fvsbin_dir,
      fvs_variant = fvs_variant,
      project_dir = project_dir,
      fiadb = NULL,
      title = title,
      mgmt_id = mgmt_id,
      stands = cfigro_plot |>
        # Filter out the 6 plots that do not get a sibling
        semi_join(
          cficop_plot_sibling,
          by = join_by(STAND_ID == MasterPlotID)
        )
#        left_join(
#          cficop_sam_wt |> select(STAND_ID = MasterPlotID, SAM_WT = AcresPerPlot),
#          by = join_by(STAND_ID)
#        ),
      trees = cfigro_trees,
      regen = cficop_estab,
#      calibration = cfigro_none_growth,
#      calib_mort = cfigro_none_death,
#      calib_years = 10,
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
