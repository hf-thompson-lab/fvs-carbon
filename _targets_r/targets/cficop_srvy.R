tar_target(
  cficop_srvy,
  {
    visits <- cfiabp_trees |>
      distinct(MasterPlotID, VisitCycle) |>
      left_join(
        tblDWSPCFIPlotVisitsComplete |>
          select(MasterPlotID, VisitCycle, MasterPlotVisitID, VisitYear),
        by = join_by(MasterPlotID, VisitCycle)
      ) |>
      mutate(MasterPlotVisitID = as.character(MasterPlotVisitID)) |>
      select(
        STAND_CN = MasterPlotVisitID,
        STAND_ID = MasterPlotID,
        INV_YEAR = VisitYear
      )
    
    # Despite its name, cfigro_plot has all plots in it
    plots_for_fvs <- cfigro_plot |>
      select(-STAND_CN, -INV_YEAR) |>
      full_join(visits, by = join_by(STAND_ID)) |>
      mutate(FIRST_YEAR = INV_YEAR, LAST_YEAR = INV_YEAR)

    # No establishment for survey runs

    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "CFICarbonOffsetProgram"
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
      fiadb = NULL,
      title = title,
      mgmt_id = mgmt_id,
      stands = plots_for_fvs,
      trees = cfigro_trees,
      num_partitions = fvs_num_partitions,
      partition = fvs_partition
    )
  },
  iteration = "vector",
  # cross() and map() are unparsed targets:: functions here:
  pattern = map(fvs_partition)
)
