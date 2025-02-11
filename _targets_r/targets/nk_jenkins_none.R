# format = "file" because it produces files outside targets' control
tar_target(
  nk_jenkins_none,
  {
    fvsbin_dir <- "/fvs/fvsbin" # TODO: put these in a config file
    fvs_variant <- "fvsne"      # TODO: put these in a config file
    data_dir <- "data/fvs"
    title <- "NKJenkins"
    mgmt_id <- "NONE"

    # We communicate with FVS through files. FVSOnline shows a model in which
    # a "project" (the inputs and outputs of a single FVS run) live in a
    # single directory; we follow that model.
    project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
    if (!dir.exists(project_dir)) {
      dir.create(project_dir)
    }

    nk_background_regen <- nk_extract_regen(nk_regen, "Background")

    fvs_run(
      fvsbin_dir,
      fvs_variant,
      project_dir,
      fiadb,
      title,
      mgmt_id,
      nk_plots_grown,
      regen = nk_background_regen,
      carb_calc = "Jenkins"
    )
  },
  format = "file"
)
