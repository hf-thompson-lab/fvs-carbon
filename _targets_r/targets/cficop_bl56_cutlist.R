tar_target(cficop_bl56_cutlist, {
  cficop_bl56_cutlist <- fvs_read_output(cficop_bl56, "FVS_CutList") |>
    filter(random_seed == min(random_seed))
})
