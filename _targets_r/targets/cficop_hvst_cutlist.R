tar_target(cficop_hvst_cutlist, {
  fvs_read_output(cficop_hvst, "FVS_CutList") |>
    filter(random_seed == min(random_seed))
})
