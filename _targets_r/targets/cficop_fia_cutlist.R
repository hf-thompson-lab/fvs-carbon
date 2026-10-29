tar_target(cficop_fia_cutlist, {
  cficop_fia_cutlist <- fvs_read_output(cficop_fia, "FVS_CutList") |>
    filter(random_seed == min(random_seed))
})
