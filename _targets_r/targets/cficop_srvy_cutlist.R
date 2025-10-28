tar_target(cficop_srvy_cutlist, {
  fvs_read_output(cficop_srvy, "FVS_CutList_East") |>
    group_by(StandID, Year) |>
    filter(row_number() == 1) |>
    ungroup()
})
