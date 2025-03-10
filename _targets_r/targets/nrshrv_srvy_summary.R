tar_target(nrshrv_srvy_summary, {
  fvs_read_output(nrs_harvest_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa)
})
