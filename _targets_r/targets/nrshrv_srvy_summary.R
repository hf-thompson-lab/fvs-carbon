tar_target(nrshrv_srvy_summary, {
  fvs_read_output(nrshrv_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa)
})
