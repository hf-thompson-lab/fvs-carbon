tar_target(cfigro_srvy_summary, {
  fvs_read_output(cfigro_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa)
})
