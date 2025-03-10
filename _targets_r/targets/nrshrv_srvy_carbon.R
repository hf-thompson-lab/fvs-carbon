tar_target(nrshrv_srvy_carbon, {
  fvs_read_output(nrs_harvest_srvy, "FVS_Carbon") |>
    select(StandID, Year, Aboveground_Total_Live)
})
