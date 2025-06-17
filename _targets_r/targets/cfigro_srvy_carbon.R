tar_target(cfigro_srvy_carbon, {
  fvs_read_output(cfigro_srvy, "FVS_Carbon") |>
    select(StandID, Year, Aboveground_Total_Live)
})
