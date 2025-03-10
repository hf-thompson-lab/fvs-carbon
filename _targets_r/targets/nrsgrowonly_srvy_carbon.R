tar_target(nrsgrowonly_srvy_carbon, {
  fvs_read_output(nrs_growonly_srvy, "FVS_Carbon") |>
    select(StandID, Year, Aboveground_Total_Live)
})
