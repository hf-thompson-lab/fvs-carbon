tar_target(nrsgro_srvy_carbon, {
  fvs_read_output(nrsgro_srvy, "FVS_Carbon") |>
    select(StandID, Year, Aboveground_Total_Live)
})
