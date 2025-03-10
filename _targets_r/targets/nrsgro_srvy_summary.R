tar_target(nrsgro_srvy_summary, {
  fvs_read_output(nrsgro_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa)
})
