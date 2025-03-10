tar_target(nrsgrowonly_srvy_summary, {
  fvs_read_output(nrs_growonly_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa)
})
