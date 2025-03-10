tar_target(nrsgrowonly_none_summary, {
  fvs_read_output(nrs_growonly_none, "FVS_Summary2_East") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
