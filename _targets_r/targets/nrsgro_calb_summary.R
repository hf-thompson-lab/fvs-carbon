tar_target(nrsgro_calb_summary, {
  fvs_read_output(nrsgro_calb, "FVS_Summary2_East") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
