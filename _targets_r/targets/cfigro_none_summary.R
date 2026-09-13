tar_target(cfigro_none_summary, {
  fvs_read_output(cfigro_none, "FVS_Summary2") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
