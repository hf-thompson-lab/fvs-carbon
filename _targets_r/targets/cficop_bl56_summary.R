tar_target(cficop_bl56_summary, {
  cficop_bl56_summary <- fvs_read_output(cficop_bl56, "FVS_Summary2") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
