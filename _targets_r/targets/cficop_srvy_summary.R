tar_target(cficop_srvy_summary, {
  fvs_read_output(cficop_srvy, "FVS_Summary2_East") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
