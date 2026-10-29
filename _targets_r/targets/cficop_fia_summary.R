tar_target(cficop_fia_summary, {
  cficop_fia_summary <- fvs_read_output(cficop_fia, "FVS_Summary2") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
