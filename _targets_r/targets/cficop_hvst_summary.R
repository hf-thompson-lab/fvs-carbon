tar_target(cficop_hvst_summary, {
  fvs_read_output(cficop_hvst, "FVS_Summary2") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
