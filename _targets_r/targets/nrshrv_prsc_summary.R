tar_target(nrshrv_prsc_summary, {
  fvs_read_output(nrs_harvest_prsc, "FVS_Summary2_East") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
