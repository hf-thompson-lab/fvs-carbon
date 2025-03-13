tar_target(nrshrv_prsc_summary, {
  fvs_read_output(nrshrv_prsc, "FVS_Summary2_East") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup() |>
    left_join(
      nrshrv_plot_stats |>
        filter(!is.na(HRVYR)) |>
        distinct(STAND_ID, HRVYR) |>
        rename(StandID = STAND_ID, HarvestYear = HRVYR),
      by = join_by(StandID)
    ) |>
    mutate(
      YearsSinceHarvest = Year - HarvestYear
    )
})
