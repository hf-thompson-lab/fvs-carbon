tar_target(nrshrv_srvy_summary, {
  fvs_read_output(nrshrv_srvy, "FVS_Summary2_East") |>
    select(StandID, Year, BA, Tpa) |>
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
