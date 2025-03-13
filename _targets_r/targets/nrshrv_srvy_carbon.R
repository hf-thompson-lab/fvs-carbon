tar_target(nrshrv_srvy_carbon, {
  fvs_read_output(nrshrv_srvy, "FVS_Carbon") |>
    select(StandID, Year, Aboveground_Total_Live) |>
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
