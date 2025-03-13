tar_target(nrshrv_prsc_carbon, {
  fvs_read_output(nrshrv_prsc, "FVS_Carbon") |>
    group_by(StandID, Year) |> # Combine results from different random seeds
    summarize(
      Aboveground_Total_Live = mean(Aboveground_Total_Live),
      .groups = "keep"
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
