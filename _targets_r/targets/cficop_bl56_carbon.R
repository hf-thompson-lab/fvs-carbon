tar_target(cficop_bl56_carbon, {
  cficop_bl56_carbon <- fvs_read_output(cficop_bl56, "FVS_Carbon") |>
    group_by(StandID, Year) |> # Combine results from different random seeds
    summarize(
      Aboveground_Total_Live = mean(Aboveground_Total_Live),
      .groups = "keep"
    ) |>
    ungroup()
})
