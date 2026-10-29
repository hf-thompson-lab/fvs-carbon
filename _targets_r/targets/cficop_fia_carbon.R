tar_target(cficop_fia_carbon, {
  cficop_fia_carbon <- fvs_read_output(cficop_fia, "FVS_Carbon") |>
    group_by(StandID, Year) |> # Combine results from different random seeds
    summarize(
      Aboveground_Total_Live = mean(Aboveground_Total_Live),
      .groups = "keep"
    ) |>
    ungroup()
})
