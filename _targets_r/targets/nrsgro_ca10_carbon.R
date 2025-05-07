tar_target(nrsgro_ca10_carbon, {
  fvs_read_output(nrsgro_ca10, "FVS_Carbon") |>
    group_by(StandID, Year) |> # Combine results from different random seeds
    summarize(
      Aboveground_Total_Live = mean(Aboveground_Total_Live),
      .groups = "keep"
    ) |>
    ungroup()
})
