tar_target(nrsgro_ca10_summary, {
  fvs_read_output(nrsgro_ca10, "FVS_Summary2") |>
    group_by(StandID, Year) |>
    summarize(
      BA = mean(BA),
      Tpa = mean(Tpa),
      .groups = 'keep'
    ) |>
    ungroup()
})
