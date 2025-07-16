tar_target(cficop_estab, {
  # Each plot has a sibling plot;
  # find the estab associated with the sibling plot.
  cficop_plot_sibling |>
    left_join(
      cfigro_plot |> select(STAND_CN, MasterPlotID = STAND_ID),
      by = join_by(sibling_plot == MasterPlotID)
    ) |>
    mutate(STAND_CN = as.numeric(STAND_CN)) |>
    left_join(
      cfigro_estab,
      by = join_by(STAND_CN),
      # Each row on the left is a stand; many stands have the same STAND_CN.
      # Each row on the right is a stand-species; many rows on the right will
      # match a single stand. Therefore this is a many-to-many join.
      relationship = "many-to-many"
    ) |>
    # STAND_CN is for the sibling plot; swap that out
    # with the STAND_CN for the matched plot.
    select(-STAND_CN) |>
    left_join(
      cfigro_plot |> select(STAND_CN, MasterPlotID = STAND_ID),
      by = join_by(MasterPlotID)
    )
})
