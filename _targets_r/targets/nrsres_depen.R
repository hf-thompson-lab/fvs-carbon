tar_target(nrsres_depen, {
  nrsgro_ca10_proj_vs_meas |>
    # Filter to only the final projection for each stand
    group_by(StandID) |>
    arrange(desc(ProjectionYears)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(-BALIVE_METRIC) # BALIVE_METRIC is a copy of the one in independent vars
})
