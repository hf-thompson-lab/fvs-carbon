tar_target(nrsgro_ca10_proj_vs_meas, {
  stand_mixin <- nrsgro_plot_stats |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STAND_ID, MEASYEAR) |>
    rename(StandID = STAND_ID, StartYear = MEASYEAR)
  
  none_carbon_tmp <- nrsgro_none_carbon |>
    rename(Carbon = Aboveground_Total_Live) |>
    select(StandID, Year, Carbon) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(Carbon_Delta = Carbon - lag(Carbon)) |>
    ungroup() |>
    filter(!is.na(Carbon_Delta))
  
  ca10_carbon_tmp <- nrsgro_ca10_carbon |>
    rename(Carbon = Aboveground_Total_Live) |>
    select(StandID, Year, Carbon) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(Carbon_Delta = Carbon - lag(Carbon)) |>
    ungroup() |>
    filter(!is.na(Carbon_Delta))
  
  nrsgro_ca10_proj_vs_meas <- none_carbon_tmp |>
    rename(Carbon_None = Carbon, Carbon_Delta_None = Carbon_Delta) |>
    left_join(ca10_carbon_tmp, by = join_by(StandID, Year)) |>
    rename(Carbon_Calb = Carbon, Carbon_Delta_Calb = Carbon_Delta) |>
    left_join(stand_mixin, by = join_by(StandID)) |>
    mutate(
      Carbon_Diff = Carbon_Calb - Carbon_None,
      ProjectionYears = Year - StartYear,
      Carbon_Flux_Calb = Carbon_Delta_Calb / ProjectionYears,
      Carbon_Flux_None = Carbon_Delta_None / ProjectionYears,
      Carbon_Flux_Delta = Carbon_Flux_Calb - Carbon_Flux_None
    )
})
