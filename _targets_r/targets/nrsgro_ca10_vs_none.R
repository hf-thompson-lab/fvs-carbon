tar_target(nrsgro_ca10_vs_none, {
  stand_mixin <- nrsgro_plot_stats |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STAND_ID, MEASYEAR, BALIVE_METRIC) |>
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
  
  nrsgro_ca10_vs_none <- none_carbon_tmp |>
    rename(Carbon_None = Carbon, Carbon_Delta_None = Carbon_Delta) |>
    left_join(ca10_carbon_tmp, by = join_by(StandID, Year)) |>
    rename(Carbon_Ca10 = Carbon, Carbon_Delta_Ca10 = Carbon_Delta) |>
    left_join(stand_mixin, by = join_by(StandID)) |>
    mutate(
      Carbon_Diff = Carbon_Ca10 - Carbon_None,
      ProjectionYears = Year - StartYear,
      Carbon_Flux_Ca10 = -(Carbon_Delta_Ca10 / ProjectionYears),
      Carbon_Flux_None = -(Carbon_Delta_None / ProjectionYears),
      Carbon_Flux_Residual = Carbon_Flux_Ca10 - Carbon_Flux_None
    )
})
