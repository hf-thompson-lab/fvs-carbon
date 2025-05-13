tar_target(nrsgro_ca10_proj_vs_meas, {
  stand_mixin <- nrsgro_plot_stats |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STAND_ID, MEASYEAR) |>
    rename(StandID = STAND_ID, StartYear = MEASYEAR)
  
  srvy_carbon_tmp <- nrsgro_srvy_carbon |>
    rename(Carbon = Aboveground_Total_Live) |>
    select(StandID, Year, Carbon) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(Carbon_Delta = Carbon - lag(Carbon)) |>
    ungroup() |>
    filter(!is.na(Carbon_Delta))
  
  srvy_summary_tmp <- nrsgro_srvy_summary |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(
      BA_Delta = BA - lag(BA),
      Tpa_Delta = Tpa - lag(Tpa)
    ) |>
    ungroup() |>
    filter(!is.na(BA_Delta ))
  
  ca10_carbon_tmp <- nrsgro_ca10_carbon |>
    rename(Carbon = Aboveground_Total_Live) |>
    select(StandID, Year, Carbon) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(Carbon_Delta = Carbon - lag(Carbon)) |>
    ungroup() |>
    filter(!is.na(Carbon_Delta))
  
  nrsgro_ca10_proj_vs_meas <- srvy_carbon_tmp |>
    rename(Carbon_Srvy = Carbon, Carbon_Delta_Srvy = Carbon_Delta) |>
    left_join(ca10_carbon_tmp, by = join_by(StandID, Year)) |>
    rename(Carbon_Calb = Carbon, Carbon_Delta_Calb = Carbon_Delta) |>
    left_join(nrsgro_srvy_summary |> select(StandID, Year, BA), by = join_by(StandID, Year)) |>
    rename(BA_Srvy = BA) |>
    left_join(stand_mixin, by = join_by(StandID)) |>
    mutate(
      Carbon_Diff = Carbon_Calb - Carbon_Srvy,
      ProjectionYears = Year - StartYear,
      Carbon_Flux_Calb = Carbon_Delta_Calb / ProjectionYears,
      Carbon_Flux_Srvy = Carbon_Delta_Srvy / ProjectionYears,
      Carbon_Flux_Residual = Carbon_Flux_Calb - Carbon_Flux_Srvy
    )
})
