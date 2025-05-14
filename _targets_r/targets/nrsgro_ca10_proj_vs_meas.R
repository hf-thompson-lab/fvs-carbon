tar_target(nrsgro_ca10_proj_vs_meas, {
  stand_mixin <- nrsgro_plot_stats |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STAND_ID, MEASYEAR, BALIVE_METRIC) |>
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
    mutate(BA_METRIC = conv_multiunit(BA, "ft2 / acre", "m2 / hectare")) |>
    select(StandID, Year, BA_METRIC) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(
      BA_Delta = BA_METRIC - lag(BA_METRIC)
    ) |>
    ungroup() |>
    filter(!is.na(BA_Delta))
  
  ca10_carbon_tmp <- nrsgro_ca10_carbon |>
    rename(Carbon = Aboveground_Total_Live) |>
    select(StandID, Year, Carbon) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(Carbon_Delta = Carbon - lag(Carbon)) |>
    ungroup() |>
    filter(!is.na(Carbon_Delta))
  
  ca10_summary_tmp <- nrsgro_ca10_summary |>
    mutate(BA_METRIC = conv_multiunit(BA, "ft2 / acre", "m2 / hectare")) |>
    select(StandID, Year, BA_METRIC) |>
    group_by(StandID) |>
    arrange(Year) |>
    mutate(
      BA_Delta = BA_METRIC - lag(BA_METRIC)
    ) |>
    ungroup() |>
    filter(!is.na(BA_Delta))
  
  nrsgro_ca10_proj_vs_meas <- ca10_carbon_tmp |>
    group_by(StandID) |>
    arrange(desc(Year)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    rename(Carbon_Calb = Carbon, Carbon_Delta_Calb = Carbon_Delta) |>
    left_join(ca10_summary_tmp, by = join_by(StandID, Year)) |>
    rename(
      BA_Calb = BA_METRIC,
      BA_Delta_Calb = BA_Delta
    ) |>
    left_join(srvy_carbon_tmp, by = join_by(StandID, Year)) |>
    rename(Carbon_Srvy = Carbon, Carbon_Delta_Srvy = Carbon_Delta) |>
    left_join(srvy_summary_tmp, by = join_by(StandID, Year)) |>
    rename(
      BA_Srvy = BA_METRIC,
      BA_Delta_Srvy = BA_Delta
    ) |>
    left_join(stand_mixin, by = join_by(StandID)) |>
    mutate(
      BA_Diff = BA_Calb - BA_Srvy,
      Carbon_Diff = Carbon_Calb - Carbon_Srvy,
      ProjectionYears = Year - StartYear,
      BAI_Calb = BA_Delta_Calb / ProjectionYears,
      BAI_Srvy = BA_Delta_Srvy / ProjectionYears,
      BAI_Residual = BAI_Calb - BAI_Srvy,
      Carbon_Flux_Calb = -(Carbon_Delta_Calb / ProjectionYears),
      Carbon_Flux_Srvy = -(Carbon_Delta_Srvy / ProjectionYears),
      Carbon_Flux_Residual = Carbon_Flux_Calb - Carbon_Flux_Srvy
    )
})
