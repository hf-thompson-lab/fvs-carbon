tar_target(nrsgro_projected_vs_measured, {
  projected_carbon_tmp <- nrsgro_none_carbon |>
    select(StandID, Year, Aboveground_Total_Live) |>
    rename(Projected_Carbon = Aboveground_Total_Live)
  
  projected_ba_tmp <- nrsgro_none_summary |>
    select(StandID, Year, BA, Tpa) |>
    mutate(Projected_BA = conv_multiunit(BA, "ft2 / acre", "m2 / hectare")) |>
    rename(Projected_Tpa = Tpa)
  
  projected_tmp <- projected_carbon_tmp |>
    full_join(projected_ba_tmp, by = join_by(StandID, Year)) |>
    select(StandID, Year, Projected_Carbon, Projected_BA, Projected_Tpa)
  
  surveyed_carbon_tmp <- nrsgro_srvy_carbon |>
    select(StandID, Year, Aboveground_Total_Live) |>
    rename(Measured_Carbon = Aboveground_Total_Live)
  
  surveyed_ba_tmp <- nrsgro_srvy_summary |>
    select(StandID, Year, BA, Tpa) |>
    mutate(Measured_BA = conv_multiunit(BA, "ft2 / acre", "m2 / hectare")) |>
    rename(Measured_Tpa = Tpa)
  
  surveyed_tmp <- surveyed_carbon_tmp |>
    full_join(surveyed_ba_tmp, by = join_by(StandID, Year)) |>
    filter(!is.na(Measured_Carbon)) |> # other metrics get an extra year
    select(StandID, Year, Measured_Carbon, Measured_BA, Measured_Tpa)
  
  nrsgro_plot |>
    mutate(StandID = sprintf(paste0(
      '%04d',  '%03d',   '%05d'),
      STATECD, COUNTYCD, PLOT
    )) |>
    rename(Year = MEASYEAR) |>
    left_join(nrsgro_plot_stats, by = join_by(STATECD, COUNTYCD, PLOT, INVYR)) |>
    select(
      StandID, Year,
      STDAGE, FOREST_TYPE, FOREST_TYPE_GROUP, ECOSUBCD,
      QMD, QMD_METRIC
    ) |>
    left_join(projected_tmp, by = join_by(StandID, Year)) |>
    left_join(surveyed_tmp, by = join_by(StandID, Year)) |>
    filter_decode_forest_type_group() |>
    group_by(StandID) |>
    mutate(
      ECOCD = substr(ECOSUBCD, 1, nchar(ECOSUBCD) - 1),
      First_Year = min(Year, na.rm = TRUE),
      Last_Year = max(Year, na.rm = TRUE),
      Starting_Carbon = min(if_else(Year == First_Year, Measured_Carbon, NA), na.rm = TRUE),
      Projected_Carbon_Delta = Projected_Carbon - Starting_Carbon,
      Projected_Carbon_Flux = if_else(Year == First_Year, 0, -(Projected_Carbon_Delta / (Year - First_Year))),
      Measured_Carbon_Delta = Measured_Carbon - Starting_Carbon,
      Measured_Carbon_Flux = if_else(Year == First_Year, 0, -(Measured_Carbon_Delta / (Year - First_Year))),
      Starting_BA = min(if_else(Year == First_Year, Measured_BA, NA), na.rm = TRUE),
      Projected_BA_Delta = Projected_BA - Starting_BA,
      Projected_BAI = if_else(Year == First_Year, 0, Projected_BA_Delta / (Year - First_Year)),
      Measured_BA_Delta = Measured_BA - Starting_BA,
      Measured_BAI = if_else(Year == First_Year, 0, Measured_BA_Delta / (Year - First_Year))
    ) |>
    ungroup() |>
    filter(Year == First_Year | Year == Last_Year) |>
    filter(!is.na(Projected_Carbon) & !is.na(Measured_Carbon)) |>
    mutate(Projection_Years = Year - First_Year) |>
    mutate(BA_Residual = Projected_BA - Measured_BA) |>
    mutate(BA_Error = 2 * abs(BA_Residual) / (Projected_BA + Measured_BA)) |>
    mutate(BA_Delta_Residual = Projected_BA_Delta - Measured_BA_Delta) |>
    mutate(BA_Flux_Residual = BA_Delta_Residual / Projection_Years) |>
    mutate(Carbon_Residual = Projected_Carbon - Measured_Carbon) |>
    mutate(Carbon_Error = 2 * abs(Carbon_Residual) / (Projected_Carbon + Measured_Carbon)) |>
    mutate(Carbon_Delta_Residual = Projected_Carbon_Delta - Measured_Carbon_Delta) |>
    mutate(Carbon_Flux_Residual = Projected_Carbon_Flux - Measured_Carbon_Flux)
})
