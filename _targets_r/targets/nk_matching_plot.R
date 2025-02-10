tar_target(nk_matching_plot, {
  nk_plots_with_age <- nk_table_1_expanded |>
    select(`FIA plot code`, STATECD, INVYR, COUNTYCD, PLOT,
           `Starting stand age`, `Slope (%)`, `Aspect (degrees)`,
           `Basal area (m2/ha)`) |>
    rename(NK_INVYR=INVYR)
  
  nk_all_plot |>
    select(
      STATECD, FIA_INVYR, COUNTYCD, UNITCD, PLOT, CONDID,
      CYCLE, SUBCYCLE,
      STDAGE, FLDAGE, SLOPE, ASPECT, BALIVE
    ) |>
    mutate(
      BALIVE = conv_multiunit(BALIVE, "ft2 / acre", "m2 / hectare")
    ) |>
    left_join(
      nk_plots_with_age, by=join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    filter(
      NK_INVYR==FIA_INVYR | (
        NK_INVYR!=FIA_INVYR & `Starting stand age`==STDAGE
      ),
      FIA_INVYR<=2005
    )
})
