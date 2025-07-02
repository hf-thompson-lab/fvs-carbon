tar_target(nrsres_indep, {
  tmp_plot_stats <- nrsgro_plot_stats |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    mutate(
      FOREST_TYPE = as.factor(FOREST_TYPE),
      FOREST_TYPE_GROUP = as.factor(FOREST_TYPE_GROUP)
    ) |>
    select(
      STAND_ID,
      BALIVE_METRIC, BA_TREES, QMD,
      FOREST_TYPE_GROUP, FOREST_TYPE, STDAGE
    ) |>
    # Fix the name of this so it's not confusing
    rename(TPA = BA_TREES) |>
    # Compute Stand Density Index; this is not relative stand density, it's the
    # absolute number. See FIADB data dictionary sectino 2.5.139 SDI_RMRS,
    # and VanderSchaaf, C.L., 2013. Reineke’s stand density index: a quantitative and non-unitless
    # measure of stand density.
    # In: Guldin, James M., ed. 2013. Proceedings of the 15th biennial southern silvicultural
    # research conference. e-Gen. Tech. Rep. SRS-GTR-175. Asheville, NC:
    # US Department of Agriculture, Forest Service, Southern Research Station.
    # 577-579. (Vol. 175, pp. 577-579).
    # Here we use 1.605 as the exponent of Reineke's equation, as is commonly reported.
    mutate(SDI = 10^(log10(TPA) + 1.605*log10(QMD) - 1.605))
  
  tmp_cond_stats <- fia_conds(fiadb, nrsgro_plot) |>
    filter(!is.na(SLOPE) & !is.na(ASPECT)) |>
    # Pick just the first inventory year; this
    # will align best with other starting conditions.
    group_by(STATECD, COUNTYCD, PLOT, CONDID) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    # Pick the largest condition; this will
    # cover the most conditions on the plot
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(desc(CONDPROP_UNADJ)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    left_join(
      fiadb_2_5_35_physclcd |>
        mutate(
          SITE_MOISTURE = case_when(
            `Site Type` == "Xeric" ~ 0,
            `Site Type` == "Mesic" ~ 0.5,
            `Site Type` == "Hydric" ~ 1
          )
        ) |>
        select(PHYSCLCD, SITE_MOISTURE, PHYSIOGRAPHIC_CLASS),
      by = join_by(PHYSCLCD)
    ) |>
    filter_add_stand_id() |>
    select(STAND_ID, SLOPE, ASPECT, SITE_MOISTURE, PHYSIOGRAPHIC_CLASS) |>
    mutate(
      ASPECT = cos((45 - ASPECT)*pi/180) + 1 # Beers Transformation
    )
  
  nrsgro_plot |>
    filter_add_stand_id() |>
    group_by(STAND_ID) |>
    arrange(INVYR) |>
    mutate(ELEV = max(ELEV, na.rm = TRUE)) |> # some ELEV are NA; replace them
    filter(row_number() == 1) |>
    ungroup() |>
    select(STAND_ID, LAT, LON, ELEV, ECOSUBCD) |>
    mutate(
      ECOCD = substr(ECOSUBCD, 1, nchar(ECOSUBCD) - 1),
      ECOSUBCD = as.factor(ECOSUBCD),
      ECOCD = as.factor(ECOCD)
    ) |>
    left_join(tmp_plot_stats, by = join_by(STAND_ID))  |>
    left_join(tmp_cond_stats, by = join_by(STAND_ID))
})
