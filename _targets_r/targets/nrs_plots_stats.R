tar_target(nrs_plots_stats, {
  fia_plots_filtered(fiadb, nrs_plots_grown, \(.data, con) {
      plots <- .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR)
      plots_join_by <- join_by(STATECD, COUNTYCD, PLOT, INVYR)
  
      forest_type <- tbl(con, "REF_FOREST_TYPE") |>
        select(VALUE, MEANING) |>
        rename(FORTYPCD = VALUE) |>
        rename(FORTYPE = MEANING)
      
      tree_stats <- tbl(con, "TREE") |>
        inner_join(plots |> select(STATECD, COUNTYCD, PLOT, INVYR), by = plots_join_by) |>
        select(STATECD, COUNTYCD, PLOT, INVYR, DIA, CARBON_AG, TPA_UNADJ) |>
        group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
        summarize(
          CARBON_AG = sum(CARBON_AG, na.rm = TRUE),
          CPA = sum(CARBON_AG * TPA_UNADJ, na.rm = TRUE),
          # Restrict to trees that match those used in QMD computation, below.
          BA_TREES = sum(if_else(DIA >= 1, TPA_UNADJ, 0), na.rm = TRUE),
          .groups = "keep"
        )
      
      cond_stats <- tbl(con, "COND") |>
        inner_join(plots |> select(STATECD, COUNTYCD, PLOT, INVYR), by = plots_join_by) |>
        select(STATECD, COUNTYCD, PLOT, INVYR, STDAGE, BALIVE, FORTYPCD) |>
        group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
        # QMD = sqrt(sum(DIA^2) / n)
        # Which is equivalent to
        # QMD = sqrt(sum(BALIVE * TPA_UNADJ) / (n * k)), where n is number of trees,
        # and k is π/576 ≅ 0.005454 for B in square feet and QMD in inches;
        # and k is π/40000 ≅ 0.0000785 for B in square meters and QMD in centimeters.
        # (see https://www.sciencedirect.com/science/article/pii/S2197562023000453 ,
        # https://doi.org/10.1016/j.fecs.2023.100114 )
        # When computing QMD from BA we need to use the same trees for BA and n;
        # FIADB data dictionary 2.5.51 BALIVE says "Basal area in square feet per
        # acre of all live trees ω1.0 inch d.b.h/d.r.c sampled in the condition."
        summarize(
          BALIVE = sum(BALIVE, na.rm = TRUE),
          FORTYPCD = max(FORTYPCD, na.rm = TRUE),
          STDAGE = max(STDAGE, na.rm = TRUE),
          .groups = "keep"
        ) |>
        ungroup()
      
      .data |>
        select(STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR) |>
        left_join(cond_stats, by = plots_join_by) |>
        left_join(tree_stats, by = plots_join_by) |>
        left_join(forest_type, by = join_by(FORTYPCD)) |>
        rename(FOREST_TYPE = FORTYPE) |>
        mutate(
          STDAGE = if_else(STDAGE < 0, NA, STDAGE),
          FRTYGRCD = floor(FORTYPCD / 10) * 10
        ) |>
        left_join(forest_type, by = join_by(FRTYGRCD == FORTYPCD)) |>
        rename(FOREST_TYPE_GROUP = FORTYPE)
    }) |>
    filter_decode_forest_type_group() |>
    mutate(
      BALIVE_METRIC = conv_multiunit(BALIVE, "ft2 / acre", "m2 / hectare"),
      QMD = sqrt(BALIVE / (BA_TREES * (pi / 576))),
      QMD_METRIC = sqrt(BALIVE_METRIC / (BA_TREES * (pi / 40000))),
      CARBON_METRIC = conv_multiunit(CPA, "lbs / acre", "Mg / hectare")
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    mutate(
      BALIVE_START = if_else(MEASYEAR == min(MEASYEAR, na.rm = TRUE), BALIVE_METRIC, NA),
      BALIVE_DELTA = BALIVE_METRIC - max(BALIVE_START, na.rm = TRUE),
      YEARS = MEASYEAR - min(MEASYEAR, na.rm = TRUE)
    ) |> 
    ungroup()
})
