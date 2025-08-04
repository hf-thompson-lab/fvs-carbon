tar_target(nrshrv_plot_stats_all, {
  fia_plots_filtered(fiadb, nrshrv_plot_all, \(.data, con) {
      plots <- .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR)
      plots_join_by <- join_by(STATECD, COUNTYCD, PLOT, INVYR)
  
      forest_type <- tbl(con, "REF_FOREST_TYPE") |>
        select(VALUE, MEANING, TYPGRPCD) |>
        rename(FORTYPCD = VALUE) |>
        rename(FORTYPE = MEANING)
      
      forest_type_group <- tbl(con, "REF_FOREST_TYPE_GROUP") |>
        select(VALUE, MEANING) |>
        rename(
          TYPGRPCD = VALUE,
          FOREST_TYPE_GROUP = MEANING
        )
      
      tree_stats <- tbl(con, "TREE") |>
        inner_join(plots, by = plots_join_by) |>
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
        inner_join(plots, by = plots_join_by) |>
        select(STATECD, COUNTYCD, PLOT, INVYR, STDAGE, BALIVE, CONDPROP_UNADJ, FORTYPCD) |>
        group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
        # QMD = sqrt(sum((DIA/2)^2) / n)
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
          BALIVE = sum(BALIVE * CONDPROP_UNADJ, na.rm = TRUE),
          FORTYPCD = max(FORTYPCD, na.rm = TRUE),
          STDAGE = max(STDAGE, na.rm = TRUE),
          .groups = "keep"
        ) |>
        ungroup()
      
      harvest_mixin <- tbl(con, "COND") |>
        inner_join(plots, by = plots_join_by) |>
        # Label each condition for harvest
        mutate(
          HARVEST =
            (!is.na(TRTCD1) & TRTCD1 == 10) |
            (!is.na(TRTCD2) & TRTCD2 == 10) |
            (!is.na(TRTCD3) & TRTCD3 == 10),
          HRVYR1 = if_else(!is.na(TRTCD1) & (TRTCD1 == 10), TRTYR1, NA),
          HRVYR2 = if_else(!is.na(TRTCD2) & (TRTCD2 == 10), TRTYR2, NA),
          HRVYR3 = if_else(!is.na(TRTCD3) & (TRTCD3 == 10), TRTYR3, NA),
          HRVYR = coalesce(HRVYR3, HRVYR2, HRVYR1)
        ) |>
        # Collapse conditions up to plot level
        group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
        summarize(
          HARVEST = any(HARVEST, na.rm = TRUE),
          HRVYR = max(HRVYR, na.rm = TRUE),
          .groups = "keep"
        ) |>
        ungroup() |>
        # Give ordinals to plot inventories
        group_by(STATECD, COUNTYCD, PLOT) |>
        window_order(INVYR) |>
        mutate(
          INVNUM = row_number(),
          HARVEST = HARVEST & (INVNUM > 1) # Don't consider harvest in the first inventory
        ) |>
        ungroup() |>
        select(STATECD, COUNTYCD, PLOT, INVYR, INVNUM, HARVEST, HRVYR)
      
      pre_harvest_mixin <- harvest_mixin |>
        filter(HARVEST == 1) |>
        mutate(INVNUM = INVNUM - 1) |> # the inventory prior to harvest is pre-harvest
        rename(PRE_HARVEST = HARVEST) |>
        select(STATECD, COUNTYCD, PLOT, INVNUM, PRE_HARVEST)
      
      .data |>
        select(CN, STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR) |>
        left_join(cond_stats, by = plots_join_by) |>
        left_join(tree_stats, by = plots_join_by) |>
        left_join(harvest_mixin, by = plots_join_by) |>
        left_join(pre_harvest_mixin, by = join_by(STATECD, COUNTYCD, PLOT, INVNUM)) |>
        left_join(forest_type, by = join_by(FORTYPCD)) |>
        left_join(forest_type_group, by = join_by(TYPGRPCD)) |>
        mutate(
          PRE_HARVEST = if_else(is.na(PRE_HARVEST), 0, PRE_HARVEST),
          STDAGE = if_else(STDAGE < 0, NA, STDAGE)
        ) |>
        # Mark inventories that come before the pre-harvest inventory
        mutate(PREHRVYEAR = if_else(PRE_HARVEST == 1, MEASYEAR, NA)) |>
        group_by(STATECD, COUNTYCD, PLOT) |>
        window_order(MEASYEAR) |>
        mutate(PRE_PRE_HARVEST = (MEASYEAR < min(PREHRVYEAR, na.rm = TRUE))) |>
        ungroup()
    }) |>
    filter_decode_forest_type_group() |>
    mutate(
      STAND_ID = sprintf("%04d%03d%05d", STATECD, COUNTYCD, PLOT),
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
