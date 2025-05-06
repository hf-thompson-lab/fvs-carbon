tar_target(nrsgro_ca10_calibration, {
  tmp_plot_1 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |> 
    ungroup()
  
  # All the trees we'll give to FVS for calibration
  tmp_tre <- fia_trees(fiadb, tmp_plot_1) |>
    filter(STATUSCD == 1) |>
    select(CN, PLT_CN, STATECD, COUNTYCD, PLOT, INVYR, DIA, HT)
  
  tmp_plot_2 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 2) |>
    ungroup()
  
  tmp_tre_grow_2 <- fia_trees(fiadb, tmp_plot_2) |>
    filter(STATUSCD == 1) |>
    select(CN, PREV_TRE_CN, DIA, HT) |>
    rename(
      DIA2 = DIA,
      HT2 = HT,
      PREV_TRE_CN = CN,
      CN = PREV_TRE_CN
    )
  
  tmp_plot_3 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 3) |>
    ungroup()
  
  tmp_tre_grow_3 <- fia_trees(fiadb, tmp_plot_3) |>
    filter(STATUSCD == 1) |>
    select(PREV_TRE_CN, DIA, HT) |>
    rename(
      DIA3 = DIA,
      HT3 = HT
    )
  
  tmp_tre_grow <- tmp_tre_grow_2 |>
    left_join(tmp_tre_grow_3, by = join_by(PREV_TRE_CN))
  
  tree_growth <- tmp_tre |>
    left_join(tmp_tre_grow, by = join_by(CN))
  
  # For FVS, we need:
  # STAND_CN
  # TREE_CN
  # DG
  # HTG
  # The rest is assumed
  
  tree_growth |>
    filter(!is.na(DIA3) & !is.na(HT3)) |>
    select(PLT_CN, CN, DIA3, HT3) |>
    rename(
      STAND_CN = PLT_CN,
      TREE_CN = CN,
      DG = DIA3,
      HTG = HT3
    )
})
