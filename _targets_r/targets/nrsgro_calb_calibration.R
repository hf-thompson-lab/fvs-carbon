tar_target(nrsgro_calb_calibration, {
  starting_plots <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |> 
    ungroup()
  
  tmp_tre <- fia_trees(fiadb, starting_plots) |>
    select(CN, PLT_CN, STATECD, COUNTYCD, PLOT, INVYR, PREV_TRE_CN, DIA, HT)
  
  tmp_nxt_tre <- fia_trees_filtered(
    fiadb,
    plots = NULL,
    \(.data, con) {
      .data |>
        inner_join(
          tmp_tre |>
            select(CN) |>
            rename(PREV_TRE_CN = CN),
          by = join_by(PREV_TRE_CN),
          copy = TRUE
        ) |>
        select(PREV_TRE_CN, CN, PLT_CN, DIA, HT) |>
        rename(
          NEXT_TRE_CN = CN,
          CN = PREV_TRE_CN,
          NEXT_PLT_CN = PLT_CN,
          NEXT_DIA = DIA,
          NEXT_HT = HT
        )
    }
  )
  
  tmp_plt <- nrsgro_plot |>
    select(CN, REMPER) |>
    rename(NEXT_PLT_CN = CN)
  
  # For FVS, we need:
  # STAND_CN
  # TREE_CN
  # DG
  # HTG
  # The rest is assumed
  
  tmp_tre |>
    left_join(tmp_nxt_tre, by = join_by(CN)) |>
    left_join(tmp_plt, by = join_by(NEXT_PLT_CN)) |>
    mutate(
      ANN_DIA_GROWTH = (NEXT_DIA - DIA) / REMPER,
      DG = DIA + ANN_DIA_GROWTH * 5,
      ANN_HT_GROWTH = (NEXT_HT - HT) / REMPER,
      HTG = HT + ANN_HT_GROWTH * 5
    ) |>
    filter(!is.na(DG) & !is.na(HTG)) |>
    select(PLT_CN, CN, DG, HTG) |>
    rename(
      STAND_CN = PLT_CN,
      TREE_CN = CN
    )
})
