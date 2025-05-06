tar_target(nrsgro_ca10_mortality, {
  # All the trees we'll give to FVS for calibration
  # There are 7 sets of trees:
  # 1. Trees in INVNUM==1 that die in INVNUM==2
  # 2. Long-term dead in INVNUM==2
  # 3. Trees in INVNUM==1 that die in INVNUM==3
  # 4. Long-term dead in INVNUM==3 that was missed in INVNUM==2
  # 5. Trees in INVNUM==2 that die in INVNUM==3
  # 6. Trees that establish and die in INVNUM==2
  # 7. Trees that establish and die in INVNUM==3
  
  # Trees in INVNUM==1
  # In theory, we could mine some mortality from INVNUM==1
  # (e.g. trees that ingrew and died in the past 5 years), but
  # it's messy and unreliable so we skip it.
  # We need dead trees in INVNUM==1 so we can check long-term mortality.
  tmp_plot_1 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, INVYR, CN)
  
  tmp_tre_1 <- fia_trees(fiadb, tmp_plot_start) |>
    select(
      CN, PREV_TRE_CN, PLT_CN,
      STATECD, COUNTYCD, PLOT, SUBP, CONDID, TREE, INVYR,
      TPA_UNADJ, SPCD, STATUSCD, PREV_STATUS_CD, DIA, HT
    ) |>
    rename(
      TRE_CN0 = PREV_TRE_CN,
      TRE_CN1 = CN,
      STAT0 = PREV_STATUS_CD,
      STAT1 = STATUSCD,
      TPA1 = TPA_UNADJ,
      DIA1 = DIA,
      HT1 = HT
    )
  
  # Trees in INVNUM==2
  # We are interrested in dead and live trees.
  # This will include both recent and long-term mortality
  tmp_plot_2 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 2) |>
    ungroup()
  
  tmp_tre_2 <- fia_trees(fiadb, tmp_plot_2) |>
    select(
      CN, PREV_TRE_CN, PLT_CN,
      STATECD, COUNTYCD, PLOT, SUBP, CONDID, TREE, INVYR,
      TPA_UNADJ, SPCD, STATUSCD, PREV_STATUS_CD, DIA, PREVDIA, HT
    ) |>
    rename(
      TRE_CN1 = PREV_TRE_CN,
      TRE_CN2 = CN,
      STAT1 = PREV_STATUS_CD,
      STAT2 = STATUSCD,
      TPA2 = TPA_UNADJ,
      DIA1 = PREVDIA,
      DIA2 = DIA,
      HT2 = HT
    )
  
  # Trees in INVNUM==3
  # We are interested only in dead trees.
  # This will include both recent and long-term mortality
  tmp_plot_3 <- nrsgro_plot |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(INVYR) |>
    filter(row_number() == 3) |>
    ungroup()
  
  tmp_tre_3 <- fia_trees(fiadb, tmp_plot_3) |>
    filter(STATUSCD == 2) |>
    select(
      CN, PREV_TRE_CN, PLT_CN,
      STATECD, COUNTYCD, PLOT, SUBP, CONDID, TREE, INVYR,
      TPA_UNADJ, SPCD, STATUSCD, PREV_STATUS_CD, DIA, PREVDIA, HT
    ) |>
    rename(
      TRE_CN2 = PREV_TRE_CN,
      TRE_CN3 = CN,
      STAT2 = PREV_STATUS_CD,
      STAT3 = STATUSCD,
      TPA3 = TPA_UNADJ,
      DIA2 = PREVDIA,
      DIA3 = DIA,
      HT3 = HT
    )
  
  # 1. Trees in INVNUM==1 that die in INVNUM==2
  # These will have STAT1==1 and STAT2 == 2
  # And
  # 2. Long-term dead in INVNUM==2
  # These will have STAT1==2 and STAT2 == 2
  # 6. Trees that establish and die in INVNUM==2
  # These will have STAT2==2 and TRE_CN1==NA
  tmp_tre_mort_2 <- tmp_tre_2 |>
    filter(STAT2 == 2) |>
    left_join(
      tmp_tre_1 |> select(TRE_CN1, STAT1, TPA1, DIA1, HT1),
      by = join_by(TRE_CN1)
    ) |>
    mutate(
      STAT1 = coalesce(STAT1.y, STAT1.x),
      DIA1 = coalesce(DIA1.y, DIA1.x)
    ) |>
    select(!ends_with(".x") & !ends_with(".y")) |>
    filter(
      is.na(TRE_CN1) | # Case 6
        STAT1 == 1 |   # Case 1
        STAT1 == 2     # Case 2
    )
  
  # 3. Trees in INVNUM==1 that die in INVNUM==3
  # 4. Long-term dead in INVNUM==3 that was missed in INVNUM==2
  # 5. Trees in INVNUM==2 that die in INVNUM==3
  # 7. Trees that establish and die in INVNUM==3
  tmp_tre_mort_3 <- tmp_tre_3 |>
    filter(STAT3 == 2) |>
    left_join(
      tmp_tre_2 |> select(TRE_CN2, STAT2, TPA2, DIA2, DIA1, HT2),
      by = join_by(TRE_CN2)
    ) |>
    mutate(
      STAT2 = coalesce(STAT2.y, STAT2.x),
      DIA2 = coalesce(DIA2.y, DIA2.x)
    ) |>
    select(!ends_with(".x") & !ends_with(".y")) |>
    filter(
      is.na(TRE_CN2) | # Case 7 and 4
        STAT2 == 1     # Case 3 and 5
    )
  
  # Combine mortality from INVNUM==2 and INVNUM==3 into one table
  tmp_tre_mort <- tmp_tre_mort_2 |>
    # Adapt schema
    mutate(
      TRE_CN3 = NA,
      TPA3 = NA,
      STAT3 = NA,
      DIA3 = NA,
      HT3 = NA
    ) |>
    union_all(
      tmp_tre_mort_3 |>
        mutate(
          STAT1 = NA,
          TRE_CN1 = NA,
          TPA1 = NA,
          DIA1 = NA,
          HT1 = NA
        )
    )
  
  # No tree should be included from both INVYR==2 and INVYR==3
  stopifnot(
    tmp_tre_mort |>
      group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
      summarize(N = n(), .groups = "keep") |>
      ungroup() |>
      filter(N>1) |>
      nrow() == 0
  )
  
  # Add remaining info
  tmp_cond_mixin <- fia_conds(
      fiadb,
      nrsgro_plot |>
        distinct(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    select(
      STATECD, COUNTYCD, PLOT, CONDID, INVYR, SLOPE, ASPECT
    )
  
  tmp_tre_mort |>
    # Don't bother with TREE - this'll get clobbered later anyway.
    left_join(tmp_cond_mixin, by = join_by(STATECD, COUNTYCD, PLOT, CONDID, INVYR)) |>
    select(-PLT_CN) |>
    left_join(
      tmp_plot_1 |>
        select(STATECD, COUNTYCD, PLOT, CN) |>
        rename(PLT_CN = CN),
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    filter_add_stand_id() |>
    mutate(
      STAND_CN = PLT_CN,
      STAND_ID = STAND_ID,
      PLOT_CN = PLT_CN,
      PLOT_ID = SUBP,
      STANDPLOT_CN = paste0(STAND_CN, "_", SUBP),
      STANDPLOT_ID = paste0(STAND_ID, "_", SUBP),
      TREE_CN = coalesce(TRE_CN1, TRE_CN2, TRE_CN3),
      TREE_ID = TREE,
      TREE_COUNT = coalesce(TPA3, TPA2, TPA1),
      SPECIES = sprintf("%03d", SPCD),
      HISTORY = if_else(
          # Note: if observed dead in INVNUM==1, then long-term dead;
          # otherwise it's either recent dead or new establishment death
          (!is.na(STAT1) & STAT1 == 2), 8, 6
        ),
      DIAMETER = coalesce(DIA3, DIA2, DIA1),
      HT = coalesce(HT3, HT2, HT1),
      SLOPE = SLOPE,
      ASPECT = ASPECT
    ) |>
    select(
      STAND_CN,
      STAND_ID,
      PLOT_CN,
      PLOT_ID,
      STANDPLOT_CN,
      STANDPLOT_ID,
      TREE_CN,
      TREE_ID,
      TREE_COUNT,
      SPECIES,
      HISTORY,
      DIAMETER,
      HT,
      SLOPE,
      ASPECT
    )
  
})
