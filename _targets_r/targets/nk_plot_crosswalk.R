tar_target(nk_plot_crosswalk, {
  left <- nk_matching_plot |>
    mutate(
      STATECD  = as.numeric(STATECD),
      UNITCD   = as.numeric(UNITCD),
      COUNTYCD = as.numeric(COUNTYCD),
      PLOT     = as.numeric(PLOT)
    ) |>
    mutate(FVS_STAND_ID=sprintf(paste0(
      '%02d',  '%02d',           '%03d',   '%05d'),
      STATECD, FIA_INVYR %% 100, COUNTYCD, PLOT
    )) |>
    mutate(STAND_ID_PLOT=sprintf(paste0(
      '%04d' , '%04d'    , '%02d', '%02d'  , '%02d', '%03d'  , '%05d'),
      STATECD, FIA_INVYR , CYCLE , SUBCYCLE, UNITCD, COUNTYCD, PLOT
    )) |>
    mutate(STAND_ID_COND=paste0(STAND_ID_PLOT, CONDID))
  
  con = DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE, after = FALSE)
    
  plot_mixin <- tbl(con, 'PLOT') |>
    inner_join(
      left |>
        distinct(STATECD, COUNTYCD, PLOT, FIA_INVYR) |>
        rename(INVYR = FIA_INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
      copy = TRUE
    ) |>
    select(CN, STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR) |>
    rename(STAND_CN = CN, FIA_INVYR = INVYR) |>
    collect()
    
  fvs_stands <- left |>
    left_join(plot_mixin, by = join_by(STATECD, COUNTYCD, PLOT, FIA_INVYR)) |>
    select(
      `FIA plot code`,
      STAND_CN, STATECD, COUNTYCD, PLOT, FIA_INVYR, MEASYEAR,
      FVS_STAND_ID, STAND_ID_PLOT, STAND_ID_COND
    )
  
  matching_plotinit_plot_grp <- tbl(con, 'FVS_PLOTINIT_PLOT') |>
    right_join(
      fvs_stands |> rename(STAND_ID=STAND_ID_PLOT) |> select(STAND_ID),
      by=join_by(STAND_ID),
      copy=TRUE
    ) |>
    group_by(STAND_ID) |>
    summarize(NUM_PLOTS=n(), .groups = "keep") |>
    rename(FVS_PLOTINIT_PLOT=STAND_ID) |>
    collect()
    
  matching_standinit_cond_grp <- tbl(con, 'FVS_STANDINIT_COND') |>
    right_join(
      fvs_stands |>
        rename(STAND_ID=STAND_ID_COND),
        by=join_by(STAND_ID),
      copy=TRUE
    ) |>
    group_by(STAND_ID) |>
    summarize(NUM_CONDS=n(), .groups = "keep") |>
    rename(FVS_STANDINIT_COND=STAND_ID) |>
    collect()
    
  matching_standinit_plot_grp <- tbl(con, 'FVS_STANDINIT_PLOT') |>
    right_join(
      fvs_stands |>
        rename(STAND_ID=STAND_ID_PLOT),
        by=join_by(STAND_ID),
      copy=TRUE
    ) |>
    group_by(STAND_ID) |>
    summarize(NUM_COND_PLOTS=n(), .groups = "keep") |>
    rename(FVS_STANDINIT_PLOT=STAND_ID) |>
    collect()
    
    fvs_stands |>
      left_join(matching_plotinit_plot_grp, by=join_by(STAND_ID_PLOT==FVS_PLOTINIT_PLOT)) |>
      left_join(matching_standinit_cond_grp, by=join_by(STAND_ID_COND==FVS_STANDINIT_COND)) |>
      left_join(matching_standinit_plot_grp, by=join_by(STAND_ID_PLOT==FVS_STANDINIT_PLOT))
})
