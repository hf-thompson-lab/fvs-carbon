read_nk_table1 <- function(file) {
  read_csv(
    file,
    col_types = cols(`FIA plot code` = col_character())
  )
}

compute_nk_table1_expanded <- function(nk_table1) {
  nk_table1_expanded <- nk_table1 |>
    mutate(STATECD = substr(`FIA plot code`, 1, 2)) |>
    mutate(INVYR = substr(`FIA plot code`, 3, 6)) |>
    mutate(UNITCD = substr(`FIA plot code`, 7, 8)) |>
    mutate(COUNTYCD = substr(`FIA plot code`, 9, 11)) |>
    mutate(PLOT = substr(`FIA plot code`, 12, 16))
}

load_nk_matching_cond <- function(fiadb, nk_table1_expanded)
{
  fia = DBI::dbConnect(RSQLite::SQLite(), fiadb)
  on.exit(dbDisconnect(fia))
  
  tbl(fia, 'COND') |>
    rename(FIA_INVYR=INVYR) |>
    right_join(
      nk_table1_expanded |>
        select(STATECD, UNITCD, COUNTYCD, PLOT, INVYR) |>
        rename(NK_INVYR=INVYR),
      by=join_by(STATECD, UNITCD, COUNTYCD, PLOT),
      copy=TRUE
    ) |>
    mutate(INVYR_MATCHES=ifelse(NK_INVYR==FIA_INVYR, 1, 0)) |>
    collect()
}

compute_nk_all_cond <- function(nk_table1_expanded, nk_matching_cond) {
  nk_plots_with_age <- nk_table1_expanded |>
    select(`FIA plot code`, STATECD, INVYR, UNITCD, COUNTYCD, PLOT,
           `Starting stand age`, `Slope (%)`, `Aspect (degrees)`,
           `Basal area (m2/ha)`) |>
    rename(NK_INVYR=INVYR)

  fia_cond_with_age <- nk_matching_cond |>
    select(STATECD, FIA_INVYR, CYCLE, SUBCYCLE, CONDID, UNITCD, COUNTYCD, PLOT,
           STDAGE, FLDAGE, SLOPE, ASPECT, BALIVE) |>
    left_join(
      nk_plots_with_age, by=join_by(STATECD, UNITCD, COUNTYCD, PLOT)
    )
  fia_cond_with_age |>
    filter(
      NK_INVYR==FIA_INVYR | (
        NK_INVYR!=FIA_INVYR & `Starting stand age`==STDAGE
      ),
      FIA_INVYR<=2005
    )
}

compute_nk_to_fia <- function(nk_all_cond) {
  nk_all_cond |>
    select(`FIA plot code`, NK_INVYR, 
           STATECD, FIA_INVYR, CYCLE, SUBCYCLE, CONDID, UNITCD, COUNTYCD, PLOT) |>
    rename(INVYR = FIA_INVYR)
}

nk_transalte_stand_ids <- function(nk_stands) {
  nk_stands |>
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
    mutate(STAND_ID_COND=paste0(STAND_ID_PLOT, CONDID)) |>
    select(`FIA plot code`, FVS_STAND_ID, STAND_ID_PLOT, STAND_ID_COND)
}

compute_nk_to_fvs <- function(fiadb, fvs_stands) {
  fia = DBI::dbConnect(RSQLite::SQLite(), fiadb)
  on.exit(dbDisconnect(fia))
  
  matching_plotinit_plot_grp <- tbl(fia, 'FVS_PLOTINIT_PLOT') |>
    right_join(
      fvs_stands |> rename(STAND_ID=STAND_ID_PLOT) |> select(STAND_ID),
      by=join_by(STAND_ID),
      copy=TRUE
    ) |>
    group_by(STAND_ID) |>
    summarize(NUM_PLOTS=n(), .groups = "keep") |>
    rename(FVS_PLOTINIT_PLOT=STAND_ID) |>
    collect()
  
  matching_standinit_cond_grp <- tbl(fia, 'FVS_STANDINIT_COND') |>
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
  
  matching_standinit_plot_grp <- tbl(fia, 'FVS_STANDINIT_PLOT') |>
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
}