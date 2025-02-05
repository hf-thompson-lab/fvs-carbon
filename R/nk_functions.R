nk_read_table_1 <- function(filename) {
  read_csv(
    filename,
    col_types = cols(`FIA plot code` = col_character())
  )
}

nk_read_fig_2 <- function(dir) {
  nk_read_fig_2_helper <- function(dir, column) {
    filename <- file.path(dir, paste0("NK_Fig2_", column, ".csv"))
    read_csv(
      filename,
      col_names = c("Year", "value"),
      # Read columns as double, since that is how they were written
      col_types = "dd"
    ) |>
    # Round columns to nearest integer, since that is what was intended
    mutate(
      Year = round(Year),
      name = column,
      value = round(value)
    )
  }
  columns <- c(
    "NoManagement",
    "ClearcutHigh", "ClearcutLow",
    "ShelterwoodHigh", "ShelterwoodLow",
    "ITS_LowHigh", "ITS_LowLow", "ITS_HighHigh", "ITS_HighLow"
  )
  bind_rows(lapply(columns, \(column) nk_read_fig_2_helper(dir, column) )) |>
    pivot_wider(id_cols = "Year") |>
    # The last year in Fig2 is 2164; nudge it to 2165
    # to align with contemporary FVS runs.
    mutate(Year = if_else(Year==2164,2165,Year)) |>
    arrange(Year)
}

nk_extract_cols_from_plot_code <- function(.data) {
  .data |>
    mutate(STATECD = substr(`FIA plot code`, 1, 2)) |>
    mutate(INVYR = substr(`FIA plot code`, 3, 6)) |>
    mutate(UNITCD = substr(`FIA plot code`, 7, 8)) |>
    mutate(COUNTYCD = substr(`FIA plot code`, 9, 11)) |>
    mutate(PLOT = substr(`FIA plot code`, 12, 16))
}

nk_load_plot_all_invyr <- function(.data, fiadb)
{
  fia = DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(fia), add = TRUE, after = FALSE)
  
  tbl(fia, 'COND') |>
    rename(FIA_INVYR=INVYR) |>
    right_join(
      .data |>
        select(STATECD, COUNTYCD, PLOT, INVYR) |>
        rename(NK_INVYR=INVYR),
      by=join_by(STATECD, COUNTYCD, PLOT),
      copy=TRUE
    ) |>
    mutate(INVYR_MATCHES=ifelse(NK_INVYR==FIA_INVYR, 1, 0)) |>
    collect()
}

nk_match_plots <- function(nk_plots, fia_plots) {
  nk_plots_with_age <- nk_plots |>
    select(`FIA plot code`, STATECD, INVYR, COUNTYCD, PLOT,
           `Starting stand age`, `Slope (%)`, `Aspect (degrees)`,
           `Basal area (m2/ha)`) |>
    rename(NK_INVYR=INVYR)

  fia_plots_with_age <- fia_plots |>
    select(STATECD, FIA_INVYR, CYCLE, SUBCYCLE, UNITCD, COUNTYCD, PLOT, CONDID,
           STDAGE, FLDAGE, SLOPE, ASPECT, BALIVE) |>
    left_join(
      nk_plots_with_age, by=join_by(STATECD, COUNTYCD, PLOT)
    )
  fia_plots_with_age |>
    filter(
      NK_INVYR==FIA_INVYR | (
        NK_INVYR!=FIA_INVYR & `Starting stand age`==STDAGE
      ),
      FIA_INVYR<=2005
    )
}

nk_transalte_to_fia <- function(fiadb, nk_stands) {
  left <- nk_stands |>
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
    semi_join(
      left |>
        distinct(STATECD, COUNTYCD, PLOT, FIA_INVYR) |>
        rename(INVYR = FIA_INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
      copy = TRUE
    ) |>
    select(CN, STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR) |>
    rename(STAND_CN = CN, FIA_INVYR = INVYR) |>
    collect()
  
  left |>
    left_join(plot_mixin, by = join_by(STATECD, COUNTYCD, PLOT, FIA_INVYR)) |>
    select(
      `FIA plot code`,
      STAND_CN, STATECD, COUNTYCD, PLOT, FIA_INVYR, MEASYEAR,
      FVS_STAND_ID, STAND_ID_PLOT, STAND_ID_COND
    )
}

nk_translate_to_fvs <- function(fvs_stands, fiadb) {
  fia = DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(fia), add = TRUE, after = FALSE)
  
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

nk_read_table_4 <- function(filename) {
  read_csv(
    filename,
    col_types = cols(
      `Management scenario` = col_character(),
      .default = col_number(),
    )
  )
}

nk_generate_regen <- function(nk_table_4, species_crosswalk) {
  # Values in NK are seedlings per hectare; FVS needs seedlings per acre.
  # Convert using hectares_per_acre
  hectares_per_acre <- conv_unit(1, "acre", "hectare")

  # table4 has species in columns and management scenarios in rows.
  # We wish to pivot to have species in rows and management scenarios in columns.
  nk_table_4 |>
    pivot_longer(cols = !`Management scenario`) |>
    pivot_wider(names_from = `Management scenario`) |>
    rename(SCIENTIFIC_NAME = name) |>
    left_join(species_crosswalk, by = join_by(SCIENTIFIC_NAME)) |>
    mutate(Clearcut = round(Clearcut * hectares_per_acre)) |>
    mutate(Shelterwood = round(Shelterwood * hectares_per_acre)) |>
    mutate(`ITS_Low Retention` = round(`ITS_Low Retention` * hectares_per_acre)) |>
    mutate(`ITS_High Retention` = round(`ITS_High Retention` * hectares_per_acre)) |>
    mutate(Background = round(Background * hectares_per_acre))
}

nk_extract_regen <- function(nk_regen, mgmt_id) {
  nk_regen |>
    select(FVS_SPCD, {{mgmt_id}}) |>
    rename(SPECIES = FVS_SPCD, DENSITY = {{mgmt_id}}) |>
    filter(!is.na(DENSITY)) |>
    mutate(STAND_CN = NA, YEAR = NA)
}

nk_project_grow <- function(
    fiadb,
    stand_type,
    nk_to_fia,
    nk_regen
) {
  fvsbin_dir <- "/fvs/fvsbin"
  fvs_variant <- "fvsne"
  data_dir <- "data/fvs"
  title <- "NKByPlot"
  mgmt_id <- "NONE"
  
  project_dir <- file.path(data_dir, paste0("FVS_", title, "_", mgmt_id))
  if (!dir.exists(project_dir)) {
    dir.create(project_dir)
  }

  nk_background_regen <- nk_extract_regen(nk_regen, "Background")

  nk_grow_only_stands <- nk_to_fia |>
    rename(
      STAND_ID = `FIA plot code`,
      FIRST_YEAR = MEASYEAR
    ) |>
    mutate(LAST_YEAR = 2165) |>
    select(STAND_ID, STAND_CN, FIRST_YEAR, LAST_YEAR)

  fvs_run(
    fvsbin_dir,
    fvs_variant,
    project_dir,
    fiadb,
    stand_type,
    title,
    mgmt_id,
    nk_grow_only_stands,
    nk_background_regen
  )
}
