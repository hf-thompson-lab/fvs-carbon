fiadb_version <- function(fiadb) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "REF_FIADB_VERSION") |>
    filter(INSTALL_TYPE == "RELEASE") |>
    arrange(desc(CREATED_DATE)) |>
    head(1) |>
    collect()
}

fiadb_create_index_maybe <- function(con, tbl, col) {
  idx_name <- paste0("IDX_", tbl, "_", col)
  sql <- paste0(
    "CREATE INDEX IF NOT EXISTS ", idx_name, " ON ", tbl, " (", col, ")"
  )
  DBI::dbExecute(con, sql) > 0 # return TRUE if index created
}

fiadb_create_plot_indexs_maybe <- function(con, tbl, has_invyr) {
  idx_name <- paste0("IDX_", tbl, "_PLOT")
  sql <- paste0(
    "CREATE INDEX IF NOT EXISTS ", idx_name, " ON ", tbl, " (STATECD, COUNTYCD, PLOT)"
  )
  DBI::dbExecute(con, sql)
  if (has_invyr) {
    idx_name <- paste0("IDX_", tbl, "_PLOT_INVYR")
    sql <- paste0(
      "CREATE INDEX IF NOT EXISTS ", idx_name, " ON ", tbl, " (STATECD, COUNTYCD, PLOT, INVYR)"
    )
    DBI::dbExecute(con, sql)
  }
}

fiadb_cn_columns <- function(con, tbl) {
  # list the Control Number (CN) primary and foreign keys in a table
  cols <- DBI::dbListFields(con, tbl)
  cols[grepl("^CN$", cols) | grepl("_CN$", cols)]
}

fiadb_table_has_plot_cols <- function(con, tbl) {
  cols <- DBI::dbListFields(con, tbl)
  # result[1] - true if table has plot columns
  # result[2] - true if table has invyr
  c(
    "STATECD" %in% cols &
      "COUNTYCD" %in% cols &
      "PLOT" %in% cols,
    "INVYR" %in% cols
  )
}

fiadb_create_indexes <- function() {
  # TODO: depend on the zip file or something for targets
  fiadb <- "data/raw/SQLite_FIADB_ENTIRE.db"
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO) # not SQLITE_RC
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbls <- DBI::dbListTables(con)
  lapply(tbls, \(tbl) {
    lapply(fiadb_cn_columns(con, tbl), \(col) {
      fiadb_create_index_maybe(con, tbl, col)
    })
    plot_cols <- fiadb_table_has_plot_cols(con, tbl)
    if (plot_cols[1]) {
      fiadb_create_plot_indexs_maybe(con, tbl, plot_cols[2])
    }
  })
  fiadb
}

fia_plots <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "PLOT") |>
    semi_join(
      plots |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT),
      copy = TRUE
    ) |>
    collect()
}

fia_plots_filtered <- function(fiadb, plots, filter) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "PLOT") |>
    semi_join(
      plots |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT),
      copy = TRUE
    ) |>
    filter(con) |>
    collect()
}

fia_conds <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  fia_ref_forest_type = tbl(con, 'REF_FOREST_TYPE') |>
    select(VALUE, MEANING) |>
    rename(FORTYPCD = VALUE, FORTYPE = MEANING)

  tbl(con, "COND") |>
    semi_join(
      plots |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT),
      copy = TRUE
    ) |>
    # Dereference forest type codes
    left_join(fia_ref_forest_type, by = join_by(FORTYPCD)) |>
    collect()
}
