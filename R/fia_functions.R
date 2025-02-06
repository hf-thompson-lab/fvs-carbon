fia_version <- function(fiadb) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "REF_FIADB_VERSION") |>
    filter(INSTALL_TYPE == "RELEASE") |>
    arrange(desc(CREATED_DATE)) |>
    head(1) |>
    collect()
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
