fia_version <- function(fiadb) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, 'REF_FIADB_VERSION') |>
    filter(INSTALL_TYPE == 'RELEASE') |>
    arrange(desc(CREATED_DATE)) |>
    head(1) |>
    collect()
}
