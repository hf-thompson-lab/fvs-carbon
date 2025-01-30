fia_version <- function(fiadb) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, 'REF_FIADB_VERSION') |>
    filter(INSTALL_TYPE == 'RELEASE') |>
    arrange(desc(CREATED_DATE)) |>
    head(1) |>
    collect()
}

fia_read_table_11_5_17 <- function(filename) {
  read_csv(
    filename, 
    col_types = cols(JENKINS_SPGRPCD = col_integer())
  )
}
