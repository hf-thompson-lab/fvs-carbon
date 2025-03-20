fia_version <- function(fiadb) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "REF_FIADB_VERSION") |>
    filter(INSTALL_TYPE == "RELEASE") |>
    arrange(desc(CREATED_DATE)) |>
    head(1) |>
    collect()
}

fia_create_index_maybe <- function(con, tbl, col) {
  idx_name <- paste0("IDX_", tbl, "_", col)
  sql <- paste0(
    "CREATE INDEX IF NOT EXISTS ", idx_name, " ON ", tbl, " (", col, ")"
  )
  DBI::dbExecute(con, sql) > 0 # return TRUE if index created
}

fia_create_plot_indexes_maybe <- function(con, tbl, has_invyr) {
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

fia_cn_columns <- function(con, tbl) {
  # list the Control Number (CN) primary and foreign keys in a table
  cols <- DBI::dbListFields(con, tbl)
  cols[grepl("^CN$", cols) | grepl("_CN$", cols)]
}

fia_table_has_plot_cols <- function(con, tbl) {
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

fia_fiadb_indexed <- function() {
  # TODO: depend on the zip file
  # As it stands, this function is side-effect only, which means
  # targets has no way to track whether it's been run. We work around this
  # by not telling targets about the un-indexed database, and making indexing
  # the only way for targets to know about it.
  # Ideally, we'd have a three step pipeline here:
  # 1. The URL to fetch the database; targets can tell when the remote has
  #    been updated, and download only when necessary.
  # 2. Prepare the database: unzip the download, create indexes, and anything
  #    else we need to do to make the download practical to use.
  # 3. Notify targets that the database is in the appropriate files.
  fiadb <- "data/raw/SQLite_FIADB_ENTIRE.db"
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO) # not SQLITE_RC
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbls <- DBI::dbListTables(con)
  lapply(tbls, \(tbl) {
    lapply(fia_cn_columns(con, tbl), \(col) {
      fia_create_index_maybe(con, tbl, col)
    })
    plot_cols <- fia_table_has_plot_cols(con, tbl)
    if (plot_cols[1]) {
      fia_create_plot_indexes_maybe(con, tbl, plot_cols[2])
    }
  })
  fiadb
}

fia_plots <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  if ("INVYR" %in% names(plots)) {
    tbl(con, "PLOT") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT, INVYR),
        by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
        copy = TRUE
      ) |>
      collect()
  } else {
    tbl(con, "PLOT") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT),
        by = join_by(STATECD, COUNTYCD, PLOT),
        copy = TRUE
      ) |>
      collect()
  }
}

fia_plots_by_cn <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "PLOT") |>
    inner_join(plots |> distinct(CN), by = join_by(CN), copy = TRUE) |>
    collect()
}

fia_plots_filtered <- function(fiadb, plots = NULL, filter) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  if (is.null(plots)) {
    fia_plots <- tbl(con, "PLOT")
  } else if ("INVYR" %in% names(plots)) {
    fia_plots <- tbl(con, "PLOT") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT, INVYR),
        by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
        copy = TRUE
      )
  } else {
    fia_plots <- tbl(con, "PLOT") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT),
        by = join_by(STATECD, COUNTYCD, PLOT),
        copy = TRUE
      )
  }
  fia_plots |>
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
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT),
        by = join_by(STATECD, COUNTYCD, PLOT),
        copy = TRUE
      ) |>
      # Dereference forest type codes
      left_join(fia_ref_forest_type, by = join_by(FORTYPCD)) |>
      collect()
}

fia_trees <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  if ("INVYR" %in% names(plots)) {
    tbl(con, "TREE") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT, INVYR),
        by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
        copy = TRUE
      ) |>
      collect()
  } else {
    tbl(con, "TREE") |>
      inner_join(
        plots |> select(STATECD, COUNTYCD, PLOT),
        by = join_by(STATECD, COUNTYCD, PLOT),
        copy = TRUE
      ) |>
      collect()
  }
}

fia_trees_by_cn <- function(fiadb, cns) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  tbl(con, "TREE") |>
    inner_join(cns |> select(CN), by = join_by(CN), copy = TRUE) |>
    collect()
}

fia_trees_filtered <- function(fiadb, plots, filter) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  if (is.null(plots)) {
    fia_trees <- tbl(con, "TREE")
  } else if ("INVYR" %in% names(plots)) {
    fia_trees <- tbl(con, "TREE") |>
      inner_join(
        plots |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
        by = join_by(STATECD, COUNTYCD, PLOT, INVYR),
        copy = TRUE
      )
  } else {
    fia_trees <- tbl(con, "TREE") |>
      inner_join(
        plots |> distinct(STATECD, COUNTYCD, PLOT),
        by = join_by(STATECD, COUNTYCD, PLOT),
        copy = TRUE
      )
  }
  fia_trees |>
    filter(con) |>
    collect()
}

fia_grm_ingrowth <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

  tbl(con, "TREE_GRM_COMPONENT") |>
    inner_join(plots |> select(CN), by = join_by(PLT_CN == CN), copy = TRUE) |>
    filter(
      MICR_COMPONENT_AL_FOREST == 'INGROWTH' |
        SUBP_COMPONENT_AL_FOREST == 'INGROWTH'
    ) |>
    select(
      TRE_CN, PREV_TRE_CN, PLT_CN,
      DIA_BEGIN, DIA_MIDPT, DIA_END, ANN_DIA_GROWTH, ANN_HT_GROWTH,
      MICR_COMPONENT_AL_FOREST, MICR_TPAGROW_UNADJ_AL_FOREST,
      SUBP_COMPONENT_AL_FOREST, SUBP_TPAGROW_UNADJ_AL_FOREST
    ) |>
    collect()
}

fia_grm_mortality <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbl(con, "TREE_GRM_COMPONENT") |>
    inner_join(plots |> select(CN), by = join_by(PLT_CN == CN), copy = TRUE) |>
    filter(
      MICR_COMPONENT_AL_FOREST == 'MORTALITY1' |
        MICR_COMPONENT_AL_FOREST == 'MORTALITY2' |
        SUBP_COMPONENT_AL_FOREST == 'MORTALITY1' |
        SUBP_COMPONENT_AL_FOREST == 'MORTALITY2'
    ) |>
    select(
      TRE_CN, PREV_TRE_CN, PLT_CN,
      DIA_BEGIN, DIA_MIDPT, DIA_END, ANN_DIA_GROWTH, ANN_HT_GROWTH,
      MICR_COMPONENT_AL_FOREST, MICR_TPAGROW_UNADJ_AL_FOREST, MICR_TPAMORT_UNADJ_AL_FOREST,
      SUBP_COMPONENT_AL_FOREST, SUBP_TPAGROW_UNADJ_AL_FOREST, SUBP_TPAMORT_UNADJ_AL_FOREST
    ) |>
    collect()
}

fia_grm_estn <- function(fiadb, trees) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbl(con, "TREE_GRM_ESTN") |>
    inner_join(
      trees |>
        select(CN),
      by = join_by(TRE_CN == CN),
      copy = TRUE
    ) |>
    collect()
}

fia_seedlings <- function(fiadb, plots) {
  con <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  
  tbl(con, "SEEDLING") |>
    inner_join(plots |> select(CN), by = join_by(PLT_CN == CN), copy = TRUE) |>
    collect()
}
