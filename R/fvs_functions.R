fvs_kwd0 <- function(kwd) {
  sprintf('%-10s', kwd)
}

fvs_kwd1 <- function(kwd, arg1) {
  arg1 <- as.character(arg1)
  sprintf('%-10s%10s', kwd, arg1)
}

fvs_kwd2 <- function(kwd, arg1, arg2) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  sprintf('%-10s%10s%10s', kwd, arg1, arg2)
}

fvs_kwd3 <- function(kwd, arg1, arg2, arg3) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  arg3 <- as.character(arg3)
  sprintf('%-10s%10s%10s%10s', kwd, arg1, arg2, arg3)
}

fvs_kwd4 <- function(kwd, arg1, arg2, arg3, arg4) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  arg3 <- as.character(arg3)
  arg4 <- as.character(arg4)
  sprintf('%-10s%10s%10s%10s%10s', kwd, arg1, arg2, arg3, arg4)
}

fvs_kwd5 <- function(kwd, arg1, arg2, arg3, arg4, arg5) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  arg3 <- as.character(arg3)
  arg4 <- as.character(arg4)
  arg5 <- as.character(arg5)
  sprintf('%-10s%10s%10s%10s%10s%10s', kwd, arg1, arg2, arg3, arg4, arg5)
}

fvs_kwd6 <- function(kwd, arg1, arg2, arg3, arg4, arg5, arg6) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  arg3 <- as.character(arg3)
  arg4 <- as.character(arg4)
  arg5 <- as.character(arg5)
  arg6 <- as.character(arg6)
  sprintf('%-10s%10s%10s%10s%10s%10s%10s', kwd, arg1, arg2, arg3, arg4, arg5, arg6)
}

fvs_kwd7 <- function(kwd, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
  arg1 <- as.character(arg1)
  arg2 <- as.character(arg2)
  arg3 <- as.character(arg3)
  arg4 <- as.character(arg4)
  arg5 <- as.character(arg5)
  arg6 <- as.character(arg6)
  arg7 <- as.character(arg7)
  sprintf('%-10s%10s%10s%10s%10s%10s%10s%10s', kwd, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}

fvs_TimeConfig <- function(FirstYear, LastYear, Timestep) {
  FirstYear <- as.integer(FirstYear)
  LastYear <- as.integer(LastYear)
  Timestep <- as.integer(Timestep)
  TimeConfig <- NULL
  if (FirstYear == LastYear) {
    # Produce a single one-year cycle
    TimeConfig <- c(
      fvs_kwd1("InvYear", FirstYear),
      fvs_kwd2("TimeInt", 0, 1),
      fvs_kwd1("NumCycle", 1)
    )      
  } else {
    # LastYear must be the first year of the last cycle, so
    # add an extra cycle at the end
    NumCycles <- as.integer((LastYear - FirstYear) / Timestep) + 1
    ShortCycle <- (LastYear - FirstYear) %% Timestep
    if (ShortCycle > 0) {
      # Produce a single short cycle followed by 10-year cycles
      NumCycles <- NumCycles + 1
      TimeConfig <- c(
        fvs_kwd1("InvYear", FirstYear),
        fvs_kwd2("TimeInt", 0, Timestep),
        fvs_kwd2("TimeInt", 1, ShortCycle),
        fvs_kwd2("TimeInt", NumCycles, 1),
        fvs_kwd1("NumCycle", NumCycles)
      )
    } else {
      # No need for an initial short cycle
      TimeConfig <- c(
        fvs_kwd1("InvYear", FirstYear),
        fvs_kwd2("TimeInt", 0, Timestep),
        fvs_kwd2("TimeInt", NumCycles, 1),
        fvs_kwd1("NumCycle", NumCycles)
      )
    }
  }
  return(TimeConfig)
}

fvs_Estab <- function(rows) {
  natural_regen <- function(row) {
    year <- 0
    species <- row["species"]
    density <- row["density"] # TPA
    survival <- 100 # percent
    age <- ''
    if ("height" %in% names(row)) {
      height <- row["height"]
    } else {
      height <- ''
    }
    shade <- 0
    fvs_kwd7("Natural", year, species, density, survival, age, height, shade)
  }
  if (!is.null(rows)) {
    c(
      fvs_kwd1("If", 0),
      fvs_kwd0("mod(cycle,1) eq 0"),
      fvs_kwd0("Then"),
      fvs_kwd1("Estab", 0),
      fvs_kwd2("MechPrep", 0, 0),
      fvs_kwd2("BurnPrep", 0, 0),
      fvs_kwd0("Sprout"),
      apply(rows, 1, natural_regen),
      fvs_kwd0("End"),
      fvs_kwd0("EndIf")
    )
  } else {
    c()
  }
}

fvsne_read_table_3_2_1 <- function(filename) {
  read_csv(
    filename,
    col_types = "iiciccc"
  )
}

# Given a dataframe with column STAND_CN, create a SQLite database
# of input stands and trees for FVS.
fvs_fia_input <- function(stands, fiadb, out_dir, title, mgmt_id) {
  fia <- dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(fia), add = TRUE, after = FALSE)
  
  out_file <- file.path(out_dir, paste0("FVS_", title, "_", mgmt_id, ".db"))
  out <- DBI::dbConnect(RSQLite::SQLite(), out_file, flags = SQLITE_RWC)
  on.exit(dbDisconnect(out), add = TRUE, after = FALSE)
  
  stand_cns <- stands |> distinct(STAND_CN)
  tables <- c("FVS_StandInit_Plot", "FVS_TreeInit_Plot")

  # In theory, we could ATTACH the output database and do this copy
  # internal to SQLite, but that is a bit tricky to manage so we
  # pull it into R and write it back out.
  lapply(tables, \(table){
    dbWriteTable(
      out,
      table,
      tbl(fia, table) |>
        semi_join(stand_cns, by = join_by(STAND_CN), copy = TRUE) |>
        collect(),
      overwrite = TRUE
    )
  })

  # Return the name of the file created
  out_file
}

fvs_keywordfile_section <- function(
    title,
    mgmt_id,
    stand_id,
    stand_cn,
    first_year,
    last_year,
    regen
) {
  c(
    fvs_kwd0("StdIdent"),
    paste0(stand_id, " ", title),
    fvs_kwd0("StandCN"),
    stand_cn,
    fvs_kwd0("MgmtId"),
    mgmt_id,
    fvs_TimeConfig(first_year, last_year, 10),
    fvs_kwd0("FMIn"), # Fire and Fuels Extension
    fvs_kwd0("CarbRept"),
    fvs_kwd0("CarbCut"),
    fvs_kwd5("CarbCalc", 1, 1, 0.0425, 9, 11),
    fvs_kwd0("FuelOut"),
    fvs_kwd0("FuelRept"),
    fvs_kwd0("End"), # FMIn
    fvs_kwd0("Database"), # Database extension
    fvs_kwd0("DSNIn"),
    paste0("FVS_", title, "_", mgmt_id, ".db"),
    fvs_kwd0("StandSQL"),
    "SELECT * FROM FVS_StandInit_Plot WHERE Stand_CN = '%Stand_CN%'",
    fvs_kwd0("EndSQL"), # StandSQL
    fvs_kwd0("TreeSQL"),
    "SELECT * FROM FVS_TreeInit_Plot WHERE Stand_CN = '%Stand_CN%'",
    fvs_kwd0("EndSQL"), # TreeSQL
    fvs_kwd0("DSNOut"),
    paste0("FVS_", title, "_", mgmt_id, "_Results.db"),
    fvs_kwd1("Summary",  2),
    fvs_kwd2("Computdb", 0, 1),
    fvs_kwd1("MisRpts",  2),
    fvs_kwd1("CarbReDB", 2),
    fvs_kwd1("FuelReDB", 2),
    fvs_kwd1("FuelsOut", 2),
    fvs_kwd0("End"), # Database
    fvs_Estab(regen),
    fvs_kwd0("Process")
  )
}


fvs_write_keyword_file <- function(filename, title, mgmt_id, stands, regen = NULL) {
  unlink(filename)
  apply(stands, 1, \(row) {
    write_lines(
      fvs_keywordfile_section(
        title,
        mgmt_id,
        row["stand_id"],
        row["stand_cn"],
        row["first_year"],
        row["last_year"],
        regen
      ),
      filename,
      append = TRUE
    )
  })
  write_lines("Stop", filename, append = TRUE)

  # The result is the name of the file written
  filename
}
