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
    species <- row["SPECIES"]
    density <- row["DENSITY"] # TPA
    survival <- 100 # percent
    age <- ''
    if ("height" %in% names(row)) {
      height <- row["HEIGHT"]
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

# Given a dataframe with column STAND_CN, create a SQLite database
# of input stands and trees for FVS.
fvs_fia_input <- function(stands, fiadb, filename) {
  fia <- dbConnect(RSQLite::SQLite(), fiadb, flags = SQLITE_RO)
  on.exit(dbDisconnect(fia), add = TRUE, after = FALSE)
  
  out <- DBI::dbConnect(RSQLite::SQLite(), filename, flags = SQLITE_RWC)
  on.exit(dbDisconnect(out), add = TRUE, after = FALSE)
  
  stand_cns <- stands |> distinct(STAND_CN)
  # StandInit_Plot / TreeInit_Plot - Used when a stand is a plot.
  # StandInit_Cond / TreeInit_Cond - Used when a stand is a condition.
  # PlotInit_Plot / TreeInit_Plot - Used when a stand is a subplot.
  # 
  # These tables are described in the FIA Data Quick Start guide.
  tables <- c(
    "FVS_PlotInit_Plot",
    "FVS_StandInit_Cond",
    "FVS_StandInit_Plot",
    "FVS_TreeInit_Cond",
    "FVS_TreeInit_Plot"
  )
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

  # Return the name of the file created / modified
  filename
}

fvs_keywordfile_section <- function(
    title,
    mgmt_id,
    input_db,
    output_db,
    stand_type,
    stand_id,
    stand_cn,
    first_year,
    last_year,
    regen
) {
  if (stand_type == "plot") {
    stand_table <- "FVS_StandInit_Plot"
    tree_table <- "FVS_TreeInit_Plot"
  } else if (stand_type == "cond") {
    stop("Only stand_type=\"plot\" is currenlty supported")
    # TODO nik: cond stand_id and stand_cn have cond appended
    stand_table <- "FVS_StandInit_Cond"
    tree_table <- "FVS_TreeInit_Cond"
  } else if (stand_type == "subplot") {
    stop("Only stand_type=\"plot\" is currenlty supported")
    # TODO nik: subplot stand_id and stand_cn have subp appended
    stand_table <- "FVS_PlotInit_Plot"
    tree_table <- "FVS_TreeInit_Plot"
  } else {
    stop(paste0("Unknown stand type: ", stand_type))
  }
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
    input_db,
    fvs_kwd0("StandSQL"),
    paste0("SELECT * FROM ", stand_table," WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd0("EndSQL"), # StandSQL
    fvs_kwd0("TreeSQL"),
    paste0("SELECT * FROM ", tree_table," WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd0("EndSQL"), # TreeSQL
    fvs_kwd0("DSNOut"),
    output_db,
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


fvs_write_keyword_file <- function(
  keyword_filename,
  input_db,
  output_db,
  stand_type,
  title,
  mgmt_id,
  stands,
  regen = NULL
) {
  unlink(keyword_filename)
  apply(stands, 1, \(row) {
    write_lines(
      fvs_keywordfile_section(
        title,
        mgmt_id,
        input_db,
        output_db,
        stand_type,
        row["STAND_ID"],
        row["STAND_CN"],
        row["FIRST_YEAR"],
        row["LAST_YEAR"],
        regen
      ),
      keyword_filename,
      append = TRUE
    )
  })
  write_lines("Stop", keyword_filename, append = TRUE)

  # The result is the name of the file written
  keyword_filename
}

fvs_run <- function(
    fvsbin_dir,
    fvs_variant,
    project_dir,
    fiadb,
    stand_type,
    title,
    mgmt_id,
    stands,
    regen
) {
  fvs_keyword_filename <- file.path(project_dir, paste0("FVS_", title, "_", mgmt_id, ".key"))
  if (file.exists(fvs_keyword_filename)) {
    unlink(fvs_keyword_filename)
  }
  
  fvs_input_db <- fvs_fia_input(stands, fiadb, file.path(project_dir, "FVS_Input.db"))
  fvs_output_db <- file.path(project_dir, "FVS_Output.db")
  if (file.exists(fvs_output_db)) {
    unlink(fvs_output_db)
  }
  
  # FVS will generate its output in a file with the same name as the keyword
  # file, but with the extension ".out"
  fvs_output_filename <- sub("\\.key$", ".out", fvs_keyword_filename)
  if (file.exists(fvs_output_filename)) {
    unlink(fvs_output_filename)
  }
  
  # Invented Here:
  # Write the exit status to a separate file so it can be read later
  fvs_error_filename <- sub("\\.key$", ".err", fvs_keyword_filename)
  if (file.exists(fvs_error_filename)) {
    unlink(fvs_error_filename)
  }
  
  fvs_write_keyword_file(
    fvs_keyword_filename,
    basename(fvs_input_db), # FVS will run in the same directory
    basename(fvs_output_db), # FVS will run in the same directory
    stand_type,
    title,
    mgmt_id,
    stands,
    regen
  )
  
  # Return the name of the keyword file and output database
  fvs <- processx::process$new(
    file.path(fvsbin_dir, fvs_variant),
    paste0("--keywordfile=", basename(fvs_keyword_filename)),
    wd = dirname(fvs_keyword_filename)
  )
  fvs$wait()
  write_lines(fvs$get_exit_status(), fvs_error_filename)
  
  # Give back three pieces of information:
  # 1 - a file containing the exit status
  # 2 - a file containing the textual output
  # 3 - a file containing the data output
  c(fvs_error_filename, fvs_output_filename, fvs_output_db)
}

fvs_read_output <- function(fvs_output_db, table) {
  out <- DBI::dbConnect(RSQLite::SQLite(), fvs_output_db, flags = SQLITE_RO)
  on.exit(dbDisconnect(out), add = TRUE, after = FALSE)

  tbl(out, table) |> collect()
}
