fvs_kwd <- function(kwd, ...) {
  paste0(
    sprintf("%-10s", kwd),
    paste0(
      lapply(
        list(...),
        \(arg) sprintf("%10s", as.character(arg))
      ),
      collapse = ""
    )
  )
}

fvs_TimeConfig <- function(FirstYear, LastYear, Timestep) {
  FirstYear <- as.integer(FirstYear)
  LastYear <- as.integer(LastYear)
  Timestep <- as.integer(Timestep)
  TimeConfig <- NULL
  if (FirstYear == LastYear) {
    # Produce a single one-year cycle
    TimeConfig <- c(
      fvs_kwd("InvYear", FirstYear),
      fvs_kwd("TimeInt", 0, 1),
      fvs_kwd("NumCycle", 1)
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
        fvs_kwd("InvYear", FirstYear),
        fvs_kwd("TimeInt", 0, Timestep),
        fvs_kwd("TimeInt", 1, ShortCycle),
        fvs_kwd("TimeInt", NumCycles, 1),
        fvs_kwd("NumCycle", NumCycles)
      )
    } else {
      # No need for an initial short cycle
      TimeConfig <- c(
        fvs_kwd("InvYear", FirstYear),
        fvs_kwd("TimeInt", 0, Timestep),
        fvs_kwd("TimeInt", NumCycles, 1),
        fvs_kwd("NumCycle", NumCycles)
      )
    }
  }
  return(TimeConfig)
}

fvs_Estab <- function(rows) {
  natural_regen <- function(row) {
    year <- 0
    species <- row["SPECIES"]
    density <- as.numeric(row["DENSITY"]) # TPA
    survival <- 100 # percent
    age <- ''
    if ("HEIGHT" %in% names(row)) {
      height <- row["HEIGHT"]
    } else {
      height <- ''
    }
    shade <- 0
    #    result <- c()
    # FVS has a limit of 1000 trees per tree record. Each establishment
    # keyword creates one tree record, so if there are more than 1000 trees
    # split it into multiple records.
#    while (density > 1000) {
#      result <- append(result, fvs_kwd("Natural", year, species, 1000, survival, age, height, shade))
#      density <- density - 1000
#    }
#    append(result, fvs_kwd("Natural", year, species, density, survival, age, height, shade))
    fvs_kwd("Natural", year, species, density, survival, age, height, shade)
  }
  if (!is.null(rows)) {
    c(
      fvs_kwd("If", 0),
      fvs_kwd("mod(cycle,1) eq 0"),
      fvs_kwd("Then"),
      fvs_kwd("Estab", 0),
      fvs_kwd("MechPrep", 0, 0),
      fvs_kwd("BurnPrep", 0, 0),
      fvs_kwd("Sprout"),
      apply(rows, 1, natural_regen),
      fvs_kwd("End"),
      fvs_kwd("EndIf")
    )
  } else {
    c()
  }
}

fvs_ThinPRSC <- function(rows) {
  thinprsc <- function(row) {
    year <- row["YEAR"]
    if ("PERCENT" %in% names(row) & !is.na(row["PERCENT"])) {
      percent <- row["PERCENT"] # CUTEFF - Efficiency, 0 - 1
    } else {
      percent <- 1
    }
    prsc <- row["PRESCRIPTION"]
    c(
      fvs_kwd("CycleAt", as.numeric(year) - 1), # harvest is done in the first cycle year
      fvs_kwd("ThinPRSC", year, percent, prsc)
    )
  }

  if (!is.null(rows)) {
    apply(rows |> distinct(PRESCRIPTION, YEAR, .keep_all = TRUE), 1, thinprsc)
  } else {
    c()
  }
}

# Given a dataframe with column STAND_CN, create a SQLite database
# of input stands and trees for FVS.
# > TODO nik: this should take FIA plots, conds, or subplots, and
# > Do The Right Thing. fvs_run() would need to take the same
# > input for stands.
fvs_fia_input <- function(fiadb, stands, harvest, filename) {
  fia <- DBI::dbConnect(RSQLite::SQLite(), fiadb, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(fia), add = TRUE, after = FALSE)
  
  out <- DBI::dbConnect(RSQLite::SQLite(), filename, flags = RSQLite::SQLITE_RWC)
  on.exit(DBI::dbDisconnect(out), add = TRUE, after = FALSE)
  
  stand_cns <- stands |> distinct(STAND_CN)
  # StandInit_Plot / TreeInit_Plot - Used when a stand is a plot.
  # StandInit_Cond / TreeInit_Cond - Used when a stand is a condition.
  # PlotInit_Plot / TreeInit_Plot - Used when a stand is a subplot.
  # 
  # These tables are described in the FIA Data Quick Start guide.
  if (is.null(harvest)) {
    tables <- c(
      "FVS_PlotInit_Plot",
      "FVS_StandInit_Cond",
      "FVS_StandInit_Plot",
      "FVS_TreeInit_Cond",
      "FVS_TreeInit_Plot"
    )
    tree_tables <- c()
  } else {
    tables <- c(
      "FVS_PlotInit_Plot",
      "FVS_StandInit_Cond",
      "FVS_StandInit_Plot"
    )
    tree_tables <- c(
      "FVS_TreeInit_Cond",
      "FVS_TreeInit_Plot"
    )
  }
  
  # In theory, we could ATTACH the output database and do this copy
  # internal to SQLite, but that is a bit tricky to manage so we
  # pull it into R and write it back out.
  lapply(tables, \(table){
    DBI::dbWriteTable(
      out,
      table,
      tbl(fia, table) |>
        inner_join(
          stand_cns |> select(STAND_CN),
          by = join_by(STAND_CN),
          copy = TRUE
        ) |>
        collect(),
      overwrite = TRUE
    )
  })
  
  lapply(tree_tables, \(table){
    DBI::dbWriteTable(
      out,
      table,
      tbl(fia, table) |>
        inner_join(
          stand_cns |> select(STAND_CN),
          by = join_by(STAND_CN),
          copy = TRUE
        ) |>
        collect() |>
        select(!PRESCRIPTION) |>
        left_join(
          harvest |> select(TREE_CN, PRESCRIPTION),
          by = join_by(TREE_CN),
          copy = TRUE
        ),
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
    stand_id,
    stand_cn,
    first_year,
    last_year,
    regen,
    harvest,
    carb_calc,
    random_seed
) {
  stand_table <- "FVS_StandInit_Plot"
  tree_table <- "FVS_TreeInit_Plot"
  # for cond, we would use:
  #   stand_table <- "FVS_StandInit_Cond"
  #   tree_table <- "FVS_TreeInit_Cond"
  # but need to match StandPlot_CN instead of STAND_CN
  # for subplot we would use:
  #   stand_table <- "FVS_PlotInit_Plot"
  #   tree_table <- "FVS_TreeInit_Plot"
  # but need to match StandPlot_CN instead of STAND_CN
  
  if (carb_calc == "FFE") { cc_code = 0 }
  else if (carb_calc == "Jenkins") { cc_code = 1 }
  else { stop(paste0("Unknown carbon calculation method: ", carb_calc)) }

  # Remove any regen destined for other plots
  if (!is.null(regen) & ("STAND_CN" %in% names(regen))) {
    regen <- bind_rows(
      regen |> filter(STAND_CN == stand_cn), # regen for this stand
      regen |> filter(is.na(STAND_CN)) # regen for all stands
    )
  }
  
  # Set up thinning
  if (!is.null(harvest) & ("STAND_CN" %in% names(harvest))) {
    harvest <- bind_rows(
      harvest |> filter(STAND_CN == stand_cn), # harvest for this stand
      harvest |> filter(is.na(STAND_CN)) # harvest for all stands
    )
  }
  
  if (!is.null(random_seed)) {
    RannSeed <- fvs_kwd("RanNSeed", random_seed)
  } else {
    RannSeed <- c()
  }
  
  c(
    fvs_kwd("StdIdent"),
    paste0(stand_id, " ", title),
    fvs_kwd("StandCN"),
    stand_cn,
    fvs_kwd("MgmtId"),
    mgmt_id,
    fvs_TimeConfig(first_year, last_year, 10),
    RannSeed,
    fvs_kwd("Stats"),
    fvs_kwd("CutList", 0),
    fvs_kwd("ATrtList", 0),
    fvs_kwd("TreeList", 0),
    fvs_kwd("FMIn"), # Fire and Fuels Extension
    fvs_kwd("CarbRept"),
    fvs_kwd("CarbCut"),
    fvs_kwd("CarbCalc", cc_code, 1),
    fvs_kwd("FuelOut"),
    fvs_kwd("FuelRept"),
    fvs_kwd("End"), # FMIn
    fvs_kwd("Database"), # Database extension
    fvs_kwd("DSNIn"),
    input_db,
    fvs_kwd("StandSQL"),
    paste0("SELECT * FROM ", stand_table," WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd("EndSQL"), # StandSQL
    fvs_kwd("TreeSQL"),
    paste0("SELECT * FROM ", tree_table," WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd("EndSQL"), # TreeSQL
    fvs_kwd("DSNOut"),
    output_db,
    fvs_kwd("ATrtLiDB", 2),
    fvs_kwd("CalbStDb", 2),
    fvs_kwd("CarbReDB", 2),
    fvs_kwd("Computdb", 0, 1),
    fvs_kwd("CutLiDB",  2),
    fvs_kwd("FuelReDB", 2),
    fvs_kwd("FuelsOut", 2),
    fvs_kwd("InvStats", 2),
    fvs_kwd("MisRpts",  2),
    fvs_kwd("RegRepts"),
    fvs_kwd("Summary",  2),
    fvs_kwd("TreeLiDB", 2),
    fvs_kwd("End"), # Database
    fvs_ThinPRSC(harvest),
    fvs_Estab(regen),
    fvs_kwd("Process")
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
  regen,
  harvest,
  carb_calc,
  random_seed
) {
  unlink(keyword_filename)
  apply(stands, 1, \(row) {
    write_lines(
      fvs_keywordfile_section(
        title = title,
        mgmt_id = mgmt_id,
        input_db = input_db,
        output_db = output_db,
        stand_id = row["STAND_ID"],
        stand_cn = row["STAND_CN"],
        first_year = row["FIRST_YEAR"],
        last_year = row["LAST_YEAR"],
        regen = regen,
        harvest = harvest,
        carb_calc = carb_calc,
        random_seed = random_seed
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
    title,
    mgmt_id,
    stands,
    harvest = NULL,
    regen = NULL,
    carb_calc = "Jenkins",
    num_partitions = NULL,
    partition = NULL,
    random_seed = NULL
) {
  # We need to separate inputs and outputs for each partition and each replica
  # Each partition will process a different subset of records;
  # Each replica will process using a different random seed
  file_basename <- paste0(
    "FVS_", title,
    "_", mgmt_id,
    if (!is.null(partition)) {
      paste0("_P", partition)
    },
    if (!is.null(random_seed)) {
      paste0("_R", random_seed)
    }
  )
  
  fvs_keyword_filename <- file.path(project_dir, paste0(file_basename, ".key"))
  if (file.exists(fvs_keyword_filename)) {
    unlink(fvs_keyword_filename)
  }
  
  if (!is.null(partition)) {
    stands <- stands |>
      filter((digest::digest2int(STAND_CN) %% num_partitions) == (partition - 1))
  }
  
  if (!is.null(harvest)) {
    harvest <- harvest |>
      filter((digest::digest2int(STAND_CN) %% num_partitions) == (partition - 1))
  }
  
  fvs_input_db <- fvs_fia_input(
    fiadb,
    stands,
    harvest,
    file.path(project_dir, paste0(file_basename, "_Input.db"))
  )

  fvs_output_db <- file.path(project_dir, paste0(file_basename, "_Output.db"))
  if (file.exists(fvs_output_db)) {
    unlink(fvs_output_db)
  }
  
  # FVS will generate its output in a file with the same name as the keyword
  # file, but with the extension ".out"
  fvs_output_filename <- sub("\\.key$", ".out", fvs_keyword_filename)
  if (file.exists(fvs_output_filename)) {
    unlink(fvs_output_filename)
  }
  
  fvs_write_keyword_file(
    keyword_filename = fvs_keyword_filename,
    input_db = basename(fvs_input_db), # FVS will run in the same directory
    output_db = basename(fvs_output_db), # FVS will run in the same directory
    title = title,
    mgmt_id = mgmt_id,
    stands = stands,
    regen = regen,
    harvest = harvest,
    carb_calc = carb_calc,
    random_seed = random_seed
  )
  
  # Return the name of the keyword file and output database
  fvs <- processx::process$new(
    file.path(fvsbin_dir, fvs_variant),
    paste0("--keywordfile=", basename(fvs_keyword_filename)),
    wd = dirname(fvs_keyword_filename)
  )
  fvs$wait()

  tibble::tibble(
    title = title,
    mgmt_id = mgmt_id,
    num_partitions = if (!is.null(num_partitions)) {num_partitions} else {NA},
    partition = if (!is.null(partition)) {partition} else {NA},
    random_seed = if (!is.null(random_seed)) {random_seed} else {NA},
    exit_status = fvs$get_exit_status(),
    output = fvs_output_filename,
    keyword_file = fvs_keyword_filename,
    input_db = fvs_input_db,
    output_db = fvs_output_db
  )
}

fvs_read_output <- function(fvs_output, table) {
  bind_rows(
    lapply(1:nrow(fvs_output), \(n) {
      row <- fvs_output[n,]
      output_db <- row[["output_db"]]
      con <- DBI::dbConnect(RSQLite::SQLite(), output_db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
      
      tbl(con, table) |>
        collect() |>
        cross_join(row) # Attach all the run metadata to each row
    })
  )
}
