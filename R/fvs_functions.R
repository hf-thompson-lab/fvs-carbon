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
    year <- row["YEAR"]
    species <- row["SPECIES"]
    density <- as.numeric(row["DENSITY"]) # TPA
    survival <- 100 # percent
    age <- ""
    if ("HEIGHT" %in% names(row)) {
      height <- row["HEIGHT"]
    } else {
      height <- ""
    }
    shade <- 0
    fvs_kwd("Natural", year, species, density, survival, age, height, shade)
  }
  if (!is.null(rows)) {
    # if YEAR isn't in rows, add it
    if (!"YEAR" %in% names(rows)) {
      rows["YEAR"] <- 0
    } else {
      rows$YEAR[is.na(rows$YEAR)] <- 0
    }

    # FVS has a limit of 1000 trees per tree record. Each establishment
    # keyword creates one tree record, so if there are more than 1000 trees
    # split it into multiple records.
    rows <- rows |>
      group_by() |>
      mutate(ROW_NUMBER = row_number()) |>
      ungroup() |>
      mutate(REPLICATES = trunc(DENSITY / 1000) + 1) |>
      uncount(REPLICATES, .remove = FALSE) |>
      group_by(ROW_NUMBER) |>
      mutate(DENSITY = case_when(
        REPLICATES == 1 ~ DENSITY,
        (REPLICATES > 1) & (row_number() == 1) ~ DENSITY %% 1000,
        (REPLICATES > 1) & (row_number() > 1) ~ 1000
      )) |>
      ungroup() |>
      select(-any_of(c("ROW_NUMBER", "REPLICATES")))
    if (any(rows$YEAR > 0)) {
      years <- rows |>
        filter(YEAR > 0) |>
        distinct(YEAR) |>
        _$YEAR
      scheduled <- lapply(years, \(year){
        c(
          fvs_kwd("Estab", year),
          fvs_kwd("MechPrep", 0, 0),
          fvs_kwd("BurnPrep", 0, 0),
          fvs_kwd("Sprout"),
          apply(rows |> filter(YEAR == year), 1, natural_regen),
          fvs_kwd("End")
        )
      })
    } else {
      scheduled <- c()
    }
    if (any(rows$YEAR == 0)) {
      background <- c(
        fvs_kwd("If", 0),
        fvs_kwd("mod(cycle,1) eq 0"),
        fvs_kwd("Then"),
        fvs_kwd("Estab", 0),
        fvs_kwd("MechPrep", 0, 0),
        fvs_kwd("BurnPrep", 0, 0),
        fvs_kwd("Sprout"),
        apply(rows |> filter(YEAR == 0), 1, natural_regen),
        fvs_kwd("End"),
        fvs_kwd("EndIf")
      )
    } else {
      background <- c()
    }
    c(scheduled, background, recursive = TRUE)
  } else {
    c()
  }
}

fvs_ThinDBH <- function(rows) {
  cycleat <- function(years) {
    lapply(years, \(year) fvs_kwd("CycleAt", year))
  }
  thindbh <- function(row) {
    year <- row["YEAR"]
    dbh_min <- row["DBH_MIN"]
    dbh_max <- row["DBH_MAX"]
    spcd <- row["SPCD"]
    tpa <- row["TPA"]
    ba <- row["BA"]
    if ("PERCENT" %in% names(row) & !is.na(row["PERCENT"])) {
      percent <- row["PERCENT"] # CUTEFF - Efficiency, 0 - 1
    } else {
      percent <- 1
    }
    # ThinDBH fields:
    # 1 - year
    # 2 - smallest dbh (>=; 0)
    # 3 - largest dbh (<; 999)
    # 4 - efficiency (0 - 1; 1)
    # 5 - species (0)
    # 6 - target tpa (0)
    # 7 - target BA (0)
    fvs_kwd("ThinDBH", year, dbh_min, dbh_max, percent, spcd, tpa, ba)
  }
  c(
    cycleat(unique(rows$YEAR)),
    apply(rows, 1, thindbh)
  )
}

fvs_ThinQFA <- function(rows) {
  cycleat <- function(years) {
    lapply(years, \(year) fvs_kwd("CycleAt", year))
  }
  thinqfa <- function(row) {
    year <- row["YEAR"]
    dbh_min <- row["DBH_MIN"]
    dbh_max <- row["DBH_MAX"]
    spcd <- row["SPCD"]
    if ("BA" %in% names(row) & !is.na(row["BA"])) {
      residual <- row["BA"]
      method <- 0
    } else if ("TPA" %in% names(row) & !is.na(row["TPA"])) {
      residual <- row["TPA"]
      method <- 1
    } else if ("SDI" %in% names(row) & !is.na(row["SDI"])) {
      residual <- row["SDI"]
      method <- 2
    } else {
      stop(paste("Can't determine units for ThinQFA residual density"))
    }
    # ThinQFA fields:
    # 1 - year
    # 2 - smallest dbh (>=; 0)
    # 3 - largest dbh (<; 999)
    # 4 - species (0)
    # 5 - q-factor (1.4)
    # 6 - diameter class width (2)
    # 7 - residual density (0)
    # Supplemental record: 0 = BA, 1 = TPA, 2 = SDI
    c(
      fvs_kwd("ThinQFA", year, dbh_min, dbh_max, spcd, 1.4, 2, residual),
      fvs_kwd(method) # supplemental record to specify units
    )
  }
  c(
    cycleat(unique(rows$YEAR)),
    apply(rows, 1, thinqfa)
  )
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
      fvs_kwd("CycleAt", year),
      fvs_kwd("ThinPRSC", year, percent, prsc)
    )
  }

  apply(rows |> distinct(PRESCRIPTION, YEAR, .keep_all = TRUE), 1, thinprsc)
}

# Figure out what kind of thinning to do and insert appropriate
# keywords
fvs_thin <- function(rows) {
  if (is.null(rows)) {
    c()
  } else if ("PRESCRIPTION" %in% names(rows)) {
    fvs_ThinPRSC(rows)
  } else if ("QFA" %in% names(rows)) {
    fvs_ThinQFA(rows)
  } else if ("DBH_MIN" %in% names(rows)) {
    fvs_ThinDBH(rows)
  } else {
    stop(paste("Unrecognized harvest schema:", names(rows)))
  }
}

# Given a dataframe with column STAND_CN, create a SQLite database
# of input stands and trees for FVS.
# > TODO nik: this should take FIA plots, conds, or subplots, and
# > Do The Right Thing. fvs_run() would need to take the same
# > input for stands.
fvs_fia_input <- function(
    fiadb,
    stands,
    trees,
    calibration,
    calib_mort,
    harvest,
    filename) {
  out <- DBI::dbConnect(RSQLite::SQLite(), filename, flags = RSQLite::SQLITE_RWC)
  on.exit(DBI::dbDisconnect(out), add = TRUE, after = FALSE)

  stand_cns <- stands |> distinct(STAND_CN)

  if (is.null(fiadb)) {
    # If the user provides the stands, do exactly as they say, except
    # we made them add FIRST_YEAR and LAST_YEAR
    standinit_plot <- stands |>
      select(!any_of(c("FIRST_YEAR", "LAST_YEAR")))
  } else {
    # Right now, only Plot is supported.
    # StandInit_Plot / TreeInit_Plot - Used when a stand is a plot.
    # StandInit_Cond / TreeInit_Cond - Used when a stand is a condition.
    # PlotInit_Plot / TreeInit_Plot - Used when a stand is a subplot.
    # In theory, we could ATTACH the output database and do this copy
    # internal to SQLite, but that is a bit tricky to manage so we
    # pull it into R and write it back out.
    standinit_plot <- fia_tbl(fiadb, "FVS_StandInit_Plot", \(.data, con) {
      .data |> inner_join(stand_cns, by = join_by(STAND_CN), copy = TRUE)
    })
  }

  DBI::dbWriteTable(
    out,
    "FVS_StandInit_Plot",
    standinit_plot,
    overwrite = TRUE
  )

  if (is.null(calibration)) {
    swizzle_growth <- function(.data) {
      .data
    }
  } else {
    swizzle_growth <- function(.data) {
      .data |>
        select(-any_of(c("DG", "HTG"))) |>
        left_join(
          calibration |> select(TREE_CN, DG, HTG),
          by = join_by(TREE_CN)
        )
    }
  }

  # If Harvest has prescription, swap out the prescription in the
  # schema and replace it with the one provided.
  if (is.null(harvest) | !"PRESCRIPTION" %in% names(harvest)) {
    append_prescription <- function(.data) {
      .data
    }
  } else {
    append_prescription <- function(.data) {
      .data |>
        select(-PRESCRIPTION) |>
        left_join(
          harvest |> select(TREE_CN, PRESCRIPTION),
          by = join_by(TREE_CN)
        )
    }
  }

  check_baf <- function(.data) {
    stopifnot(!any(.data$BASAL_AREA_FACTOR > 0, na.rm = TRUE))
    .data
  }

  if (is.null(calib_mort)) {
    filter_mort <- function(.data) {
      .data
    }
  } else {
    # filter_mort() will filter out the FIA provided mortality
    # and sub in the user provided mortality
    filter_mort <- function(.data) {
      bind_rows(
        .data |> filter(!HISTORY %in% 6:9),
        # Filter down to just the relevant mortality
        calib_mort |> inner_join(stand_cns, by = join_by(STAND_CN))
      )
    }
  }

  if (is.null(fiadb)) {
    treeinit_plot <- trees |>
      left_join(
        standinit_plot |>
          group_by(STAND_CN) |>
          filter(row_number() == 1) |>
          select(STAND_CN, BRK_DBH, BASAL_AREA_FACTOR, INV_PLOT_SIZE),
        by = join_by(STAND_CN)
      )
  } else {
    treeinit_plot <- fia_tbl(fiadb, "FVS_TreeInit_Plot", \(.data, con) {
      .data |>
        inner_join(stand_cns, by = join_by(STAND_CN), copy = TRUE) |>
        # Graft on TPA information
        left_join(
          tbl(con, "FVS_StandInit_Plot") |>
            select(STAND_CN, BRK_DBH, BASAL_AREA_FACTOR, INV_PLOT_SIZE),
          by = join_by(STAND_CN)
        )
    })
  }

  DBI::dbWriteTable(
    out,
    "FVS_TreeInit_Plot",
    # Read the FIA trees
    treeinit_plot |>
      # If calibration data is provided, replace the default growth data
      swizzle_growth() |>
      filter_mort() |>
      # Append the prescription column
      append_prescription() |>
      # Break up records representing more than 1000 trees
      # This is generally only an issue for trees on the microplot
      # where trees with DIAMETER < BRK_DBH have their TREE_COUNT
      # multiplied by INV_PLOT_SIZE to get TPA. INV_PLOT_SIZE
      # is 1/acre, which is 300 for a standard microplot, so any record
      # representing more than 3 1/3 trees will expand to more than 1000 TPA.
      # For non-microplot trees, BASAL_AREA_FACTOR is the multiplier to get
      # TPA; if BASAL_AREA_FACTOR < 0, then it is the negative of 1/acre for
      # a large-tree fixed area plot; if BASAL_AREA_FACTOR > 0 then it is
      # basal area factor for horizontal angle gauge in ft2/acre/tree.
      # This code does not currently handle horizontal angle gauge.
      check_baf() |>
      mutate(REPLICATES = case_when(
        (DIAMETER < BRK_DBH) & ((TREE_COUNT * INV_PLOT_SIZE) > 1000) ~
          trunc(TREE_COUNT * INV_PLOT_SIZE / 1000) + 1,
        (DIAMETER >= BRK_DBH) & ((TREE_COUNT * -BASAL_AREA_FACTOR) > 1000) ~
          trunc(TREE_COUNT * -BASAL_AREA_FACTOR / 1000) + 1,
        .default = 1
      )) |>
      uncount(REPLICATES, .remove = FALSE) |>
      group_by(TREE_CN) |>
      mutate(
        TREE_COUNT = case_when(
          (REPLICATES == 1) ~ TREE_COUNT,
          (DIAMETER < BRK_DBH) & (row_number() == 1) ~
            ((TREE_COUNT * INV_PLOT_SIZE) %% 1000) / INV_PLOT_SIZE,
          (DIAMETER < BRK_DBH) & (row_number() > 1) ~
            (1000 / INV_PLOT_SIZE),
          (DIAMETER >= BRK_DBH) & (row_number() == 1) ~
            ((TREE_COUNT * -BASAL_AREA_FACTOR) %% 1000) / -BASAL_AREA_FACTOR,
          (DIAMETER >= BRK_DBH) & (row_number() > 1) ~
            (1000 / -BASAL_AREA_FACTOR)
        )
      ) |>
      ungroup() |>
      # Create new TREE_IDs. We can try to be clever, but it's really not worth it.
      group_by(STAND_CN, PLOT_ID) |>
      arrange(TREE_ID, TREE_COUNT) |> # Maintain the same order
      mutate(TREE_ID = row_number()) |>
      ungroup() |>
      select(-any_of(c("BRK_DBH", "BASAL_AREA_FACTOR", "INV_PLOT_SIZE", "REPLICATES"))),
    overwrite = TRUE
  )

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
    calibration,
    calib_mort,
    calib_years,
    regen,
    harvest,
    carb_calc,
    random_seed) {
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

  # If calibration data is provided, we assume it is taken
  # from subsequent FIA measurement 5 years after the initial
  # stand inventory.
  if (!is.null(calibration)) {
    # Field 1: Diameter method
    # Field 2: Length of growth remeasurement (years)
    # Field 3: Height method
    # Field 4: Length of height remeasurement (years)
    # Field 5: Mortality observation period (years)
    # We could adjust Field 5 according to calib_mort, but
    # the default is 5 and the desired is 5, so no change.
    growth <- fvs_kwd("GROWTH", 3, calib_years, 3, calib_years, calib_years)
  } else {
    growth <- c()
  }

  if (carb_calc == "FFE") {
    cc_code <- 0
  } else if (carb_calc == "Jenkins") {
    cc_code <- 1
  } else {
    stop(paste0("Unknown carbon calculation method: ", carb_calc))
  }

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
    # GROWTH must proceed TREEDATA / TreeSQL
    # Since it's affiliated with time, we include it here
    growth,
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
    paste0("SELECT * FROM ", stand_table, " WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd("EndSQL"), # StandSQL
    fvs_kwd("TreeSQL"),
    paste0("SELECT * FROM ", tree_table, " WHERE Stand_CN = '%Stand_CN%'"),
    fvs_kwd("EndSQL"), # TreeSQL
    fvs_kwd("DSNOut"),
    output_db,
    fvs_kwd("ATrtLiDB", 2),
    fvs_kwd("CalbStDb", 2),
    fvs_kwd("CarbReDB", 2),
    fvs_kwd("Computdb", 0, 1),
    fvs_kwd("CutLiDB", 2),
    fvs_kwd("FuelReDB", 2),
    fvs_kwd("FuelsOut", 2),
    fvs_kwd("InvStats", 2),
    fvs_kwd("MisRpts", 2),
    fvs_kwd("RegRepts"),
    fvs_kwd("Summary", 2),
    fvs_kwd("TreeLiDB", 2),
    fvs_kwd("End"), # Database
    fvs_thin(harvest),
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
    calibration,
    calib_mort,
    calib_years,
    regen,
    harvest,
    carb_calc,
    random_seed) {
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
        calibration = calibration,
        calib_mort,
        calib_years,
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
    calibration = NULL,
    calib_mort = NULL,
    calib_years = 5,
    harvest = NULL,
    trees = NULL,
    regen = NULL,
    carb_calc = "Jenkins",
    num_partitions = NULL,
    partition = NULL,
    random_seed = NULL) {
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

    if (!is.null(harvest)) {
      harvest <- harvest |>
        filter((digest::digest2int(STAND_CN) %% num_partitions) == (partition - 1))
    }
  }

  fvs_input_db <- fvs_fia_input(
    fiadb,
    stands,
    trees,
    calibration,
    calib_mort,
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
    calibration = calibration,
    calib_mort = calib_mort,
    calib_years = calib_years,
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
    timestamp = Sys.time(),
    num_partitions = if (!is.null(num_partitions)) {
      num_partitions
    } else {
      NA
    },
    partition = if (!is.null(partition)) {
      partition
    } else {
      NA
    },
    random_seed = if (!is.null(random_seed)) {
      random_seed
    } else {
      NA
    },
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
      row <- fvs_output[n, ]
      output_db <- row[["output_db"]]
      con <- DBI::dbConnect(RSQLite::SQLite(), output_db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

      tbl(con, table) |>
        collect() |>
        cross_join(row) # Attach all the run metadata to each row
    })
  )
}

fvs_read_input <- function(fvs_output, table) {
  bind_rows(
    lapply(1:nrow(fvs_output), \(n) {
      row <- fvs_output[n, ]
      input_db <- row[["input_db"]]
      con <- DBI::dbConnect(RSQLite::SQLite(), input_db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

      tbl(con, table) |>
        collect() |>
        cross_join(row)
    })
  )
}


#' fvs_species_composition
#'
#' Retrieve the species composition (BA, TPA) of a stand over time.
#'
#' Output is square feet per acre / trees per acre.
#'
#' @param fvs_output
#'
#' @returns a data frame with StandID, Year, SpeciesFIA, BA, TPA
#' @export
#'
#' @examples
fvs_species_composition <- function(fvs_output) {
  bind_rows(
    lapply(1:nrow(fvs_output), \(n) {
      row <- fvs_output[n, ]
      output_db <- row[["output_db"]]
      con <- DBI::dbConnect(RSQLite::SQLite(), output_db, flags = RSQLite::SQLITE_RO)
      on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

      tbl(con, "FVS_TreeList_East") |>
        mutate(BA = TPA * pi * (DBH / 12 / 2)^2) |>
        group_by(StandID, Year, SpeciesFIA) |>
        summarize(BA = sum(BA, na.rm = TRUE), TPA = sum(TPA, na.rm = TRUE), .groups = "drop") |>
        collect() |>
        cross_join(row) # Attach all the run metadata to each row
    })
  )
}
