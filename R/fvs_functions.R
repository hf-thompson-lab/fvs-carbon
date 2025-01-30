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
  Estab <- c(
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
  return(Estab)
}

fvsne_read_table_3_2_1 <- function(filename) {
  read_csv(
    filename,
    col_types = "iiciccc"
  )
}
