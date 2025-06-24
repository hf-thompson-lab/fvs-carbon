# cfi_with_plot_info ------------------------------------------------------

cfi_with_plot_info <- function(.data, tblDWSPCFIPlotsComplete) {
  .data |>
    left_join(
      tblDWSPCFIPlotsComplete |>
        select(
          MasterPlotID, GPSLatitude, GPSLongitude,
          Slope, Aspect, TerrainPosition
        ),
      by = join_by(MasterPlotID)
    )
}

# cfi_with_visit_info -----------------------------------------------------

cfi_with_visit_info <- function(.data, tblDWSPCFIPlotVisitsComplete) {
  .data |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        select(MasterPlotVisitID, MasterPlotID, Watershed, VisitCycle, VisitYear),
      by = join_by(MasterPlotVisitID)
    )
}

# cfi_with_tree_info ------------------------------------------------------

cfi_with_tree_info <- function(.data, tblDWSPCFITreesComplete) {
  .data |>
    left_join(
      tblDWSPCFITreesComplete |>
        select(MasterTreeID, TreeNumber, SpeciesCode),
      by = join_by(MasterTreeID)
    )
}

# cfi_abp -------------------------------------------------------------
# Restrict rows to those in the ABP 2025 data
cfi_abp <- function(.data, cfiabp_trees) {
  .data |>
    inner_join(
      cfiabp_trees |>
        distinct(MasterPlotID, VisitCycle),
      by = join_by(MasterPlotID, VisitCycle)
    ) |>
    left_join(
      cfiabp_trees |>
        select(
          MasterPlotID, VisitCycle, MasterTreeID,
          StatusB, Status6, dbhcm, stems.ha.All, CutAnyTime, CutSinceLastVisit,
          Status.prior6, dbh_prior
        ),
      by = join_by(MasterPlotID, VisitCycle, MasterTreeID)
    )
}


# cfi_topocode ------------------------------------------------------------

# Convert CFI terrain position to FVS topocode
#
# CFI Terrain Position:
#  1 = Top of slope; convex region.
#  2 = Upper slope; convex region at upper edge of slope.
#  3 = Mid-slope; uniform, fairly straight region.
#  4 = Bench; area of level land with slopes above and below.
#  5 = Lower slope; concave region at the lower edge of slope.
#  6 = Bottomland; horizontal region in low-lying areas, may be subject to occasional flooding.
#  7 = Flatland; regions not part of or related to slopes; may have minimal elevation changes - less than 5% slope.
# FVS TOPOCODE, See EssentialFVS 5.4.1.2:
#  1 = bottom
#  2 = lower slope
#  3 = mid-slope
#  4 = upper slope
#  5 = ridge top
cfi_topocode <- function(.data) {
  .data |>
    mutate(TOPOCODE = case_match(
      TerrainPosition,
      1 ~ 5, # top of slope
      2 ~ 4, # upper slope
      3 ~ 3, # mid-slope
      4 ~ 3, # translate bench to mid-slope
      5 ~ 2, # lower slope
      6 ~ 1, # bottom
      7 ~ 1 # translate flatland to bottom
    ))
}


# cfi_status_live ---------------------------------------------------------
# Convert a VisitTreeStatusCode to a boolean, TRUE if the status means alive
#
# Tree status can be alive, dead, administrative, unknown, NA


# CFI TreeStatusCode:
# - 1 - Live
# - 2-3 - Dead
# - 4 - Out of Population
# - 5-9 - Ingrowth
# - 10 - Live
# - 11 - Dead
# - 12 - Cut
# - 13-14 - Administrative
# - 15 - Ingrowth
# - 16 - Live
# - 17-22 - Dead

cfi_status_live <- function(x) {
  x %in% c(1, 5:10, 15, 16)
}


# cfi_status_dead ---------------------------------------------------------

# Convert a VisitTreeStatusCode to a boolean, TRUE if the status means dead
# See cfi_status_live

cfi_status_dead <- function(x) {
  x %in% c(2, 3, 17:22)
}



# cfi_history -------------------------------------------------------------

# Convert CFI Status + ABP Status to FVS History
#
# ABP Status:
# - L = live now & at prior visit;
# - R = grew into 6" diameter class since last visit & live now;
# - D = live at prior visit but dead now;
# - C = live at prior visit but cut since last visit;
# - NA = tree that hadn't yet recruited into the 6" minimum or trees that were dead or cut in prior visit(s)
#
# FVS History:
# - Tree history codes of 0-5 are used to represent live tree records that are proejcted by FVS. FVS does not distinguish between the various live tree codes.
# - Tree history codes 6, 7, 8 and 9 indicates types of tree records that are not projected.
# - The codes 6 and 7 trees are assumed to have died during the mortality observation period. FVS makes no distinction between tree records coded with a tree history of 6 or 7.
# - The codes 8 and 9 represent trees that have been dead for longer periods of time. These records are included in the inventory list of trees but are not included in stand densities during calibration. FVS makes no distinction between tree records coded with a tree history of 8 or 9.
#
# (Essential FVS, 4.2.1 Sample Tree Data Description)

cfi_history <- function(.data) {
  .data |>
    group_by(MasterTreeID) |>
    arrange(VisitCycle) |>
    mutate(PrevTreeStatusCode = lag(VisitTreeStatusCode)) |>
    ungroup() |>
    mutate(
      HISTORY = case_when(
        cfi_status_live(VisitTreeStatusCode) ~ 1, # Live
        cfi_status_dead(VisitTreeStatusCode) &
          cfi_status_live(PrevTreeStatusCode) ~ 6, # Newly Dead
        cfi_status_dead(VisitTreeStatusCode) &
          cfi_status_dead(PrevTreeStatusCode) ~ 8 # Oldly Dead
      )
    )
}


# cfi_harvested -----------------------------------------------------------
# Try to follow the FIA definition of harvest:
# 2.5.44 TRTCD1
# ... The area affected by the treatment must be at least 1 acre in size.
# Sadly, we don't have access to the area affected.
# Appendix H: Damage Agent Codes and Thresholds
# 71000 Removal of 10% cubic volume.
# We likewise do not have volume, but can approximate this by using biomass.
#
# Input is plotvisittree records.
#
# Output is plotvisittree records with logical PlotVisitHarvested and PlotHarvested columns appended.
cfi_harvested <- function(.data) {
  .data |>
    group_by(MasterTreeID) |>
    arrange(VisitCycle) |>
    mutate(
      PreviousTreeStatusCode = lag(VisitTreeStatusCode),
      PreviousStatus6 = lag(Status6),
      PreviousTreeDIAM = lag(VisitTreeDIAM),
      PreviousDbhCM = lag(dbhcm)
    ) |>
    ungroup() |>
    filter(!is.na(PreviousTreeStatusCode) & !is.na(PreviousStatus6)) |>
    group_by(MasterPlotID, VisitCycle) |>
    summarize(
      # Live AGB prior to cutting = sum of AGB of trees that were previously live
      BA_Live = sum(if_else(
        PreviousStatus6 %in% c("R", "L"), # Live Tree
        pi * (PreviousDbhCM / 2)^2,
        0,
      ), na.rm = TRUE),
      # Cut AGB
      BA_Cut = sum(if_else(
        StatusB == "C",
        pi * (PreviousDbhCM / 2)^2,
        0
      ), na.rm = TRUE),
      Cut_Frac = BA_Cut / BA_Live,
      PlotVisitHarvested = BA_Cut / BA_Live >= 0.1,
      CutSinceLastVisit = any(CutSinceLastVisit, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    group_by(MasterPlotID) |>
    summarize(
      FIAHarvested = any(PlotVisitHarvested, na.rm = TRUE),
      CFIHarvested = any(CutSinceLastVisit, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup()
}

# cfi_disturbed -----------------------------------------------------------
# Return a data.frame of plots and whether they are dusturbed.
#
# Input is plotvisit or plotvisittree records.
#
# Output is plots with Disturbed column appended.
cfi_disturbed <- function(.data, tblDWSPCFIPlotVisitDisturbances) {
  rhs_visitcycle <- .data |>
    distinct(MasterPlotVisitID, VisitCycle, VisitYear)

  rhs_cfi_dstrb <- tblDWSPCFIPlotVisitDisturbances |>
    filter(
      # 0 - no disturbance
      # Others are harvest or harvest-related
      !PlotVisitDisturbanceCode %in% c(0, 8, 9, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22)
    ) |>
    mutate(PlotVisitDisturbanceSeverity = if_else(
      is.na(PlotVisitDisturbanceSeverity),
      3, # If disturbance severity was not recorded, use "Severe"
      PlotVisitDisturbanceSeverity
    )) |>
    inner_join(rhs_visitcycle, by = join_by(MasterPlotVisitID)) |>
    filter(
      # Visit cycles are 10 years; ignore disturbance that was recorded
      # in the previous visit cycle.
      PlotVisitDisturbanceYearCorrected > VisitYear - 10 &
        # 9999 is a flag value indicating NULL
        PlotVisitDisturbanceYearCorrected < 9999
    ) |>
    group_by(MasterPlotVisitID, VisitCycle) |>
    # disturbance severity 1 is lowest, 3 is highest
    summarize(
      PlotVisitDisturbanceSeverity = max(PlotVisitDisturbanceSeverity, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    mutate(
      CFIPlotVisitDisturbed = PlotVisitDisturbanceSeverity > 1
    )

  rhs_fia_dstrb <- .data |>
    group_by(MasterTreeID) |>
    arrange(VisitCycle) |>
    mutate(
      PreviousTreeStatusCode = lag(VisitTreeStatusCode),
      PreviousStatusB = lag(StatusB),
      PreviousTreeDIAM = lag(VisitTreeDIAM),
      PreviousStatus6 = lag(Status6),
      PreviousDbhCm = lag(dbhcm)
    ) |>
    ungroup() |>
    filter(!is.na(PreviousTreeStatusCode) & !is.na(PreviousStatus6)) |>
    group_by(MasterPlotID, VisitCycle) |>
    summarize(
      # Live Stems prior to cutting
      Stems_Live = sum(if_else(
        PreviousStatus6 %in% c("R", "L"), # Live Tree
        1,
        0,
      ), na.rm = TRUE),
      # Dead Stems
      Stems_Dead = sum(if_else(
        Status6 == "D",
        1,
        0
      ), na.rm = TRUE),
      PlotVisitDisturbed = Stems_Dead / Stems_Live >= 0.25,
      .groups = "keep"
    ) |>
    ungroup() |>
    group_by(MasterPlotID) |>
    summarize(
      FIADisturbed = any(PlotVisitDisturbed, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup()

  .data |>
    left_join(
      rhs_cfi_dstrb,
      by = join_by(MasterPlotVisitID, VisitCycle)
    ) |>
    group_by(MasterPlotID) |>
    summarize(CFIDisturbed = any(CFIPlotVisitDisturbed, na.rm = TRUE)) |>
    ungroup() |>
    left_join(
      rhs_fia_dstrb,
      by = join_by(MasterPlotID)
    )
}
