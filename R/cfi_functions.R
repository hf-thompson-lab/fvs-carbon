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
          StatusB, stems.ha.All, CutAnyTime, CutSinceLastVisit
        ),
      by = join_by(MasterPlotID, VisitCycle, MasterTreeID)
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
      PreviousStatusB = lag(StatusB),
      PreviousTreeDIAM = lag(VisitTreeDIAM)
    ) |>
    ungroup() |>
    filter(!is.na(PreviousTreeStatusCode) & !is.na(PreviousStatusB)) |>
    group_by(MasterPlotID, VisitCycle) |>
    summarize(
      # Live AGB prior to cutting = sum of AGB of trees that were previously live
      BA_Live = sum(if_else(
        PreviousStatusB %in% c("R", "L"), # Live Tree
        pi * (PreviousTreeDIAM / 2)^2,
        0,
      ), na.rm = TRUE),
      # Cut AGB
      BA_Cut = sum(if_else(
        StatusB == "C",
        pi * (PreviousTreeDIAM / 2)^2,
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

  rhs_dstrb <- tblDWSPCFIPlotVisitDisturbances |>
    filter(
      !PlotVisitDisturbanceCode %in% c(0, 8, 9, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22)
    ) |>
    mutate(PlotVisitDisturbanceSeverity = if_else(
      is.na(PlotVisitDisturbanceSeverity),
      3, # If disturbance severity was not recorded, use "Severe"
      PlotVisitDisturbanceSeverity
    )) |>
    inner_join(rhs_visitcycle, by = join_by(MasterPlotVisitID)) |>
    filter(
      PlotVisitDisturbanceYearCorrected > VisitYear - 10 &
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
      PlotVisitDisturbed = PlotVisitDisturbanceSeverity > 1
    )

  .data |>
    left_join(
      rhs_dstrb,
      by = join_by(MasterPlotVisitID, VisitCycle)
    ) |>
    group_by(MasterPlotID) |>
    summarize(Disturbed = any(PlotVisitDisturbed, na.rm = TRUE)) |>
    ungroup()
}
