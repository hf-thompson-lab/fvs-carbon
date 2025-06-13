# cfi_with_visit_info -----------------------------------------------------

cfi_with_visit_info <- function(.data, tblDWSPCFIPlotVisitsComplete) {
  .data |>
    left_join(
      tblDWSPCFIPlotVisitsComplete |>
        select(MasterPlotVisitID, MasterPlotID, Watershed, VisitCycle, VisitYear) |>
        rename(VisitIDNumberDetail = MasterPlotVisitID),
      by = join_by(VisitIDNumberDetail)
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
# Mark rows as disturbed in each visit cycle, and the entire plot
# if it is ever disturbed.
#
# Input is plotvisit or plotvisittree records with disturbance.
#
# Output is the same, with PlotVisitDisturbed and PlotDisturbed columns appended.
cfi_disturbed <- function(.data) {
  .data |>
    group_by(MasterPlotID, VisitCycle) |>
    mutate(
      PlotVisitDisturbed =
      # > 0 means disturbed
        PlotVisitDisturbanceCode > 0 &
          # 10 means harvested; >10 is not currently used
          PlotVisitDisturbanceCode < 10 &
          # if severity is provided, only count severity > 1
          (is.na(PlotVisitDisturbanceSeverity) | PlotVisitDisturbanceSeverity > 1) &
          # if the disturbance was prior to the previous visit, it is tracked there,
          # so don't track it here.
          (PlotVisitDisturbandYearCorrected > VisitYear - 10) &
          # Year 9999 is a flag value
          (PlotVisitDisturbandYearCorrected < 9999)
    ) |>
    group_by(MasterPlotID) |>
    mutate(PlotDisturbed = any(PlotVisitDisturbed, na.rm = TRUE)) |>
    ungroup()
}
