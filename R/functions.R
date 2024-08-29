northeastern_plots_filter <- function(.data) {
  # Region or Station code (RSCD) consolidated multiple regions
  # into region 24, Northeastern Research Station (NERS).
  .data |> filter(RSCD == 24)
}

modern_plots_filter <- function(.data) {
  # FIADB database description Appendix G describes plot designs.
  # DESIGNCD 1 is the modern plot design.
  # Many other plot designs may be compatible with DESIGNCD == 1,
  # but there are enough with DESIGNCD==1 that we can keep our lives simple.
  .data |> filter(DESIGNCD == 1)
}

long_measurement_filter <- function(.data) {
  # Only retain plots that have measurements a long time apart
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter((max(MEASYEAR, na.rm = TRUE) - min(MEASYEAR, na.rm = TRUE)) >= 10) |>
    ungroup()
}

forested_plots_filter <- function(.data) {
  # Condition status is forested
  # Filter out the entire plot if it was not forested for the entire period
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(COND_STATUS_CD, na.rm = TRUE) == 1) |>
    ungroup()
}

undisturbed_plots_filter <- function(.data) {
  # Filter out the entire plot if it was disturbed in any year
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      (is.na(max(DSTRBCD1, na.rm = TRUE)) | max(DSTRBCD1, na.rm = TRUE) == 0) & 
        (is.na(max(DSTRBCD2, na.rm = TRUE)) | max(DSTRBCD2, na.rm = TRUE) == 0) &
        (is.na(max(DSTRBCD3, na.rm = TRUE)) | max(DSTRBCD3, na.rm = TRUE) == 0)
    ) |>
    ungroup()
}

untreated_plots_filter <- function(.data) {
  # Filter out the entire plot if it was treated in any year
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      (is.na(max(TRTCD1, na.rm = TRUE)) | max(TRTCD1, na.rm = TRUE) == 0) &
        (is.na(max(TRTCD2, na.rm = TRUE)) | max(TRTCD2, na.rm = TRUE) == 0) &
        (is.na(max(TRTCD3, na.rm = TRUE)) | max(TRTCD3, na.rm = TRUE) == 0)
    ) |>
    ungroup()
}

single_condition_plots_filter <- function(.data) {
  # Filter out the entire plot if it ever had more than one condition
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(CONDID, na.rm = TRUE) == 1) |>
    ungroup()
}

has_trees_filter <- function(.data) {
  .data |>
    # Filter out inventory years with no BALIVE
    group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
    filter(
      sum(if_else(is.na(BALIVE), 0, BALIVE), na.rm = TRUE) > 0
    ) |>
    ungroup()
}

consolidate_forest_type_groups_filter <- function(.data) {
  # Consolidate a few rare forest type groups into a single 'Other' group:
  # Exotic hardwoods group
  # Exotic softwoods group
  # Other eastern softwoods group
  # Other hardwoods group
  .data |>
    mutate(`Forest Type Group` = 
             if_else(startsWith(`Forest Type Group`, 'Other'), 'Other',
                     if_else(startsWith(`Forest Type Group`, 'Exotic'), 'Other',
                             `Forest Type Group`
                     )
             )
    )
}
