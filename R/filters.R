filter_plots_fvsne <- function(.data, con) {
  # FIA.PLOTGEOM.FVS_VARIANT tells which FVS variant to use for a given plot.
  fia_plotgeom <- tbl(con, "PLOTGEOM") |>
    # Narrow and rename columns to facilitate join
    filter(FVS_VARIANT == "NE") |>
    select(CN, FVS_VARIANT)
  
  .data |>
    # inner_join will both filter and add column(s)
    inner_join(fia_plotgeom, by = join_by(CN))
}

filter_plots_ners <- function(.data, con) {
  # FIA.SURVEY.RCSD tells which research station administers a plot.
  fia_survey <- tbl(con, 'SURVEY') |>
    filter(RSCD == 24) |> # NERS has RSCD 24
    select(CN, RSCD) |>
    rename(SRV_CN = CN)
  .data |> 
    # inner_join will both filter and add column(s)
    inner_join(fia_survey, by = join_by(SRV_CN))
}

filter_plots_modern <- function(.data, con) {
  # FIADB database description Appendix G describes plot designs.
  # DESIGNCD 1 is the modern plot design.
  # Many other plot designs are compatible with DESIGNCD == 1
  #.data |> filter(
  #  (DESIGNCD == 1) |
  #  (DESIGNCD > 100 & DESIGNCD < 200) |
  #  (DESIGNCD >= 220 & DESIGNCD < 299) |
  #  (DESIGNCD > 300 & DESIGNCD < 325) |
  #  (DESIGNCD == 328) |
  #  (DESIGNCD > 500 & DESIGNCD < 550) |
  #  (DESIGNCD == 553) |
  #  (DESIGNCD == 554) |
  #  (DESIGNCD > 600 & DESIGNCD < 700)
  #)
  # ---
  # Simpler: since all plots were switched to the modern design
  # by 2005, then if the plot was part of the 2005 or later inventories,
  # it's modern.
  plots_modern <- tbl(con, "PLOT") |>
    filter(INVYR >= 2005) |>
    distinct(STATECD, COUNTYCD, PLOT)
  
  .data |>
    semi_join(plots_modern, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_long_measurement <- function(.data, con) {
  # Only retain plots that have measurements a long time apart
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter((max(MEASYEAR, na.rm = TRUE) - min(MEASYEAR, na.rm = TRUE)) >= 10) |>
    ungroup()
}

filter_plots_forested <- function(.data, con) {
  plots_forested <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(COND_STATUS_CD, na.rm = TRUE) == 1) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    semi_join(plots_forested, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_undisturbed <- function(.data, con) {
  plots_undisturbed <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      (is.na(max(DSTRBCD1, na.rm = TRUE)) | max(DSTRBCD1, na.rm = TRUE) == 0) & 
        (is.na(max(DSTRBCD2, na.rm = TRUE)) | max(DSTRBCD2, na.rm = TRUE) == 0) &
        (is.na(max(DSTRBCD3, na.rm = TRUE)) | max(DSTRBCD3, na.rm = TRUE) == 0)
    ) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    semi_join(plots_undisturbed, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_untreated <- function(.data, con) {
  plots_untreated <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      (is.na(max(TRTCD1, na.rm = TRUE)) | max(TRTCD1, na.rm = TRUE) == 0) &
        (is.na(max(TRTCD2, na.rm = TRUE)) | max(TRTCD2, na.rm = TRUE) == 0) &
        (is.na(max(TRTCD3, na.rm = TRUE)) | max(TRTCD3, na.rm = TRUE) == 0)
    ) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    semi_join(plots_untreated, by = join_by(STATECD, COUNTYCD, PLOT))
}

# Note that this is a POSITIVE filter:
# unlike other filters, it RETAINS plots that are harvested
filter_plots_harvested <- function(.data, con) {
  plots_harvested_bycond <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      sum(
        if_else(!is.na(TRTCD1) & TRTCD1 == 10, 1,
          if_else(!is.na(TRTCD2) & TRTCD2 == 10, 1,
            if_else(!is.na(TRTCD3) & TRTCD3 == 10, 1, 0)
          )
        ),
        na.rm = TRUE
      ) > 0
    ) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  # Some conditions are not marked as harvested, but they
  # contain trees that are harvested. We mark a plot as
  # harvested if it contains either conditions or trees that
  # are harvested.
  plots_harvested_bytree <- tbl(con, "TREE") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    filter(STATUSCD == 3) |> # STATUSCD == 3: Harvested
    # Note that we do not group by INVYR
    distinct(STATECD, COUNTYCD, PLOT)
  
  plots_harvested <- union(plots_harvested_bytree, plots_harvested_bycond)

  .data |>
    semi_join(plots_harvested, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_unfertilized <- function(.data, con) {
  # Filter out the entire plot if it was treated to encourage growth
  # TRTCD 30 - Artificial regeneration - planting or direct seeding
  # TRTCD 50 - Other silvicultural treatment - fertilizers, herbicides, etc.
  plots_unfertilized <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |> 
    #    filter(
    #      sum(
    #        if_else(!is.na(TRTCD1) & TRTCD1 == 30, 1,
    #          if_else(!is.na(TRTCD2) & TRTCD2 == 30, 1,
    #            if_else(!is.na(TRTCD3) & TRTCD3 == 30, 1, 0)
    #          )
    #        ),
    #        na.rm = TRUE
    #      ) == 0
    #    ) |>
    filter(
      sum(
        if_else(!is.na(TRTCD1) & TRTCD1 == 50, 1,
          if_else(!is.na(TRTCD2) & TRTCD2 == 50, 1,
            if_else(!is.na(TRTCD3) & TRTCD3 == 50, 1, 0)
          )
        ),
        na.rm = TRUE
      ) == 0
    ) |>
    ungroup()

  .data |>
    semi_join(plots_unfertilized, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_measured_pre_post_harvest <- function(.data, con) {
  # Remove the entire plot if:
  # 1. The plot was not measured prior to the earliest harvest in the window,
  # and
  # 2. The plot was not measured 10 years after the most recent harvest.
  # Note that a single condition can have multiple harvest years,
  # meaning more than one of TRTCD1, TRTCD2 and TRTCD3 is 10, and
  # TRTYR1, TRTYR2 and TRTYR3 are different.
  # We want the latest of the latest harvests.
  plots_harvested_bycond <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    mutate(
      MIN_HRVYR1 = min(if_else(!is.na(TRTCD1) & (TRTCD1 == 10), TRTYR1, 9999), na.rm = TRUE),
      MAX_HRVYR1 = max(if_else(!is.na(TRTCD1) & (TRTCD1 == 10), TRTYR1, 0), na.rm = TRUE),
      MIN_HRVYR2 = min(if_else(!is.na(TRTCD2) & (TRTCD2 == 10), TRTYR2, 9999), na.rm = TRUE),
      MAX_HRVYR2 = max(if_else(!is.na(TRTCD2) & (TRTCD2 == 10), TRTYR2, 0), na.rm = TRUE),
      MIN_HRVYR3 = min(if_else(!is.na(TRTCD3) & (TRTCD3 == 10), TRTYR3, 9999), na.rm = TRUE),
      MAX_HRVYR3 = max(if_else(!is.na(TRTCD3) & (TRTCD3 == 10), TRTYR3, 0), na.rm = TRUE)
    ) |>
    summarize(
      MIN_HRVYR_COND =
        if_else(
          MIN_HRVYR1 < MIN_HRVYR2,
          if_else(
            MIN_HRVYR1 < MIN_HRVYR3,
            MIN_HRVYR1,
            MIN_HRVYR3
          ),
          if_else(
            MIN_HRVYR2 < MIN_HRVYR3,
            MIN_HRVYR2,
            MIN_HRVYR3
          )
        ),
      MAX_HRVYR_COND =
        if_else(
          MAX_HRVYR1 > MAX_HRVYR2,
          if_else(
            MAX_HRVYR1 > MAX_HRVYR3,
            MAX_HRVYR1,
            MAX_HRVYR3
          ),
          if_else(
            MAX_HRVYR2 > MAX_HRVYR3,
            MAX_HRVYR2,
            MAX_HRVYR3
          )
        ),
      .groups = "keep"
    ) |> 
    ungroup() |>
    mutate(
      MIN_HRVYR_COND = if_else(is.na(MIN_HRVYR_COND), 9999, MIN_HRVYR_COND),
      MAX_HRVYR_COND = if_else(is.na(MAX_HRVYR_COND),    0, MAX_HRVYR_COND)
    ) |>
    select(STATECD, COUNTYCD, PLOT, MIN_HRVYR_COND, MAX_HRVYR_COND)
  
  plots_harvested_bytree <- tbl(con, "TREE") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    filter(STATUSCD == 3) |> # STATUSCD == 3: Harvested
    # TREE.MORTYR may have the estimated year of harvest, but it's
    # very rarely populated (<<1%), so use MEASYEAR as an approximation
    left_join(
      tbl(con, "PLOT") |> select(CN, MEASYEAR) |> rename(PLT_CN = CN),
      by = join_by(PLT_CN)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    summarize(
      MIN_HRVYR_TREE = min(MEASYEAR, na.rm = TRUE),
      MAX_HRVYR_TREE = max(MEASYEAR, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    mutate(
      MIN_HRVYR_TREE = if_else(is.na(MIN_HRVYR_TREE), 9999, MIN_HRVYR_TREE),
      MAX_HRVYR_TREE = if_else(is.na(MAX_HRVYR_TREE),    0, MAX_HRVYR_TREE)
    ) |>
    select(STATECD, COUNTYCD, PLOT, MIN_HRVYR_TREE, MAX_HRVYR_TREE)
  
  plots_harvested <- plots_harvested_bycond |>
    full_join(
      plots_harvested_bytree,
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    mutate(
      MIN_HRVYR = if_else(MIN_HRVYR_COND < MIN_HRVYR_TREE, MIN_HRVYR_COND, MIN_HRVYR_TREE),
      MAX_HRVYR = if_else(MAX_HRVYR_COND > MAX_HRVYR_TREE, MAX_HRVYR_COND, MAX_HRVYR_TREE)
    ) |>
    select(STATECD, COUNTYCD, PLOT, MIN_HRVYR, MAX_HRVYR)
  
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    mutate(
      MIN_MEASYEAR = min(MEASYEAR, na.rm = TRUE),
      MAX_MEASYEAR = max(MEASYEAR, na.rm = TRUE)
    ) |>
    ungroup() |>
    left_join(plots_harvested, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    filter(
      (MIN_MEASYEAR < MIN_HRVYR) &
      (MAX_MEASYEAR > MAX_HRVYR + 10)
    )
}

filter_plots_single_cond <- function(.data, con) {
  plots_single_cond <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(CONDID, na.rm = TRUE) == 1) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    semi_join(plots_single_cond, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_trees <- function(.data, con) {
  plots_trees <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    semi_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(
      sum(if_else(is.na(BALIVE) | (BALIVE == 0), 1, 0), na.rm = TRUE) == 0
    ) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    semi_join(plots_trees, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_ba_frac <- function(.data, con, spcds, frac) {
  plots_ba_frac <- tbl(con, "TREE") |>
    semi_join(.data |> distinct(STATECD, COUNTYCD, PLOT), by = join_by(STATECD, COUNTYCD, PLOT)) |>
    filter(STATUSCD == 1) |> # only live trees
    # Only consider the most recent survey of the plot
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(INVYR == max(INVYR, na.rm = TRUE)) |>
    mutate(BA_TOTAL = sum(6 * pi * DIA^2, na.rm = TRUE)) |>
    ungroup() |>
    semi_join(spcds |> distinct(SPCD), by = join_by(SPCD), copy = TRUE) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    summarize(
      BA_FVS = sum(6 * pi * DIA^2, na.rm = TRUE),
      BA_TOTAL = max(BA_TOTAL, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    mutate(BA_FVS_FRAC = BA_FVS / BA_TOTAL) |>
    filter(BA_FVS_FRAC >= frac)
  
  .data |>
    semi_join(plots_ba_frac, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_decode_forest_type_group <- function(.data) {
  # Consolidate a few rare forest type groups into a single 'Other' group:
  # Exotic hardwoods group
  # Exotic softwoods group
  # Other eastern softwoods group
  # Other hardwoods group
  .data |>
    mutate(FOREST_TYPE_GROUP = case_when(
      startsWith(FOREST_TYPE_GROUP, "Other") ~ "Other",
      startsWith(FOREST_TYPE_GROUP, "Exotic") ~ "Other",
      .default = str_replace(FOREST_TYPE_GROUP, ' group', '')
    ))
}

filter_decode_large_end_diameter_class <- function(.data) {
  .data |>
    mutate(
      LARGE_END_DIA_CLASS = case_when(
        DIA < 3 ~ "0.0 - 2.9",
        DIA < 5 ~ "3.0 - 4.9",
        DIA < 9 ~ "5.0 - 8.9",
        DIA < 15 ~ "9.0 - 14.9",
        DIA < 21 ~ "15.0 - 20.9",
        DIA < 40 ~ "21.0 - 39.9",
        .default = "40.0 +"
      )
    )
}

filter_decode_cclcd <- function(.data) {
  .data |>
    mutate(
      CCL = case_when(
        CCLCD == 1 ~"Open grown",
        CCLCD == 2 ~"Dominant",
        CCLCD == 3 ~"Codominant",
        CCLCD == 4 ~"Intermediate",
        CCLCD == 5 ~"Overtopped"
      )
    )
}
