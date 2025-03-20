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
    inner_join(plots_modern, by = join_by(STATECD, COUNTYCD, PLOT))
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
    inner_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Note that we do not group by INVYR
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(COND_STATUS_CD, na.rm = TRUE) == 1) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    inner_join(plots_forested, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_undisturbed <- function(.data, con) {
  plots_undisturbed <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
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
    inner_join(plots_undisturbed, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_untreated <- function(.data, con) {
  plots_untreated <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
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
    inner_join(plots_untreated, by = join_by(STATECD, COUNTYCD, PLOT))
}

# Note that this is a POSITIVE filter:
# unlike other filters, it RETAINS plots that are harvested
filter_plots_harvested <- function(.data, con) {
  plots_harvested_bycond <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
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
  # contain trees that are harvested.
  # TREE.STATUSCD == 3 indicates that the tree was harvested
  # TREE.MORTYR may have the estimated year of harvest, but it's
  # very rarely populated (<<1%), so use MEASYEAR as an approximation
  # TREE.DSTRBCD1 says only "significant disturbance" is registered, and that
  # "significant disturbance" is disturbance that causes "mortality or
  # damage to 25 percent of the trees in the condition"
  # Testing confirms that when the number of stems (TPA_UNADJ) represented
  # by TREE.STATUSCD == 3 is >=25% of the pre-harvest number of stems on the
  # plot, the condition is marked as harvested.
  # To be consistent across all kinds of disturbance, we use the FIA
  # definitions for all disturbance, including harvest, and therefore
  # do NOT mark a plot as harvested if there are harvested trees.
  #plots_harvested_bytree <- tbl(con, "TREE") |>
  #  # Expectation is that this filter will come late enough in the chain
  #  # that it's more efficient to filter conditions prior to grouping
  #  inner_join(
  #    .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
  #    by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
  #  ) |>
  #  mutate(
  #    TPA_HRVST = if_else(STATUSCD == 3, TPA_UNADJ, NA)
  #  ) |>
  #  group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
  #  summarize(
  #    NUM_TREES = sum(TPA_UNADJ, na.rm = TRUE),
  #    NUM_HRVST = sum(TPA_HRVST, na.rm = TRUE),
  #    .groups = "keep"
  #  ) |>
  #  ungroup() |>
  #  # TREE.DSTRBCD1 says significant disturbance causes "mortality or
  #  # damage to 25 percent of the trees in the condition"
  #  filter(NUM_HRVST / NUM_TREES >= 0.25) |>
  #  distinct(STATECD, COUNTYCD, PLOT)
  #  
  #plots_harvested <- union(plots_harvested_bytree, plots_harvested_bycond)

  .data |>
    inner_join(plots_harvested_bycond, by = join_by(STATECD, COUNTYCD, PLOT))
}

# Note that this is a POSITIVE filter:
# unlike other filters, it RETAINS plots that are harvested exactly once
filter_plots_single_harvest <- function(.data, con) {
  plots_single_harvest <- tbl(con, "COND") |>
    inner_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Each condition can be harvested separately; gather
    # all they inventories in which any condition was harvested
    mutate(
      HARVEST_INVENTORY =
        if_else(!is.na(TRTCD1) & TRTCD1 == 10, INVYR,
          if_else(!is.na(TRTCD2) & TRTCD2 == 10, INVYR,
            if_else(!is.na(TRTCD3) & TRTCD3 == 10, INVYR, NA)
          )
        )
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    # count the number of distinct HARVEST_INVENTORY
    summarize(
      NUM_HARVEST = n_distinct(HARVEST_INVENTORY),
      .groups = "keep"
    ) |>
    ungroup() |>
    filter(NUM_HARVEST == 1)

  .data |>
    inner_join(plots_single_harvest, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_unfertilized <- function(.data, con) {
  # Filter out the entire plot if it was treated to encourage growth
  # TRTCD 30 - Artificial regeneration - planting or direct seeding
  # TRTCD 50 - Other silvicultural treatment - fertilizers, herbicides, etc.
  plots_unfertilized <- tbl(con, "COND") |>
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
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
    summarize(.groups = "keep") |>
    ungroup()

  .data |>
    inner_join(plots_unfertilized, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_measured_pre_post_harvest <- function(.data, con) {
  # Remove the entire plot if:
  # 1. The plot was not measured prior to the most recent harvest in the window,
  # and
  # 2. The plot was not measured 10 years after the most recent harvest.
  # Note that a single condition can have multiple harvest years,
  # meaning more than one of TRTCD1, TRTCD2 and TRTCD3 is 10, and
  # TRTYR1, TRTYR2 and TRTYR3 are different.
  # We want the latest of the latest harvests.
  plots_harvested_bycond <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    mutate(
      HRVYR1 = if_else(!is.na(TRTCD1) & (TRTCD1 == 10), TRTYR1, NA),
      HRVYR2 = if_else(!is.na(TRTCD2) & (TRTCD2 == 10), TRTYR2, NA),
      HRVYR3 = if_else(!is.na(TRTCD3) & (TRTCD3 == 10), TRTYR3, NA),
      MIN_HRVYR = coalesce(HRVYR1, HRVYR2, HRVYR3),
      MAX_HRVYR = coalesce(HRVYR3, HRVYR2, HRVYR1)
    ) |>
    filter(!is.na(MIN_HRVYR)) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    summarize(
      MIN_HRVYR = min(MIN_HRVYR, na.rm = TRUE),
      MAX_HRVYR = max(MAX_HRVYR, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup()
  
  # See note in filter_plots_harvested for why we no not look at
  # harvest at the tree level.
  #plots_harvested_bytree <- tbl(con, "TREE") |>
  #  # Expectation is that this filter will come late enough in the chain
  #  # that it's more efficient to filter conditions prior to grouping
  #  inner_join(
  #    .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
  #    by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
  #  ) |>
  #  left_join(
  #    tbl(con, "PLOT") |> select(CN, MEASYEAR) |> rename(PLT_CN = CN),
  #    by = join_by(PLT_CN)
  #  ) |>
  #  # TREE.STATUSCD == 3 indicates that the tree was harvested
  #  # TREE.MORTYR may have the estimated year of harvest, but it's
  #  # very rarely populated (<<1%), so use MEASYEAR as an approximation
  #  mutate(
  #    TPA_HRVST = if_else(STATUSCD == 3, 74.96528, NA), #TPA_UNADJ is NA for dead trees
  #    HRVYR     = if_else(STATUSCD == 3, MEASYEAR, NA)
  #  ) |>
  #  # Note that we do not group by INVYR
  #  group_by(STATECD, COUNTYCD, PLOT, INVYR) |>
  #  summarize(
  #    MIN_HRVYR = min(HRVYR, na.rm = TRUE),
  #    MAX_HRVYR = max(HRVYR, na.rm = TRUE),
  #    NUM_TREES = sum(TPA_UNADJ, na.rm = TRUE),
  #    NUM_HRVST = sum(TPA_HRVST, na.rm = TRUE),
  #    .groups = "keep"
  #  ) |>
  #  ungroup() |>
  #  # TREE.DSTRBCD1 says significant disturbance causes "mortality or
  #  # damage to 25 percent of the trees in the condition"
  #  filter(NUM_HRVST / (NUM_TREES + NUM_HRVST) >= 0.25) |>
  #  group_by(STATECD, COUNTYCD, PLOT) |>
  #  summarize(
  #    MIN_HRVYR = min(MIN_HRVYR, na.rm = TRUE),
  #    MAX_HRVYR = max(MAX_HRVYR, na.rm = TRUE),
  #    .groups = "keep"
  #  )

  #plots_harvested <- plots_harvested_bycond |>
  #  union_all(plots_harvested_bytree) |>
  #  group_by(STATECD, COUNTYCD, PLOT) |>
  #  summarize(
  #    MIN_HRVYR = min(MIN_HRVYR, na.rm = TRUE),
  #    MAX_HRVYR = max(MAX_HRVYR, na.rm = TRUE),
  #    .groups = "keep"
  #  ) |>
  #  ungroup()
  
  .data |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    mutate(
      MIN_MEASYEAR = min(MEASYEAR, na.rm = TRUE),
      MAX_MEASYEAR = max(MEASYEAR, na.rm = TRUE)
    ) |>
    ungroup() |>
    inner_join(plots_harvested_bycond, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    filter(
      (MIN_MEASYEAR < MIN_HRVYR) &
      (MAX_MEASYEAR > MAX_HRVYR + 10)
    )
}

filter_plots_single_cond <- function(.data, con) {
  plots_single_cond <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT, INVYR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(max(CONDID, na.rm = TRUE) == 1) |>
    summarize(.groups = "keep") |>
    ungroup()
  
  .data |>
    inner_join(plots_single_cond, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_trees <- function(.data, con) {
  plots_trees <- tbl(con, "COND") |> 
    # Expectation is that this filter will come late enough in the chain
    # that it's more efficient to filter conditions prior to grouping
    inner_join(
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
    inner_join(plots_trees, by = join_by(STATECD, COUNTYCD, PLOT))
}

filter_plots_ba_frac <- function(.data, con, spcds, frac) {
  plots_ba_frac <- tbl(con, "TREE") |>
    inner_join(
      .data |> distinct(STATECD, COUNTYCD, PLOT),
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    filter(STATUSCD == 1) |> # only live trees
    # Only consider the most recent survey of the plot
    group_by(STATECD, COUNTYCD, PLOT) |>
    filter(INVYR == max(INVYR, na.rm = TRUE)) |>
    mutate(BA_TOTAL = sum(6 * pi * DIA^2, na.rm = TRUE)) |>
    ungroup() |>
    inner_join(spcds |> distinct(SPCD), by = join_by(SPCD), copy = TRUE) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    summarize(
      BA_FVS = sum(6 * pi * DIA^2, na.rm = TRUE),
      BA_TOTAL = max(BA_TOTAL, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    mutate(BA_FVS_FRAC = BA_FVS / BA_TOTAL) |>
    filter(BA_FVS_FRAC >= frac) |>
    select(STATECD, COUNTYCD, PLOT)
  
  .data |>
    inner_join(plots_ba_frac, by = join_by(STATECD, COUNTYCD, PLOT))
}


#' Estimate Height for Establishment
#'
#' Given FIADB.TREE_GRM_COMPONENT records for INGROWTH,
#' fill in heights, either actual height for trees <= 2.95" DBH,
#' or estimated height at 3" DBH for larger trees.
#' 
#' Estimated height is taken from trees sampled from:
#' 1. The same species on the same plot;
#' 2. The same species in the ecosubregion;
#' 3. The same species in the ecoregion;
#' 4. The same species in all NRS-managed plots,
#' whichever first provides at least min_sample_size trees. If none does,
#' then height is taken from all NRS-managed plots with no minimum sample size.
#' 
#' Trees used for estimation are restricted to those with diameter between
#' min_sample_dia and max_sample_dia.
#'
#' @param estab dataframe of rows from FIADB.TREE_GRM_COMPONENT
#' @param fiadb path to SQLite_FIADB_ENTIRE.db
#' @param min_sample_size Minimum number of trees in sample
#' @param max_sample_size Maximum number of trees in sample
#' @param min_sample_dia Minimum diameter for trees sampled
#' @param max_sample_dia Maximum diameter for trees sampled
#'
#' @returns input data framw with added column ESTAB_HT
#' @export
#'
#' @examples
filter_estab_height <- function(
    estab,
    fiadb,
    min_sample_size = 3,
    max_sample_size = 9,
    min_sample_dia = 2.5,
    max_sample_dia = 6
) {
  plot_cns <- estab |> select(PLT_CN) |> rename(CN = PLT_CN)
  
  ecocd_mixin <- fia_plots_by_cn(fiadb, plot_cns) |>
    group_by(STATECD, COUNTYCD, PLOT) |>
    arrange(desc(INVYR)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(STATECD, COUNTYCD, PLOT, ECOSUBCD) |>
    mutate(ECOCD = substr(ECOSUBCD, 1, nchar(ECOSUBCD) - 1))

  plot_mixin <- fia_plots_by_cn(fiadb, plot_cns) |>
    select(CN, STATECD, COUNTYCD, PLOT, INVYR) |>
    rename(PLT_CN = CN)
  
  tmp_trees <- fia_trees(fiadb, plot_mixin)
  
  prev_tre_mixin <- fia_trees_by_cn(
    fiadb,
    tmp_trees |>
      select(PREV_TRE_CN) |>
      rename(CN = PREV_TRE_CN)
  ) |>
    select(CN, DIA, HT) |>
    rename(
      PREV_TRE_CN = CN,
      PREV_DIA = DIA,
      PREV_HT = HT
    )

  trees_for_ht_estimation <- tmp_trees |>
    left_join(prev_tre_mixin, by = join_by(PREV_TRE_CN)) |>
    # Overcome a data issue: some trees have no HT, but have PREV_HT
    mutate(HT = coalesce(HT, PREV_HT), DIA = coalesce(DIA, PREV_DIA)) |>
    filter(!is.na(HT) & !is.na(DIA)) |>
    filter(min_sample_dia < DIA & DIA < max_sample_dia) |>
    # Each tree will be in the data many times; take only the
    # record for each tree where the diameter is closest to 3" DBH
    group_by(STATECD, COUNTYCD, PLOT, SUBP, TREE) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() == 1) |>
    ungroup() |>
    left_join(ecocd_mixin, by = join_by(STATECD, COUNTYCD, PLOT))

  estab_height_plot <- trees_for_ht_estimation |>
    group_by(STATECD, COUNTYCD, PLOT, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_sample_size) |>
    mutate(HT_PLOT = HT * 3 / DIA) |>
    filter(n() >= min_sample_size) |>
    summarize(ESTAB_HT_PLOT = mean(HT_PLOT), .groups = "keep") |>
    ungroup()
  
  estab_height_ecosubcd <- trees_for_ht_estimation |>
    group_by(ECOSUBCD, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_sample_size) |>
    mutate(HT_ECOSUBCD = HT * 3 / DIA) |>
    filter(n() >= min_sample_size) |>
    summarize(ESTAB_HT_ECOSUBCD = mean(HT_ECOSUBCD), .groups = "keep") |>
    ungroup()
  
  estab_height_ecocd <- trees_for_ht_estimation |>
    group_by(ECOCD, SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_sample_size) |>
    mutate(HT_ECOCD = HT * 3 / DIA) |>
    filter(n() >= min_sample_size) |>
    summarize(ESTAB_HT_ECOCD = mean(HT_ECOCD), .groups = "keep") |>
    ungroup()
  
  estab_height_ne <- trees_for_ht_estimation |>
    group_by(SPCD) |>
    arrange(abs(DIA - 3)) |>
    filter(row_number() <= max_sample_size) |>
    mutate(HT_NE = HT * 3 / DIA) |>
    filter(n() >= min_sample_size) |>
    summarize(ESTAB_HT_NE = mean(HT_NE), .groups = "keep") |>
    ungroup()
  
  # height_catchall ignores the minimum number of trees,
  # and will produce values even for a single tree
  # anywhere in the region. It is a last-ditch catch-all.
  estab_height_catchall <- trees_for_ht_estimation |>
    group_by(SPCD) |>
    arrange(abs(DIA - 3)) |> # closest to 3" first
    filter(row_number() <= max_sample_size) |> 
    mutate(HT_CATCHALL = HT * 3 / DIA) |>
    summarize(ESTAB_HT_CATCHALL = mean(HT_CATCHALL)) |>
    ungroup()
  
  spcd_mixin <- fia_trees_by_cn(
    fiadb,
    estab |> select(TRE_CN) |> rename(CN = TRE_CN)
  ) |>
    select(CN, SPCD) |>
    rename(TRE_CN = CN)
  
  estab |>
    left_join(plot_mixin, by = join_by(PLT_CN)) |>
    left_join(ecocd_mixin, by = join_by(STATECD, COUNTYCD, PLOT)) |>
    left_join(spcd_mixin, by = join_by(TRE_CN)) |>
    left_join(estab_height_plot, by = join_by(STATECD, COUNTYCD, PLOT, SPCD)) |>
    left_join(estab_height_ecosubcd, by = join_by(ECOSUBCD, SPCD)) |>
    left_join(estab_height_ecocd, by = join_by(ECOCD, SPCD)) |>
    left_join(estab_height_ne, by = join_by(SPCD)) |>
    left_join(estab_height_catchall, by = join_by(SPCD)) |>
    mutate(
      HT_TREE_BEGIN = (DIA_BEGIN / ANN_DIA_GROWTH) * ANN_HT_GROWTH,
      HT_TREE_MIDPT = (DIA_MIDPT / ANN_DIA_GROWTH) * ANN_HT_GROWTH,
      HT_TREE_END = (DIA_END / ANN_DIA_GROWTH) * ANN_HT_GROWTH,
      ESTAB_HT_TREE = case_when(
        DIA_END <= 3 ~ HT_TREE_END,
        DIA_MIDPT <= 3 ~ HT_TREE_MIDPT,
        DIA_BEGIN <= 3 ~ HT_TREE_BEGIN
      ),
      ESTAB_HT = coalesce(
        ESTAB_HT_TREE,
        ESTAB_HT_PLOT,
        ESTAB_HT_ECOSUBCD,
        ESTAB_HT_ECOCD,
        ESTAB_HT_NE,
        ESTAB_HT_CATCHALL
      )
    )
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
