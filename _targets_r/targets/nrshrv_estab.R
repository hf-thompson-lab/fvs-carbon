tar_target(nrshrv_estab, {
  estab <- fia_grm_ingrowth(fiadb, nrshrv_plot)
  
  nrshrv_estab <- estab |>
    filter_fvs_spcd(fiadb, species_crosswalk) |> # Species handled by FVSne
    filter_estab_height(fiadb) |> # Height of 3" DBH trees
    mutate(
      SIZE_CLASS = if_else(DIA_END < 5, "sapling", "tree"),
      # TREE_GRM_COMPONENT will specify microplot TPAUNADJ for trees that hit
      # 5" DBH on the microplot, which is wrong. We just hard code appropriate
      # TAP for saplings vs. trees (assuming standard plot design).
      DENSITY = if_else(SIZE_CLASS == "sapling", 75, 6)
    ) |>
    group_by(STATECD, COUNTYCD, PLOT, INVYR, SPCD, SIZE_CLASS) |>
    summarize(
      HEIGHT = mean(ESTAB_HT, na.rm = TRUE),
      DENSITY = sum(DENSITY),
      .groups = "keep"
    ) |>
    ungroup() |>
    mutate(
      # Adjust height for small trees
      HEIGHT = if_else(SIZE_CLASS == "sapling", HEIGHT / 3, HEIGHT)
    ) |>
    left_join(
      nrshrv_plot |> select(STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    left_join(
      nrshrv_plot |>
        group_by(STATECD, COUNTYCD, PLOT) |>
        arrange(INVYR) |>
        filter(row_number() == 1) |>
        ungroup() |>
        select(STATECD, COUNTYCD, PLOT, CN),
      by = join_by(STATECD, COUNTYCD, PLOT)
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SPCD)
    ) |>
    # There are some (rather popular) Genus that FVSne doesn't handle, e.g. 934
    # Sorbus, the mountain-ash. We throw our hands up on these and cry a little,
    # but hopefully nobody notices as we remove them and carry on our way,
    # sniffling about a common and charismatic Genus native to the northeast
    # that can't be modeled in the northeast variant.
    filter(!is.na(FVS_SPCD)) |>
    mutate(
      STAND_CN = CN,
      YEAR = MEASYEAR,
      SPECIES = FVS_SPCD,
      HEIGHT = trunc(HEIGHT * 10) / 10 # nearest 1/10 foot
    ) |>
    select(STAND_CN, YEAR, SPECIES, DENSITY, HEIGHT, SIZE_CLASS)
})
