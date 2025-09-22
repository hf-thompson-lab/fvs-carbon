tar_target(cficop_dflt_estab, {
  tibble(
    Species = c(
      "White pine", "Hemlock", "Spruce", "Red Pine",
      "Sugar Maple", "Red maple", "Oak", "Black birch",
      "Other birch", "White ash", "Other hardwoods"
    ),
    Stems_Per_Acre = c(
      45, 13, 4, 10,
      8, 63, 54, 21,
      10, 16, 10
    ),
    Percent = c(
      10, 5, 2, 4,
      3, 25, 21, 8,
      4, 6, 4
    )
  ) |>
    mutate(
      SPCD = case_match(
        Species,
        "Red maple" ~ 316,
        "Oak" ~ 833,
        "White pine" ~ 129,
        "Black birch" ~ 372,
        "White ash" ~ 541,
        "Hemlock" ~ 261,
        "Red Pine" ~ 125,
        "Other birch" ~ 375,
        "Other hardwoods" ~ 762,
        "Sugar Maple" ~ 318,
        "Spruce" ~ 97
      )
    ) |>
    left_join(
      species_crosswalk |> select(SPCD, FVS_SPCD),
      by = join_by(SPCD)
    ) |>
    select(SPECIES = FVS_SPCD, DENSITY = Stems_Per_Acre) |>
    mutate(STAND_CN = NA, YEAR = NA) |>
    arrange(desc(DENSITY))
})
