tar_target(cficop_hvst_estab, {
  cficop_hvst_estab <- cficop_dflt_estab |>
    mutate(
      STAND_ID = NA,
      HEIGHT = 0
    ) |>
    union_all(
      cficop_hvst_estab_1980
    ) |>
    union_all(
      cficop_hvst_estab_post_harvest
    )
})
