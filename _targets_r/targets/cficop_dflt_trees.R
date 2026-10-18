tar_target(cficop_dflt_trees, {
  cficop_dflt_trees <- cfigro_trees |>
    union_all(cficop_dflt_saplings)
})
