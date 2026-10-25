tar_target(cficop_dflt_trees, {
  cficop_dflt_trees <- cfigro_trees |>
    union_all(cficop_dflt_saplings) |>
    union_all(cficop_dflt_trees_missing) |>
    group_by(STAND_ID) |>
    # TREE_ID needs to be unique; we could try to be clever and
    # preserve MasterTreeID where possible, but it's not worth it.
    mutate(TREE_ID = row_number()) |>
    ungroup()
})
