tar_target(nrs_trees_grown, {
  nrs_trees_grown <- fia_trees(fiadb, nrs_plots_grown) |>
      filter(CONDID == 1) # only live trees
})
