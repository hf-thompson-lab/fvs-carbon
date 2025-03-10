tar_target(nrsgro_tree, {
  nrsgro_tree <- fia_trees(fiadb, nrsgro_plot) |>
      filter(CONDID == 1) # only live trees
})
