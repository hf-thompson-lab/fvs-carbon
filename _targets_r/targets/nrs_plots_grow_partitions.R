tar_target(nrs_plots_grow_partitions, {
  # This is a list of partitons; we can come up with it however we want.
  # For now, one partition per worker is resonable.
  partitions <- 1
  crew_controller <- tar_option_get(controller)
  if (!is.null(crew_controller)) {
    partitions <- crew_controller$launcher$workers
  }
  partitions
})
