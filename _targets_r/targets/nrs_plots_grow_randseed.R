tar_target(nrs_plots_grow_randseed, {
  # We can have as many seeds as we want. We'll convert these to valid FVS
  # random seeds later.
  # Start with 3, so it's reasonably fast but we get some benefit from it.
  # FVS requires positive, odd-numbered seeds, so multiply by 2 and subtract 1.
  sample.int(500000, 3) * 2 - 1
})
