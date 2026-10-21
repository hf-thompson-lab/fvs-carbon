tar_target(cficop_dflt_treelist, {
  cficop_dflt_treelist <- fvs_read_output(cficop_dflt, "FVS_TreeList") |>
    filter(random_seed == min(random_seed))
})
