tar_target(nrsgro_tree_grm_all, {
  fia_trees_by_cn(fiadb, nrsgro_grm_all |> select(TRE_CN) |> rename(CN = TRE_CN))
})
