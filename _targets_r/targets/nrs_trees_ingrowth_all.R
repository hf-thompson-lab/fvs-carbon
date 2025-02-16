tar_target(nrs_trees_ingrowth_all, {
  fia_trees_by_cn(fiadb, nrs_grm_ingrowth_all |> select(TRE_CN) |> rename(CN = TRE_CN))
})
