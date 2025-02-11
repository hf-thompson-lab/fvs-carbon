# TODO: fvs_run by condition doesn't work b/c stand_cn needs fixed
tarchetypes::tar_file_read(
  nk_bycond_none_carbon,
  "data/fvs/FVS_NKByCondition_NONE_Carbon.csv",
  read_csv(!!.x, col_types = cols(StandID = col_character()))
)
