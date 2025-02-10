# TODO: fvs_run by subplot doesn't work b/c stand_cn needs fixed
tarchetypes::tar_file_read(
  nk_bysubplot_none_carbon,
  "data/fvs/FVS_NKBySubplot_NONE_Carbon.csv",
  read_csv(!!.x, col_types = cols(StandID = col_character()))
)
