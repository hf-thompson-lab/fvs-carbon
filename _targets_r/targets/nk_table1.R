# tar_file_read is a tarchetypes meta-target, so can't use tar_simple
tarchetypes::tar_file_read(
  nk_table_1,
  "data/raw/NK_Table_1.csv",
  read_csv(!!.x, col_types = cols(`FIA plot code` = col_character()))
)
