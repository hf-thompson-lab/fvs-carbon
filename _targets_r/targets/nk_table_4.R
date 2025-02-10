tarchetypes::tar_file_read(
  nk_table_4,
  "data/raw/NK_Table_4.csv",
  read_csv(!!.x, col_types = cols(`Management scenario` = col_character(), .default = col_number()))
)
