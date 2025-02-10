# tar_file_read is a tarchetypes metatarget, so can't use tar_simple
tarchetypes::tar_file_read(
  fvsne_table_3_2_1,
  "data/raw/FVSne_Overview_Table_3.2.1.csv",
  read_csv(!!.x, col_types = "iiciccc")
)
