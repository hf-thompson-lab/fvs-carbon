# tar_file_read is a tarchetypes metatarget, so can't use tar_simple
tarchetypes::tar_file_read(
  fia_table_11_5_17,
  "data/raw/FIADB_11.5.17_JENKINS_SPGRPCD.csv",
  read_csv(!!.x, col_types = cols(JENKINS_SPGRPCD = col_integer()))
)
