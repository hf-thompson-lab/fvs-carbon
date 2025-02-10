# Cannot use tar_simple = TRUE for targets with format = "file"
tar_target(fiadb, fia_fiadb_indexed(), format = "file")
