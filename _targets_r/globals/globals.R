options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "tarchetypes",
    "tidyverse",
    "dbplyr",
    "RSQLite",
    "measurements",
    "reshape2",
    "mlr3verse",
    "maps",
    "pbapply",
    "digest",
    "readxl",
    "terra"
  ),
  controller = crew::crew_controller_local(workers = 8)
)
tar_source()
