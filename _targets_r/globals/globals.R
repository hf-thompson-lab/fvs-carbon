options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c(
    "tarchetypes",
    "tidyverse",
    "RSQLite",
    "measurements",
    "reshape2",
    "maps",
    "pbapply"
  )
)
tar_source()
