tarchetypes::tar_file_read(
  tblSuppTreeStatusCodes,
  "data/raw/DWSP_CFI/tblSuppTreeStatusCodes.xlsx",
  read_excel(
    !!.x,
    col_types = c(
      "numeric", "text", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric",
      "numeric", "numeric"
    )
  )
)

