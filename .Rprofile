if (requireNamespace("here", quietly = TRUE)) {
  try(here::i_am("README.md"), silent = TRUE)
}

# Use UTF-8 before project scripts are parsed so Spanish graph text is preserved on Windows.
if (identical(.Platform$OS.type, "windows")) {
  active_ctype <- Sys.setlocale("LC_CTYPE", ".UTF-8")
  if (is.na(active_ctype)) {
    stop("The project requires a Windows UTF-8 locale for Spanish graph text.", call. = FALSE)
  }
}

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  readr.show_col_types = FALSE
)
