# Run the testthat suite only when testthat is installed in the local R library.
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary")
}
