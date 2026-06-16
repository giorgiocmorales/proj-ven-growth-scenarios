# Project lint profile: keep default linters but relax formatting rules that
# conflict with readable analytical pipelines.
libters <- lintr::linters_with_defaults(
  line_length_linter = NULL,
  pipe_consistency_linter = NULL,
  pipe_continuation_linter = NULL
)
