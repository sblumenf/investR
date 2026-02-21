# Extracted from test-fct_group_metrics.R:129

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
open_metrics <- calculate_dashboard_metrics(status_filter = "open")
expect_equal(nrow(open_metrics$closed_metrics), 1)
