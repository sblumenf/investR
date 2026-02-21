# Extracted from test-fct_group_metrics.R:134

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
open_metrics <- calculate_dashboard_metrics(status_filter = "open")
expect_equal(nrow(open_metrics$closed_metrics), 1)
expect_equal(open_metrics$closed_metrics$count, 0)
closed_metrics <- calculate_dashboard_metrics(status_filter = "closed")
expect_equal(nrow(closed_metrics$open_metrics), 1)
