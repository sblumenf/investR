# Extracted from test-fct_group_metrics.R:113

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
metrics <- calculate_dashboard_metrics()
expect_type(metrics, "list")
expect_true("open_metrics" %in% names(metrics))
expect_true("closed_metrics" %in% names(metrics))
expect_true("count" %in% names(metrics$open_metrics))
expect_true("total_cost_basis" %in% names(metrics$open_metrics))
