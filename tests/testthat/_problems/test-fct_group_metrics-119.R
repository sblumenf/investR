# Extracted from test-fct_group_metrics.R:119

# test -------------------------------------------------------------------------
skip_on_cran()
local_test_db()
metrics <- calculate_dashboard_metrics()
expect_type(metrics, "list")
expect_true("open_metrics" %in% names(metrics))
expect_true("closed_metrics" %in% names(metrics))
expect_true("count" %in% names(metrics$open_metrics))
expect_true("total_cost_basis" %in% names(metrics$open_metrics))
expect_true("total_cash_collected" %in% names(metrics$open_metrics))
expect_true("total_projected_income" %in% names(metrics$open_metrics))
expect_true("count" %in% names(metrics$closed_metrics))
expect_true("total_realized_pnl" %in% names(metrics$closed_metrics))
