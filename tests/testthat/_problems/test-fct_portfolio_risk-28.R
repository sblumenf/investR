# Extracted from test-fct_portfolio_risk.R:28

# test -------------------------------------------------------------------------
set.seed(123)
n_paths <- 1000
n_positions <- 3
position_pnl_matrix <- matrix(rnorm(n_paths * n_positions, mean = 100, sd = 50), nrow = n_paths, ncol = n_positions)
portfolio_pnl <- rowSums(position_pnl_matrix)
portfolio_var <- quantile(portfolio_pnl, 0.05)
positions <- tibble::tibble(
    group_id = paste0("group_", 1:n_positions),
    ticker = c("AAPL", "MSFT", "GOOGL")
  )
contributions <- investR:::calculate_position_contributions(
    positions = positions,
    portfolio_pnl = portfolio_pnl,
    position_pnl_matrix = position_pnl_matrix,
    portfolio_var = portfolio_var
  )
