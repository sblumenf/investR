# Extracted from test-fct_portfolio_risk.R:135

# test -------------------------------------------------------------------------
set.seed(456)
n_paths <- 1000
pos1_pnl <- rnorm(n_paths, mean = 100, sd = 20)
pos2_pnl <- rnorm(n_paths, mean = 10, sd = 50)
portfolio_pnl <- pos1_pnl + pos2_pnl
position_pnl_matrix <- cbind(pos1_pnl, pos2_pnl)
portfolio_var <- quantile(portfolio_pnl, 0.05)
positions <- tibble::tibble(
    group_id = c("group_1", "group_2"),
    ticker = c("SAFE", "RISKY")
  )
contributions <- investR:::calculate_position_contributions(
    positions = positions,
    portfolio_pnl = portfolio_pnl,
    position_pnl_matrix = position_pnl_matrix,
    portfolio_var = portfolio_var
  )
