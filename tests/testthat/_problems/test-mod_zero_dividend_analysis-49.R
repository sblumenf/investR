# Extracted from test-mod_zero_dividend_analysis.R:49

# test -------------------------------------------------------------------------
ui <- mod_zero_dividend_analysis_ui("test")
ui_html <- as.character(ui)
expect_true(grepl('data-min="30"', ui_html))
expect_true(grepl('data-max="1000"', ui_html))
expect_true(grepl('data-from="1000"', ui_html) ||
             grepl('value="1000"', ui_html))
