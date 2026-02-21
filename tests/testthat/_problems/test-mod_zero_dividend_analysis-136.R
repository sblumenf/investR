# Extracted from test-mod_zero_dividend_analysis.R:136

# test -------------------------------------------------------------------------
ui <- mod_zero_dividend_analysis_ui("test")
ui_html <- as.character(ui)
expect_true(grepl("Deeper ITM", ui_html) ||
             grepl("Lower risk", ui_html))
expect_true(grepl("1000 days", ui_html) ||
             grepl("longest", ui_html))
