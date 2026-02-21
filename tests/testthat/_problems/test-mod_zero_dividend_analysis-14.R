# Extracted from test-mod_zero_dividend_analysis.R:14

# test -------------------------------------------------------------------------
ui <- mod_zero_dividend_analysis_ui("test")
expect_s3_class(ui, "shiny.tag.list")
ui_html <- as.character(ui)
expect_true(grepl("strike_threshold", ui_html))
expect_true(grepl("target_days", ui_html))
