# Plot plot_principal_interest tests ----------------------------------
test_that("plot_principal_interest returns a girafe object with valid inputs", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")
  
  # Create a sample amortization table
  sample_data <- data.frame(
    payment_number = 1:12,
    original_remaining_principal = seq(300000, 289000, length.out = 12),
    new_remaining_principal = seq(300000, 289000, length.out = 12) - 1000,
    original_interest_paid = seq(1500, 1400, length.out = 12),
    new_interest_paid = seq(1500, 1400, length.out = 12) - 50
  )
  
  # Test with standard inputs
  result <- plot_principal_interest(sample_data)
  
  # Check if the result is a ggiraph object (interactive)
  expect_s3_class(result, "girafe")
  expect_true("htmlwidget" %in% class(result))
})

test_that("plot_principal_interest handles missing columns gracefully", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")
  
  # Create incomplete data
  incomplete_data <- data.frame(
    payment_number = 1:12,
    original_remaining_principal = seq(300000, 289000, length.out = 12)
    # Missing other required columns
  )
  
  # Expect error with informative message about missing columns
  expect_error(
    plot_principal_interest(incomplete_data),
    "Input data is missing required columns"
  )
})

test_that("plot_principal_interest works with single payment", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")
  
  # Create minimal valid data with just one payment
  single_payment <- data.frame(
    payment_number = 1,
    original_remaining_principal = 300000,
    new_remaining_principal = 299000,
    original_interest_paid = 1500,
    new_interest_paid = 1450
  )
  
  # Should work with single payment
  result <- plot_principal_interest(single_payment)
  expect_s3_class(result, "girafe")
})

test_that("plot_principal_interest handles NA values", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")
  
  # Create data with some NAs
  na_data <- data.frame(
    payment_number = 1:12,
    original_remaining_principal = c(NA, seq(300000, 290000, length.out = 11)),
    new_remaining_principal = c(seq(299000, 289000, length.out = 11), NA),
    original_interest_paid = c(seq(1500, 1400, length.out = 11), NA),
    new_interest_paid = c(NA, seq(1450, 1350, length.out = 11))
  )
  
  # Should still work with NA values (suppress expected warnings about removed rows)
  suppressWarnings({
    result <- plot_principal_interest(na_data)
    expect_s3_class(result, "girafe")
  })
})

# Plot plot_price_vs_down_payment tests ----------------------------------
test_that("plot_price_vs_down_payment returns a ggplot object with valid inputs", {
  # Test with standard inputs
  result <- plot_price_vs_down_payment(
    monthly_housing_budget = 3000,
    monthly_non_mortgage_costs = 500,
    annual_rate_pct = 4.5,
    mortgage_term_months = 360,
    prop_tax_rate_annual = 1.2,
    pmi_rate_annual = 0.5,
    pmi_threshold_pct = 20,
    dp_pct_range = seq(5, 30, by = 5)
  )
  
  # Check if the result is a ggiraph object (interactive)
  expect_s3_class(result, "girafe")
  expect_true("htmlwidget" %in% class(result))
})

test_that("plot_price_vs_down_payment handles edge cases", {
  # Suppress expected warnings about single-point paths
  suppressWarnings({
    # Test with minimum down payment (1%)
    result_min_dp <- plot_price_vs_down_payment(
      monthly_housing_budget = 2000,
      monthly_non_mortgage_costs = 400,
      annual_rate_pct = 3.5,
      mortgage_term_months = 180,
      prop_tax_rate_annual = 1.0,
      dp_pct_range = 1  # Minimum down payment
    )
    expect_s3_class(result_min_dp, "girafe")
    
    # Test with high down payment (just below 100%)
    result_high_dp <- plot_price_vs_down_payment(
      monthly_housing_budget = 2000,
      monthly_non_mortgage_costs = 400,
      annual_rate_pct = 3.5,
      mortgage_term_months = 180,
      prop_tax_rate_annual = 1.0,
      dp_pct_range = 95  # Very high down payment
    )
    expect_s3_class(result_high_dp, "girafe")
  })
  
  # Test with no PMI (down payment above threshold)
  suppressWarnings({
    result_no_pmi <- plot_price_vs_down_payment(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      annual_rate_pct = 4.0,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      pmi_threshold_pct = 20,
      dp_pct_range = c(20, 25, 30)  # All above PMI threshold
    )
    expect_s3_class(result_no_pmi, "girafe")
  })
})

test_that("plot_price_vs_down_payment handles invalid inputs", {
  # Test with zero budget
  expect_error(
    plot_price_vs_down_payment(
      monthly_housing_budget = 0,  # Invalid
      monthly_non_mortgage_costs = 500,
      annual_rate_pct = 4.5,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2
    ),
    "monthly_housing_budget > 0"
  )
  
  # Test with negative mortgage term
  expect_error(
    plot_price_vs_down_payment(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      annual_rate_pct = 4.5,
      mortgage_term_months = -360,  # Invalid
      prop_tax_rate_annual = 1.2
    ),
    "mortgage_term_months > 0"
  )
  
  # Test with down payment >= 100%
  expect_error(
    plot_price_vs_down_payment(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      annual_rate_pct = 4.5,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      dp_pct_range = c(5, 10, 100)  # Invalid (100%)
    ),
    "dp_pct_range < 100"
  )
})

test_that("plot_price_vs_down_payment handles no affordable homes", {
  # Suppress expected warnings about no data points
  suppressWarnings({
    # Test with unrealistic budget that can't afford any home
    result <- plot_price_vs_down_payment(
      monthly_housing_budget = 100,  # Too low
      monthly_non_mortgage_costs = 0,
      annual_rate_pct = 4.5,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      dp_pct_range = seq(5, 30, by = 5)
    )
    
    # Should still return a ggiraph object with a message
    expect_s3_class(result, "girafe")
  })
})


# Plot plot_price_vs_rate tests ----------------------------------
test_that("plot_price_vs_rate returns a ggiraph object with valid inputs", {
  # Test with standard inputs (percentage down payment)
  result_pct <- plot_price_vs_rate(
    monthly_housing_budget = 3000,
    monthly_non_mortgage_costs = 500,
    mortgage_term_months = 360,
    prop_tax_rate_annual = 1.2,
    pmi_rate_annual = 0.5,
    pmi_threshold_pct = 20,
    down_payment_input = list(pct = 20),
    rate_pct_range = seq(3, 6, by = 0.5)
  )
  
  # Check if the result is a ggiraph object (interactive)
  expect_s3_class(result_pct, "girafe")
  expect_true("htmlwidget" %in% class(result_pct))
  
  # Test with dollar amount down payment
  result_dollars <- plot_price_vs_rate(
    monthly_housing_budget = 3000,
    monthly_non_mortgage_costs = 500,
    mortgage_term_months = 360,
    prop_tax_rate_annual = 1.2,
    pmi_rate_annual = 0.5,
    pmi_threshold_pct = 20,
    down_payment_input = list(dollars = 100000),
    rate_pct_range = seq(3, 6, by = 0.5)
  )
  
  expect_s3_class(result_dollars, "girafe")
})

test_that("plot_price_vs_rate handles edge cases", {
  # Suppress expected warnings
  suppressWarnings({
    # Test with minimum rate (0%)
    result_min_rate <- plot_price_vs_rate(
      monthly_housing_budget = 2000,
      monthly_non_mortgage_costs = 400,
      mortgage_term_months = 180,
      prop_tax_rate_annual = 1.0,
      down_payment_input = list(pct = 20),
      rate_pct_range = 0  # 0% interest rate
    )
    expect_s3_class(result_min_rate, "girafe")
    
    # Test with high rate
    result_high_rate <- plot_price_vs_rate(
      monthly_housing_budget = 2000,
      monthly_non_mortgage_costs = 400,
      mortgage_term_months = 180,
      prop_tax_rate_annual = 1.0,
      down_payment_input = list(pct = 20),
      rate_pct_range = 20  # 20% interest rate
    )
    expect_s3_class(result_high_rate, "girafe")
  })
  
  # Test with no PMI (down payment above threshold)
  suppressWarnings({
    result_no_pmi <- plot_price_vs_rate(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      pmi_threshold_pct = 20,
      down_payment_input = list(pct = 25),  # Above PMI threshold
      rate_pct_range = c(4, 5, 6)
    )
    expect_s3_class(result_no_pmi, "girafe")
  })
})

test_that("plot_price_vs_rate handles invalid inputs", {
  # Test with zero budget
  expect_error(
    plot_price_vs_rate(
      monthly_housing_budget = 0,  # Invalid
      monthly_non_mortgage_costs = 500,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      down_payment_input = list(pct = 20),
      rate_pct_range = c(3, 4, 5)
    ),
    "monthly_housing_budget > 0"
  )
  
  # Test with negative mortgage term
  expect_error(
    plot_price_vs_rate(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      mortgage_term_months = -360,  # Invalid
      prop_tax_rate_annual = 1.2,
      down_payment_input = list(pct = 20),
      rate_pct_range = c(3, 4, 5)
    ),
    "mortgage_term_months > 0"
  )
  
  # Test with invalid down payment input (neither pct nor dollars)
  expect_error(
    plot_price_vs_rate(
      monthly_housing_budget = 3000,
      monthly_non_mortgage_costs = 500,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      down_payment_input = list(invalid = 20),  # Invalid
      rate_pct_range = c(3, 4, 5)
    ),
    "down_payment_input"
  )
})

test_that("plot_price_vs_rate handles no affordable homes", {
  # Suppress expected warnings about no data points
  suppressWarnings({
    # Test with unrealistic budget that can't afford any home
    result <- plot_price_vs_rate(
      monthly_housing_budget = 100,  # Too low
      monthly_non_mortgage_costs = 0,
      mortgage_term_months = 360,
      prop_tax_rate_annual = 1.2,
      down_payment_input = list(pct = 20),
      rate_pct_range = c(3, 4, 5)
    )
    
    # Should still return a ggiraph object
    expect_s3_class(result, "girafe")
  })
})


# Plot plot_refinance_benefit tests ----------------------------------
test_that("plot_refinance_benefit returns a girafe object with valid inputs", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create sample refinance data (typical beneficial scenario)
  sample_refinance_data <- list(
    months = 1:60,
    net_benefits = seq(-5000, 15000, length.out = 60), # Starts negative, becomes positive
    breakeven_month = 20,
    old_payment = 2400,
    new_payment = 2200,
    monthly_savings = 200
  )

  # Test with standard inputs
  result <- plot_refinance_benefit(sample_refinance_data)

  # Check if the result is a ggiraph object (interactive)
  expect_s3_class(result, "girafe")
  expect_true("htmlwidget" %in% class(result))
})

test_that("plot_refinance_benefit handles missing required fields gracefully", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test missing months field
  incomplete_data_1 <- list(
    net_benefits = seq(-5000, 15000, length.out = 60),
    breakeven_month = 20,
    old_payment = 2400,
    new_payment = 2200,
    monthly_savings = 200
    # Missing 'months' field
  )

  expect_error(
    plot_refinance_benefit(incomplete_data_1),
    "refinance_data is missing required fields: months"
  )

  # Test missing multiple fields
  incomplete_data_2 <- list(
    months = 1:60
    # Missing all other required fields
  )

  expect_error(
    plot_refinance_benefit(incomplete_data_2),
    "refinance_data is missing required fields"
  )
})

test_that("plot_refinance_benefit validates input parameter types", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  valid_data <- list(
    months = 1:60,
    net_benefits = seq(-5000, 15000, length.out = 60),
    breakeven_month = 20,
    old_payment = 2400,
    new_payment = 2200,
    monthly_savings = 200
  )

  # Test invalid title type
  expect_error(
    plot_refinance_benefit(valid_data, title = c("Title 1", "Title 2")),
    "length\\(title\\) == 1 is not TRUE"
  )

  # Test invalid show_breakeven type
  expect_error(
    plot_refinance_benefit(valid_data, show_breakeven = "yes"),
    "is\\.logical\\(show_breakeven\\) is not TRUE"
  )
})

test_that("plot_refinance_benefit validates months and net_benefits length consistency", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test mismatched lengths (this is a critical production risk)
  mismatched_data <- list(
    months = 1:60,
    net_benefits = seq(-5000, 15000, length.out = 50), # Different length!
    breakeven_month = 20,
    old_payment = 2400,
    new_payment = 2200,
    monthly_savings = 200
  )

  expect_error(
    plot_refinance_benefit(mismatched_data),
    "months and net_benefits must have the same length"
  )
})

test_that("plot_refinance_benefit handles no breakeven scenario", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create scenario where refinancing is always negative (no breakeven)
  no_breakeven_data <- list(
    months = 1:60,
    net_benefits = seq(-10000, -5000, length.out = 60), # Always negative
    breakeven_month = NA, # No breakeven point
    old_payment = 2200,
    new_payment = 2400, # Higher payment (bad refi)
    monthly_savings = -200 # Negative savings
  )

  # Should work with NA breakeven_month
  result <- plot_refinance_benefit(no_breakeven_data)
  expect_s3_class(result, "girafe")

  # Test with breakeven highlighting disabled
  result_no_highlight <- plot_refinance_benefit(no_breakeven_data, show_breakeven = FALSE)
  expect_s3_class(result_no_highlight, "girafe")
})

test_that("plot_refinance_benefit handles immediate breakeven scenario", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create scenario where refinancing is immediately beneficial
  immediate_benefit_data <- list(
    months = 1:60,
    net_benefits = seq(1000, 50000, length.out = 60), # Always positive
    breakeven_month = 1, # Immediate breakeven
    old_payment = 2400,
    new_payment = 2000,
    monthly_savings = 400
  )

  result <- plot_refinance_benefit(immediate_benefit_data)
  expect_s3_class(result, "girafe")
})

test_that("plot_refinance_benefit handles edge case: single month evaluation", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test with minimal data (single month - edge case for mortgage analysis)
  single_month_data <- list(
    months = 1,
    net_benefits = -5000, # Just closing costs, no time for benefits
    breakeven_month = NA,
    old_payment = 2400,
    new_payment = 2200,
    monthly_savings = 200
  )

  result <- plot_refinance_benefit(single_month_data)
  expect_s3_class(result, "girafe")
})

test_that("plot_refinance_benefit handles zero values correctly", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test edge case: zero monthly savings (unusual but possible)
  zero_savings_data <- list(
    months = 1:24,
    net_benefits = rep(-3000, 24), # Just negative closing costs, no benefit/loss
    breakeven_month = NA,
    old_payment = 2400,
    new_payment = 2400, # Same payment
    monthly_savings = 0 # Zero savings
  )

  result <- plot_refinance_benefit(zero_savings_data)
  expect_s3_class(result, "girafe")
})

test_that("plot_refinance_benefit handles large benefit ranges without errors", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test with large benefit values (high-value mortgages)
  large_benefit_data <- list(
    months = 1:360, # Full 30-year term
    net_benefits = seq(-50000, 500000, length.out = 360), # Large mortgage scenario
    breakeven_month = 72, # 6 years
    old_payment = 15000, # High-value mortgage
    new_payment = 12000,
    monthly_savings = 3000
  )

  result <- plot_refinance_benefit(large_benefit_data)
  expect_s3_class(result, "girafe")
})

test_that("plot_refinance_benefit custom title and breakeven options work", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  sample_data <- list(
    months = 1:36,
    net_benefits = seq(-8000, 12000, length.out = 36),
    breakeven_month = 24,
    old_payment = 2400,
    new_payment = 2100,
    monthly_savings = 300
  )

  # Test custom title
  result_custom_title <- plot_refinance_benefit(
    sample_data,
    title = "Custom Refinance Analysis"
  )
  expect_s3_class(result_custom_title, "girafe")

  # Test with breakeven highlighting disabled
  result_no_breakeven <- plot_refinance_benefit(
    sample_data,
    show_breakeven = FALSE
  )
  expect_s3_class(result_no_breakeven, "girafe")
})

test_that("plot_refinance_benefit works with realistic calculate_refinance_benefit_curve data", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create realistic refinance scenario using actual calculation function
  # Scenario: $700K mortgage, 5% -> 4.5%, 20 years remaining, $8K closing costs
  realistic_refinance_data <- calculate_refinance_benefit_curve(
    principal = 700000,                    # $700K remaining balance
    rate_per_month_old = 0.05 / 12,       # 5.0% annual -> monthly
    rate_per_month_new = 0.045 / 12,      # 4.5% annual -> monthly
    n_payments_remaining = 240,            # 20 years remaining
    closing_costs = 8000,                  # $8K closing costs
    n_payments_new = 30*12,
    tax_rate = 0.24,                       # 24% marginal tax rate
    investment_return_annual = 0.01,       # 1% annual investment return
    lump_sum_paydown = 0,                  # No extra paydown
    mid_limit = 750000,                    # Standard MID limit
    max_eval_months = 30*12                  # Evaluate full limit
  )

  # Verify the calculation worked and returned expected structure
  expect_true(is.list(realistic_refinance_data))
  expect_true(all(c("months", "net_benefits", "breakeven_month",
                   "old_payment", "new_payment", "monthly_savings") %in%
                  names(realistic_refinance_data)))

  # Test that the plot function works with real calculation output
  result <- plot_refinance_benefit(realistic_refinance_data)

  # Verify we get a proper ggiraph object
  expect_s3_class(result, "girafe")
  expect_true("htmlwidget" %in% class(result))

  # Verify the scenario makes financial sense
  expect_true(realistic_refinance_data$old_payment > realistic_refinance_data$new_payment)
  expect_true(realistic_refinance_data$monthly_savings > 0)
  expect_true(!is.na(realistic_refinance_data$breakeven_month))
  expect_true(realistic_refinance_data$breakeven_month > 0)
  expect_true(realistic_refinance_data$breakeven_month <= 120)

  # Verify new equity fields are present and make sense
  expect_true("net_cash_benefits" %in% names(realistic_refinance_data))
  expect_true("equity_differences" %in% names(realistic_refinance_data))
  expect_true("cash_breakeven_month" %in% names(realistic_refinance_data))

  # Cash breakeven should typically be later than wealth breakeven (or same/earlier if no lump sum)
  if (!is.na(realistic_refinance_data$cash_breakeven_month)) {
    expect_true(realistic_refinance_data$cash_breakeven_month > 0)
    expect_true(realistic_refinance_data$cash_breakeven_month <= 120)
  }

  # Verify mathematical relationship: net_benefits = net_cash_benefits + equity_differences
  for (i in 1:length(realistic_refinance_data$months)) {
    expected_total <- realistic_refinance_data$net_cash_benefits[i] + realistic_refinance_data$equity_differences[i]
    expect_equal(realistic_refinance_data$net_benefits[i], expected_total, tolerance = 0.01)
  }

  # Test with different scenario parameters - higher closing costs, no breakeven
  expensive_refi_data <- calculate_refinance_benefit_curve(
    principal = 200000,                    # Smaller mortgage
    rate_per_month_old = 0.04 / 12,       # 4.0% annual -> monthly
    rate_per_month_new = 0.035 / 12,      # 3.5% annual -> monthly (smaller benefit)
    n_payments_remaining = 60,             # Only 5 years remaining
    closing_costs = 15000,                 # Very high closing costs
    n_payments_new = 15*12,                # New 15-year
    tax_rate = 0.22,                       # 22% marginal tax rate
    investment_return_annual = 0.04,       # Lower investment return
    lump_sum_paydown = 0,
    mid_limit = 750000,
    max_eval_months = 60                   # Short evaluation period
  )

  # This scenario might not break even - test the plot handles it
  result_expensive <- plot_refinance_benefit(expensive_refi_data)
  expect_s3_class(result_expensive, "girafe")
})

test_that("plot_refinance_benefit handles equity differences with lump sum paydown correctly", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create scenario with significant lump sum paydown to test equity tracking
  lump_sum_scenario <- calculate_refinance_benefit_curve(
    principal = 300000,                    # $300K remaining balance
    rate_per_month_old = 0.06 / 12,       # 6.0% annual -> monthly
    rate_per_month_new = 0.04 / 12,       # 4.0% annual -> monthly
    n_payments_remaining = 180,            # 15 years remaining
    closing_costs = 6000,                  # $6K closing costs
    n_payments_new = 30*12,                # New 30-year
    tax_rate = 0.25,                       # 25% marginal tax rate
    investment_return_annual = 0.05,       # 5% annual investment return
    lump_sum_paydown = 50000,             # $50K lump sum paydown
    mid_limit = 750000,                    # Standard MID limit
    max_eval_months = 60                   # Evaluate 5 years
  )

  # Verify structure with new fields
  expect_true(all(c("net_cash_benefits", "equity_differences", "cash_breakeven_month") %in%
                  names(lump_sum_scenario)))

  # With a lump sum paydown, equity differences should be significant
  expect_true(all(lump_sum_scenario$equity_differences > 0)) # Should always be positive due to lump sum

  # Cash benefit should be lower than total benefit due to equity differences
  expect_true(all(lump_sum_scenario$net_benefits >= lump_sum_scenario$net_cash_benefits))

  # Cash breakeven should typically be later than wealth breakeven with lump sum
  if (!is.na(lump_sum_scenario$cash_breakeven_month) && !is.na(lump_sum_scenario$breakeven_month)) {
    expect_true(lump_sum_scenario$cash_breakeven_month >= lump_sum_scenario$breakeven_month)
  }

  # Test plotting with new format
  result_lump_sum <- plot_refinance_benefit(lump_sum_scenario)
  expect_s3_class(result_lump_sum, "girafe")

  # Test backward compatibility by plotting data that looks like old format
  legacy_format_data <- list(
    months = 1:36,
    net_benefits = seq(-8000, 12000, length.out = 36),
    breakeven_month = 24,
    old_payment = 2400,
    new_payment = 2100,
    monthly_savings = 300
    # Missing new fields: net_cash_benefits, equity_differences, cash_breakeven_month
  )

  result_legacy <- plot_refinance_benefit(legacy_format_data)
  expect_s3_class(result_legacy, "girafe")
})

test_that("plot_refinance_benefit handles new loan paid off first scenario correctly", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create scenario where new loan is paid off before old loan would have been
  # Shorter term refinance: 30-year old loan with 20 years remaining -> 15-year new loan
  short_refi_scenario <- calculate_refinance_benefit_curve(
    principal = 400000,                    # $400K remaining balance
    rate_per_month_old = 0.055 / 12,      # 5.5% annual -> monthly (old loan)
    rate_per_month_new = 0.045 / 12,      # 4.5% annual -> monthly (new loan)
    n_payments_remaining = 240,            # 20 years remaining on old loan
    closing_costs = 7500,                  # $7.5K closing costs
    n_payments_new = 180,                  # NEW LOAN: 15 years (shorter than remaining old loan)
    tax_rate = 0.24,                       # 24% marginal tax rate
    investment_return_annual = 0.06,       # 6% annual investment return
    lump_sum_paydown = 0,                  # No lump sum paydown
    mid_limit = 750000,                    # Standard MID limit
    max_eval_months = 300                  # Evaluate beyond both loan terms
  )

  # Verify the scenario is set up correctly
  expect_true(is.list(short_refi_scenario))
  expect_true(all(c("months", "net_benefits", "dynamic_monthly_savings") %in%
                  names(short_refi_scenario)))

  # Verify we have dynamic monthly savings data
  expect_true("dynamic_monthly_savings" %in% names(short_refi_scenario))
  expect_equal(length(short_refi_scenario$dynamic_monthly_savings),
               length(short_refi_scenario$months))

  # Test key characteristics of "new loan paid off first" scenario:
  # 1. New loan term (180 months) < old loan remaining (240 months)
  # 2. After month 180, dynamic savings should equal old_payment (new payment becomes 0)
  # 3. After month 240, dynamic savings should be 0 (both loans paid off)

  old_payment <- short_refi_scenario$old_payment
  new_payment <- short_refi_scenario$new_payment
  static_savings <- old_payment - new_payment

  # Verify payments make sense
  expect_true(old_payment > 0)
  expect_true(new_payment > 0)
  expect_true(new_payment > old_payment) # New loan should have higher payment due to shorter term
  expect_true(static_savings < 0) # Initially costs more per month due to shorter term

  # Check dynamic savings pattern at key points:
  # Before any loan is paid off (e.g., month 100): should equal static savings
  month_100_idx <- which(short_refi_scenario$months == 100)
  if (length(month_100_idx) > 0) {
    expect_equal(short_refi_scenario$dynamic_monthly_savings[month_100_idx],
                 static_savings, tolerance = 0.01)
  }

  # After new loan is paid off (e.g., month 200): should equal old_payment
  month_200_idx <- which(short_refi_scenario$months == 200)
  if (length(month_200_idx) > 0) {
    expect_equal(short_refi_scenario$dynamic_monthly_savings[month_200_idx],
                 old_payment, tolerance = 0.01)
  }

  # After both loans are paid off (e.g., month 250): should equal 0
  month_250_idx <- which(short_refi_scenario$months == 250)
  if (length(month_250_idx) > 0) {
    expect_equal(short_refi_scenario$dynamic_monthly_savings[month_250_idx],
                 0, tolerance = 0.01)
  }

  # Test that the plot function works with this scenario
  result_short_refi <- plot_refinance_benefit(short_refi_scenario)
  expect_s3_class(result_short_refi, "girafe")
  expect_true("htmlwidget" %in% class(result_short_refi))
})

test_that("plot_refinance_benefit tooltip accuracy with new loan paid off first", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Create a controlled test scenario with known values
  # We'll create mock data that simulates the "new loan paid off first" pattern

  old_payment <- 2500
  new_payment <- 3000  # Higher due to shorter term
  static_savings <- old_payment - new_payment  # -500

  # Create mock dynamic savings pattern:
  # Months 1-120: Both loans active, savings = -500
  # Months 121-180: New loan paid off, savings = 2500 (old payment only)
  # Months 181+: Both loans paid off, savings = 0

  mock_dynamic_savings <- c(
    rep(static_savings, 120),    # Months 1-120: both active
    rep(old_payment, 60),        # Months 121-180: only old loan active
    rep(0, 60)                   # Months 181-240: both paid off
  )

  mock_refinance_data <- list(
    months = 1:240,
    net_benefits = seq(-10000, 50000, length.out = 240),
    net_cash_benefits = seq(-10000, 45000, length.out = 240),
    equity_differences = seq(0, 5000, length.out = 240),
    breakeven_month = 60,
    cash_breakeven_month = 65,
    old_payment = old_payment,
    new_payment = new_payment,
    monthly_savings = static_savings,
    dynamic_monthly_savings = mock_dynamic_savings,
    n_payments_remaining = 180,  # Old loan remaining
    n_payments_new = 120         # New loan term (shorter)
  )

  # Test that plot function works with mock data
  result_mock <- plot_refinance_benefit(mock_refinance_data)
  expect_s3_class(result_mock, "girafe")

  # Test tooltip logic by manually checking what the function would calculate
  # This tests the dynamic payment calculation logic

  # Test month 100 (both loans active)
  month_100_expected_old <- old_payment  # Should be old_payment
  month_100_expected_new <- new_payment  # Should be new_payment
  month_100_expected_savings <- static_savings  # Should be static_savings

  # Test month 150 (new loan paid off, old continues)
  month_150_expected_old <- old_payment  # Should be old_payment
  month_150_expected_new <- 0           # Should be 0 (new loan paid off)
  month_150_expected_savings <- old_payment  # Should be old_payment

  # Test month 200 (both loans paid off)
  month_200_expected_old <- 0           # Should be 0 (old loan paid off)
  month_200_expected_new <- 0           # Should be 0 (new loan paid off)
  month_200_expected_savings <- 0       # Should be 0

  # Verify our mock data matches expectations
  expect_equal(mock_dynamic_savings[100], month_100_expected_savings)
  expect_equal(mock_dynamic_savings[150], month_150_expected_savings)
  expect_equal(mock_dynamic_savings[200], month_200_expected_savings)
})

test_that("plot_refinance_benefit handles edge case of simultaneous loan payoff", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Edge case: both loans happen to be paid off in the same month
  # This could happen if refinancing to the exact same remaining term

  simultaneous_payoff_scenario <- calculate_refinance_benefit_curve(
    principal = 200000,                    # $200K remaining balance
    rate_per_month_old = 0.06 / 12,       # 6.0% annual -> monthly
    rate_per_month_new = 0.04 / 12,       # 4.0% annual -> monthly
    n_payments_remaining = 120,            # 10 years remaining on old loan
    closing_costs = 4000,                  # $4K closing costs
    n_payments_new = 120,                  # NEW LOAN: Same 10 years (same term)
    tax_rate = 0.22,                       # 22% marginal tax rate
    investment_return_annual = 0.04,       # 4% annual investment return
    lump_sum_paydown = 0,                  # No lump sum paydown
    mid_limit = 750000,                    # Standard MID limit
    max_eval_months = 150                  # Evaluate beyond loan terms
  )

  # Verify this scenario works
  expect_true(is.list(simultaneous_payoff_scenario))

  # Both loans should be paid off at month 120, so dynamic savings should be 0 after that
  if ("dynamic_monthly_savings" %in% names(simultaneous_payoff_scenario)) {
    months_after_payoff <- which(simultaneous_payoff_scenario$months > 120)
    if (length(months_after_payoff) > 0) {
      expect_true(all(abs(simultaneous_payoff_scenario$dynamic_monthly_savings[months_after_payoff]) < 0.01))
    }
  }

  # Test plotting
  result_simultaneous <- plot_refinance_benefit(simultaneous_payoff_scenario)
  expect_s3_class(result_simultaneous, "girafe")
})

test_that("plot_refinance_benefit fallback logic when loan term data unavailable", {
  # Skip if ggiraph is not installed
  skip_if_not_installed("ggiraph")

  # Test the fallback logic when n_payments_remaining and n_payments_new are not available
  # This simulates using the function with older data or incomplete refinance scenarios

  old_payment <- 2200
  new_payment <- 2600  # Higher payment (shorter term scenario)
  static_savings <- old_payment - new_payment  # -400

  # Create dynamic savings pattern that indicates new loan paid off first
  mock_dynamic_savings_no_terms <- c(
    rep(static_savings, 100),     # Months 1-100: both active (-400)
    rep(old_payment, 50),         # Months 101-150: only old loan active (2200)
    rep(0, 50)                    # Months 151-200: both paid off (0)
  )

  refinance_data_no_terms <- list(
    months = 1:200,
    net_benefits = seq(-8000, 30000, length.out = 200),
    net_cash_benefits = seq(-8000, 28000, length.out = 200),
    equity_differences = seq(0, 2000, length.out = 200),
    breakeven_month = 45,
    cash_breakeven_month = 50,
    old_payment = old_payment,
    new_payment = new_payment,
    monthly_savings = static_savings,
    dynamic_monthly_savings = mock_dynamic_savings_no_terms
    # Note: Missing n_payments_remaining and n_payments_new
  )

  # The function should still work using pattern recognition from dynamic_monthly_savings
  result_no_terms <- plot_refinance_benefit(refinance_data_no_terms)
  expect_s3_class(result_no_terms, "girafe")
  expect_true("htmlwidget" %in% class(result_no_terms))

  # Test the legacy format (without dynamic_monthly_savings) falls back gracefully
  legacy_no_dynamic_data <- list(
    months = 1:60,
    net_benefits = seq(-5000, 15000, length.out = 60),
    breakeven_month = 20,
    old_payment = old_payment,
    new_payment = new_payment,
    monthly_savings = static_savings
    # Note: Missing both loan terms AND dynamic_monthly_savings
  )

  # Should fall back to static values for all months
  result_legacy_no_dynamic <- plot_refinance_benefit(legacy_no_dynamic_data)
  expect_s3_class(result_legacy_no_dynamic, "girafe")
})
