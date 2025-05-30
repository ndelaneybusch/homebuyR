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
