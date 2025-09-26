# --- Test Setup ---
# Define common refinance scenarios to avoid repetition

# Scenario 1: Standard Refinance (300k remaining, 6% to 4%, 25 years left)
p1 <- 300000
r_old1 <- 0.06 / 12
r_new1 <- 0.04 / 12
n_rem1 <- 300
closing1 <- 5000
tax1 <- 0.25
inv1 <- 0.05
mid1 <- 750000

# Scenario 2: Rate Increase Refinance (200k remaining, 3% to 5%, 15 years left)
p2 <- 200000
r_old2 <- 0.03 / 12
r_new2 <- 0.05 / 12
n_rem2 <- 180
closing2 <- 3000
tax2 <- 0.30
inv2 <- 0.04
mid2 <- 750000

# Scenario 3: Zero Interest Cases
p3 <- 100000
r_old3 <- 0.0
r_new3 <- 0.0
n_rem3 <- 120
closing3 <- 1000
tax3 <- 0.0
inv3 <- 0.0
mid3 <- 750000

# Scenario 4: High Balance MID Limit Case
p4 <- 800000
r_old4 <- 0.07 / 12
r_new4 <- 0.05 / 12
n_rem4 <- 360
closing4 <- 8000
tax4 <- 0.37
inv4 <- 0.06
mid4 <- 750000

# --- Testing calculate_invested_savings_fv ---

test_that("calculate_invested_savings_fv calculates correctly", {
  # Standard case with positive return
  monthly_payment <- 200
  rate_per_month <- 0.04 / 12
  n_periods <- 60

  # Manual calculation: FV of annuity = PMT * ((1+r)^n - 1) / r
  expected_fv <- monthly_payment * ((1 + rate_per_month)^n_periods - 1) / rate_per_month
  expect_equal(calculate_invested_savings_fv(monthly_payment, rate_per_month, n_periods), expected_fv)

  # Zero return case
  expect_equal(calculate_invested_savings_fv(200, 0, 60), 12000)
  expect_equal(calculate_invested_savings_fv(150, 0, 24), 3600)
})

test_that("calculate_invested_savings_fv handles edge cases", {
  # Zero periods
  expect_equal(calculate_invested_savings_fv(200, 0.04 / 12, 0), 0)
  expect_equal(calculate_invested_savings_fv(0, 0.04 / 12, 60), 0)

  # Very small return rates (near zero but not zero)
  small_rate <- 0.0001 / 12
  expect_gt(calculate_invested_savings_fv(200, small_rate, 60), 12000)
  expect_lt(calculate_invested_savings_fv(200, small_rate, 60), 12100)
})

test_that("calculate_invested_savings_fv input validation works", {
  expect_error(calculate_invested_savings_fv(-100, 0.04 / 12, 60))  # Negative payment
  expect_error(calculate_invested_savings_fv(200, -0.01, 60))       # Negative rate
  expect_error(calculate_invested_savings_fv(200, 0.04 / 12, -1))   # Negative periods
  expect_error(calculate_invested_savings_fv(200, 0.04 / 12, 60.5)) # Non-integer periods
  expect_error(calculate_invested_savings_fv("abc", 0.04 / 12, 60)) # Non-numeric payment
})

test_that("calculate_invested_savings_fv sensitivity", {
  base_fv <- calculate_invested_savings_fv(200, 0.04 / 12, 60)

  # Higher payment -> higher FV
  expect_gt(calculate_invested_savings_fv(220, 0.04 / 12, 60), base_fv)

  # Higher rate -> higher FV
  expect_gt(calculate_invested_savings_fv(200, 0.05 / 12, 60), base_fv)

  # More periods -> higher FV
  expect_gt(calculate_invested_savings_fv(200, 0.04 / 12, 72), base_fv)
})

# --- Testing calculate_tax_savings_differential ---

test_that("calculate_tax_savings_differential calculates correctly", {
  # Case where old loan has higher interest (typical refinance scenario)
  old_int <- 15000
  new_int <- 12000
  old_bal <- 300000
  new_bal <- 280000  # Lower due to lump sum paydown
  tax_rate <- 0.25
  mid_limit <- 750000

  # Both loans are under MID limit, so full deduction
  old_deduction <- old_int * tax_rate
  new_deduction <- new_int * tax_rate
  expected_diff <- new_deduction - old_deduction  # Should be negative

  expect_equal(calculate_tax_savings_differential(old_int, new_int, old_bal, new_bal, tax_rate, mid_limit),
               expected_diff)
})

test_that("calculate_tax_savings_differential handles MID limit correctly", {
  # Case where balance exceeds MID limit
  old_int <- 20000
  new_int <- 18000
  old_bal <- 800000  # Over 750k limit
  new_bal <- 760000  # Still over but closer
  tax_rate <- 0.30
  mid_limit <- 750000

  # Calculate limited deductions
  old_deductible <- old_int * (mid_limit / old_bal)
  new_deductible <- new_int * (mid_limit / new_bal)
  old_savings <- old_deductible * tax_rate
  new_savings <- new_deductible * tax_rate
  expected_diff <- new_savings - old_savings

  expect_equal(calculate_tax_savings_differential(old_int, new_int, old_bal, new_bal, tax_rate, mid_limit),
               expected_diff)
})

test_that("calculate_tax_savings_differential handles zero cases", {
  # Zero tax rate
  expect_equal(calculate_tax_savings_differential(10000, 8000, 300000, 280000, 0, 750000), 0)

  # Zero interest paid
  expect_equal(calculate_tax_savings_differential(0, 0, 300000, 280000, 0.25, 750000), 0)

  # Zero balances
  expect_equal(calculate_tax_savings_differential(10000, 8000, 0, 0, 0.25, 750000), 0)
})

test_that("calculate_tax_savings_differential input validation works", {
  expect_error(calculate_tax_savings_differential(-1000, 8000, 300000, 280000, 0.25, 750000))  # Negative old interest
  expect_error(calculate_tax_savings_differential(10000, -8000, 300000, 280000, 0.25, 750000)) # Negative new interest
  expect_error(calculate_tax_savings_differential(10000, 8000, -300000, 280000, 0.25, 750000)) # Negative old balance
  expect_error(calculate_tax_savings_differential(10000, 8000, 300000, -280000, 0.25, 750000)) # Negative new balance
  expect_error(calculate_tax_savings_differential(10000, 8000, 300000, 280000, -0.25, 750000)) # Negative tax rate
  expect_error(calculate_tax_savings_differential(10000, 8000, 300000, 280000, 1.25, 750000))  # Tax rate > 1
  expect_error(calculate_tax_savings_differential(10000, 8000, 300000, 280000, 0.25, -750000)) # Negative MID limit
})

# --- Testing calculate_refinance_benefit_curve ---

test_that("calculate_refinance_benefit_curve basic functionality", {
  result <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1)

  # Check return structure
  expect_true(is.list(result))
  expect_true(all(c("months", "net_benefits", "breakeven_month", "old_payment", "new_payment", "monthly_savings") %in% names(result)))

  # Check dimensions
  expect_equal(length(result$months), n_rem1)
  expect_equal(length(result$net_benefits), n_rem1)

  # Check payment calculations
  expected_old_payment <- compute_monthly_payment(p1, r_old1, n_rem1)
  expected_new_payment <- compute_monthly_payment(p1, r_new1, n_rem1)
  expect_equal(result$old_payment, expected_old_payment)
  expect_equal(result$new_payment, expected_new_payment)
  expect_equal(result$monthly_savings, expected_old_payment - expected_new_payment)

  # Check monotonic improvement (benefits should generally increase over time for good refinance)
  expect_lt(result$net_benefits[1], result$net_benefits[n_rem1])
})

test_that("calculate_refinance_benefit_curve handles rate increase scenario", {
  # When new rate is higher, refinancing is typically bad
  result <- calculate_refinance_benefit_curve(p2, r_old2, r_new2, n_rem2, closing2)

  # Monthly savings should be negative (new payment higher)
  expect_lt(result$monthly_savings, 0)

  # Benefits should likely be negative throughout (bad refinance)
  expect_lt(tail(result$net_benefits, 1), 0)

  # Breakeven should likely never occur
  expect_true(is.na(result$breakeven_month))
})

test_that("calculate_refinance_benefit_curve handles zero interest rates", {
  result <- calculate_refinance_benefit_curve(p3, r_old3, r_new3, n_rem3, closing3)

  # With same zero rates, only difference should be closing costs and lump sum
  expect_equal(result$monthly_savings, 0)
  expect_true(all(result$net_benefits == -closing3))  # Just negative closing costs
  expect_true(is.na(result$breakeven_month))
})

test_that("calculate_refinance_benefit_curve with lump sum paydown", {
  lump_sum <- 50000
  result_no_lump <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1)
  result_with_lump <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                                      lump_sum_paydown = lump_sum)

  # New payment should be lower with lump sum
  expect_lt(result_with_lump$new_payment, result_no_lump$new_payment)

  # Monthly savings should be higher with lump sum
  expect_gt(result_with_lump$monthly_savings, result_no_lump$monthly_savings)

  # Benefits should be better with lump sum (all else equal)
  expect_gt(tail(result_with_lump$net_benefits, 1), tail(result_no_lump$net_benefits, 1))
})

test_that("calculate_refinance_benefit_curve with investment return", {
  result_no_inv <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                                   investment_return_annual = 0)
  result_with_inv <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                                     investment_return_annual = inv1)

  # Benefits should be better with investment return (payment savings grow)
  expect_gt(tail(result_with_inv$net_benefits, 1), tail(result_no_inv$net_benefits, 1))

  # Early benefits may be similar, later benefits should diverge more
  expect_lt(result_with_inv$net_benefits[12] - result_no_inv$net_benefits[12],
           result_with_inv$net_benefits[120] - result_no_inv$net_benefits[120])
})

test_that("calculate_refinance_benefit_curve handles MID limit scenarios", {
  # Test with high balance over MID limit
  result <- calculate_refinance_benefit_curve(p4, r_old4, r_new4, n_rem4, closing4,
                                            tax_rate = tax4, mid_limit = mid4)

  # Should still produce valid results
  expect_true(is.list(result))
  expect_equal(length(result$net_benefits), n_rem4)

  # Compare with very high MID limit (effectively no limit)
  result_no_limit <- calculate_refinance_benefit_curve(p4, r_old4, r_new4, n_rem4, closing4,
                                                     tax_rate = tax4, mid_limit = 10000000)

  # Benefits should be different due to tax treatment
  expect_false(all(result$net_benefits == result_no_limit$net_benefits))
})

test_that("calculate_refinance_benefit_curve breakeven logic", {
  # Create a scenario that should break even
  small_closing <- 1000  # Small closing costs
  result <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, small_closing,
                                            investment_return_annual = inv1)

  # Should find a breakeven month
  expect_false(is.na(result$breakeven_month))
  expect_true(result$breakeven_month >= 1 && result$breakeven_month <= n_rem1)

  # Benefit at breakeven month should be positive
  expect_gt(result$net_benefits[result$breakeven_month], 0)

  # Previous month should be negative (if breakeven > 1)
  if (result$breakeven_month > 1) {
    expect_lt(result$net_benefits[result$breakeven_month - 1], 0)
  }
})

test_that("calculate_refinance_benefit_curve input validation works", {
  expect_error(calculate_refinance_benefit_curve(0, r_old1, r_new1, n_rem1, closing1))         # Zero principal
  expect_error(calculate_refinance_benefit_curve(-p1, r_old1, r_new1, n_rem1, closing1))       # Negative principal
  expect_error(calculate_refinance_benefit_curve(p1, -r_old1, r_new1, n_rem1, closing1))       # Negative old rate
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, -r_new1, n_rem1, closing1))       # Negative new rate
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, 0, closing1))             # Zero payments remaining
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, -closing1))       # Negative closing costs
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                               tax_rate = -0.1))                                # Negative tax rate
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                               tax_rate = 1.1))                                 # Tax rate > 1
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                               investment_return_annual = -0.05))               # Negative investment return
  expect_error(calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                               lump_sum_paydown = p1 + 1))                     # Lump sum > principal
})

test_that("calculate_refinance_benefit_curve max_eval_months parameter", {
  eval_months <- 120
  result <- calculate_refinance_benefit_curve(p1, r_old1, r_new1, n_rem1, closing1,
                                            max_eval_months = eval_months)

  expect_equal(length(result$months), eval_months)
  expect_equal(length(result$net_benefits), eval_months)
  expect_equal(max(result$months), eval_months)
})

# --- Test Edge Cases and Numerical Stability ---

test_that("refinance functions handle floating point precision", {
  # Very small rate differences
  tiny_diff <- 0.0001 / 12
  result <- calculate_refinance_benefit_curve(p1, r_old1, r_old1 - tiny_diff, n_rem1, 100)

  # Should still produce sensible results
  expect_true(is.numeric(result$monthly_savings))
  expect_false(is.nan(result$monthly_savings))
  expect_false(is.infinite(result$monthly_savings))

  # Monthly savings should be small but positive
  expect_gt(result$monthly_savings, 0)
  expect_lt(result$monthly_savings, 10)  # Should be very small difference
})

test_that("tax calculations handle MID limit edge cases precisely", {
  # Balance exactly at MID limit
  exact_balance <- 750000
  result1 <- calculate_tax_savings_differential(10000, 8000, exact_balance, exact_balance - 10000, 0.25, 750000)

  # Should produce finite result
  expect_true(is.finite(result1))

  # Balance very close to MID limit
  close_balance <- 750001
  result2 <- calculate_tax_savings_differential(10000, 8000, close_balance, close_balance - 10000, 0.25, 750000)

  # Results should be different but both finite
  expect_true(is.finite(result2))
  expect_false(result1 == result2)
})

# --- Test Realistic Scenarios ---

test_that("refinance scenarios match real-world expectations", {
  # Scenario: Good refinance (2% rate drop, reasonable closing costs)
  good_refi <- calculate_refinance_benefit_curve(
    principal = 400000,
    rate_per_month_old = 0.065 / 12,
    rate_per_month_new = 0.045 / 12,
    n_payments_remaining = 240,  # 20 years left
    closing_costs = 4000,
    tax_rate = 0.24,
    investment_return_annual = 0.06
  )

  # Should have positive monthly savings
  expect_gt(good_refi$monthly_savings, 0)

  # Should break even within reasonable time (< 60 months typically)
  expect_false(is.na(good_refi$breakeven_month))
  expect_lt(good_refi$breakeven_month, 60)

  # Should have substantial positive benefit at end of evaluation period
  expect_gt(tail(good_refi$net_benefits, 1), 50000)

  # Scenario: Marginal refinance (small rate drop, high closing costs)
  marginal_refi <- calculate_refinance_benefit_curve(
    principal = 300000,
    rate_per_month_old = 0.05 / 12,
    rate_per_month_new = 0.045 / 12,  # Only 0.5% drop
    n_payments_remaining = 180,
    closing_costs = 8000,  # High closing costs
    tax_rate = 0.22
  )

  # Might break even, but later and with smaller benefits
  if (!is.na(marginal_refi$breakeven_month)) {
    expect_gt(marginal_refi$breakeven_month, good_refi$breakeven_month)
  }

  # Final benefit should be much smaller
  expect_lt(tail(marginal_refi$net_benefits, 1), tail(good_refi$net_benefits, 1))
})