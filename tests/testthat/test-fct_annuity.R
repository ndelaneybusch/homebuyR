# --- Test Setup ---
# Define common scenarios to avoid repetition

# Scenario 1: Standard Loan (300k, 30yr, 6% APR)
p1 <- 300000
r_yr1 <- 0.06
r_m1 <- r_yr1 / 12
n_yr1 <- 30
n_tot1 <- n_yr1 * 12
# Expected payment (calculated accurately for reference)
m_pay1 <- p1 * r_m1 / (1 - (1 + r_m1)^-n_tot1) # Approx 1798.6515

# Scenario 2: Zero Interest Loan (12k, 1yr, 0% APR)
p2 <- 12000
r_yr2 <- 0.00
r_m2 <- r_yr2 / 12 # which is 0
n_yr2 <- 1
n_tot2 <- n_yr2 * 12
m_pay2 <- p2 / n_tot2 # Exactly 1000

# Scenario 3: Shorter High-Interest Loan (50k, 5yr, 10% APR)
p3 <- 50000
r_yr3 <- 0.10
r_m3 <- r_yr3 / 12
n_yr3 <- 5
n_tot3 <- n_yr3 * 60
m_pay3 <- p3 * r_m3 / (1 - (1 + r_m3)^-n_tot3) # Approx 1062.352


# --- Testing compute_monthly_payment ---

test_that("compute_monthly_payment calculates correctly", {
  expect_equal(compute_monthly_payment(p1, r_m1, n_tot1), m_pay1)
  expect_equal(compute_monthly_payment(p2, r_m2, n_tot2), m_pay2)
  expect_equal(compute_monthly_payment(p3, r_m3, n_tot3), m_pay3)
})

test_that("compute_monthly_payment handles zero interest", {
  expect_equal(compute_monthly_payment(principal = 12000, rate_per_month = 0, n_payments_total = 12), 1000)
  expect_equal(compute_monthly_payment(principal = 5000, rate_per_month = 0, n_payments_total = 10), 500)
})

test_that("compute_monthly_payment input validation works", {
  expect_error(compute_monthly_payment(principal = -100, r_m1, n_tot1)) # Negative principal
  expect_error(compute_monthly_payment(p1, rate_per_month = -0.01, n_tot1)) # Negative rate
  expect_error(compute_monthly_payment(p1, r_m1, n_payments_total = 0)) # Zero payments
  expect_error(compute_monthly_payment(p1, r_m1, n_payments_total = -10)) # Negative payments
  expect_error(compute_monthly_payment(p1, r_m1, n_payments_total = 10.5)) # Non-integer payments
  expect_error(compute_monthly_payment(principal = "abc", r_m1, n_tot1)) # Non-numeric principal
})

test_that("compute_monthly_payment sensitivity", {
  # Higher rate -> higher payment
  expect_gt(compute_monthly_payment(p1, r_m1 * 1.1, n_tot1), m_pay1)
  # Higher principal -> higher payment
  expect_gt(compute_monthly_payment(p1 * 1.1, r_m1, n_tot1), m_pay1)
  # More payments -> lower payment
  expect_lt(compute_monthly_payment(p1, r_m1, n_tot1 + 12), m_pay1)

})


# --- Testing compute_principal ---

test_that("compute_principal calculates correctly", {
  expect_equal(compute_principal(m_pay1, r_m1, n_tot1), p1)
  expect_equal(compute_principal(m_pay2, r_m2, n_tot2), p2)
  expect_equal(compute_principal(m_pay3, r_m3, n_tot3), p3)
})

test_that("compute_principal handles zero interest", {
  expect_equal(compute_principal(monthly_payment = 1000, rate_per_month = 0, n_payments_total = 12), 12000)
  expect_equal(compute_principal(monthly_payment = 500, rate_per_month = 0, n_payments_total = 10), 5000)
})

test_that("compute_principal input validation works", {
  expect_error(compute_principal(monthly_payment = -100, r_m1, n_tot1))
  expect_error(compute_principal(m_pay1, rate_per_month = -0.01, n_tot1))
  expect_error(compute_principal(m_pay1, r_m1, n_payments_total = 0))
  expect_error(compute_principal(m_pay1, r_m1, n_payments_total = -10))
  expect_error(compute_principal(m_pay1, r_m1, n_payments_total = 10.5))
  expect_error(compute_principal(monthly_payment = "abc", r_m1, n_tot1))
})

test_that("compute_principal sensitivity", {
  # Higher payment -> higher principal
  expect_gt(compute_principal(m_pay1 * 1.1, r_m1, n_tot1), p1)
  # Higher rate -> lower principal (for fixed payment)
  expect_lt(compute_principal(m_pay1, r_m1 * 1.1, n_tot1), p1)
  # More payments -> higher principal
  expect_gt(compute_principal(m_pay1, r_m1, n_tot1 + 12), p1)
})


# --- Testing compute_principal_remaining ---

test_that("compute_principal_remaining calculates correctly", {
  # Scenario 1 after 5 years (60 payments made, 300 remaining)
  n_rem1_5yr <- n_tot1 - 60
  p_rem1_5yr_expected <- m_pay1 * calculate_annuity_pv_factor(r_m1, n_rem1_5yr) 
  expect_equal(compute_principal_remaining(m_pay1, r_m1, n_rem1_5yr), p_rem1_5yr_expected)

  # Scenario 3 after 2 years (24 payments made, 36 remaining)
  n_rem3_2yr <- n_tot3 - 24
  p_rem3_2yr_expected <- m_pay3 * calculate_annuity_pv_factor(r_m3, n_rem3_2yr) 
  expect_equal(compute_principal_remaining(m_pay3, r_m3, n_rem3_2yr), p_rem3_2yr_expected)
})

test_that("compute_principal_remaining handles zero interest", {
  # Scenario 2 after 6 months (6 made, 6 remaining)
  expect_equal(compute_principal_remaining(m_pay2, r_m2, 6), 6000)
  expect_equal(compute_principal_remaining(m_pay2, r_m2, 0), 0)
})

test_that("compute_principal_remaining handles edge cases", {
  # End of loan (0 remaining payments)
  expect_equal(compute_principal_remaining(m_pay1, r_m1, 0), 0)
  expect_equal(compute_principal_remaining(m_pay3, r_m3, 0), 0)
  # Beginning of loan (all payments remaining) - should equal original principal
  expect_equal(compute_principal_remaining(m_pay1, r_m1, n_tot1), p1)
  expect_equal(compute_principal_remaining(m_pay3, r_m3, n_tot3), p3)
})

test_that("compute_principal_remaining input validation works", {
  expect_error(compute_principal_remaining(monthly_payment = -100, r_m1, 10))
  expect_error(compute_principal_remaining(m_pay1, rate_per_month = -0.01, 10))
  expect_error(compute_principal_remaining(m_pay1, r_m1, n_payments_remaining = -1))
  expect_error(compute_principal_remaining(m_pay1, r_m1, n_payments_remaining = 10.5))
  expect_error(compute_principal_remaining(monthly_payment = "abc", r_m1, 10))
})

test_that("compute_principal_remaining sensitivity", {
  n_rem_test <- 180 # halfway point for scenario 1
  p_rem_base <- compute_principal_remaining(m_pay1, r_m1, n_rem_test)
  # Fewer remaining payments -> lower remaining principal
  expect_lt(compute_principal_remaining(m_pay1, r_m1, n_rem_test - 1), p_rem_base)
  # Higher rate -> lower remaining principal (PV effect)
  expect_lt(compute_principal_remaining(m_pay1, r_m1 * 1.1, n_rem_test), p_rem_base)
  # Higher payment -> higher remaining principal
  expect_gt(compute_principal_remaining(m_pay1 * 1.1, r_m1, n_rem_test), p_rem_base)
})


# --- Testing compute_principal_paid ---

test_that("compute_principal_paid calculates correctly", {
  # Scenario 1 after 5 years (60 made, 300 remaining)
  n_rem1_5yr <- n_tot1 - 60
  p_rem1_5yr <- compute_principal_remaining(m_pay1, r_m1, n_rem1_5yr)
  p_paid1_5yr_expected <- p1 - p_rem1_5yr # Approx 21280.97
  expect_equal(compute_principal_paid(p1, m_pay1, r_m1, n_rem1_5yr), p_paid1_5yr_expected)

  # Scenario 3 after 2 years (24 made, 36 remaining)
  n_rem3_2yr <- n_tot3 - 24
  p_rem3_2yr <- compute_principal_remaining(m_pay3, r_m3, n_rem3_2yr)
  p_paid3_2yr_expected <- p3 - p_rem3_2yr # Approx 16581.98
  expect_equal(compute_principal_paid(p3, m_pay3, r_m3, n_rem3_2yr), p_paid3_2yr_expected)
})

test_that("compute_principal_paid handles zero interest", {
  # Scenario 2 after 6 months (6 made, 6 remaining)
  expect_equal(compute_principal_paid(p2, m_pay2, r_m2, 6), 6000) # 12000 - 6000
  expect_equal(compute_principal_paid(p2, m_pay2, r_m2, 0), p2) # Paid in full
  expect_equal(compute_principal_paid(p2, m_pay2, r_m2, n_tot2), 0) # Paid nothing yet
})

test_that("compute_principal_paid handles edge cases", {
  # End of loan (0 remaining payments) -> principal paid = original principal
  expect_equal(compute_principal_paid(p1, m_pay1, r_m1, 0), p1)
  expect_equal(compute_principal_paid(p3, m_pay3, r_m3, 0), p3)
  # Beginning of loan (all payments remaining) -> principal paid = 0
  expect_equal(compute_principal_paid(p1, m_pay1, r_m1, n_tot1), 0)
  expect_equal(compute_principal_paid(p3, m_pay3, r_m3, n_tot3), 0)
})

test_that("compute_principal_paid input validation works", {
  expect_error(compute_principal_paid(principal = -100, m_pay1, r_m1, 10)) # Invalid principal
  # Errors from compute_principal_remaining are implicitly tested
  expect_error(compute_principal_paid(p1, monthly_payment = -100, r_m1, 10))
  expect_error(compute_principal_paid(p1, m_pay1, rate_per_month = -0.01, 10))
  expect_error(compute_principal_paid(p1, m_pay1, r_m1, n_payments_remaining = -1))
  expect_error(compute_principal_paid(p1, m_pay1, r_m1, n_payments_remaining = 10.5))
})

test_that("compute_principal_paid sensitivity", {
  n_rem_test <- 180 # halfway point for scenario 1
  p_paid_base <- compute_principal_paid(p1, m_pay1, r_m1, n_rem_test)
  # Fewer remaining payments -> more principal paid
  expect_gt(compute_principal_paid(p1, m_pay1, r_m1, n_rem_test - 1), p_paid_base)
})

# --- Testing compute_total_interest_remaining ---

test_that("compute_total_interest_remaining calculates correctly", {
  # Scenario 1 after 5 years (300 remaining)
  n_rem1_5yr <- n_tot1 - 60
  p_rem1_5yr <- compute_principal_remaining(m_pay1, r_m1, n_rem1_5yr)
  i_rem1_5yr_expected <- (m_pay1 * n_rem1_5yr) - p_rem1_5yr # Approx 260876.42
  expect_equal(compute_total_interest_remaining(m_pay1, r_m1, n_rem1_5yr), i_rem1_5yr_expected)

  # Scenario 3 after 2 years (36 remaining)
  n_rem3_2yr <- n_tot3 - 24
  p_rem3_2yr <- compute_principal_remaining(m_pay3, r_m3, n_rem3_2yr)
  i_rem3_2yr_expected <- (m_pay3 * n_rem3_2yr) - p_rem3_2yr # Approx 4806.65
  expect_equal(compute_total_interest_remaining(m_pay3, r_m3, n_rem3_2yr), i_rem3_2yr_expected)
})

test_that("compute_total_interest_remaining handles zero interest", {
  expect_equal(compute_total_interest_remaining(m_pay2, r_m2, 6), 0)
  expect_equal(compute_total_interest_remaining(m_pay2, r_m2, 0), 0)
})

test_that("compute_total_interest_remaining handles edge cases", {
  # End of loan (0 remaining payments) -> 0 interest remaining
  expect_equal(compute_total_interest_remaining(m_pay1, r_m1, 0), 0)
  expect_equal(compute_total_interest_remaining(m_pay3, r_m3, 0), 0)
})

test_that("compute_total_interest_remaining input validation works", {
  # Errors from compute_principal_remaining are implicitly tested
  expect_error(compute_total_interest_remaining(monthly_payment = -100, r_m1, 10))
  expect_error(compute_total_interest_remaining(m_pay1, rate_per_month = -0.01, 10))
  expect_error(compute_total_interest_remaining(m_pay1, r_m1, n_payments_remaining = -1))
  expect_error(compute_total_interest_remaining(m_pay1, r_m1, n_payments_remaining = 10.5))
})

test_that("compute_total_interest_remaining sensitivity", {
  n_rem_test <- 180 # halfway point for scenario 1
  i_rem_base <- compute_total_interest_remaining(m_pay1, r_m1, n_rem_test)
  # Fewer remaining payments -> less interest remaining
  expect_lt(compute_total_interest_remaining(m_pay1, r_m1, n_rem_test - 1), i_rem_base)
  # Higher rate -> more interest remaining
  expect_gt(compute_total_interest_remaining(m_pay1, r_m1 * 1.1, n_rem_test), i_rem_base)
})

# --- Testing compute_interest_paid ---

test_that("compute_interest_paid calculates correctly", {
  # Scenario 1 after 5 years (60 made, 300 remaining)
  n_rem1_5yr <- n_tot1 - 60
  n_made1_5yr <- 60
  p_paid1_5yr <- compute_principal_paid(p1, m_pay1, r_m1, n_rem1_5yr)
  i_paid1_5yr_expected <- (m_pay1 * n_made1_5yr) - p_paid1_5yr # Approx 86638.12
  expect_equal(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_rem1_5yr), i_paid1_5yr_expected)

  # Scenario 3 after 2 years (24 made, 36 remaining)
  n_rem3_2yr <- n_tot3 - 24
  n_made3_2yr <- 24
  p_paid3_2yr <- compute_principal_paid(p3, m_pay3, r_m3, n_rem3_2yr)
  i_paid3_2yr_expected <- (m_pay3 * n_made3_2yr) - p_paid3_2yr # Approx 8914.47
  expect_equal(compute_interest_paid(p3, m_pay3, r_m3, n_tot3, n_rem3_2yr), i_paid3_2yr_expected)
})

test_that("compute_interest_paid handles zero interest", {
  expect_equal(compute_interest_paid(p2, m_pay2, r_m2, n_tot2, 6), 0)
  expect_equal(compute_interest_paid(p2, m_pay2, r_m2, n_tot2, 0), 0)
  expect_equal(compute_interest_paid(p2, m_pay2, r_m2, n_tot2, n_tot2), 0)
})

test_that("compute_interest_paid handles edge cases", {
  # End of loan (0 remaining payments) -> interest paid = total paid - principal
  total_paid1 <- m_pay1 * n_tot1
  total_interest1 <- total_paid1 - p1
  expect_equal(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, 0), total_interest1)

  total_paid3 <- m_pay3 * n_tot3
  total_interest3 <- total_paid3 - p3
  expect_equal(compute_interest_paid(p3, m_pay3, r_m3, n_tot3, 0), total_interest3)

  # Beginning of loan (all payments remaining) -> interest paid = 0
  expect_equal(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_tot1), 0)
  expect_equal(compute_interest_paid(p3, m_pay3, r_m3, n_tot3, n_tot3), 0)
})

test_that("compute_interest_paid input validation works", {
  # Errors from compute_principal_paid are implicitly tested
  expect_error(compute_interest_paid(principal = -100, m_pay1, r_m1, n_tot1, 10))
  expect_error(compute_interest_paid(p1, monthly_payment = -100, r_m1, n_tot1, 10))
  expect_error(compute_interest_paid(p1, m_pay1, rate_per_month = -0.01, n_tot1, 10))
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_payments_total = 0, 0))
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_payments_total = -10, -11))
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_payments_total = 10.5, 5))
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_payments_remaining = -1))
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_payments_remaining = 10.5))
  # Remaining cannot exceed total
  expect_error(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_tot1 + 1))

})

test_that("compute_interest_paid sensitivity", {
  n_rem_test <- 180 # halfway point for scenario 1
  i_paid_base <- compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_rem_test)
  # Fewer remaining payments -> more interest paid
  expect_gt(compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_rem_test - 1), i_paid_base)
})

# --- Test Relationships ---
test_that("Relationships between amounts hold", {
  # Use Scenario 1 after 5 years (60 made, 300 remaining)
  n_rem <- n_tot1 - 60
  n_made <- 60

  p_rem <- compute_principal_remaining(m_pay1, r_m1, n_rem)
  p_paid <- compute_principal_paid(p1, m_pay1, r_m1, n_rem)
  i_rem <- compute_total_interest_remaining(m_pay1, r_m1, n_rem)
  i_paid <- compute_interest_paid(p1, m_pay1, r_m1, n_tot1, n_rem)

  # Principal Paid + Principal Remaining = Original Principal
  expect_equal(p_paid + p_rem, p1)

  # Interest Paid + Interest Remaining = Total Interest Over Life
  total_interest = (m_pay1 * n_tot1) - p1
  expect_equal(i_paid + i_rem, total_interest)

  # Principal Paid + Interest Paid = Total Payments Made
  expect_equal(p_paid + i_paid, m_pay1 * n_made)

  # Principal Remaining + Interest Remaining = Total Remaining Payments
  expect_equal(p_rem + i_rem, m_pay1 * n_rem)

})

# --- Testing calculate_annuity_pv_factor --- 

test_that("calculate_annuity_pv_factor calculates correctly", {
  # Expected factors based on scenarios
  pvf1_expected <- (1 - (1 + r_m1)^-n_tot1) / r_m1 # Approx 166.7916
  pvf2_expected <- n_tot2 # Exactly 12
  pvf3_expected <- (1 - (1 + r_m3)^-n_tot3) / r_m3 # Approx 47.0654
  
  expect_equal(calculate_annuity_pv_factor(r_m1, n_tot1), pvf1_expected)
  expect_equal(calculate_annuity_pv_factor(r_m2, n_tot2), pvf2_expected)
  expect_equal(calculate_annuity_pv_factor(r_m3, n_tot3), pvf3_expected)
})

test_that("calculate_annuity_pv_factor handles zero interest", {
  expect_equal(calculate_annuity_pv_factor(rate_per_period = 0, n_periods = 12), 12)
  expect_equal(calculate_annuity_pv_factor(rate_per_period = 0, n_periods = 1), 1)
  expect_equal(calculate_annuity_pv_factor(rate_per_period = 0, n_periods = 360), 360)
})

test_that("calculate_annuity_pv_factor input validation works", {
  expect_error(calculate_annuity_pv_factor(rate_per_period = -0.01, n_periods = 10)) # Negative rate
  expect_error(calculate_annuity_pv_factor(rate_per_period = 0.01, n_periods = 0))   # Zero periods
  expect_error(calculate_annuity_pv_factor(rate_per_period = 0.01, n_periods = -10)) # Negative periods
  expect_error(calculate_annuity_pv_factor(rate_per_period = 0.01, n_periods = 10.5))# Non-integer periods
  expect_error(calculate_annuity_pv_factor(rate_per_period = "abc", n_periods = 10)) # Non-numeric rate
  expect_error(calculate_annuity_pv_factor(rate_per_period = 0.01, n_periods = "abc"))# Non-numeric periods
})

test_that("calculate_annuity_pv_factor sensitivity", {
  pvf_base <- calculate_annuity_pv_factor(0.005, 180) # Base case
  # Higher rate -> lower factor
  expect_lt(calculate_annuity_pv_factor(0.006, 180), pvf_base)
  # More periods -> higher factor
  expect_gt(calculate_annuity_pv_factor(0.005, 181), pvf_base)
})
