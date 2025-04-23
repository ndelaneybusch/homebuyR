library(testthat)

# Tests for fct_budgeting.R

test_that("housing_budget_from_dpi: expected, edge, error, and responsiveness", {
  # Expected case
  expect_equal(housing_budget_from_dpi(3000, 0.3, 0.4, 200), 900)
  # Edge: zero income
  expect_equal(housing_budget_from_dpi(0, 0.3, 0.4, 0), 0)
  # Edge: zero housing pct
  expect_equal(housing_budget_from_dpi(3000, 0, 0.4, 0), 0)
  # Edge: zero debt pct
  expect_equal(housing_budget_from_dpi(3000, 0.3, 0, 0), 0)
  # Edge: other_debts exceeds total_debt_max
  expect_equal(housing_budget_from_dpi(3000, 0.5, 0.4, 2000), 0)
  # Responsiveness: increasing net_income increases budget (housing_max constraint active)
  expect_gt(housing_budget_from_dpi(4000, 0.3, 0.3, 0), housing_budget_from_dpi(3000, 0.3, 0.3, 0))
  # Responsiveness: increasing max_housing_pct increases budget (housing_max constraint active)
  expect_gt(housing_budget_from_dpi(3000, 0.4, 0.4, 0), housing_budget_from_dpi(3000, 0.3, 0.4, 0))
  # Responsiveness: increasing max_total_debt_pct increases budget (total_debt_max constraint active)
  expect_gt(housing_budget_from_dpi(3000, 0.5, 0.4, 800), housing_budget_from_dpi(3000, 0.5, 0.3, 800))
  # Responsiveness: increasing other_debts decreases budget (total_debt_max constraint active)
  expect_lt(housing_budget_from_dpi(3000, 0.5, 0.4, 900), housing_budget_from_dpi(3000, 0.5, 0.4, 800))
  # Error: negative income
  expect_error(housing_budget_from_dpi(-1, 0.3, 0.4, 0))
  # Error: negative housing pct
  expect_error(housing_budget_from_dpi(3000, -0.1, 0.4, 0))
  # Error: housing pct > 1
  expect_error(housing_budget_from_dpi(3000, 1.1, 0.4, 0))
  # Error: negative debt pct
  expect_error(housing_budget_from_dpi(3000, 0.3, -0.1, 0))
  # Error: debt pct > 1
  expect_error(housing_budget_from_dpi(3000, 0.3, 1.1, 0))
  # Error: negative other_debts
  expect_error(housing_budget_from_dpi(3000, 0.3, 0.4, -1))
  # Error: NA
  expect_error(housing_budget_from_dpi(NA, 0.3, 0.4, 0))
  # Error: Inf
  expect_error(housing_budget_from_dpi(Inf, 0.3, 0.4, 0))
})

test_that("housing_budget_from_stressed_dti: expected, edge, error, and responsiveness", {
  # Expected: default stress
  expect_equal(housing_budget_from_stressed_dti(5000, 500, 1000, 0, 1), 1500)
  # Edge: zero income
  expect_equal(housing_budget_from_stressed_dti(0, 0, 0, 0, 1), 0)
  # Edge: zero debts and essentials
  expect_equal(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, max_total_dti_stress=1), 4000)
  # Edge: zero shock (no stress)
  expect_equal(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=0, shock_duration_months=1, max_total_dti_stress=1), 5000)
  # Edge: max shock (all income lost)
  expect_equal(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=1, shock_duration_months=1, max_total_dti_stress=1), 0)
  # Responsiveness: increasing gross_monthly_income increases budget (max_total_dti_stress=1, no debts/essentials)
  expect_gt(housing_budget_from_stressed_dti(6000, 0, 0, 0, 1, max_total_dti_stress=1), housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, max_total_dti_stress=1))
  # Responsiveness: increasing other_debts decreases budget (max_total_dti_stress=1, no essentials)
  expect_lt(housing_budget_from_stressed_dti(5000, 500, 0, 0, 1, max_total_dti_stress=1), housing_budget_from_stressed_dti(5000, 400, 0, 0, 1, max_total_dti_stress=1))
  # Responsiveness: increasing non_housing_essentials decreases budget (max_total_dti_stress=1, no debts)
  expect_lt(housing_budget_from_stressed_dti(5000, 0, 200, 0, 1, max_total_dti_stress=1), housing_budget_from_stressed_dti(5000, 0, 100, 0, 1, max_total_dti_stress=1))
  # Responsiveness: increasing income_shock_pct decreases budget (max_total_dti_stress=1, no debts/essentials)
  expect_lt(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=0.2, max_total_dti_stress=1), housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=0.1, max_total_dti_stress=1))
  # Responsiveness: increasing max_total_dti_stress increases budget (income_shock_pct=0, no debts/essentials)
  expect_gt(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=0, max_total_dti_stress=0.9), housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=0, max_total_dti_stress=0.8))
  # Error: negative input
  expect_error(housing_budget_from_stressed_dti(-1, 0, 0, 0, 1))
  expect_error(housing_budget_from_stressed_dti(5000, -1, 0, 0, 1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, -1, 0, 1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, -0.1, 1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 0))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=-0.1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, income_shock_pct=1.1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, shock_duration_months=0))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, max_total_dti_stress=-0.1))
  expect_error(housing_budget_from_stressed_dti(5000, 0, 0, 0, 1, max_total_dti_stress=1.1))
  # Error: NA
  expect_error(housing_budget_from_stressed_dti(NA, 0, 0, 0, 1))
  # Error: Inf
  expect_error(housing_budget_from_stressed_dti(Inf, 0, 0, 0, 1))
})

test_that("housing_budget_from_gross_pct: expected, edge, error, and responsiveness", {
  # Expected
  expect_equal(housing_budget_from_gross_pct(5000, 30), 1500)
  # Edge: zero income
  expect_equal(housing_budget_from_gross_pct(0, 30), 0)
  # Edge: zero percent
  expect_equal(housing_budget_from_gross_pct(5000, 0), 0)
  # Edge: 100 percent
  expect_equal(housing_budget_from_gross_pct(5000, 100), 5000)
  # Responsiveness: increasing gross_monthly_income increases budget
  expect_gt(housing_budget_from_gross_pct(6000, 30), housing_budget_from_gross_pct(5000, 30))
  # Responsiveness: increasing housing_percent increases budget
  expect_gt(housing_budget_from_gross_pct(5000, 40), housing_budget_from_gross_pct(5000, 30))
  # Error: negative income
  expect_error(housing_budget_from_gross_pct(-1, 30))
  # Error: negative percent
  expect_error(housing_budget_from_gross_pct(5000, -1))
  # Error: percent > 100
  expect_error(housing_budget_from_gross_pct(5000, 101))
  # Error: NA
  expect_error(housing_budget_from_gross_pct(NA, 30))
  # Error: Inf
  expect_error(housing_budget_from_gross_pct(Inf, 30))
})
