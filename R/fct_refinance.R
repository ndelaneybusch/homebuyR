# R Functions for Refinance Analysis

#' Calculate Future Value of Monthly Savings
#'
#' Computes the future value of monthly payment savings invested at a given rate.
#' Handles edge case of zero investment return.
#'
#' @param monthly_payment Numeric. Monthly payment difference to invest.
#'   Must be non-negative.
#' @param rate_per_month Numeric. Monthly investment return rate (as decimal).
#'   Must be non-negative.
#' @param n_periods Integer. Number of months to compound.
#'   Must be non-negative.
#'
#' @return Numeric. Future value of invested savings.
#' @export
#' @keywords internal
#'
#' @examples
#' # Future value of $200/month savings invested at 4% annual return for 60 months
#' calculate_invested_savings_fv(200, 0.04 / 12, 60)
#'
#' # Future value with zero return (simple sum)
#' calculate_invested_savings_fv(200, 0, 60)
#' # Expected: 12000
calculate_invested_savings_fv <- function(monthly_payment, rate_per_month, n_periods) {
  stopifnot(is.numeric(monthly_payment), monthly_payment >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_periods), n_periods >= 0, n_periods == floor(n_periods))

  if (n_periods == 0) {
    return(0)
  }

  if (rate_per_month == 0) {
    # Simple sum with no investment growth
    return(monthly_payment * n_periods)
  } else {
    # Future value of ordinary annuity formula
    return(monthly_payment * ((1 + rate_per_month)^n_periods - 1) / rate_per_month)
  }
}

#' Calculate Mortgage Interest Deduction Differential
#'
#' Computes the tax savings difference between old and new loans,
#' accounting for mortgage interest deduction limits.
#'
#' @param old_interest_paid Numeric. Cumulative interest paid on old loan.
#'   Must be non-negative.
#' @param new_interest_paid Numeric. Cumulative interest paid on new loan.
#'   Must be non-negative.
#' @param old_balance Numeric. Current balance of old loan (for deduction limit).
#'   Must be non-negative.
#' @param new_balance Numeric. Current balance of new loan (for deduction limit).
#'   Must be non-negative.
#' @param tax_rate Numeric. Marginal tax rate (as decimal).
#'   Must be between 0 and 1.
#' @param mid_limit Numeric. Mortgage interest deduction limit.
#'   Must be non-negative.
#'
#' @return Numeric. Tax savings differential (positive = new loan saves more).
#' @export
#' @keywords internal
#'
#' @examples
#' # Tax savings with old loan having higher interest, both under MID limit
#' calculate_tax_savings_differential(15000, 12000, 300000, 280000, 0.25, 750000)
#'
#' # Tax savings with balance over MID limit
#' calculate_tax_savings_differential(20000, 18000, 800000, 760000, 0.25, 750000)
calculate_tax_savings_differential <- function(old_interest_paid,
                                             new_interest_paid,
                                             old_balance,
                                             new_balance,
                                             tax_rate,
                                             mid_limit) {
  stopifnot(is.numeric(old_interest_paid), old_interest_paid >= 0)
  stopifnot(is.numeric(new_interest_paid), new_interest_paid >= 0)
  stopifnot(is.numeric(old_balance), old_balance >= 0)
  stopifnot(is.numeric(new_balance), new_balance >= 0)
  stopifnot(is.numeric(tax_rate), tax_rate >= 0, tax_rate <= 1)
  stopifnot(is.numeric(mid_limit), mid_limit >= 0)

  # Calculate deductible factors based on MID limit
  old_deductible_factor <- if (old_balance > 0) min(1, mid_limit / old_balance) else 0
  new_deductible_factor <- if (new_balance > 0) min(1, mid_limit / new_balance) else 0

  # Apply deductibility limits
  old_deductible_interest <- old_interest_paid * old_deductible_factor
  new_deductible_interest <- new_interest_paid * new_deductible_factor

  # Calculate tax savings
  old_tax_savings <- old_deductible_interest * tax_rate
  new_tax_savings <- new_deductible_interest * tax_rate

  # Return differential (positive = new loan provides more tax savings)
  return(new_tax_savings - old_tax_savings)
}

#' Calculate Refinance Benefit Over Time
#'
#' Computes the cumulative net benefit of refinancing vs. keeping existing loan
#' for each month of a specified holding period. Handles both beneficial refinances
#' (rate reductions) and detrimental refinances (rate increases) by properly
#' accounting for investment opportunities or additional costs.
#'
#' @param principal Numeric. Remaining principal on existing loan.
#'   Must be positive.
#' @param rate_per_month_old Numeric. Monthly interest rate of existing loan (as decimal).
#'   Must be non-negative.
#' @param rate_per_month_new Numeric. Monthly interest rate of refinance loan (as decimal).
#'   Must be non-negative.
#' @param n_payments_remaining Integer. Months remaining on existing loan.
#'   Must be positive.
#' @param closing_costs Numeric. Total cost of refinancing.
#'   Must be non-negative.
#' @param tax_rate Numeric. Marginal tax rate for interest deduction (as decimal).
#'   Must be between 0 and 1. Default: 0.25.
#' @param investment_return_annual Numeric. After-tax annual return rate for investing savings (as decimal).
#'   Must be non-negative. Default: 0.0.
#' @param lump_sum_paydown Numeric. Optional additional principal payment at refinance.
#'   Must be non-negative. Default: 0.
#' @param mid_limit Numeric. Mortgage interest deduction limit.
#'   Must be non-negative. Default: 750000.
#' @param max_eval_months Integer. Maximum months to evaluate.
#'   Must be positive. Default: n_payments_remaining.
#'
#' @return List containing:
#'   - months: Vector of evaluation months (1 to max_eval_months)
#'   - net_benefits: Vector of cumulative net benefits for each month
#'   - breakeven_month: First month where benefit > 0 (NA if none)
#'   - old_payment: Monthly payment on existing loan
#'   - new_payment: Monthly payment on refinance loan
#'   - monthly_savings: Difference in monthly payments
#'
#' @export
#'
#' @examples
#' # Basic refinance analysis: 300k remaining, 6% to 4%, 25 years left
#' result <- calculate_refinance_benefit_curve(
#'   principal = 300000,
#'   rate_per_month_old = 0.06 / 12,
#'   rate_per_month_new = 0.04 / 12,
#'   n_payments_remaining = 300,
#'   closing_costs = 5000
#' )
#'
#' # With investment return and lump sum paydown
#' result2 <- calculate_refinance_benefit_curve(
#'   principal = 300000,
#'   rate_per_month_old = 0.06 / 12,
#'   rate_per_month_new = 0.04 / 12,
#'   n_payments_remaining = 300,
#'   closing_costs = 5000,
#'   investment_return_annual = 0.05,
#'   lump_sum_paydown = 20000
#' )
calculate_refinance_benefit_curve <- function(principal,
                                            rate_per_month_old,
                                            rate_per_month_new,
                                            n_payments_remaining,
                                            closing_costs,
                                            tax_rate = 0.25,
                                            investment_return_annual = 0.0,
                                            lump_sum_paydown = 0,
                                            mid_limit = 750000,
                                            max_eval_months = n_payments_remaining) {
  # Input validation
  stopifnot(is.numeric(principal), principal > 0)
  stopifnot(is.numeric(rate_per_month_old), rate_per_month_old >= 0)
  stopifnot(is.numeric(rate_per_month_new), rate_per_month_new >= 0)
  stopifnot(is.numeric(n_payments_remaining), n_payments_remaining > 0, n_payments_remaining == floor(n_payments_remaining))
  stopifnot(is.numeric(closing_costs), closing_costs >= 0)
  stopifnot(is.numeric(tax_rate), tax_rate >= 0, tax_rate <= 1)
  stopifnot(is.numeric(investment_return_annual), investment_return_annual >= 0)
  stopifnot(is.numeric(lump_sum_paydown), lump_sum_paydown >= 0)
  stopifnot(is.numeric(mid_limit), mid_limit >= 0)
  stopifnot(is.numeric(max_eval_months), max_eval_months > 0, max_eval_months == floor(max_eval_months))
  if (lump_sum_paydown > principal) {
    stop("Lump sum paydown cannot exceed remaining principal")
  }

  # Calculate monthly investment return rate
  investment_rate_per_month <- investment_return_annual / 12

  # Calculate monthly payments for both scenarios
  old_payment <- compute_monthly_payment(principal, rate_per_month_old, n_payments_remaining)
  new_principal <- principal - lump_sum_paydown
  new_payment <- compute_monthly_payment(new_principal, rate_per_month_new, n_payments_remaining)

  # Calculate monthly payment savings
  monthly_savings <- old_payment - new_payment

  # Initialize result vectors
  months <- seq_len(max_eval_months)
  net_benefits <- numeric(max_eval_months)

  # Calculate net benefit for each month
  for (k in months) {
    # Calculate future value of investment savings (handle negative savings)
    if (monthly_savings >= 0) {
      fv_savings <- calculate_invested_savings_fv(monthly_savings, investment_rate_per_month, k)
    } else {
      # Negative savings means additional costs - calculate future value of additional payments
      fv_savings <- -calculate_invested_savings_fv(-monthly_savings, investment_rate_per_month, k)
    }

    # Calculate remaining balances for both scenarios
    old_balance <- compute_principal_remaining(old_payment, rate_per_month_old,
                                             max(0, n_payments_remaining - k))
    new_balance <- compute_principal_remaining(new_payment, rate_per_month_new,
                                             max(0, n_payments_remaining - k))

    # Calculate equity difference (additional equity in new loan from lump sum)
    equity_diff <- old_balance - new_balance

    # Calculate cumulative interest paid for tax deduction analysis
    old_interest_paid <- compute_interest_paid(principal, old_payment, rate_per_month_old,
                                             n_payments_remaining, max(0, n_payments_remaining - k))
    new_interest_paid <- compute_interest_paid(new_principal, new_payment, rate_per_month_new,
                                             n_payments_remaining, max(0, n_payments_remaining - k))

    # Calculate tax savings differential
    tax_savings_diff <- calculate_tax_savings_differential(old_interest_paid, new_interest_paid,
                                                         old_balance, new_balance,
                                                         tax_rate, mid_limit)

    # Calculate net benefit
    net_benefits[k] <- fv_savings - closing_costs + equity_diff + tax_savings_diff
  }

  # Find breakeven month (first month with positive benefit)
  breakeven_month <- which(net_benefits > 0)[1]
  if (length(breakeven_month) == 0) {
    breakeven_month <- NA
  }

  return(list(
    months = months,
    net_benefits = net_benefits,
    breakeven_month = breakeven_month,
    old_payment = old_payment,
    new_payment = new_payment,
    monthly_savings = monthly_savings
  ))
}