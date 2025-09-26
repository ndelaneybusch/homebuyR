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
#' @param n_payments_new Integer. Term of new refinanced loan in months.
#'   Must be positive. Default: n_payments_remaining (keep same term).
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
#'   - net_benefits: Vector of cumulative net benefits for each month (total wealth impact)
#'   - net_cash_benefits: Vector of cumulative cash benefits for each month (excluding equity)
#'   - equity_differences: Vector of equity differences for each month (old_balance - new_balance)
#'   - breakeven_month: First month where total benefit > 0 (NA if none)
#'   - cash_breakeven_month: First month where cash benefit > 0 (NA if none)
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
                                            n_payments_new = 12 * 30,
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
  stopifnot(is.numeric(n_payments_new), n_payments_new > 0, n_payments_new == floor(n_payments_new))
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
  new_payment <- compute_monthly_payment(new_principal, rate_per_month_new, n_payments_new)

  # Calculate monthly payment savings
  monthly_savings <- old_payment - new_payment

  # Initialize result vectors
  months <- seq_len(max_eval_months)
  net_benefits <- numeric(max_eval_months)
  net_cash_benefits <- numeric(max_eval_months)
  equity_differences <- numeric(max_eval_months)
  cumulative_investments <- numeric(max_eval_months)
  fv_savings_track <- numeric(max_eval_months)
  tax_savings_diffs <- numeric(max_eval_months)

  # Initialize cumulative calculations (incremental approach)
  cumulative_investment <- 0
  cumulative_deductible_interest_old <- 0
  cumulative_deductible_interest_new <- 0

  # Calculate net benefit for each month
  for (k in months) {
    # Calculate remaining balances for both scenarios
    # Old loan: continues with remaining term, reduced by months elapsed
    old_balance <- compute_principal_remaining(old_payment, rate_per_month_old,
                                             max(0, n_payments_remaining - k))
    # New loan: starts fresh with new term, reduced by months elapsed since refinance
    new_balance <- compute_principal_remaining(new_payment, rate_per_month_new,
                                             max(0, n_payments_new - k))

    # Calculate current month's cash flow difference
    current_old_payment <- if (k <= n_payments_remaining) old_payment else 0
    current_new_payment <- if (k <= n_payments_new) new_payment else 0
    current_monthly_savings <- current_old_payment - current_new_payment

    # Update cumulative investment value incrementally
    # Grow previous investment by one month, then add current month's cash flow
    cumulative_investment <- cumulative_investment * (1 + investment_rate_per_month) + current_monthly_savings
    fv_savings <- cumulative_investment

    # Calculate equity difference (additional equity in new loan from lump sum paydown)
    equity_diff <- old_balance - new_balance

    # Update cumulative deductible interest incrementally (like Python code)
    # Old loan: add this month's deductible interest if balance > 0
    if (old_balance > 0.005) {
      monthly_interest_old <- old_balance * rate_per_month_old
      deductible_factor_old <- if (old_balance <= mid_limit) 1.0 else (mid_limit / old_balance)
      cumulative_deductible_interest_old <- cumulative_deductible_interest_old + (monthly_interest_old * deductible_factor_old)
    }

    # New loan: add this month's deductible interest if balance > 0
    if (new_balance > 0.005) {
      monthly_interest_new <- new_balance * rate_per_month_new
      deductible_factor_new <- if (new_balance <= mid_limit) 1.0 else (mid_limit / new_balance)
      cumulative_deductible_interest_new <- cumulative_deductible_interest_new + (monthly_interest_new * deductible_factor_new)
    }

    # Calculate tax savings differential using cumulative deductible interest
    tax_savings_diff <- tax_rate * (cumulative_deductible_interest_new - cumulative_deductible_interest_old)

    # Calculate cash benefit (excludes equity differences)
    net_cash_benefits[k] <- fv_savings - closing_costs + tax_savings_diff

    # Calculate total benefit (includes equity differences - total wealth impact)
    net_benefits[k] <- net_cash_benefits[k] + equity_diff

    # Store equity difference for plotting
    equity_differences[k] <- equity_diff
    cumulative_investments[k] <- cumulative_investment
    fv_savings_track[k] <- fv_savings
    tax_savings_diffs[k] <- tax_savings_diff
  }

  # Find breakeven months
  breakeven_month <- which(net_benefits > 0)[1]
  if (length(breakeven_month) == 0) {
    breakeven_month <- NA
  }

  cash_breakeven_month <- which(net_cash_benefits > 0)[1]
  if (length(cash_breakeven_month) == 0) {
    cash_breakeven_month <- NA
  }

  # Calculate dynamic monthly savings for display purposes
  dynamic_monthly_savings <- sapply(months, function(k) {
    current_old_payment <- if (k <= n_payments_remaining) old_payment else 0
    current_new_payment <- if (k <= n_payments_new) new_payment else 0
    current_old_payment - current_new_payment
  })

  return(list(
    months = months,
    net_benefits = net_benefits,
    net_cash_benefits = net_cash_benefits,
    equity_differences = equity_differences,
    cumulative_investments = cumulative_investments,
    cumulative_fv_savings = fv_savings_track,
    cumulative_tax_advantage = tax_savings_diffs,
    breakeven_month = breakeven_month,
    cash_breakeven_month = cash_breakeven_month,
    old_payment = old_payment,
    new_payment = new_payment,
    monthly_savings = monthly_savings,
    dynamic_monthly_savings = dynamic_monthly_savings
  ))
}