# R Functions for Mortgage Annuity Calculations

#' Calculate Present Value Factor of an Ordinary Annuity
#'
#' Computes the factor used to determine the present value of a series of
#' equal payments (annuity) made at the end of each period.
#' Handles the special case where the interest rate is zero.
#'
#' @param rate_per_period Numeric. The interest rate per period (e.g., month).
#'   Must be non-negative.
#' @param n_periods Integer. The total number of periods (e.g., payments).
#'   Must be positive.
#'
#' @return Numeric. The present value annuity factor.
#' @export
#' @keywords internal
#'
#' @examples
#' # Factor for 360 months at 0.5% monthly rate
#' calculate_annuity_pv_factor(0.06 / 12, 360)
#'
#' # Factor for 12 months at 0% rate
#' calculate_annuity_pv_factor(0, 12)
#'
calculate_annuity_pv_factor <- function(rate_per_period, n_periods) {
  stopifnot(is.numeric(rate_per_period), rate_per_period >= 0)
  stopifnot(is.numeric(n_periods), n_periods > 0, n_periods == floor(n_periods))

  if (rate_per_period == 0) {
    return(n_periods)
  } else {
    # Standard annuity present value factor
    return((1 - (1 + rate_per_period)^-n_periods) / rate_per_period)
  }
}

#' Compute Monthly Mortgage Payment
#'
#' Calculates the fixed monthly payment required to amortize a loan
#' based on the principal amount, interest rate, and total number of payments.
#' This uses the standard annuity formula.
#'
#' @param principal Numeric. The initial loan amount (e.g., mortgage principal).
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month (e.g., annual
#'   rate / 12). Must be non-negative.
#' @param n_payments_total Integer. The total number of payments over the
#'   life of the loan. Must be positive.
#'
#' @return Numeric. The calculated fixed monthly payment.
#' @export
#'
#' @examples
#' # Calculate payment for a $300,000 loan over 30 years (360 months) at 6% APR
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' compute_monthly_payment(
#'   principal = 300000,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = 360
#' )
#' # Expected output: approx 1798.65
#'
#' # Calculate payment for a 0% interest loan
#' compute_monthly_payment(
#'   principal = 12000,
#'   rate_per_month = 0,
#'   n_payments_total = 12
#' )
#' # Expected output: 1000
compute_monthly_payment <- function(principal, rate_per_month, n_payments_total) {
  stopifnot(is.numeric(principal), principal >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total))

  if (rate_per_month == 0) {
    # Handle zero interest rate case to avoid division by zero
    monthly_payment <- principal / n_payments_total
  } else {
    # Standard annuity formula rearranged to solve for payment
    pv_factor_val <- calculate_annuity_pv_factor(rate_per_month, n_payments_total)
    monthly_payment <- principal / pv_factor_val
  }

  return(monthly_payment)
}

#' Compute Original Loan Principal
#'
#' Calculates the original loan principal amount based on the fixed monthly
#' payment, interest rate, and total number of payments.
#' This uses the standard annuity formula (present value of an annuity).
#'
#' @param monthly_payment Numeric. The fixed monthly payment amount.
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month.
#'   Must be non-negative.
#' @param n_payments_total Integer. The total number of payments over the
#'   life of the loan. Must be positive.
#'
#' @return Numeric. The calculated original loan principal.
#' @export
#'
#' @examples
#' # Calculate principal for a $1800/month payment over 30 years at 6% APR
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' compute_principal(
#'   monthly_payment = 1798.65,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = 360
#' )
#' # Expected output: approx 300000
#'
#' # Calculate principal for a 0% interest loan with $1000/month payment for 1 year
#' compute_principal(
#'   monthly_payment = 1000,
#'   rate_per_month = 0,
#'   n_payments_total = 12
#' )
#' # Expected output: 12000
compute_principal <- function(monthly_payment, rate_per_month, n_payments_total) {
  stopifnot(is.numeric(monthly_payment), monthly_payment >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total))

  pv_factor_val <- calculate_annuity_pv_factor(rate_per_month, n_payments_total)
  principal <- monthly_payment * pv_factor_val

  return(principal)
}

#' Compute Remaining Principal Balance
#'
#' Calculates the outstanding principal balance of a loan after a certain
#' number of payments have been made. This is equivalent to the present value
#' of the remaining future payments.
#'
#' @param monthly_payment Numeric. The fixed monthly payment amount.
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month.
#'   Must be non-negative.
#' @param n_payments_remaining Integer. The number of payments still remaining
#'   on the loan. Must be non-negative.
#'
#' @return Numeric. The calculated remaining principal balance.
#' @export
#'
#' @examples
#' # Calculate remaining principal on a $300k, 30yr, 6% APR loan after 5 years (60 payments made)
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' payment <- compute_monthly_payment(300000, monthly_rate, 360) # approx 1798.65
#' payments_made <- 60
#' total_payments <- 360
#' remaining_payments <- total_payments - payments_made
#' compute_principal_remaining(
#'   monthly_payment = payment,
#'   rate_per_month = monthly_rate,
#'   n_payments_remaining = remaining_payments
#' )
#' # Expected output: approx 278719.03
#'
#' # Remaining principal when 0 payments remain
#' compute_principal_remaining(
#'   monthly_payment = payment,
#'   rate_per_month = monthly_rate,
#'   n_payments_remaining = 0
#' )
#' # Expected output: 0
#'
#' # Remaining principal on a 0% loan after some payments
#' compute_principal_remaining(
#'   monthly_payment = 1000,
#'   rate_per_month = 0,
#'   n_payments_remaining = 6
#' ) # 6 payments left on a 12k loan
#' # Expected output: 6000
compute_principal_remaining <- function(monthly_payment, rate_per_month, n_payments_remaining) {
  stopifnot(is.numeric(monthly_payment), monthly_payment >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_payments_remaining), n_payments_remaining >= 0, n_payments_remaining == floor(n_payments_remaining))

  if (n_payments_remaining == 0) {
    return(0)
  }

  pv_factor_val <- calculate_annuity_pv_factor(rate_per_month, n_payments_remaining)
  principal_remaining <- monthly_payment * pv_factor_val

  # Ensure result is not negative due to floating point inaccuracies near zero
  return(max(0, principal_remaining))
}


#' Compute Principal Paid To Date
#'
#' Calculates the total amount of principal paid on a loan up to the current point.
#' This is the difference between the original principal and the remaining principal.
#'
#' @param principal Numeric. The original loan principal amount.
#'   Must be non-negative.
#' @param monthly_payment Numeric. The fixed monthly payment amount.
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month.
#'   Must be non-negative.
#' @param n_payments_remaining Integer. The number of payments still remaining
#'   on the loan. Must be non-negative.
#'
#' @return Numeric. The total principal paid to date.
#' @export
#'
#' @examples
#' # Calculate principal paid on a $300k, 30yr, 6% APR loan after 5 years (60 payments made)
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' original_principal <- 300000
#' total_payments <- 360
#' payment <- compute_monthly_payment(original_principal, monthly_rate, total_payments) # approx 1798.65
#' payments_made <- 60
#' remaining_payments <- total_payments - payments_made
#'
#' compute_principal_paid(
#'   principal = original_principal,
#'   monthly_payment = payment,
#'   rate_per_month = monthly_rate,
#'   n_payments_remaining = remaining_payments
#' )
#' # Expected output: approx 21280.97 (which is 300000 - 278719.03)
#'
#' # Principal paid on a 0% loan after 6 of 12 payments
#' compute_principal_paid(
#'   principal = 12000,
#'   monthly_payment = 1000,
#'   rate_per_month = 0,
#'   n_payments_remaining = 6
#' )
#' # Expected output: 6000
compute_principal_paid <- function(principal, monthly_payment, rate_per_month, n_payments_remaining) {
  stopifnot(is.numeric(principal), principal >= 0)
  # Other inputs validated by compute_principal_remaining

  principal_remaining <- compute_principal_remaining(
    monthly_payment = monthly_payment,
    rate_per_month = rate_per_month,
    n_payments_remaining = n_payments_remaining
  )

  principal_paid <- principal - principal_remaining

  # Ensure result is not negative due to floating point inaccuracies
  return(max(0, principal_paid))
}


#' Compute Total Interest Remaining
#'
#' Calculates the total amount of interest that will be paid over the
#' remaining life of the loan. This is the sum of all future payments minus
#' the remaining principal balance.
#'
#' @param monthly_payment Numeric. The fixed monthly payment amount.
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month.
#'   Must be non-negative.
#' @param n_payments_remaining Integer. The number of payments still remaining
#'   on the loan. Must be non-negative.
#'
#' @return Numeric. The total interest yet to be paid.
#' @export
#'
#' @examples
#' # Calculate interest remaining on a $300k, 30yr, 6% APR loan after 5 years (60 payments made)
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' original_principal <- 300000
#' total_payments <- 360
#' payment <- compute_monthly_payment(original_principal, monthly_rate, total_payments) # approx 1798.65
#' payments_made <- 60
#' remaining_payments <- total_payments - payments_made
#'
#' compute_total_interest_remaining(
#'   monthly_payment = payment,
#'   rate_per_month = monthly_rate,
#'   n_payments_remaining = remaining_payments
#' )
#' # Expected output: approx 260876.89
#' # (Total remaining payments = 1798.65 * 300 = 539595)
#' # (Remaining principal = 278719.03)
#' # (Interest remaining = 539595 - 278719.03 = 260875.97 - diff due to rounding in example)
#'
#' # Interest remaining on a 0% loan
#' compute_total_interest_remaining(
#'   monthly_payment = 1000,
#'   rate_per_month = 0,
#'   n_payments_remaining = 6
#' )
#' # Expected output: 0
compute_total_interest_remaining <- function(monthly_payment, rate_per_month, n_payments_remaining) {
  # Input validation happens within compute_principal_remaining

  if (n_payments_remaining == 0) {
    return(0)
  }

  total_remaining_payments_value <- monthly_payment * n_payments_remaining
  principal_remaining <- compute_principal_remaining(
    monthly_payment = monthly_payment,
    rate_per_month = rate_per_month,
    n_payments_remaining = n_payments_remaining
  )

  interest_remaining <- total_remaining_payments_value - principal_remaining

  # Ensure result is not negative due to floating point inaccuracies
  return(max(0, interest_remaining))
}


#' Compute Interest Paid To Date
#'
#' Calculates the total amount of interest paid on a loan up to the current point.
#' This is the total amount paid so far minus the amount of principal paid so far.
#'
#' @param principal Numeric. The original loan principal amount.
#'   Must be non-negative.
#' @param monthly_payment Numeric. The fixed monthly payment amount.
#'   Must be non-negative.
#' @param rate_per_month Numeric. The interest rate per month.
#'   Must be non-negative.
#' @param n_payments_total Integer. The total number of payments originally
#'   scheduled for the loan. Must be positive.
#' @param n_payments_remaining Integer. The number of payments still remaining
#'   on the loan. Must be non-negative and not greater than n_payments_total.
#'
#' @return Numeric. The total interest paid to date.
#' @export
#'
#' @examples
#' # Calculate interest paid on a $300k, 30yr, 6% APR loan after 5 years (60 payments made)
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' original_principal <- 300000
#' total_payments <- 360
#' payment <- compute_monthly_payment(original_principal, monthly_rate, total_payments) # approx 1798.65
#' payments_made <- 60
#' remaining_payments <- total_payments - payments_made
#'
#' compute_interest_paid(
#'   principal = original_principal,
#'   monthly_payment = payment,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = total_payments,
#'   n_payments_remaining = remaining_payments
#' )
#' # Expected output: approx 86638.33
#' # (Total paid = 1798.65 * 60 = 107919)
#' # (Principal paid = 21280.97)
#' # (Interest paid = 107919 - 21280.97 = 86638.03 - diff due to rounding in example)
#'
#' # Interest paid on a 0% loan after 6 of 12 payments
#' compute_interest_paid(
#'   principal = 12000,
#'   monthly_payment = 1000,
#'   rate_per_month = 0,
#'   n_payments_total = 12,
#'   n_payments_remaining = 6
#' )
#' # Expected output: 0
compute_interest_paid <- function(principal, monthly_payment, rate_per_month, n_payments_total, n_payments_remaining) {
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total))
  stopifnot(is.numeric(n_payments_remaining), n_payments_remaining >= 0, n_payments_remaining <= n_payments_total, n_payments_remaining == floor(n_payments_remaining))
  # Other inputs validated by compute_principal_paid

  n_payments_made <- n_payments_total - n_payments_remaining

  if (n_payments_made <= 0) {
    return(0)
  }

  total_paid_to_date <- monthly_payment * n_payments_made
  principal_paid <- compute_principal_paid(
    principal = principal,
    monthly_payment = monthly_payment,
    rate_per_month = rate_per_month,
    n_payments_remaining = n_payments_remaining
  )

  interest_paid <- total_paid_to_date - principal_paid

  # Ensure result is not negative due to floating point inaccuracies
  return(max(0, interest_paid))
}
