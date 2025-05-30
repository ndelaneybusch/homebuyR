#' Compute Affordable Mortgage Principal
#'
#' Calculates the maximum affordable loan principal based on a total monthly
#' housing budget, other non-mortgage costs, loan terms, and optionally
#' factoring in estimated property taxes based on the derived home price.
#'
#' @param monthly_housing_budget Numeric. The total maximum amount available
#'   for all housing costs per month.
#' @param monthly_non_mortgage_costs Numeric. The sum of fixed monthly costs
#'   *excluding* mortgage P&I and property taxes (e.g., home insurance,
#'   HOA dues, other fixed monthly fees). Defaults to 0.
#' @param rate_per_month Numeric. The interest rate per month (e.g., annual
#'   rate / 12 / 100). Must be non-negative.
#' @param n_payments_total Integer. The total number of payments over the
#'   life of the loan (e.g., 360 for 30 years). Must be positive.
#' @param prop_tax_rate_annual Numeric. Optional. Estimated annual property
#'   tax rate as a percentage of home value (e.g., 1.2 for 1.2%). If provided
#'   and > 0, property tax is factored into the affordability calculation.
#'   Defaults to NULL (property tax not factored in).
#' @param down_payment_pct Numeric. Optional. The down payment as a percentage
#'   of the home price (e.g., 20 for 20%). Only used if `prop_tax_rate_annual`
#'   is also provided and > 0. Defaults to 20.
#'
#' @return Numeric. The calculated maximum affordable loan principal.
#' @export
#' @importFrom stats optimise
#'
#' @examples
#' # Example 1: Simple case without property tax factoring
#' compute_affordable_principal(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360
#' )
#' # Budget for P&I = 2500 - 300 = 2200
#' # Expected principal: compute_principal(2200, 0.005, 360) -> approx 366951
#'
#' # Example 2: Factoring in property tax (1.2%) and 20% down
#' compute_affordable_principal(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.2,
#'   down_payment_pct = 20
#' )
#' # Calculation is more complex, result should be lower than Ex 1.
#' # Expected: approx 336815
#'
#' # Example 3: 0% interest rate, with property tax
#' compute_affordable_principal(
#'   monthly_housing_budget = 2000,
#'   monthly_non_mortgage_costs = 200,
#'   rate_per_month = 0,
#'   n_payments_total = 180, # 15 years
#'   prop_tax_rate_annual = 1.0,
#'   down_payment_pct = 10
#' )
#' # Expected: approx 297521
#'
compute_affordable_principal <- function(monthly_housing_budget,
                                         monthly_non_mortgage_costs = 0,
                                         rate_per_month,
                                         n_payments_total,
                                         prop_tax_rate_annual = NULL,
                                         down_payment_pct = 20) {
  # --- Input Validation ---
  stopifnot(is.numeric(monthly_housing_budget), monthly_housing_budget >= 0)
  stopifnot(is.numeric(monthly_non_mortgage_costs), monthly_non_mortgage_costs >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total))

  # Validate prop_tax_rate_annual and down_payment_pct *if* tax rate is provided and positive
  if (!is.null(prop_tax_rate_annual)) {
    stopifnot(is.numeric(prop_tax_rate_annual), prop_tax_rate_annual >= 0)
    # Down payment validation only if tax rate is positive and will be used
    if (prop_tax_rate_annual > 0) {
      stopifnot(is.numeric(down_payment_pct), down_payment_pct >= 0, down_payment_pct < 100)
    }
  }

  # --- Calculation ---

  # Budget available *before* P&I and Property Tax
  budget_before_pi_tax <- monthly_housing_budget - monthly_non_mortgage_costs
  if (budget_before_pi_tax <= 0) {
    # Cannot afford anything if budget doesn't even cover non-mortgage costs
    return(0)
  }

  # Calculate property tax factor (monthly tax rate per dollar of principal)
  monthly_tax_rate_per_principal_dollar <- 0 # Default: no property tax impact
  if (!is.null(prop_tax_rate_annual) && prop_tax_rate_annual > 0) {
    tax_rate_annual_dec <- prop_tax_rate_annual / 100
    down_payment_dec <- down_payment_pct / 100
    if (down_payment_dec >= 1) {
      stop("Down payment percentage must be less than 100.")
    }
    # Factor = (Monthly Tax Rate) / (Loan-to-Value Ratio)
    monthly_tax_rate_per_principal_dollar <- (tax_rate_annual_dec / 12) / (1 - down_payment_dec)
  }

  # Calculate present value factor for the annuity (P&I payments)
  # This uses the exported function from fct_annuity.R
  annuity_present_value_factor <- calculate_annuity_pv_factor(
    rate_per_period = rate_per_month,
    n_periods = n_payments_total
  )

  if (annuity_present_value_factor <= 0) {
    return(0) # Should not happen with valid inputs but good for safety
  }

  # Solve for Principal (P)
  # Formula: P = B_avail * PV_factor / (1 + Tax_factor * PV_factor)
  # where B_avail = Budget Before P&I, PV_factor = annuity_present_value_factor,
  # Tax_factor = monthly_tax_rate_per_principal_dollar
  denominator_val <- 1 + (monthly_tax_rate_per_principal_dollar * annuity_present_value_factor)

  if (denominator_val <= 0) {
    # This could happen theoretically if tax factor is extremely large and negative
    return(0)
  }

  affordable_principal <- (budget_before_pi_tax * annuity_present_value_factor) / denominator_val

  # Ensure non-negative result
  return(max(0, affordable_principal))
}

#' Estimate Monthly Property Tax Based on Affordability
#'
#' Calculates an estimated monthly property tax amount. This is done by first
#' determining the affordable loan principal using \code{compute_affordable_principal},
#' then estimating the corresponding home value based on the down payment,
#' and finally calculating the monthly tax based on the annual property tax rate.
#'
#' @param monthly_housing_budget Numeric. The total maximum amount available
#'   for all housing costs per month.
#' @param monthly_non_mortgage_costs Numeric. The sum of fixed monthly costs
#'   excluding mortgage P&I and property taxes (e.g., home insurance,
#'   HOA dues, other fixed monthly fees). Defaults to 0.
#' @param rate_per_month Numeric. The interest rate per month (e.g., annual
#'   rate / 12 / 100). Must be non-negative.
#' @param n_payments_total Integer. The total number of payments over the
#'   life of the loan (e.g., 360 for 30 years). Must be positive.
#' @param prop_tax_rate_annual Numeric. Estimated annual property tax rate as a
#'   percentage of home value (e.g., 1.2 for 1.2\%). Must be non-negative.
#' @param down_payment_pct Numeric. The down payment as a percentage of the
#'   home price (e.g., 20 for 20\%). Must be between 0 (inclusive) and 100
#'   (exclusive). Defaults to 20.
#'
#' @return Numeric. The estimated monthly property tax. Returns 0 if the
#'   calculated affordable principal is zero or less, or if the
#'   \code{prop_tax_rate_annual} is zero.
#' @export
#'
#' @examples
#' # Example using the same inputs as Example 2 for compute_affordable_principal
#' # Expected principal was ~303637. Expected home price ~379546.
#' # Expected monthly tax = (379546 * 0.012) / 12 = 379.55
#' estimate_monthly_property_tax(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.2,
#'   down_payment_pct = 20
#' )
#'
#' # Example with zero tax rate
#' estimate_monthly_property_tax(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 0,
#'   down_payment_pct = 20
#' ) # Should be 0
#'
#' # Example where principal would be 0
#' estimate_monthly_property_tax(
#'   monthly_housing_budget = 300,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.0,
#'   down_payment_pct = 20
#' ) # Should be 0
estimate_monthly_property_tax <- function(monthly_housing_budget,
                                          monthly_non_mortgage_costs = 0,
                                          rate_per_month,
                                          n_payments_total,
                                          prop_tax_rate_annual,
                                          down_payment_pct = 20) {
  # --- Input Validation ---
  # Validate inputs specific to this function's direct use
  stopifnot(is.numeric(prop_tax_rate_annual), prop_tax_rate_annual >= 0)

  # Handle zero tax rate case immediately - no need to validate down_payment_pct in this case
  if (prop_tax_rate_annual == 0) {
    return(0)
  }

  # Only validate down_payment_pct if we're actually going to use it
  stopifnot(is.numeric(down_payment_pct), down_payment_pct >= 0, down_payment_pct < 100)

  # `compute_affordable_principal` will validate the other inputs

  # --- Calculation ---

  # 1. Calculate affordable principal (will handle its own validation)
  principal <- compute_affordable_principal(
    monthly_housing_budget = monthly_housing_budget,
    monthly_non_mortgage_costs = monthly_non_mortgage_costs,
    rate_per_month = rate_per_month,
    n_payments_total = n_payments_total,
    prop_tax_rate_annual = prop_tax_rate_annual,
    down_payment_pct = down_payment_pct
  )

  # If principal is non-positive, no home purchase, thus no tax
  if (principal <= 0) {
    return(0)
  }

  # 2. Estimate home price
  down_payment_dec <- down_payment_pct / 100
  # Check needed to prevent division by zero if somehow dp=100 passed validation
  if (down_payment_dec >= 1) {
    warning("Down payment percentage is 100% or more, cannot estimate home price based on principal.")
    return(NA_real_) # Or handle error appropriately
  }
  estimated_home_price <- principal / (1 - down_payment_dec)

  # 3. Calculate annual property tax
  tax_rate_annual_dec <- prop_tax_rate_annual / 100
  annual_tax <- estimated_home_price * tax_rate_annual_dec

  # 4. Calculate monthly property tax
  monthly_tax <- annual_tax / 12

  return(max(0, monthly_tax)) # Ensure non-negative
}

#' Compute Affordable Mortgage Principal Including PMI
#'
#' Calculates the maximum affordable loan principal based on a total monthly
#' housing budget, other non-mortgage costs, loan terms, property taxes, and
#' Private Mortgage Insurance (PMI) when applicable.
#'
#' Allows specifying the down payment either as a percentage of the home price
#' (`down_payment_pct`) or as a fixed dollar amount (`down_payment_dollars`).
#' Exactly one of these must be provided.
#'
#' PMI is typically required when the down payment is less than a certain
#' threshold (e.g., 20%) and is calculated as a percentage of the loan amount.
#'
#' @inheritParams compute_affordable_principal
#' @param down_payment_pct Numeric. Optional. The down payment as a percentage
#'   of the home price (e.g., 20 for 20%). Must be between 0 (inclusive) and
#'   100 (exclusive). Provide either this OR `down_payment_dollars`.
#' @param down_payment_dollars Numeric. Optional. The fixed dollar amount of the
#'   down payment. Must be non-negative. Provide either this OR
#'   `down_payment_pct`.
#' @param pmi_rate_annual Numeric. Optional. The annual PMI rate expressed as a
#'   percentage of the loan amount (e.g., 0.5 for 0.5%). Defaults to NULL (PMI
#'   not factored in unless down payment is below `pmi_threshold_pct`).
#' @param pmi_threshold_pct Numeric. Optional. The down payment percentage
#'   threshold below which PMI is applied. Defaults to 20.
#'
#' @return Numeric. The calculated maximum affordable loan principal,
#'   considering PMI.
#' @export
#'
#' @examples
#' # -- Using Percentage Down Payment --
#' # Example 1: Sufficient % down payment (20%), no PMI applied
#' compute_principal_with_pmi(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.2,
#'   down_payment_pct = 20,
#'   pmi_rate_annual = 0.5
#' )
#' # Should match compute_affordable_principal result: approx 336815
#'
#' # Example 2: Low % down payment (10%), PMI applied
#' compute_principal_with_pmi(
#'   monthly_housing_budget = 2500,
#'   monthly_non_mortgage_costs = 300,
#'   rate_per_month = 0.06 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.2,
#'   down_payment_pct = 10,
#'   pmi_rate_annual = 0.5
#' )
#' # Expect result lower than Ex 1 due to PMI cost: approx 317717
#'
#' # -- Using Dollar Down Payment --
#' # Example 3: Fixed $ down payment, sufficient to avoid PMI
#' # (Down payment $50k on a hypothetical ~$400k house > 10% threshold)
#' compute_principal_with_pmi(
#'   monthly_housing_budget = 2000,
#'   monthly_non_mortgage_costs = 200,
#'   rate_per_month = 0.07 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.0,
#'   down_payment_dollars = 50000,
#'   pmi_rate_annual = 0.6,
#'   pmi_threshold_pct = 10
#' )
#' # Expected: approx 258346 (Implies HP approx 308346, DP is ~16% > 10%)
#'
#' # Example 4: Fixed $ down payment, insufficient to avoid PMI
#' # (Down payment $20k on a hypothetical ~$300k house < 10% threshold)
#' compute_principal_with_pmi(
#'   monthly_housing_budget = 2000,
#'   monthly_non_mortgage_costs = 200,
#'   rate_per_month = 0.07 / 12,
#'   n_payments_total = 360,
#'   prop_tax_rate_annual = 1.0,
#'   down_payment_dollars = 20000,
#'   pmi_rate_annual = 0.6,
#'   pmi_threshold_pct = 10
#' )
#' # Expected: approx 250516 (Implies HP approx 270516, DP is ~7.4% < 10%)
#'
compute_principal_with_pmi <- function(monthly_housing_budget,
                                       monthly_non_mortgage_costs = 0,
                                       rate_per_month,
                                       n_payments_total,
                                       down_payment_pct = NULL, # Optional
                                       down_payment_dollars = NULL, # Optional
                                       prop_tax_rate_annual = NULL,
                                       pmi_rate_annual = NULL,
                                       pmi_threshold_pct = 20) {
  # --- Input Validation ---
  stopifnot(is.numeric(monthly_housing_budget), monthly_housing_budget >= 0)
  stopifnot(is.numeric(monthly_non_mortgage_costs), monthly_non_mortgage_costs >= 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total))

  # Validate down payment inputs
  provided_pct <- !is.null(down_payment_pct)
  provided_dollars <- !is.null(down_payment_dollars)

  if (!(provided_pct || provided_dollars)) {
    stop("Must provide either 'down_payment_pct' or 'down_payment_dollars'.")
  }
  if (provided_pct && provided_dollars) {
    stop("Cannot provide both 'down_payment_pct' and 'down_payment_dollars'.")
  }
  if (provided_pct) {
    stopifnot(is.numeric(down_payment_pct), down_payment_pct >= 0, down_payment_pct < 100)
  }
  if (provided_dollars) {
    stopifnot(is.numeric(down_payment_dollars), down_payment_dollars >= 0)
  }

  # Validate other rates/thresholds
  if (!is.null(prop_tax_rate_annual)) {
    stopifnot(is.numeric(prop_tax_rate_annual), prop_tax_rate_annual >= 0)
  }
  if (!is.null(pmi_rate_annual)) {
    stopifnot(is.numeric(pmi_rate_annual), pmi_rate_annual >= 0)
  }
  stopifnot(is.numeric(pmi_threshold_pct), pmi_threshold_pct > 0, pmi_threshold_pct <= 100)

  # --- Calculation Setup ---

  budget_avail <- monthly_housing_budget - monthly_non_mortgage_costs
  if (budget_avail <= 0) {
    return(0)
  }

  # Calculate factors that don't depend on down payment method yet
  annuity_present_value_factor <- calculate_annuity_pv_factor(
    rate_per_period = rate_per_month,
    n_periods = n_payments_total
  )

  if (annuity_present_value_factor <= 0) {
    return(0)
  }

  inv_pv_factor <- ifelse(annuity_present_value_factor == 0, 0, 1 / annuity_present_value_factor)

  tax_rate_monthly <- ifelse(is.null(prop_tax_rate_annual) || prop_tax_rate_annual == 0,
    0,
    (prop_tax_rate_annual / 100) / 12
  )

  pmi_rate_monthly <- ifelse(is.null(pmi_rate_annual) || pmi_rate_annual == 0,
    0,
    (pmi_rate_annual / 100) / 12
  )

  pmi_threshold_dec <- pmi_threshold_pct / 100

  affordable_principal <- 0 # Initialize

  # --- Calculation Logic ---

  if (provided_pct) {
    # --- Scenario 1: Down Payment provided as Percentage ---
    down_payment_dec <- down_payment_pct / 100

    # Calculate property tax factor per principal dollar
    monthly_tax_rate_per_principal_dollar <- 0
    if (tax_rate_monthly > 0) {
      if (down_payment_dec >= 1) {
        # Should be caught by validation, but defensively:
        warning("Down payment is 100% or more, tax calculation invalid.")
      } else {
        monthly_tax_rate_per_principal_dollar <- tax_rate_monthly / (1 - down_payment_dec)
      }
    }

    # Calculate PMI factor per principal dollar
    monthly_pmi_rate_per_principal_dollar <- 0
    if (pmi_rate_monthly > 0 && down_payment_dec < pmi_threshold_dec) {
      monthly_pmi_rate_per_principal_dollar <- pmi_rate_monthly
    }

    # Solve for Principal (P)
    denominator_val <- 1 +
      (monthly_tax_rate_per_principal_dollar * annuity_present_value_factor) +
      (monthly_pmi_rate_per_principal_dollar * annuity_present_value_factor)

    if (denominator_val > 0) {
      affordable_principal <- (budget_avail * annuity_present_value_factor) / denominator_val
    }
  } else if (provided_dollars) {
    dp_dollars <- down_payment_dollars
    # Precompute scenarios
    denom_no_pmi <- inv_pv_factor + tax_rate_monthly
    hp_no_pmi <- if (denom_no_pmi > 0) (budget_avail + dp_dollars * inv_pv_factor) / denom_no_pmi else 0
    denom_pmi <- inv_pv_factor + tax_rate_monthly + pmi_rate_monthly
    hp_pmi <- if (denom_pmi > 0) (budget_avail + dp_dollars * inv_pv_factor + dp_dollars * pmi_rate_monthly) / denom_pmi else 0
    pmi_thresh_dec <- pmi_threshold_pct / 100
    H_thresh <- dp_dollars / pmi_thresh_dec
    # Evaluate feasible home-price candidates
    candidates <- numeric(0)
    # 1. No-PMI scenario only if DP% ≥ threshold
    if (hp_no_pmi > dp_dollars && (dp_dollars / hp_no_pmi) * 100 >= pmi_threshold_pct) {
      candidates <- c(candidates, hp_no_pmi)
    }
    # 2. PMI scenario always feasible if > DP
    if (hp_pmi > dp_dollars) {
      candidates <- c(candidates, hp_pmi)
    }
    # 3. Threshold-capped scenario if budget allows
    if (hp_no_pmi >= H_thresh) {
      candidates <- c(candidates, H_thresh)
    }
    # Pick the max affordable home price
    if (length(candidates) > 0) {
      h_best <- max(candidates)
      affordable_principal <- h_best - dp_dollars
    } else {
      affordable_principal <- 0
    }
  }

  # Ensure non-negative result
  return(max(0, affordable_principal))
}

#' Calculate Mortgage Savings from Extra Payments
#'
#' Computes the total cash savings and months gained by making extra principal payments
#' on a mortgage. Extra payments can be made as a one-time lump sum, additional
#' monthly payments, or both.
#'
#' Lump sum payments are applied to the principal balance in advance of that month's payment
#' (decreasing the proportion of that month's payment that goes towards interest). Extra monthly
#' payments are applied to the principal balance after that month's payment (decreasing
#' the proportion of the following month's payment that goes towards interest).
#'
#' @param principal Numeric. The original loan amount. Must be positive.
#' @param rate_per_month Numeric. The monthly interest rate (annual rate as decimal / 12).
#'   Must be non-negative.
#' @param n_payments_total Integer. The total number of payments in the original loan term.
#'   Must be positive.
#' @param extra_monthly_payment Numeric. Additional amount paid toward principal each month.
#'   Defaults to 0.
#' @param lump_sum_payment Numeric. One-time additional payment toward principal.
#'   Defaults to 0.
#' @param payment_number_for_prepay_start Integer. The payment number (1-based) when the
#'   prepayments begin. Must be between 1 and n_payments_total, or NA if
#'   no lump sum payment is made. Defaults to 1 (first payment).
#' @param cumulative_output Logical. If TRUE, returns the full amortization table.
#'   Defaults to FALSE.
#'
#' @return The full amortization table if cumulative_output is TRUE, otherwise a list containing:
#'   \item{total_interest_savings}{Total interest saved over the life of the loan}
#'   \item{months_saved}{Number of months earlier the loan is paid off}
#'   \item{new_loan_term_months}{The new loan term in months with extra payments}
#'   \item{original_total_interest}{Total interest paid in the original loan}
#'   \item{new_total_interest}{Total interest paid with extra payments}
#' @export
#'
#' @examples
#' # Example: $300,000 loan at 6% APR for 30 years
#' principal <- 300000
#' annual_rate <- 0.06
#' monthly_rate <- annual_rate / 12
#' term_years <- 30
#' n_payments <- term_years * 12
#'
#' # With $200 extra monthly payment
#' savings <- calculate_mortgage_savings(
#'   principal = principal,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = n_payments,
#'   extra_monthly_payment = 200
#' )
#' # Should show interest savings and reduced term
#'
#' # With $10,000 lump sum payment at the start
#' savings_lump <- calculate_mortgage_savings(
#'   principal = principal,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = n_payments,
#'   lump_sum_payment = 10000,
#'   payment_number_for_prepay_start = 1
#' )
#'
#' # With both extra monthly and lump sum
#' savings_both <- calculate_mortgage_savings(
#'   principal = principal,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = n_payments,
#'   extra_monthly_payment = 200,
#'   lump_sum_payment = 10000,
#'   payment_number_for_prepay_start = 1
#' )
#'
#' # Return the full amortization table
#' savings_table <- calculate_mortgage_savings(
#'   principal = principal,
#'   rate_per_month = monthly_rate,
#'   n_payments_total = n_payments,
#'   extra_monthly_payment = 200,
#'   payment_number_for_prepay_start = 13,
#'   cumulative_output = TRUE
#' )
calculate_mortgage_savings <- function(principal,
                                       rate_per_month,
                                       n_payments_total,
                                       extra_monthly_payment = 0,
                                       lump_sum_payment = 0,
                                       payment_number_for_prepay_start = 1,
                                       cumulative_output = FALSE) {
  # Input validation
  stopifnot(is.numeric(principal), principal > 0)
  stopifnot(is.numeric(rate_per_month), rate_per_month >= 0)
  stopifnot(
    is.numeric(n_payments_total), n_payments_total > 0,
    n_payments_total == floor(n_payments_total)
  )
  stopifnot(is.numeric(extra_monthly_payment), extra_monthly_payment >= 0)
  stopifnot(is.numeric(lump_sum_payment), lump_sum_payment >= 0)

  # Only validate prepayment start if any prepayment is actually being made
  if (lump_sum_payment > 0 || extra_monthly_payment > 0) {
    stopifnot(
      is.numeric(payment_number_for_prepay_start),
      payment_number_for_prepay_start >= 1,
      payment_number_for_prepay_start <= n_payments_total,
      payment_number_for_prepay_start == floor(payment_number_for_prepay_start)
    )
  }

  # Calculate original monthly payment
  original_payment <- compute_monthly_payment(
    principal = principal,
    rate_per_month = rate_per_month,
    n_payments_total = n_payments_total
  )

  # Calculate original total interest
  original_total_interest <- (original_payment * n_payments_total) - principal

  # Initialize variables for tracking
  current_payment_number <- 0
  remaining_principal <- principal # Tracks remaining principal for the new (accelerated) loan
  original_remaining_principal <- principal # Tracks remaining principal for the original loan
  new_total_interest <- 0 # Accumulates total interest paid in the new scenario

  # Initialize cashflows for the *final* IRR calculation (used for both output types)
  # This vector will store the incremental cash flows over the entire original loan term.
  final_irr_cashflows <- rep(0, n_payments_total)

  # Track the payment number when the new loan is fully paid off
  new_loan_payoff_month <- NA_integer_

  # Apply lump sum if it occurs at payment number 1 (before the loop starts processing payments)
  if (lump_sum_payment > 0 && payment_number_for_prepay_start == 1) {
    remaining_principal <- max(0, remaining_principal - lump_sum_payment)
    # If lump sum pays off the entire loan, set payoff month to 0
    if (remaining_principal <= 0) {
      new_loan_payoff_month <- 0
    }
  }

  # Initialize data frame for cumulative output if requested
  if (cumulative_output) {
    out <- data.frame(
      payment_number = integer(0),
      original_principal_payment = numeric(0),
      new_principal_payment = numeric(0),
      principal_payment_extra = numeric(0),
      original_interest_payment = numeric(0),
      new_interest_payment = numeric(0),
      original_remaining_principal = numeric(0),
      new_remaining_principal = numeric(0),
      original_interest_paid = numeric(0),
      new_interest_paid = numeric(0),
      total_interest_saved = numeric(0)
    )
    # Initialize cumulative interest for the table rows
    cumulative_original_interest_paid <- 0
    cumulative_new_interest_paid <- 0
  }

  # Amortize the loan month by month, up to the original total number of payments
  # This ensures we capture all potential saved payments for IRR and cumulative output.
  for (i in 1:n_payments_total) {
    current_payment_number <- i

    # Apply lump sum if this is the designated payment number (and not month 1, already handled)
    if (lump_sum_payment > 0 && current_payment_number == payment_number_for_prepay_start && current_payment_number > 1) {
      remaining_principal <- max(0, remaining_principal - lump_sum_payment)
    }

    # --- Calculate for Original Loan Scenario ---
    original_interest_this_month <- original_remaining_principal * rate_per_month
    # Ensure original loan doesn't overpay principal if balance is low
    original_principal_this_month <- original_payment - original_interest_this_month
    if (original_remaining_principal < original_principal_this_month) {
      original_principal_this_month <- original_remaining_principal
    }
    # Update original remaining principal (cannot go below zero)
    original_remaining_principal <- max(0, original_remaining_principal - original_principal_this_month)


    # --- Calculate for New (Accelerated) Loan Scenario ---
    new_interest_this_month <- remaining_principal * rate_per_month # Interest on current balance

    # Determine the total payment for the new loan scenario (original payment + extra)
    # If loan is already paid off, no more payments or interest
    if (remaining_principal <= 0) {
      new_interest_this_month <- 0
      new_principal_this_month <- 0
      current_payment_amount_new <- 0 # No payment is made
    } else {
      current_payment_amount_new <- original_payment # Base payment
      # Add extra monthly payment if we are in the prepayment phase
      if (current_payment_number >= payment_number_for_prepay_start) {
        current_payment_amount_new <- current_payment_amount_new + extra_monthly_payment
      }

      # Calculate principal payment for new loan scenario
      new_principal_this_month <- current_payment_amount_new - new_interest_this_month

      # Ensure we don't overpay principal on the final payment for new loan
      if (remaining_principal < new_principal_this_month) {
        new_principal_this_month <- remaining_principal # Pay off the exact remaining balance
      }
    }

    # Update remaining principal and total interest for new loan
    remaining_principal <- remaining_principal - new_principal_this_month
    new_total_interest <- new_total_interest + new_interest_this_month

    # Track the month the new loan pays off (first time remaining_principal hits zero or less)
    if (is.na(new_loan_payoff_month) && remaining_principal <= 0) {
      new_loan_payoff_month <- current_payment_number
      # If paid off immediately with lump sum at payment 1, set to 0
      if (lump_sum_payment > 0 && payment_number_for_prepay_start == 1 && current_payment_number == 0) {
        new_loan_payoff_month <- 0
      }
    }

    # --- Populate Cumulative Output Data Frame ---
    if (cumulative_output) {
      cumulative_original_interest_paid <- cumulative_original_interest_paid + original_interest_this_month
      cumulative_new_interest_paid <- cumulative_new_interest_paid + new_interest_this_month

      # `principal_payment_extra` for the table: actual extra principal applied this month
      extra_principal_applied_for_table <- 0
      # Only consider extra principal if within the prepayment phase and loan is not yet paid off
      if (current_payment_number >= payment_number_for_prepay_start &&
        (is.na(new_loan_payoff_month) || current_payment_number <= new_loan_payoff_month)) {
        if (current_payment_number == payment_number_for_prepay_start) {
          extra_principal_applied_for_table <- lump_sum_payment + extra_monthly_payment
        } else {
          extra_principal_applied_for_table <- extra_monthly_payment
        }
      }

      out <- rbind(out, data.frame(
        payment_number = current_payment_number,
        original_principal_payment = original_principal_this_month,
        new_principal_payment = new_principal_this_month,
        principal_payment_extra = extra_principal_applied_for_table,
        original_interest_payment = original_interest_this_month,
        new_interest_payment = new_interest_this_month,
        original_remaining_principal = original_remaining_principal,
        new_remaining_principal = remaining_principal,
        original_interest_paid = cumulative_original_interest_paid,
        new_interest_paid = cumulative_new_interest_paid,
        total_interest_saved = cumulative_original_interest_paid - cumulative_new_interest_paid
      ))
    }
  } # End of for loop

  # If new loan wasn't fully paid off by the original term end, set payoff month to N+1
  # (This implies the new loan continues beyond the original term, though that's usually not the goal)
  if (is.na(new_loan_payoff_month)) {
    new_loan_payoff_month <- n_payments_total + 1
  }

  # --- Return Results based on Output Type ---

  if (cumulative_output) {
    return(out)
  } else { # Non-cumulative (summary) output
    # Calculate months saved and other metrics
    if (is.na(new_loan_payoff_month)) {
      # If the loan isn't paid off by the end of the original term
      new_loan_term_months <- n_payments_total
      months_saved <- 0
    } else if (new_loan_payoff_month == 0) {
      # If paid off immediately with lump sum at payment 1
      new_loan_term_months <- 0
      months_saved <- n_payments_total
    } else {
      new_loan_term_months <- new_loan_payoff_month
      months_saved <- n_payments_total - new_loan_payoff_month
    }
    total_interest_savings <- original_total_interest - new_total_interest
    # Ensure no negative savings or months saved
    total_interest_savings <- max(0, total_interest_savings)
    months_saved <- max(0, months_saved)

    return(list(
      total_interest_savings = total_interest_savings,
      months_saved = months_saved,
      new_loan_term_months = new_loan_term_months,
      original_total_interest = original_total_interest,
      new_total_interest = new_total_interest
    ))
  }
}
