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
#' compute_affordable_principal(monthly_housing_budget = 2500,
#'                              monthly_non_mortgage_costs = 300,
#'                              rate_per_month = 0.06 / 12,
#'                              n_payments_total = 360)
#' # Budget for P&I = 2500 - 300 = 2200
#' # Expected principal: compute_principal(2200, 0.005, 360) -> approx 366951
#'
#' # Example 2: Factoring in property tax (1.2%) and 20% down
#' compute_affordable_principal(monthly_housing_budget = 2500,
#'                              monthly_non_mortgage_costs = 300,
#'                              rate_per_month = 0.06 / 12,
#'                              n_payments_total = 360,
#'                              prop_tax_rate_annual = 1.2,
#'                              down_payment_pct = 20)
#' # Calculation is more complex, result should be lower than Ex 1.
#' # Expected: approx 336815
#'
#' # Example 3: 0% interest rate, with property tax
#' compute_affordable_principal(monthly_housing_budget = 2000,
#'                              monthly_non_mortgage_costs = 200,
#'                              rate_per_month = 0,
#'                              n_payments_total = 180, # 15 years
#'                              prop_tax_rate_annual = 1.0,
#'                              down_payment_pct = 10)
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
#' determining the affordable loan principal using `compute_affordable_principal`,
#' then estimating the corresponding home value based on the down payment,
#' and finally calculating the monthly tax based on the annual property tax rate.
#'
#' @inheritParams compute_affordable_principal
#' @param prop_tax_rate_annual Numeric. Estimated annual property tax rate as a
#'   percentage of home value (e.g., 1.2 for 1.2%). Must be non-negative.
#' @param down_payment_pct Numeric. The down payment as a percentage of the
#'   home price (e.g., 20 for 20%). Must be between 0 (inclusive) and 100 (exclusive).
#'   Defaults to 20.
#'
#' @return Numeric. The estimated monthly property tax. Returns 0 if the
#'   calculated affordable principal is zero or less, or if the
#'   `prop_tax_rate_annual` is zero.
#' @export
#'
#' @examples
#' # Example using the same inputs as Example 2 for compute_affordable_principal
#' # Expected principal was ~303637. Expected home price ~379546.
#' # Expected monthly tax = (379546 * 0.012) / 12 = 379.55
#' estimate_monthly_property_tax(monthly_housing_budget = 2500,
#'                               monthly_non_mortgage_costs = 300,
#'                               rate_per_month = 0.06 / 12,
#'                               n_payments_total = 360,
#'                               prop_tax_rate_annual = 1.2,
#'                               down_payment_pct = 20)
#'
#' # Example with zero tax rate
#' estimate_monthly_property_tax(monthly_housing_budget = 2500,
#'                               monthly_non_mortgage_costs = 300,
#'                               rate_per_month = 0.06 / 12,
#'                               n_payments_total = 360,
#'                               prop_tax_rate_annual = 0,
#'                               down_payment_pct = 20) # Should be 0
#'
#' # Example where principal would be 0
#' estimate_monthly_property_tax(monthly_housing_budget = 300,
#'                               monthly_non_mortgage_costs = 300,
#'                               rate_per_month = 0.06 / 12,
#'                               n_payments_total = 360,
#'                               prop_tax_rate_annual = 1.0,
#'                               down_payment_pct = 20) # Should be 0
#'
estimate_monthly_property_tax <- function(monthly_housing_budget,
                                          monthly_non_mortgage_costs = 0,
                                          rate_per_month,
                                          n_payments_total,
                                          prop_tax_rate_annual, # Required for this function
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
#' @return Numeric. The calculated maximum affordable loan principal, considering PMI.
#' @export
#'
#' @examples
#' # -- Using Percentage Down Payment --
#' # Example 1: Sufficient % down payment (20%), no PMI applied
#' compute_principal_with_pmi(monthly_housing_budget = 2500,
#'                              monthly_non_mortgage_costs = 300,
#'                              rate_per_month = 0.06 / 12,
#'                              n_payments_total = 360,
#'                              prop_tax_rate_annual = 1.2,
#'                              down_payment_pct = 20,
#'                              pmi_rate_annual = 0.5)
#' # Should match compute_affordable_principal result: approx 336815
#'
#' # Example 2: Low % down payment (10%), PMI applied
#' compute_principal_with_pmi(monthly_housing_budget = 2500,
#'                              monthly_non_mortgage_costs = 300,
#'                              rate_per_month = 0.06 / 12,
#'                              n_payments_total = 360,
#'                              prop_tax_rate_annual = 1.2,
#'                              down_payment_pct = 10,
#'                              pmi_rate_annual = 0.5)
#' # Expect result lower than Ex 1 due to PMI cost: approx 317717
#'
#' # -- Using Dollar Down Payment --
#' # Example 3: Fixed $ down payment, sufficient to avoid PMI
#' # (Down payment $50k on a hypothetical ~$400k house > 10% threshold)
#' compute_principal_with_pmi(monthly_housing_budget = 2000,
#'                            monthly_non_mortgage_costs = 200,
#'                            rate_per_month = 0.07 / 12,
#'                            n_payments_total = 360,
#'                            prop_tax_rate_annual = 1.0,
#'                            down_payment_dollars = 50000,
#'                            pmi_rate_annual = 0.6,
#'                            pmi_threshold_pct = 10)
#' # Expected: approx 258346 (Implies HP approx 308346, DP is ~16% > 10%)
#'
#' # Example 4: Fixed $ down payment, insufficient to avoid PMI
#' # (Down payment $20k on a hypothetical ~$300k house < 10% threshold)
#' compute_principal_with_pmi(monthly_housing_budget = 2000,
#'                            monthly_non_mortgage_costs = 200,
#'                            rate_per_month = 0.07 / 12,
#'                            n_payments_total = 360,
#'                            prop_tax_rate_annual = 1.0,
#'                            down_payment_dollars = 20000,
#'                            pmi_rate_annual = 0.6,
#'                            pmi_threshold_pct = 10)
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
                             (prop_tax_rate_annual / 100) / 12)

  pmi_rate_monthly <- ifelse(is.null(pmi_rate_annual) || pmi_rate_annual == 0,
                             0,
                             (pmi_rate_annual / 100) / 12)

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
    dp_dollars      <- down_payment_dollars
    # Precompute scenarios
    denom_no_pmi    <- inv_pv_factor + tax_rate_monthly
    hp_no_pmi       <- if (denom_no_pmi > 0) (budget_avail + dp_dollars * inv_pv_factor) / denom_no_pmi else 0
    denom_pmi       <- inv_pv_factor + tax_rate_monthly + pmi_rate_monthly
    hp_pmi          <- if (denom_pmi > 0) (budget_avail + dp_dollars * inv_pv_factor + dp_dollars * pmi_rate_monthly) / denom_pmi else 0
    pmi_thresh_dec  <- pmi_threshold_pct / 100
    H_thresh        <- dp_dollars / pmi_thresh_dec
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
#'   \item{annualized_rate_of_return}{The annualized internal rate of return (IRR) of the extra payments, 
#'   representing the effective annual return earned on the extra payments made toward the mortgage}
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
  stopifnot(is.numeric(n_payments_total), n_payments_total > 0,
            n_payments_total == floor(n_payments_total))
  stopifnot(is.numeric(extra_monthly_payment), extra_monthly_payment >= 0)
  stopifnot(is.numeric(lump_sum_payment), lump_sum_payment >= 0)

  if (lump_sum_payment > 0) {
    stopifnot(is.numeric(payment_number_for_prepay_start),
              payment_number_for_prepay_start >= 1,
              payment_number_for_prepay_start <= n_payments_total,
              payment_number_for_prepay_start == floor(payment_number_for_prepay_start))
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
  remaining_principal <- principal
  original_remaining_principal <- principal  # Track original loan balance
  new_total_interest <- 0
  original_total_interest_paid <- 0  # Track total interest paid on original loan
  cashflows <- numeric(0)  # For calculating IRR
  
  # Calculate the original payment amount
  original_payment <- principal * (rate_per_month * (1 + rate_per_month)^n_payments_total) / 
    ((1 + rate_per_month)^n_payments_total - 1)

  # If lump sum is at payment 1, apply it immediately
  if (lump_sum_payment > 0 && payment_number_for_prepay_start == 1) {
    remaining_principal <- max(0, remaining_principal - lump_sum_payment)
  }

  if (cumulative_output) {
    out <- data.frame(
      payment_number = numeric(0),
      original_principal_payment = numeric(0),
      new_principal_payment = numeric(0),
      principal_payment_extra = numeric(0),
      original_interest_payment = numeric(0),
      new_interest_payment = numeric(0),
      original_remaining_principal = numeric(0),
      new_remaining_principal = numeric(0),
      original_interest_paid = numeric(0),
      new_interest_paid = numeric(0),
      total_interest_saved = numeric(0),
      annualized_rate_of_return = numeric(0)
    )
  }

  # Amortize the loan with extra payments
  while (current_payment_number < n_payments_total) {
    current_payment_number <- current_payment_number + 1

    # Apply lump sum if this is the payment number for it (and not already applied)
    if (lump_sum_payment > 0 && current_payment_number == payment_number_for_prepay_start && current_payment_number > 1) {
      remaining_principal <- max(0, remaining_principal - lump_sum_payment)
    }

    # If loan is already paid off, break
    if (remaining_principal <= 0 && !cumulative_output) {
      break
    }

    # Calculate interest for this period
    interest_payment <- remaining_principal * rate_per_month

    # Calculate principal payment (standard + extra)
    principal_payment <- original_payment - interest_payment

    # factor in extra monthly payments
    if (current_payment_number >= payment_number_for_prepay_start) {
      principal_payment <- principal_payment + extra_monthly_payment
    }

    # Ensure we don't overpay on the last payment
    if (remaining_principal < principal_payment) {
      principal_payment <- remaining_principal
    }

    # Update remaining principal and total interest
    remaining_principal <- remaining_principal - principal_payment
    new_total_interest <- new_total_interest + interest_payment
    
    # Track original loan's remaining balance and interest
    original_interest_payment <- original_remaining_principal * rate_per_month
    original_principal_payment <- original_payment - original_interest_payment
    original_remaining_principal <- original_remaining_principal - original_principal_payment
    original_total_interest_paid <- original_total_interest_paid + original_interest_payment

    # Compute cashflow for IRR
    if (current_payment_number >= payment_number_for_prepay_start) {
      # For the first payment with extra payment, include the lump sum if applicable
      extra_prepay <- if (current_payment_number == payment_number_for_prepay_start) {
        lump_sum_payment + extra_monthly_payment
      } else {
        extra_monthly_payment
      }
      
      # The cash flow is the extra payment (negative) plus the interest saved (positive)
      # This represents the net cost/benefit of the extra payment
      interest_savings <- original_interest_payment - interest_payment
      cashflows <- c(cashflows, interest_savings - extra_prepay)
    } else {
      # No extra payment, no cash flow
      cashflows <- c(cashflows, 0)
    }

    if (cumulative_output) {
      original_principal_payment <- original_payment - original_interest_payment
      if (current_payment_number == 1) {
        cumulative_interest_paid <- original_interest_payment
        new_interest_paid <- interest_payment
      } else {
        cumulative_interest_paid <- cumulative_interest_paid + original_interest_payment
      }

      if (remaining_principal > 0 && current_payment_number > 1) {
        new_interest_paid <- new_interest_paid + interest_payment
      }

      if(current_payment_number == payment_number_for_prepay_start) {
        extra_prepay_this_month <- lump_sum_payment + extra_monthly_payment
      } else {
        extra_prepay_this_month <- extra_monthly_payment
      }
      # For cumulative output, we need to track the running IRR
      # We'll calculate this at the end for better performance
      annualized_return <- NA_real_  # Will be filled in after the loop
      
      # Track the cash flows for this period
      if (current_payment_number >= payment_number_for_prepay_start) {
        # This is an extra payment period
        if (remaining_principal <= 0) {
          # At payoff, we get the present value of all future interest savings as a positive cash flow
          future_interest_savings <- original_total_interest - cumulative_interest_paid - (new_total_interest - new_interest_paid)
          cashflows <- c(cashflows, future_interest_savings)
        } else {
          # Regular extra payment (negative cash flow)
          cashflows <- c(cashflows, -extra_prepay_this_month)
        }
      } else {
        # No extra payment this period
        cashflows <- c(cashflows, 0)
      }
      out <- rbind(out, data.frame(
        payment_number = current_payment_number,
        original_principal_payment = original_principal_payment,
        new_principal_payment = principal_payment,
        principal_payment_extra = extra_prepay_this_month,
        original_interest_payment = original_interest_payment,
        new_interest_payment = interest_payment,
        original_remaining_principal = original_remaining_principal,
        new_remaining_principal = remaining_principal,
        original_interest_paid = cumulative_interest_paid,
        new_interest_paid = new_interest_paid,
        total_interest_saved = cumulative_interest_paid - new_interest_paid,
        annualized_rate_of_return = annualized_return
      ))

      if (current_payment_number == n_payments_total) {
        break
      }
    } else if (remaining_principal < 0.01) {
      remaining_principal <- 0
      break
    }

  }

  # Return the full amortization table if requested
  if (cumulative_output) {
    # Calculate IRR for each period where we have extra payments
    extra_payment_periods <- which(out$principal_payment_extra > 0)
    if (length(extra_payment_periods) > 0) {
      # Only calculate IRR for periods with extra payments
      for (i in extra_payment_periods) {
        # Use cash flows up to this period
        cf <- cashflows[1:i]
        if (length(cf) > 1) {
          # Only calculate if we have at least 2 non-zero cash flows
          non_zero_idx <- which(cf != 0)
          if (length(non_zero_idx) > 1) {
            cf <- cf[non_zero_idx]
            f <- function(r) sum(cf / ((1 + r)^(seq_along(cf))))
            
            # Try to find IRR
            lower <- -0.99
            upper <- 1.0  # 100% monthly return is already very high
            
            tryCatch({
              irr_result <- uniroot(f, c(lower, upper), extendInt = "yes", maxiter = 1000)$root
              if (is.finite(irr_result) && irr_result > lower && irr_result < upper) {
                # Convert monthly to annual return
                out$annualized_rate_of_return[i] <- (1 + irr_result)^12 - 1
              }
            }, error = function(e) {
              # Silently ignore errors in IRR calculation
            })
          }
        }
      }
    }
    return(out)
  }

  # Calculate results
  total_interest_savings <- original_total_interest - new_total_interest
  new_loan_term_months <- current_payment_number
  months_saved <- n_payments_total - new_loan_term_months

  # Ensure we don't report negative savings (in case of calculation edge cases)
  total_interest_savings <- max(0, total_interest_savings)
  months_saved <- max(0, months_saved)
  
  # Add final cash flow for the interest savings at payoff
  if (remaining_principal <= 0 && length(cashflows) > 0) {
    cashflows[length(cashflows)] <- cashflows[length(cashflows)] + total_interest_savings
  }

  # Calculate IRR and annualized return
  irr_monthly <- NA_real_
  if (length(cashflows) > 0 && any(cashflows != 0)) {
    # Only use non-zero cash flows for IRR calculation
    non_zero_idx <- which(cashflows != 0)
    if (length(non_zero_idx) > 0) {
      # Only keep the non-zero cash flows
      cf <- cashflows[non_zero_idx]
      
      # If we only have one non-zero cash flow, the IRR is undefined
      if (length(cf) > 1) {
        f <- function(r) {
          if (abs(1 + r) < 1e-10) return(Inf)
          sum(cf / ((1 + r)^(seq_along(cf))))
        }
        
        # Try to find a reasonable range for the root
        lower <- -0.99  # -99% monthly return (minimum)
        upper <- 10.0   # 1000% monthly return (maximum)
        
        # Check if we can find a valid range
        f_lower <- tryCatch(f(lower), error = function(e) NA_real_)
        f_upper <- tryCatch(f(upper), error = function(e) NA_real_)
        
        if (!is.na(f_lower) && !is.na(f_upper)) {
          # Try to find the root
          irr_result <- tryCatch({
            uniroot(f, c(lower, upper), extendInt = "yes", maxiter = 1000)$root
          }, error = function(e) {
            return(NA_real_)
          })
          
          if (is.numeric(irr_result) && is.finite(irr_result) && 
              irr_result > lower && irr_result < upper) {
            irr_monthly <- irr_result
          }
        }
      }
    }
  }
  
  # Convert monthly IRR to annualized return if valid
  annualized_rate_of_return <- if (!is.na(irr_monthly) && is.finite(irr_monthly)) {
    (1 + irr_monthly)^12 - 1  # Convert monthly to annual return
  } else {
    NA_real_
  }

  # Return results
  return(list(
    total_interest_savings = total_interest_savings,
    months_saved = months_saved,
    new_loan_term_months = new_loan_term_months,
    original_total_interest = original_total_interest,
    new_total_interest = new_total_interest,
    annualized_rate_of_return = annualized_rate_of_return
  ))
}
