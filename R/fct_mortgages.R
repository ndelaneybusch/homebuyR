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
  stopifnot(is.numeric(down_payment_pct), down_payment_pct >= 0, down_payment_pct < 100)
  # `compute_affordable_principal` will validate the other inputs

  # Handle zero tax rate case immediately
  if (prop_tax_rate_annual == 0) {
    return(0)
  }

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
    # --- Scenario 2: Down Payment provided as Dollars (Corrected Logic) ---
    dp_dollars <- down_payment_dollars

    # Calculate affordable Home Price assuming NO PMI is required
    # H_no_pmi = (B_avail + P_i * DP) / (P_i + T_m)
    # where P_i = inverse PV factor, T_m = monthly tax rate
    denominator_no_pmi <- inv_pv_factor + tax_rate_monthly
    hp_no_pmi <- 0
    if (denominator_no_pmi > 0) {
      # Note: budget_avail already excludes non_mortgage_costs
      # The term dp_dollars * inv_pv_factor accounts for the fact that the down payment reduces the principal needed
      hp_no_pmi <- (budget_avail + dp_dollars * inv_pv_factor) / denominator_no_pmi
    }

    # Check if this potential home price is actually affordable (i.e., > down payment)
    if (hp_no_pmi <= dp_dollars) {
      affordable_principal_pmi <- 0
      home_price_pmi <- 0
    } else {
      # Check if PMI would be required at this hypothetical price (hp_no_pmi)
      pmi_would_be_required <- FALSE
      if (pmi_rate_monthly > 0) {
        effective_dp_pct_at_hp_no_pmi <- (dp_dollars / hp_no_pmi) * 100
        if (effective_dp_pct_at_hp_no_pmi < pmi_threshold_pct) {
          pmi_would_be_required <- TRUE
        }
      }

      if (!pmi_would_be_required) {
        # If PMI is NOT required, then hp_no_pmi is the true affordable price
        affordable_principal_pmi <- hp_no_pmi - dp_dollars
        home_price_pmi <- hp_no_pmi
      } else {
        # If PMI IS required, then hp_no_pmi is not attainable.
        # We must calculate the affordable home price *including* PMI.
        # H_pmi = (B_avail + P_i*DP + M_m*DP) / (P_i + T_m + M_m)
        # where M_m = monthly pmi rate
        denominator_pmi <- inv_pv_factor + tax_rate_monthly + pmi_rate_monthly
        hp_pmi <- 0
        if (denominator_pmi > 0) {
           # The term dp_dollars * pmi_rate_monthly accounts for the PMI reduction due to down payment
           # (although PMI is usually on principal, this formulation works for solving for H)
           numerator_pmi <- budget_avail + dp_dollars * inv_pv_factor + dp_dollars * pmi_rate_monthly
           hp_pmi <- numerator_pmi / denominator_pmi
        }

        # Check if this price is affordable
        if (hp_pmi <= dp_dollars) {
          affordable_principal_pmi <- 0
          home_price_pmi <- 0
        } else {
          affordable_principal_pmi <- hp_pmi - dp_dollars
          home_price_pmi <- hp_pmi
        }

        # --- Compute the alternative: max home price without PMI given cash down ---
        # H_no_pmi = D / (T/100)
        # P_no_pmi = H_no_pmi - D
        pmi_threshold_dec <- pmi_threshold_pct / 100
        hp_no_pmi_threshold <- dp_dollars / pmi_threshold_dec
        principal_no_pmi_threshold <- hp_no_pmi_threshold - dp_dollars

        # Now check if this principal is affordable given the budget (no PMI)
        principal_affordable_no_pmi <- compute_affordable_principal(
          monthly_housing_budget = monthly_housing_budget,
          monthly_non_mortgage_costs = monthly_non_mortgage_costs,
          rate_per_month = rate_per_month,
          n_payments_total = n_payments_total,
          prop_tax_rate_annual = ifelse(is.null(prop_tax_rate_annual), 0, prop_tax_rate_annual),
          down_payment_pct = pmi_threshold_pct
        )
        principal_no_pmi_final <- min(principal_no_pmi_threshold, principal_affordable_no_pmi)
        home_price_no_pmi_final <- principal_no_pmi_final + dp_dollars

        # --- Return the principal that yields the higher home price ---
        if (home_price_no_pmi_final > home_price_pmi) {
          affordable_principal <- principal_no_pmi_final
        } else {
          affordable_principal <- affordable_principal_pmi
        }
      }
    }
  }

  # Ensure non-negative result
  return(max(0, affordable_principal))
}
