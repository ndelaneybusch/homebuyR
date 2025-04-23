#' Compute Housing Budget from Disposable Income
#'
#' Calculates the maximum monthly housing budget based on net income and debt constraints.
#'
#' @param net_income Numeric. Net disposable income per month. Must be non-negative.
#' @param max_housing_pct Numeric. Maximum fraction of net income for housing costs (e.g., 0.30). Defaults to 0.30.
#' @param max_total_debt_pct Numeric. Maximum fraction of net income for total debt obligations (e.g., 0.45). Defaults to 0.45.
#' @param other_debts Numeric. Monthly debt payments excluding housing. Must be non-negative. Defaults to 0.
#'
#' @return Numeric. Recommended maximum monthly housing budget.
#' @export
#'
#' @examples
#' # Net income $3000, 30% housing, 40% total debt, other debts $200 -> 900
#' housing_budget_from_dpi(net_income = 3000, max_housing_pct = 0.30, max_total_debt_pct = 0.40, other_debts = 200)
housing_budget_from_dpi <- function(net_income, max_housing_pct = 0.30, max_total_debt_pct = 0.45, other_debts = 0) {
  stopifnot(is.numeric(net_income), net_income >= 0, is.finite(net_income), !is.na(net_income),
            is.numeric(max_housing_pct), max_housing_pct >= 0, max_housing_pct <= 1, is.finite(max_housing_pct), !is.na(max_housing_pct),
            is.numeric(max_total_debt_pct), max_total_debt_pct >= 0, max_total_debt_pct <= 1, is.finite(max_total_debt_pct), !is.na(max_total_debt_pct),
            is.numeric(other_debts), other_debts >= 0, is.finite(other_debts), !is.na(other_debts))
  housing_max <- net_income * max_housing_pct
  total_debt_max <- net_income * max_total_debt_pct
  remaining_for_housing <- total_debt_max - other_debts
  recommended <- min(housing_max, remaining_for_housing)
  return(max(0, recommended))
}

#' Compute Maximum Affordable Housing (Stressed DTI)
#'
#' Calculates the maximum affordable housing amount under cash-flow and debt-to-income constraints.
#'
#' @param gross_monthly_income Numeric. Gross monthly income. Must be non-negative.
#' @param other_debts Numeric. Monthly debt payments excluding housing. Must be non-negative.
#' @param non_housing_essentials Numeric. Monthly essential expenses excluding debts and housing. Must be non-negative.
#' @param rate_per_month Numeric. Monthly interest rate. Must be non-negative.
#' @param n_payments_total Integer. Total number of payments. Must be a positive integer.
#' @param savings Numeric. Total savings buffer. Must be non-negative. Defaults to 0.
#' @param income_shock_pct Numeric. Fractional income reduction during stress. Must be between 0 and 1. Defaults to 0.20.
#' @param shock_duration_months Integer. Duration of stress period in months. Must be a positive integer. Defaults to 6.
#' @param max_total_dti_stress Numeric. Maximum debt-to-income ratio under stress. Must be between 0 and 1. Defaults to 0.50.
#'
#' @return Numeric. Recommended maximum monthly housing amount.
#' @export
#'
#' @examples
#' # Gross $5000, $500 debts, $1000 essentials -> 1500
#' housing_budget_from_stressed_dti(gross_monthly_income = 5000, other_debts = 500, non_housing_essentials = 1000, rate_per_month = 0, n_payments_total = 1)
housing_budget_from_stressed_dti <- function(
  gross_monthly_income,
  other_debts,
  non_housing_essentials,
  rate_per_month,
  n_payments_total,
  savings =0,
  income_shock_pct = 0.20,
  shock_duration_months = 6,
  max_total_dti_stress = 0.50
) {
  stopifnot(is.numeric(gross_monthly_income), gross_monthly_income >= 0, is.finite(gross_monthly_income), !is.na(gross_monthly_income),
            is.numeric(other_debts), other_debts >= 0, is.finite(other_debts), !is.na(other_debts),
            is.numeric(non_housing_essentials), non_housing_essentials >= 0, is.finite(non_housing_essentials), !is.na(non_housing_essentials),
            is.numeric(rate_per_month), rate_per_month >= 0, is.finite(rate_per_month), !is.na(rate_per_month),
            is.numeric(n_payments_total), n_payments_total > 0, n_payments_total == floor(n_payments_total), is.finite(n_payments_total), !is.na(n_payments_total),
            is.numeric(savings), savings >= 0, is.finite(savings), !is.na(savings),
            is.numeric(income_shock_pct), income_shock_pct >= 0, income_shock_pct <= 1, is.finite(income_shock_pct), !is.na(income_shock_pct),
            is.numeric(shock_duration_months), shock_duration_months > 0, shock_duration_months == floor(shock_duration_months), is.finite(shock_duration_months), !is.na(shock_duration_months),
            is.numeric(max_total_dti_stress), max_total_dti_stress >= 0, max_total_dti_stress <= 1, is.finite(max_total_dti_stress), !is.na(max_total_dti_stress))
  stressed_income <- (1 - income_shock_pct) * gross_monthly_income
  buffer_per_month <- savings / shock_duration_months

  # Constraint 1: Affordability (cash-flow over T months)
  h_affordable <- stressed_income + buffer_per_month - other_debts - non_housing_essentials

  # Constraint 2: Stress DTI
  h_dti <- max_total_dti_stress * stressed_income - other_debts

  h_max <- min(h_affordable, h_dti)

  return(max(0, h_max))
}

#' Compute Monthly Housing Budget from Gross Income Percentage
#'
#' Calculates monthly housing budget as a percentage of gross income.
#'
#' @param gross_monthly_income Numeric. Gross monthly income. Must be non-negative.
#' @param housing_percent Numeric. Percentage of gross income for housing (0–100). Must be between 0 and 100.
#'
#' @return Numeric. Monthly housing budget.
#' @export
#'
#' @examples
#' # Gross $5000, 30% housing -> 1500
#' housing_budget_from_gross_pct(5000, 30)
housing_budget_from_gross_pct <- function(gross_monthly_income, housing_percent) {
  stopifnot(is.numeric(gross_monthly_income), gross_monthly_income >= 0, is.finite(gross_monthly_income), !is.na(gross_monthly_income),
            is.numeric(housing_percent), housing_percent >= 0, housing_percent <= 100, is.finite(housing_percent), !is.na(housing_percent))
  return(gross_monthly_income * (housing_percent / 100))
}