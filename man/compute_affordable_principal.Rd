% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_mortgages.R
\name{compute_affordable_principal}
\alias{compute_affordable_principal}
\title{Compute Affordable Mortgage Principal}
\usage{
compute_affordable_principal(
  monthly_housing_budget,
  monthly_non_mortgage_costs = 0,
  rate_per_month,
  n_payments_total,
  prop_tax_rate_annual = NULL,
  down_payment_pct = 20
)
}
\arguments{
\item{monthly_housing_budget}{Numeric. The total maximum amount available
for all housing costs per month.}

\item{monthly_non_mortgage_costs}{Numeric. The sum of fixed monthly costs
*excluding* mortgage P&I and property taxes (e.g., home insurance,
HOA dues, other fixed monthly fees). Defaults to 0.}

\item{rate_per_month}{Numeric. The interest rate per month (e.g., annual
rate / 12 / 100). Must be non-negative.}

\item{n_payments_total}{Integer. The total number of payments over the
life of the loan (e.g., 360 for 30 years). Must be positive.}

\item{prop_tax_rate_annual}{Numeric. Optional. Estimated annual property
tax rate as a percentage of home value (e.g., 1.2 for 1.2%). If provided
and > 0, property tax is factored into the affordability calculation.
Defaults to NULL (property tax not factored in).}

\item{down_payment_pct}{Numeric. Optional. The down payment as a percentage
of the home price (e.g., 20 for 20%). Only used if `prop_tax_rate_annual`
is also provided and > 0. Defaults to 20.}
}
\value{
Numeric. The calculated maximum affordable loan principal.
}
\description{
Calculates the maximum affordable loan principal based on a total monthly
housing budget, other non-mortgage costs, loan terms, and optionally
factoring in estimated property taxes based on the derived home price.
}
\examples{
# Example 1: Simple case without property tax factoring
compute_affordable_principal(
  monthly_housing_budget = 2500,
  monthly_non_mortgage_costs = 300,
  rate_per_month = 0.06 / 12,
  n_payments_total = 360
)
# Budget for P&I = 2500 - 300 = 2200
# Expected principal: compute_principal(2200, 0.005, 360) -> approx 366951

# Example 2: Factoring in property tax (1.2\%) and 20\% down
compute_affordable_principal(
  monthly_housing_budget = 2500,
  monthly_non_mortgage_costs = 300,
  rate_per_month = 0.06 / 12,
  n_payments_total = 360,
  prop_tax_rate_annual = 1.2,
  down_payment_pct = 20
)
# Calculation is more complex, result should be lower than Ex 1.
# Expected: approx 336815

# Example 3: 0\% interest rate, with property tax
compute_affordable_principal(
  monthly_housing_budget = 2000,
  monthly_non_mortgage_costs = 200,
  rate_per_month = 0,
  n_payments_total = 180, # 15 years
  prop_tax_rate_annual = 1.0,
  down_payment_pct = 10
)
# Expected: approx 297521

}
