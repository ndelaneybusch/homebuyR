% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_visualizations.R
\name{plot_price_vs_down_payment}
\alias{plot_price_vs_down_payment}
\title{Plot Affordable Home Price vs. Down Payment Percentage}
\usage{
plot_price_vs_down_payment(
  monthly_housing_budget,
  monthly_non_mortgage_costs,
  annual_rate_pct,
  mortgage_term_months,
  prop_tax_rate_annual,
  pmi_rate_annual = 0.5,
  pmi_threshold_pct = 20,
  dp_pct_range = seq(1, 30, by = 1)
)
}
\arguments{
\item{monthly_housing_budget}{Numeric. Total monthly housing budget.}

\item{monthly_non_mortgage_costs}{Numeric. Non-mortgage, non-tax costs per month.}

\item{annual_rate_pct}{Numeric. Annual interest rate (percentage).}

\item{mortgage_term_months}{Integer. Loan term in months.}

\item{prop_tax_rate_annual}{Numeric. Annual property tax rate (percentage).}

\item{pmi_rate_annual}{Numeric. Annual PMI rate (percentage).}

\item{pmi_threshold_pct}{Numeric. Down payment threshold for PMI (percentage).}

\item{dp_pct_range}{Numeric vector. Range of down payment percentages to plot (e.g., `seq(1, 30, by = 1)`).}
}
\value{
A ggiraph object.
}
\description{
Creates an interactive line plot showing the estimated maximum affordable
home price across a range of down payment percentages for a fixed monthly budget.
Lines are colored based on whether PMI is likely required.
}
