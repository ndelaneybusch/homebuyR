% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_annuity.R
\name{compute_principal}
\alias{compute_principal}
\title{Compute Original Loan Principal}
\usage{
compute_principal(monthly_payment, rate_per_month, n_payments_total)
}
\arguments{
\item{monthly_payment}{Numeric. The fixed monthly payment amount.
Must be non-negative.}

\item{rate_per_month}{Numeric. The interest rate per month.
Must be non-negative.}

\item{n_payments_total}{Integer. The total number of payments over the
life of the loan. Must be positive.}
}
\value{
Numeric. The calculated original loan principal.
}
\description{
Calculates the original loan principal amount based on the fixed monthly
payment, interest rate, and total number of payments.
This uses the standard annuity formula (present value of an annuity).
}
\examples{
# Calculate principal for a $1800/month payment over 30 years at 6\% APR
annual_rate <- 0.06
monthly_rate <- annual_rate / 12
compute_principal(
  monthly_payment = 1798.65,
  rate_per_month = monthly_rate,
  n_payments_total = 360
)
# Expected output: approx 300000

# Calculate principal for a 0\% interest loan with $1000/month payment for 1 year
compute_principal(
  monthly_payment = 1000,
  rate_per_month = 0,
  n_payments_total = 12
)
# Expected output: 12000
}
