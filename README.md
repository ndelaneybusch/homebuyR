
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homebuyR

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/ndelaneybusch/homebuyR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndelaneybusch/homebuyR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ndelaneybusch/homebuyR/graph/badge.svg)](https://app.codecov.io/gh/ndelaneybusch/homebuyR)
<!-- badges: end -->

**homebuyR** is a comprehensive R package that provides tools for home
financing analysis through an interactive Shiny web application. The
package helps prospective homebuyers and current homeowners make
informed financial decisions by offering:

- **Budget Planning**: Calculate affordable housing budgets using
  multiple methodologies (percentage of income, debt-to-income ratios,
  stress-tested scenarios)
- **Mortgage Analysis**: Compute affordable home prices, monthly
  payments, and explore the impact of different down payment amounts and
  interest rates
- **Payment Optimization**: Analyze the benefits of extra mortgage
  payments and compare different payment strategies
- **Refinancing Analysis**: Comprehensive refinance benefit calculations
  that account for equity/amortization, investment opportunities, tax
  implications, and break-even analysis
- **Interactive Visualizations**: Explore scenarios through interactive
  plots and detailed financial projections

See documentation [here](https://ndelaneybusch.github.io/homebuyR/).

## Installation

You can install the development version of homebuyR from GitHub using
several methods:

### Option 1: Using `remotes`

``` r
# Install remotes if you haven't already
install.packages("remotes")

# Install homebuyR
remotes::install_github("ndelaneybusch/homebuyR")
```

### Option 2: Using `devtools`

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install homebuyR
devtools::install_github("ndelaneybusch/homebuyR")
```

### For developers (with dependencies)

``` r
# Clone the repository and install with development dependencies
remotes::install_github("ndelaneybusch/homebuyR", dependencies = TRUE)
```

## Quick Start

### Launch the Interactive App

The easiest way to use homebuyR is through its interactive Shiny web
application:

``` r
homebuyR::run_app()
```

### Using the Function Library

You can also use homebuyR's functions programmatically for custom
analyses:

``` r
library(homebuyR)

# Calculate affordable mortgage principal
principal <- compute_affordable_principal(
  budget = 3000,                    # Monthly housing budget
  annual_rate = 0.065,              # 6.5% annual interest rate
  term_years = 30,                  # 30-year mortgage
  property_tax_annual = 8000,       # Annual property tax
  insurance_monthly = 200,          # Monthly insurance
  hoa_monthly = 150                 # Monthly HOA fees
)

# Calculate monthly payment for a given principal
payment <- compute_monthly_payment(
  principal = 400000,               # Loan amount
  annual_rate = 0.065,              # Annual interest rate
  term_years = 30                   # Loan term
)

# Analyze refinance benefits
refinance_analysis <- calculate_refinance_benefit_curve(
  current_principal = 300000,
  current_rate = 0.075,
  new_rate = 0.055,
  closing_costs = 5000,
  investment_return = 0.07
)
```

## Function Library Overview

homebuyR provides a comprehensive set of functions organized into five
main categories:

### 1. Annuity Calculations (`fct_annuity.R`)

Core mortgage payment mathematics:

- `compute_monthly_payment()`: Calculate standard mortgage payments
- `compute_principal()`: Reverse calculation from payment to principal
  amount
- `compute_principal_remaining()`: Outstanding balance calculations
- `compute_principal_paid()` / `compute_interest_paid()`: Payment
  breakdowns over time

### 2. Budget Planning (`fct_budgeting.R`)

Determine affordable housing budgets using different methodologies:

- `housing_budget_from_gross_pct()`: Simple percentage of gross income
  approach
- `housing_budget_from_dpi()`: Debt-to-income ratio methodology
- `housing_budget_from_stressed_dti()`: Stress-tested affordability with
  financial shock scenarios

### 3. Mortgage Analysis (`fct_mortgages.R`)

Advanced affordability calculations including taxes and PMI:

- `compute_affordable_principal()`: Core affordability with property tax
  integration
- `estimate_monthly_property_tax()`: Property tax estimation based on
  home price
- `compute_principal_with_pmi()`: PMI-aware affordability calculations
- `calculate_mortgage_savings()`: Extra payment analysis with detailed
  amortization

### 4. Refinance Analysis (`fct_refinance.R`)

Comprehensive refinance benefit analysis:

- `calculate_refinance_benefit_curve()`: Month-by-month refinance
  benefit calculation including investment returns and tax
  considerations
- `calculate_invested_savings_fv()`: Future value calculations for
  invested payment savings
- `calculate_tax_savings_differential()`: Mortgage interest deduction
  analysis

### 5. Interactive Visualizations (`fct_visualizations.R`)

Create interactive plots for exploring mortgage scenarios:

- `plot_price_vs_down_payment()`: Affordability vs. down payment
  percentage
- `plot_price_vs_rate()`: Affordability vs. interest rate with PMI zones
- `plot_principal_interest()`: Loan paydown comparison over time
- `plot_refinance_benefit()`: Interactive refinance benefit curves

------------------------------------------------------------------------

## For Developers

### Golem Framework Infrastructure

homebuyR is built using the [Golem
framework](https://thinkr-open.github.io/golem/).

#### Project Structure

    R/
    ├── app_config.R          # App configuration
    ├── app_server.R          # Main server logic
    ├── app_ui.R              # Main UI definition
    ├── run_app.R             # App entry point
    ├── fct_*.R               # Business logic functions
    └── golem_utils_*.R       # Golem utilities

    inst/
    └── golem-config.yml      # Environment-specific settings

### Environment and Dependencies Management

homebuyR uses `renv` for reproducible package management.

#### Key renv Commands

``` r
# Activate renv (done automatically via .Rprofile)
renv::activate()

# Install packages and update lockfile
install.packages("package_name")
renv::snapshot()

# Restore packages from lockfile
renv::restore()

# Check for package updates
renv::status()

# Update packages
renv::update()

# Install development dependencies
renv::install(c("devtools", "pkgdown", "covr"))
```

#### Dependency Management Workflow

``` r
# 1. Install new package
install.packages("new_package")

# 2. Add to DESCRIPTION file (if needed for package)
usethis::use_package("new_package")

# 3. Update lockfile
renv::snapshot()

# 4. Document changes
devtools::document()
```

### Testing Infrastructure

#### Running Tests

``` r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "fct_annuity")

# Run tests with coverage
covr::package_coverage()

# Interactive coverage report
covr::report()
```

#### Test Commands

``` bash
# Command line testing (if available)
Rscript -e "devtools::test()"

# Coverage report
Rscript -e "covr::package_coverage()"
```

#### Writing Tests

Tests follow the pattern `test-{filename}.R` in `tests/testthat/`:

### GitHub Workflows

The project includes CI/CD automation:

#### 1. R-CMD-check (`R-CMD-check.yaml`)

**Multi-platform automated testing:**

``` yaml
# Triggers: push to main/master, all PRs
# Platforms: macOS, Windows, Ubuntu
# R versions: devel, release, oldrel-1
```

#### 2. Test Coverage (`test-coverage.yaml`)

**Automated coverage analysis:**

``` yaml
# Triggers: push to main/master, all PRs
# Platform: Ubuntu latest
# Integration: Codecov
```

#### 3. Auto-documentation (`pkgdown.yaml`)

**Automated website generation:**

``` yaml
# Triggers: push/PR to main, releases, manual dispatch
# Deployment: GitHub Pages
# Content: Full package documentation and function references
```

#### 4. Documentation Updates (`document.yaml`)

**Automatic roxygen2 documentation:**

``` yaml
# Triggers: changes to R/ files
# Actions: Updates man/, NAMESPACE, DESCRIPTION
# Auto-commit: Yes
```

#### Developer Workflow Integration

``` r
# Before pushing changes:
devtools::document()          # Update documentation
devtools::test()              # Run tests locally
devtools::check()             # Full package check

# The CI will automatically:
# 1. Run R-CMD-check on multiple platforms
# 2. Generate coverage reports
# 3. Update documentation if needed
# 4. Deploy website updates (on main branch)
```
