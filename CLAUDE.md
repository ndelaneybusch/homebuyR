# CLAUDE.md - homebuyR

## Project Overview

**homebuyR** is an R library providing comprehensive tools for home financing analysis, built as a Shiny web application using the Golem framework. The package helps users make informed decisions about home purchases by providing budgeting tools, mortgage calculators, affordability analysis, and payment optimization features.

## Architecture & Framework

### Golem Framework
- **Structure**: Standard Golem application structure with `R/app_ui.R`, `R/app_server.R`, and `R/run_app.R`
- **Configuration**: Uses `inst/golem-config.yml` for environment-specific settings

### Key Dependencies & Environment Management

**Dependency Management**: Uses `renv` (R Environment) for reproducible package management
- **renv.lock**: Lockfile capturing exact package versions and dependencies (e.g., R 4.5.0, renv 1.1.4)
- **.Rprofile**: Automatically activates renv environment with `source("renv/activate.R")`
- **renv/settings.json**: Configuration for dependency tracking (Imports, Depends, LinkingTo fields)

**Core Dependencies**:
- **Core Shiny**: `shiny`, `shinyWidgets`, `shinyhelper` for UI components
- **Data Manipulation**: `dplyr`, `purrr`, `tidyr`, `tibble` for data processing
- **Visualization**: `ggplot2`, `ggiraph` for interactive plots
- **Tables**: `DT`, `kableExtra` for data presentation
- **Utilities**: `scales`, `lubridate`, `magrittr` for formatting and dates

## Code Organization & Conventions

### File Naming Patterns
- **Functions**: All business logic functions are prefixed with `fct_*`
  - `fct_annuity.R`: Core mortgage payment calculations
  - `fct_budgeting.R`: Budget calculation functions
  - `fct_mortgages.R`: Mortgage affordability and PMI calculations
  - `fct_refinance.R`: Refinance benefit analysis with investment and tax considerations
  - `fct_visualizations.R`: Interactive plotting functions
- **App Structure**: Standard Golem naming (`app_ui.R`, `app_server.R`, `run_app.R`)
- **Tests**: Mirror function file structure (`test-fct_*.R`)

### Code Style & Patterns
- **Documentation**: Comprehensive roxygen2 documentation with examples
- **Validation**: Extensive input validation using `stopifnot()` in all functions
- **Error Handling**: Defensive programming with validation for edge cases (zero rates, negative values)
- **Examples**: All exported functions include working examples in documentation
- **Naming**: Snake_case for functions, descriptive parameter names

## Core Function Categories

### 1. Annuity Calculations (`fct_annuity.R`)
**Purpose**: Fundamental mortgage payment mathematics
- `calculate_annuity_pv_factor()`: Present value factor calculations
- `compute_monthly_payment()`: Standard mortgage payment calculation
- `compute_principal()`: Reverse calculation from payment to principal
- `compute_principal_remaining()`: Outstanding balance calculations
- `compute_principal_paid()`, `compute_interest_paid()`: Payment breakdowns

### 2. Budgeting (`fct_budgeting.R`)
**Purpose**: Determine affordable housing budgets using different methodologies
- `housing_budget_from_gross_pct()`: Simple percentage of gross income
- `housing_budget_from_dpi()`: Debt-to-income ratio approach
- `housing_budget_from_stressed_dti()`: Stress-tested affordability with financial shock scenarios

### 3. Mortgage Calculations (`fct_mortgages.R`)
**Purpose**: Advanced affordability calculations including taxes and PMI
- `compute_affordable_principal()`: Core affordability with optional property tax integration
- `estimate_monthly_property_tax()`: Property tax estimation based on affordable home price
- `compute_principal_with_pmi()`: PMI-aware affordability calculations
- `calculate_mortgage_savings()`: Extra payment analysis with detailed amortization

### 4. Refinance Analysis (`fct_refinance.R`)
**Purpose**: Comprehensive refinance benefit analysis with investment and tax considerations
- `calculate_refinance_benefit_curve()`: Month-by-month refinance benefit calculation including investment returns, tax savings, and equity tracking
- `calculate_invested_savings_fv()`: Future value calculations for invested payment savings with zero-rate handling
- `calculate_tax_savings_differential()`: Mortgage interest deduction differential accounting for IRS limits

### 5. Visualizations (`fct_visualizations.R`)
**Purpose**: Interactive plots for exploring mortgage scenarios
- `plot_price_vs_down_payment()`: Affordability vs. down payment percentage
- `plot_price_vs_rate()`: Affordability vs. interest rate with PMI zones
- `plot_principal_interest()`: Loan paydown comparison over time
- `plot_refinance_benefit()`: Interactive refinance benefit curve with dual-line visualization for cash vs. wealth benefits

## UI Design & Layout

### Structure
- **Layout**: `sidebarLayout` with consistent input controls in sidebar
- **Tabs**: Main content organized in `tabsetPanel` with distinct functional areas:
  - "Readme": Documentation (placeholder)
  - "Budgeting": Multi-step budget calculation workflow
  - "Paying": Extra payment analysis and visualizations
  - "Refinancing": (placeholder for future features)

### Input Controls
- **Consistent Styling**: Uses `shinyWidgets::autonumericInput` for currency inputs with proper formatting
- **Help Integration**: Extensive use of `shinyhelper` for contextual tooltips
- **Validation**: Client-side input validation and server-side reactive validation

## Server Logic & Reactive Patterns

### Reactive Value Management
- **Calculated Values**: Extensive use of `reactive()` for dependent calculations
- **Input Processing**: Helper functions for income interval conversions
- **Validation**: `validate()` and `req()` for robust reactive dependencies

### Key Reactive Elements
- `monthly_rate_dec()`: Converts annual percentage to monthly decimal rate
- `monthly_non_mortgage_costs_excl_tax()`: Aggregates non-mortgage housing costs
- `estimated_monthly_tax()`: Property tax estimation
- `affordable_mortgage()`: Core affordability calculation
- `mortgage_savings()`: Extra payment analysis

### Event Handling
- **Apply Buttons**: `observeEvent()` for user-triggered calculations
- **Auto-calculation**: Reactive updates for visualization plots
- **Input Updates**: Programmatic updates using `updateAutonumericInput()` and similar

## Testing Strategy

### Test Coverage
- **Unit Tests**: All `fct_*` files have corresponding test files
- **Mathematical Validation**: Tests verify calculations against known examples
- **Edge Cases**: Testing zero rates, boundary conditions, and invalid inputs

### Test Patterns
- Uses `testthat` framework
- Tolerance-based assertions for floating-point calculations
- Example-driven tests matching function documentation

## Implementation Patterns

### Mathematical Precision
- **Zero Rate Handling**: Special cases for 0% interest rates throughout
- **Floating Point Safety**: Defensive programming against numerical precision issues
- **Formula Documentation**: Mathematical formulas clearly documented in code

### User Experience
- **Progressive Disclosure**: Advanced features hidden by default
- **Contextual Help**: Inline tooltips explain calculations and assumptions
- **Immediate Feedback**: Responsive UI updates and validation messages

### Data Flow
1. **Budget Input**: User specifies housing budget (manual or calculated)
2. **Cost Breakdown**: Non-mortgage costs and tax estimation
3. **Affordability Calculation**: Principal calculation with PMI consideration
4. **Visualization**: Interactive exploration of scenarios
5. **Optimization**: Extra payment analysis and savings calculations

## Development & Deployment

### Package Structure
- **Namespace**: Proper NAMESPACE management with roxygen2
- **Dependencies**: Well-defined DESCRIPTION with appropriate version constraints
- **Documentation**: Man pages generated from roxygen2 comments

### CI/CD Pipeline (GitHub Actions)

**Automated Testing & Quality Assurance**:
- **R-CMD-check**: Multi-platform testing (macOS, Windows, Ubuntu) across R versions (devel, release, oldrel-1)
  - Runs on push to main/master branches and all pull requests
  - Uses `r-lib/actions` for standardized R package checking
  - Includes snapshot testing and compact vignette building
- **Test Coverage**: Automated code coverage analysis using `covr` package
  - Integrates with Codecov for coverage reporting and tracking
  - Uploads test artifacts on failure for debugging
  - Runs on Ubuntu with comprehensive coverage metrics

**Documentation & Deployment**:
- **Auto-documentation**: Triggers on changes to `R/` files to update roxygen2 documentation
  - Automatically commits and pushes updated `man/`, `NAMESPACE`, and `DESCRIPTION` files
  - Maintains documentation sync with code changes
- **pkgdown Website**: Automated website generation and deployment
  - Builds on push/PR to main branches, releases, and manual dispatch
  - Deploys to GitHub Pages using `gh-pages` branch
  - Includes full package documentation, function references, and vignettes

**Workflow Features**:
- **Multi-trigger Support**: Responds to pushes, PRs, releases, and manual workflows
- **Artifact Management**: Preserves test outputs and build artifacts for debugging
- **Security**: Uses GitHub secrets for tokens and follows minimal permission principles
- **Concurrency Control**: Prevents conflicting deployments with proper job grouping
