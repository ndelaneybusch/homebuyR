# Refinance Feature Design Document

## 1. Overview and Requirements

### Purpose
Implement a comprehensive refinance analysis feature that calculates the net benefit of refinancing a mortgage over time, accounting for closing costs, investment opportunities, tax implications, and optional lump sum paydowns.

### Core Functionality (Based on Legacy Python Analysis)
- **Benefit Curve Analysis**: Calculate month-by-month net benefit of refinancing vs. keeping existing loan
- **Breakeven Analysis**: Identify the time point where refinancing becomes profitable
- **Investment Integration**: Account for investing monthly payment savings at specified after-tax returns
- **Tax Considerations**: Include mortgage interest deduction limits and tax savings differential
- **Lump Sum Paydown**: Optional additional principal payment at refinance time
- **Interactive Visualization**: Plot benefit curve with breakeven highlighting and hover details
- **Summary Tables**: Present key metrics and scenarios in tabular format

### Key Parameters
- **Loan Details**: Current principal, old rate, new rate, original term, remaining months
- **Financial**: Closing costs, tax rate, mortgage interest deduction limit
- **Investment**: Annual investment return rate for payment savings
- **Optional**: Lump sum paydown amount

## 2. Function Architecture

### New Functions (fct_refinance.R)

#### Core Calculation Functions

```r
#' Calculate Refinance Benefit Over Time
#'
#' Computes the cumulative net benefit of refinancing vs. keeping existing loan
#' for each month of a specified holding period.
#'
#' @param principal Remaining principal on existing loan
#' @param rate_per_month_old Monthly interest rate of existing loan (as decimal)
#' @param rate_per_month_new Monthly interest rate of refinance loan (as decimal)
#' @param n_payments_remaining Months remaining on existing loan
#' @param closing_costs Total cost of refinancing
#' @param tax_rate Marginal tax rate for interest deduction (as decimal)
#' @param investment_return_annual After-tax annual return rate for investing savings (as decimal)
#' @param lump_sum_paydown Optional additional principal payment at refinance (default: 0)
#' @param mid_limit Mortgage interest deduction limit (default: 750000)
#' @param max_eval_months Maximum months to evaluate (default: n_payments_remaining)
#'
#' @return List containing:
#'   - months: Vector of evaluation months (1 to max_eval_months)
#'   - net_benefits: Vector of cumulative net benefits for each month
#'   - breakeven_month: First month where benefit > 0 (NA if none)
#'   - old_payment: Monthly payment on existing loan
#'   - new_payment: Monthly payment on refinance loan
#'   - monthly_savings: Difference in monthly payments
#'
calculate_refinance_benefit_curve <- function(principal,
                                            rate_per_month_old,
                                            rate_per_month_new,
                                            n_payments_remaining,
                                            closing_costs,
                                            tax_rate = 0.25,
                                            investment_return_annual = 0.0,
                                            lump_sum_paydown = 0,
                                            mid_limit = 750000,
                                            max_eval_months = n_payments_remaining)
```

```r
#' Calculate Future Value of Monthly Savings
#'
#' Computes the future value of monthly payment savings invested at a given rate.
#' Handles edge case of zero investment return.
#'
#' @param monthly_payment Monthly payment difference to invest
#' @param rate_per_month Monthly investment return rate (as decimal)
#' @param n_periods Number of months to compound
#'
#' @return Future value of invested savings
#'
calculate_invested_savings_fv <- function(monthly_payment,
                                        rate_per_month,
                                        n_periods)
```

```r
#' Calculate Mortgage Interest Deduction Differential
#'
#' Computes the tax savings difference between old and new loans,
#' accounting for mortgage interest deduction limits.
#'
#' @param old_interest_paid Cumulative interest paid on old loan
#' @param new_interest_paid Cumulative interest paid on new loan
#' @param old_balance Current balance of old loan (for deduction limit)
#' @param new_balance Current balance of new loan (for deduction limit)
#' @param tax_rate Marginal tax rate
#' @param mid_limit Mortgage interest deduction limit
#'
#' @return Tax savings differential (positive = new loan saves more)
#'
calculate_tax_savings_differential <- function(old_interest_paid,
                                             new_interest_paid,
                                             old_balance,
                                             new_balance,
                                             tax_rate,
                                             mid_limit)
```

### Leveraged Existing Functions
- `compute_monthly_payment()` from fct_annuity.R
- `compute_principal_remaining()` from fct_annuity.R
- `compute_interest_paid()` from fct_annuity.R
- `calculate_annuity_pv_factor()` from fct_annuity.R

### New Visualization Function (fct_visualizations.R or fct_refinance.R)

```r
#' Plot Refinance Benefit Curve
#'
#' Creates an interactive plot showing the cumulative net benefit of refinancing
#' over time, with breakeven point highlighting and hover details.
#'
#' @param refinance_data List returned by calculate_refinance_benefit_curve()
#' @param title Optional plot title
#' @param show_breakeven Whether to highlight breakeven point (default: TRUE)
#'
#' @return Interactive ggplot2 + ggiraph plot object
#'
plot_refinance_benefit <- function(refinance_data,
                                 title = "Refinance Benefit Over Time",
                                 show_breakeven = TRUE)
```

## 3. Mathematical Implementation

### Monthly Benefit Calculation
For each evaluation month k:

```
Net_Benefit_k = FV_Savings_k - Closing_Costs + Equity_Diff_k + Tax_Savings_Diff_k
```

Where:
- **FV_Savings_k**: Future value of k months of payment savings invested
- **Closing_Costs**: One-time refinancing costs
- **Equity_Diff_k**: Difference in remaining balances (old_balance - new_balance)
- **Tax_Savings_Diff_k**: Cumulative tax savings differential

### Investment Return Calculation
```r
# Monthly after-tax return rate
monthly_rate <- investment_return_annual / 12

# Future value of monthly savings (annuity formula)
if (monthly_rate == 0) {
  fv <- monthly_savings * months
} else {
  fv <- monthly_savings * ((1 + monthly_rate)^months - 1) / monthly_rate
}
```

### Tax Considerations
```r
# Deductible interest (capped by MID limit)
deductible_factor <- min(1, mid_limit / current_balance)
deductible_interest <- total_interest * deductible_factor
tax_savings <- deductible_interest * tax_rate
```

## 4. UI/UX Design

### Refinance Tab Layout (app_ui.R)

```r
# In tabPanel("Refinancing", ...)
div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 5px;",
  h3("Refinance Analysis"),
  p(em("Analyze the benefit of refinancing your current mortgage, including investment opportunities from payment savings.")),

  fluidRow(
    # Refinance-specific inputs (leverage existing values where possible)
    column(4,
      h4("Refinance Details"),

      # New interest rate (primary new input)
      shinyhelper::helper(
        numericInput("refi_new_rate_pct",
                    "New Interest Rate (%)",
                    value = 5.0,
                    min = 0,
                    step = 0.01,
                    max = 20),
        content = "Enter the interest rate offered for the refinanced loan.",
        type = "inline"
      ),

      # Closing costs
      shinyhelper::helper(
        autonumericInput("refi_closing_costs",
                        "Closing Costs ($)",
                        value = 3000,
                        currencySymbol = "$",
                        currencySymbolPlacement = "p",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        minimumValue = 0),
        content = "Total cost to refinance, including loan origination fees, appraisal, title insurance, etc.",
        type = "inline"
      ),

      # Optional lump sum paydown
      shinyhelper::helper(
        autonumericInput("refi_lump_sum_paydown",
                        "Optional Lump Sum Paydown ($)",
                        value = 0,
                        currencySymbol = "$",
                        currencySymbolPlacement = "p",
                        decimalCharacter = ".",
                        digitGroupSeparator = ",",
                        minimumValue = 0),
        content = "Additional principal payment made at time of refinance.",
        type = "inline"
      ),

      h4("Financial Assumptions"),

      # Tax rate
      shinyhelper::helper(
        numericInput("refi_tax_rate_pct",
                    "Marginal Tax Rate (%)",
                    value = 25.0,
                    min = 0,
                    max = 50,
                    step = 0.1),
        content = "Your marginal income tax rate for calculating mortgage interest deduction value.",
        type = "inline"
      ),

      # Investment return
      shinyhelper::helper(
        numericInput("refi_investment_return_pct",
                    "After-Tax Investment Return (%)",
                    value = 4.0,
                    min = 0,
                    max = 15,
                    step = 0.1),
        content = "Expected annual after-tax return if you invest monthly payment savings.",
        type = "inline"
      ),

      # MID limit (advanced)
      conditionalPanel(
        condition = "input.advanced_controls",
        shinyhelper::helper(
          autonumericInput("refi_mid_limit",
                          "Mortgage Interest Deduction Limit ($)",
                          value = 750000,
                          currencySymbol = "$",
                          currencySymbolPlacement = "p",
                          decimalCharacter = ".",
                          digitGroupSeparator = ",",
                          minimumValue = 0),
          content = "IRS limit on mortgage balance eligible for interest deduction (current limit is $750,000).",
          type = "inline"
        )
      ),

      br(),
      actionButton("apply_refinance_analysis", "Calculate Refinance Benefit",
                  class = "btn-primary")
    ),

    column(8,
      # Current loan summary (leveraging existing app values)
      div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        h5("Current Loan Summary"),
        uiOutput("current_loan_summary")
      ),

      # Results tabs
      tabsetPanel(
        tabPanel("Benefit Curve",
          br(),
          ggiraph::girafeOutput("refinance_benefit_plot", height = "500px")
        ),
        tabPanel("Summary Table",
          br(),
          DT::dataTableOutput("refinance_summary_table")
        )
      )
    )
  )
)
```

### Help Integration
- Tooltips for complex concepts (MID limit, after-tax returns)
- Input validation messages
- Calculation status indicators

## 5. Server Logic (app_server.R)

### Reactive Values
```r
# Current loan details (computed from existing app values)
current_loan_details <- reactive({
  req(input$loan_amount,
      monthly_rate_dec(),
      input$mortgage_term,
      input$loan_start)

  # Calculate remaining months from loan start date
  loan_start_month <- as.Date(cut(input$loan_start, "month"))
  current_month <- as.Date(cut(Sys.Date(), "month"))
  months_elapsed <- as.integer(
    (lubridate::year(current_month) - lubridate::year(loan_start_month)) * 12 +
    (lubridate::month(current_month) - lubridate::month(loan_start_month))
  )
  months_elapsed <- max(0, months_elapsed) # Ensure non-negative
  n_payments_remaining <- max(0, as.numeric(input$mortgage_term) - months_elapsed)

  # Calculate current principal balance
  monthly_payment <- compute_monthly_payment(
    principal = input$loan_amount,
    rate_per_month = monthly_rate_dec(),
    n_payments_total = as.numeric(input$mortgage_term)
  )

  current_principal <- compute_principal_remaining(
    monthly_payment = monthly_payment,
    rate_per_month = monthly_rate_dec(),
    n_payments_remaining = n_payments_remaining
  )

  list(
    principal = current_principal,
    monthly_payment = monthly_payment,
    n_payments_remaining = n_payments_remaining,
    rate_per_month = monthly_rate_dec(),
    original_amount = input$loan_amount
  )
})

# Current loan summary output
output$current_loan_summary <- renderUI({
  req(current_loan_details())

  details <- current_loan_details()

  tagList(
    p(strong("Original Loan Amount:"), scales::dollar(details$original_amount)),
    p(strong("Current Principal Balance:"), scales::dollar(details$principal)),
    p(strong("Current Interest Rate:"), paste0(input$annual_rate_pct, "%")),
    p(strong("Monthly Payment:"), scales::dollar(details$monthly_payment)),
    p(strong("Payments Remaining:"), details$n_payments_remaining, "months")
  )
})

# Refinance calculations (leveraging computed values)
refinance_data <- eventReactive(input$apply_refinance_analysis, {
  req(current_loan_details())

  current_loan <- current_loan_details()

  validate(
    need(current_loan$principal > 0, "Current principal balance must be positive"),
    need(input$refi_new_rate_pct > 0, "New interest rate must be positive"),
    need(current_loan$n_payments_remaining > 0, "Must have remaining payments"),
    need(input$refi_closing_costs >= 0, "Closing costs cannot be negative")
  )

  # Get MID limit (use advanced control if available, otherwise default)
  mid_limit <- if(!is.null(input$refi_mid_limit)) input$refi_mid_limit else 750000

  calculate_refinance_benefit_curve(
    principal = current_loan$principal,
    rate_per_month_old = current_loan$rate_per_month,
    rate_per_month_new = input$refi_new_rate_pct / 100 / 12,
    n_payments_remaining = current_loan$n_payments_remaining,
    closing_costs = input$refi_closing_costs,
    tax_rate = input$refi_tax_rate_pct / 100,
    investment_return_annual = input$refi_investment_return_pct / 100,
    lump_sum_paydown = input$refi_lump_sum_paydown,
    mid_limit = mid_limit
  )
})
```

### Plot Output
```r
output$refinance_benefit_plot <- ggiraph::renderGirafe({
  req(refinance_data())

  plot_obj <- plot_refinance_benefit(refinance_data())

  ggiraph::girafe(
    ggobj = plot_obj,
    options = list(
      ggiraph::opts_hover_inv(css = "opacity:0.3;"),
      ggiraph::opts_hover(css = "stroke-width:2;"),
      ggiraph::opts_selection(type = "none"),
      ggiraph::opts_toolbar(saveaspng = FALSE)
    )
  )
})
```

### Summary Table
```r
output$refinance_summary_table <- DT::renderDataTable({
  req(refinance_data())

  data <- refinance_data()

  # Create summary table with key metrics
  summary_data <- tibble::tibble(
    Metric = c("Old Monthly Payment",
               "New Monthly Payment",
               "Monthly Payment Savings",
               "Breakeven Point (Months)",
               "Net Benefit at Breakeven",
               "Net Benefit at End of Term",
               "Closing Costs",
               "Lump Sum Paydown"),
    Value = c(scales::dollar(data$old_payment),
              scales::dollar(data$new_payment),
              scales::dollar(data$monthly_savings),
              ifelse(is.na(data$breakeven_month), "Never",
                    paste(data$breakeven_month, "months")),
              ifelse(is.na(data$breakeven_month), scales::dollar(0),
                    scales::dollar(data$net_benefits[data$breakeven_month])),
              scales::dollar(tail(data$net_benefits, 1)),
              scales::dollar(input$refi_closing_costs %||% 0),
              scales::dollar(input$refi_lump_sum_paydown %||% 0))
  )

  DT::datatable(summary_data,
                options = list(dom = 't', pageLength = -1),
                rownames = FALSE) %>%
    DT::formatStyle("Metric", fontWeight = "bold")
})
```

## 6. Integration Points

### Key Integration Advantages
This design leverages existing app architecture in several important ways:

**Computed Values Reused:**
- **Current Principal Balance**: Calculated from `input$loan_amount`, `monthly_rate_dec()`, `input$mortgage_term`, and `input$loan_start` using existing functions
- **Current Interest Rate**: Direct use of `input$annual_rate_pct` and `monthly_rate_dec()`
- **Remaining Payments**: Computed from loan start date vs. current date
- **Monthly Payment**: Calculated using `compute_monthly_payment()` from existing loan details

**UI Consistency:**
- Uses same `autonumericInput` patterns with currency formatting
- Leverages `shinyhelper` for consistent tooltips
- Follows existing `conditionalPanel` patterns for advanced controls
- Uses same `ggiraph` integration for interactive plots
- Maintains `actionButton` styling and placement patterns

**Server Patterns:**
- Follows `eventReactive()` pattern for calculation triggers
- Uses consistent `validate()` and `req()` patterns
- Leverages existing `monthly_rate_dec()` reactive
- Maintains pattern of showing current state before analysis

### Dependencies
- Leverage `fct_annuity.R` functions for payment calculations
- Use `fct_visualizations.R` patterns for plot styling
- Follow existing validation and error handling patterns
- Maintain consistency with existing reactive patterns

### Testing Strategy
- Unit tests for mathematical functions in `test-fct_refinance.R`
- Edge case testing (zero rates, negative values, extreme scenarios)
- Integration tests for UI reactive behavior
- Mathematical validation against known scenarios

### Documentation
- Comprehensive roxygen2 documentation
- Working examples in all function docs
- Vignette section on refinance analysis
- Integration with existing pkgdown site

## 7. Implementation Roadmap

### Phase 1: Core Functions
1. Implement `calculate_refinance_benefit_curve()` with full mathematical logic
2. Add helper functions for FV calculations and tax differentials
3. Create comprehensive unit tests
4. Document all functions with examples

### Phase 2: Visualization
1. Implement `plot_refinance_benefit()` with interactive features
2. Add breakeven highlighting and hover tooltips
3. Ensure consistent styling with existing plots
4. Test across different data scenarios

### Phase 3: UI Integration
1. Add refinance tab to main UI
2. Implement all input controls with validation
3. Create server logic with reactive calculations
4. Add summary table with key metrics

### Phase 4: Polish & Testing
1. Integration testing of full workflow
2. Performance optimization for large datasets
3. Enhanced error handling and user feedback
4. Documentation and help system updates

## 8. Technical Considerations

### Performance
- Cache expensive calculations in reactive values
- Limit maximum evaluation periods for responsiveness
- Optimize plot rendering for large datasets

### Robustness
- Handle edge cases (zero rates, extreme values)
- Graceful degradation for calculation failures
- Clear error messages for invalid inputs

### Extensibility
- Design functions to accommodate future enhancements
- Modular architecture for easy testing and maintenance
- Consistent API with existing library functions