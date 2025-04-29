#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyhelper
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session) {
  # Required for shinyhelper
  shinyhelper::observe_helpers()

  # --- Reactive Values and Intermediate Calculations ---

  # Calculate monthly decimal rate from annual percentage rate input
  monthly_rate_dec <- reactive({
    req(input$annual_rate_pct) # Ensure the input is available
    # Validate that the rate is non-negative
    validate(
      need(input$annual_rate_pct >= 0, "Annual rate cannot be negative.")
    )
    # Convert annual percentage rate to monthly decimal rate
    input$annual_rate_pct / 100 / 12
  })

  # Calculate combined monthly non-mortgage costs (excluding property tax)
  monthly_non_mortgage_costs_excl_tax <- reactive({
    req(input$home_insurance_annual,
       input$other_insurance_annual,
       input$hoa_dues_monthly)

    # Treat NA other insurance and HOA dues as 0
    other_ins_annual <- ifelse(is.na(input$other_insurance_annual), 0, input$other_insurance_annual)
    hoa_dues_monthly <- ifelse(is.na(input$hoa_dues_monthly), 0, input$hoa_dues_monthly)

    monthly_home_insurance <- input$home_insurance_annual / 12
    monthly_other_insurance <- other_ins_annual / 12

    monthly_home_insurance + monthly_other_insurance + hoa_dues_monthly
  })

  # Estimate monthly property tax based on affordability
  estimated_monthly_tax <- reactive({
    req(input$monthly_housing_budget,
        input$prop_tax_rate_annual,
        input$mortgage_term) # Need mortgage term for calculation
    req(monthly_rate_dec()) # Need calculated monthly rate
    req(monthly_non_mortgage_costs_excl_tax()) # Need other costs

    # Validate tax rate is non-negative
    validate(
        need(input$prop_tax_rate_annual >= 0, "Annual tax rate cannot be negative.")
    )

    estimate_monthly_property_tax(
      monthly_housing_budget = input$monthly_housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage_costs_excl_tax(),
      rate_per_month = monthly_rate_dec(),
      n_payments_total = as.numeric(input$mortgage_term), # Convert term from character to numeric
      prop_tax_rate_annual = input$prop_tax_rate_annual,
      down_payment_pct = 20 # Assuming 20% down as requested
    )
  })

  # Your application server logic

  # --- Budgeting Tab --- #

  # Update default values for budget model inputs
  observeEvent(input$budget_model, {
    if (input$budget_model == "Income %") {
      updateNumericInput(session = session,
                         inputId = "housing_percent",
                         value = 28)
    } else if (input$budget_model == "Debt to Income Ratio") {
      updateNumericInput(session = session,
                         inputId = "housing_percent",
                         value = 34)
    }
  })

  # Monthly Housing Budget Calculation (if Apply button is used)
  observeEvent(input$apply_budget_calc, {
    req(input$budget_model)
    model <- input$budget_model
    budget <- NA

    # Helper to get monthly income from interval and amount
    get_monthly_income <- function(amount, interval) {
      switch(interval,
        "Weekly" = amount * 52 / 12,
        "Two Weeks" = amount * 26 / 12,
        "Twice Monthly" = amount * 2,
        "Monthly" = amount,
        "Annual" = amount / 12,
        NA)
    }

    if (model == "Income %") {
      req(input$income_interval, input$income_amount, input$housing_percent)
      gross_monthly_income <- get_monthly_income(input$income_amount, input$income_interval)
      budget <- housing_budget_from_gross_pct(gross_monthly_income, input$housing_percent)
    } else if (model == "Debt to Income Ratio") {
      req(input$income_interval, input$take_home_income_amount, input$housing_percent, input$max_total_debt_pct, input$other_debts)
      net_monthly_income <- get_monthly_income(input$take_home_income_amount, input$income_interval)
      # Convert percent to fraction for housing_percent and max_total_debt_pct
      max_housing_pct <- input$housing_percent / 100
      max_total_debt_pct <- input$max_total_debt_pct
      budget <- housing_budget_from_dpi(net_monthly_income, max_housing_pct, max_total_debt_pct, input$other_debts)
    } else if (model == "Financial Stress Resilience") {
      req(input$income_interval, input$income_amount, input$other_debts, input$non_housing_essentials, input$savings, input$income_shock_pct, input$shock_duration_months, input$max_total_dti_stress)
      gross_monthly_income <- get_monthly_income(input$income_amount, input$income_interval)
      rate_per_month <- if (!is.null(input$annual_rate_pct)) input$annual_rate_pct / 100 / 12 else 0
      n_payments_total <- if (!is.null(input$mortgage_term)) as.numeric(input$mortgage_term) else 360
      budget <- housing_budget_from_stressed_dti(
        gross_monthly_income = gross_monthly_income,
        other_debts = input$other_debts,
        non_housing_essentials = input$non_housing_essentials,
        rate_per_month = rate_per_month,
        n_payments_total = n_payments_total,
        savings = input$savings,
        income_shock_pct = input$income_shock_pct,
        shock_duration_months = input$shock_duration_months,
        max_total_dti_stress = input$max_total_dti_stress
      )
    }
    # Manual model: do nothing (user enters budget directly)

    if (!is.na(budget)) {
      shinyWidgets::updateAutonumericInput(session = session,
                                           inputId = "monthly_housing_budget",
                                           value = budget)
    }
  })

  # Monthly Mortgage Budget Calculation
  monthly_mortgage_budget <- reactive({
    req(input$monthly_housing_budget,
       estimated_monthly_tax(),
       monthly_non_mortgage_costs_excl_tax())

    # Calculate final mortgage budget
    input$monthly_housing_budget - monthly_non_mortgage_costs_excl_tax() - estimated_monthly_tax()
  })

  # Monthly Mortgage Budget Calculation Table
  output$mortgage_budget_table <- renderUI({
    # Use the reactive results which already have req() inside
    req(estimated_monthly_tax(),
        monthly_non_mortgage_costs_excl_tax(),
        monthly_mortgage_budget(),
        input$other_insurance_annual,
        input$hoa_dues_monthly,
        input$monthly_housing_budget,
        input$home_insurance_annual) # Home insurance used directly

    # Other insurance and HOA already handled in monthly_non_mortgage_costs_excl_tax
    # but needed individually for the table breakdown:
    monthly_other_insurance <- ifelse(is.na(input$other_insurance_annual), 0, input$other_insurance_annual) / 12
    monthly_hoa_dues <- ifelse(is.na(input$hoa_dues_monthly), 0, input$hoa_dues_monthly)

    # Create data frame for the table
    df <- data.frame(
      Item = c("Monthly Housing Budget",
               "Less: Home Insurance (Monthly)",
               "Less: Other Insurance (Monthly)",
               "Less: Estimated Property Tax (Monthly, assumes 20% down)", # Updated label
               "Less: HOA Dues (Monthly)",
               "Equals: Monthly Mortgage Budget (P&I)"),
      Amount = scales::dollar(c(input$monthly_housing_budget,
                  -input$home_insurance_annual / 12,
                  -monthly_other_insurance,
                  -estimated_monthly_tax(), # Use estimated tax
                  -monthly_hoa_dues,
                  monthly_mortgage_budget()),
                  # Ensure negative numbers are displayed correctly with parentheses
                  style_negative = "parens")
    )

    # Use kable and kableExtra for styling
    shiny::HTML(kable(df, format = "html", escape = FALSE, align = 'lr', table.attr = "style='width:auto;'") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"),
                    full_width = FALSE) %>%
      row_spec(nrow(df), bold = TRUE, background = "#D6EAF8")) # Style the last row

  })

  # Affordable Mortgage Principal Calculation
  affordable_mortgage <- reactive({
    # Require inputs needed for the summary sentence
    req(input$monthly_housing_budget,
        monthly_non_mortgage_costs_excl_tax(),
        monthly_rate_dec(),
        input$mortgage_term,
        input$annual_rate_pct,
        input$prop_tax_rate_annual)

    # Get reactive values/inputs
    monthly_non_mortgage <- monthly_non_mortgage_costs_excl_tax()
    rate_monthly <- monthly_rate_dec()
    term_months <- as.numeric(input$mortgage_term)
    tax_rate_annual <- input$prop_tax_rate_annual
    housing_budget <- input$monthly_housing_budget
    annual_rate <- input$annual_rate_pct

    # Calculate Affordable Principal
    affordable_principal <- compute_affordable_principal(
      monthly_housing_budget = housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage,
      rate_per_month = rate_monthly,
      n_payments_total = term_months,
      prop_tax_rate_annual = tax_rate_annual,
      down_payment_pct = 20 # Assuming 20% down
    )

    affordable_principal
  })

  output$affordable_mortgage_summary_text <- renderUI({
    req(input$monthly_housing_budget,
        affordable_mortgage(),
        input$mortgage_term,
        input$annual_rate_pct)

    # Get reactive values/inputs
    term_months <- as.numeric(input$mortgage_term)
    affordable_mortgage <- affordable_mortgage()

    affordable_principal_fmt <- scales::dollar(0)

    if (affordable_mortgage > 0) {
      affordable_principal_fmt <- scales::dollar(affordable_mortgage)
    } else {
      # If cannot afford anything, set descriptive text
      affordable_principal_fmt <- "$0 (cannot afford)"
    }

    # Format mortgage term for display
    term_years_map <- c("60"="5 year", "120"="10 year", "180"="15 year", "240"="20 year", "360"="30 year")
    term_display <- term_years_map[as.character(term_months)]
    if (is.na(term_display)) term_display <- paste(term_months, "months") # Fallback

    # Construct the sentence
    summary_sentence <- sprintf(
      "Given a %.2f%% rate for a %s term and an estimated monthly mortgage budget of %s (from a housing budget of %s), assuming no PMI (i.e. at least 20%% down) results in an estimated affordable mortgage of %s.",
      input$annual_rate_pct,
      term_display,
      scales::dollar(max(0, monthly_mortgage_budget())),
      scales::dollar(max(0, input$monthly_housing_budget)),
      affordable_principal_fmt
    )

    tags$p(tags$i(summary_sentence), style="color: #555;") # Display in italics and grey
    
  })

  # Monthly Housing Budget Calculation (if Apply button is used)
  observeEvent(input$apply_mortgage_calc_assume_20_pct_down, {
    req(monthly_mortgage_budget(),
       monthly_rate_dec(),
       input$mortgage_term)

    # Update the monthly_housing_budget input
    loan_amount <- compute_principal(monthly_mortgage_budget(), monthly_rate_dec(), as.numeric(input$mortgage_term))
    shinyWidgets::updateAutonumericInput(session = session,
                                        inputId = "loan_amount",
                                        value = loan_amount)
  })

  # --- 3) Money Down Calculation ---
  observeEvent(input$calc_20_pct_down, {
    # Required inputs for calculation
    req(input$monthly_housing_budget,
        monthly_non_mortgage_costs_excl_tax(),
        monthly_rate_dec(),
        input$mortgage_term,
        input$prop_tax_rate_annual) # Need tax rate for principal calculation

    # Get reactive values
    monthly_non_mortgage <- monthly_non_mortgage_costs_excl_tax()
    rate_monthly <- monthly_rate_dec()
    term_months <- as.numeric(input$mortgage_term)
    tax_rate_annual <- input$prop_tax_rate_annual
    housing_budget <- input$monthly_housing_budget

    # Calculate affordable principal assuming 20% down
    principal_at_20_pct_down <- compute_affordable_principal(
      monthly_housing_budget = housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage,
      rate_per_month = rate_monthly,
      n_payments_total = term_months,
      prop_tax_rate_annual = tax_rate_annual,
      down_payment_pct = 20 # Use 20% for this calculation
    )

    down_payment_20_pct <- 0 # Default if cannot afford
    if (principal_at_20_pct_down > 0) {
        # Calculate home price based on principal being 80% of it
        home_price_at_20_pct_down <- principal_at_20_pct_down / 0.80
        # Calculate 20% of that home price
        down_payment_20_pct <- home_price_at_20_pct_down * 0.20
    }

    # Update the down payment dollar input
    shinyWidgets::updateAutonumericInput(
        session = session,
        inputId = "down_payment_dollars",
        value = down_payment_20_pct
    )
  })

  # --- Affordability Visualization Plots --- #

  # Reactive to calculate down payment details needed for plots
  down_payment_details_for_plots <- reactive({
    req(input$monthly_housing_budget,
        monthly_non_mortgage_costs_excl_tax(),
        monthly_rate_dec(),
        input$mortgage_term,
        input$prop_tax_rate_annual,
        input$pmi_rate_annual,       # Need PMI inputs now
        input$pmi_threshold_pct)

    housing_budget <- input$monthly_housing_budget
    monthly_non_mortgage <- monthly_non_mortgage_costs_excl_tax()
    rate_monthly <- monthly_rate_dec()
    term_months <- as.numeric(input$mortgage_term)
    tax_rate_annual <- input$prop_tax_rate_annual
    pmi_rate <- input$pmi_rate_annual
    pmi_thresh <- input$pmi_threshold_pct

    dp_dollars_input <- input$down_payment_dollars
    dp_dollars_to_use <- NA
    center_dp_pct <- 20 # Default if input is empty

    # Check if down payment input is provided and positive
    if (!is.null(dp_dollars_input) && !is.na(dp_dollars_input) && dp_dollars_input > 0) {
        dp_dollars_to_use <- dp_dollars_input

        # Estimate current affordable principal/price to get the center % for the first plot
        current_principal <- compute_principal_with_pmi(
                                monthly_housing_budget = housing_budget,
                                monthly_non_mortgage_costs = monthly_non_mortgage,
                                rate_per_month = rate_monthly,
                                n_payments_total = term_months,
                                prop_tax_rate_annual = tax_rate_annual,
                                down_payment_dollars = dp_dollars_to_use,
                                pmi_rate_annual = pmi_rate,
                                pmi_threshold_pct = pmi_thresh
                              )

        if(current_principal > 0) {
          current_home_price <- current_principal + dp_dollars_to_use
          if (current_home_price > 0) { # Avoid division by zero
            center_dp_pct <- (dp_dollars_to_use / current_home_price) * 100
          }
        }
        # If principal/price is 0, center_dp_pct remains the default (20), which might be ok

    } else {
      # If input is empty/zero, calculate the 20% down value
       principal_at_20_pct_down <- compute_affordable_principal(
          monthly_housing_budget = housing_budget,
          monthly_non_mortgage_costs = monthly_non_mortgage,
          rate_per_month = rate_monthly,
          n_payments_total = term_months,
          prop_tax_rate_annual = tax_rate_annual,
          down_payment_pct = 20
       )
       if (principal_at_20_pct_down > 0) {
          home_price_at_20_pct_down <- principal_at_20_pct_down / 0.80
          dp_dollars_to_use <- home_price_at_20_pct_down * 0.20
       } else {
          dp_dollars_to_use <- 0 # Can't afford anything
       }
       center_dp_pct <- 20 # Use 20% as the center explicitly
    }

    # Define range for the dp plot based on center_dp_pct
    dp_range_low <- max(0, center_dp_pct - 20)
    dp_range_high <- min(100, center_dp_pct + 20)
    # Ensure range has distinct values, default to 0-40 if calculation failed
    if (dp_range_low >= dp_range_high) {
        dp_range_low <- 0
        dp_range_high <- 40
    }
    dp_pct_range_for_plot <- seq(dp_range_low, dp_range_high, by = 0.5)

    # Return list of calculated values
    list(
      dp_dollars = dp_dollars_to_use,
      center_pct = center_dp_pct, # Might be useful for debugging
      dp_pct_range = dp_pct_range_for_plot
    )
  })

  # 4a) Affordable Price vs. Down Payment Plot
  output$price_vs_dp_plot <- ggiraph::renderGirafe({
    # Require essential inputs for calculation
    req(input$monthly_housing_budget,
        monthly_non_mortgage_costs_excl_tax(),
        input$annual_rate_pct,
        input$mortgage_term,
        input$prop_tax_rate_annual,
        input$pmi_rate_annual,
        input$pmi_threshold_pct,
        down_payment_details_for_plots()) # Need the calculated range

    # Get calculated down payment range
    dp_details <- down_payment_details_for_plots()

    # Call the plotting function
    plot_price_vs_down_payment(
      monthly_housing_budget = input$monthly_housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage_costs_excl_tax(),
      annual_rate_pct = input$annual_rate_pct,
      mortgage_term_months = as.numeric(input$mortgage_term),
      prop_tax_rate_annual = input$prop_tax_rate_annual,
      pmi_rate_annual = input$pmi_rate_annual,
      pmi_threshold_pct = input$pmi_threshold_pct,
      dp_pct_range = dp_details$dp_pct_range
    )
  })

  # 4b) Affordable Price vs. Rate Plot
  output$price_vs_rate_plot <- ggiraph::renderGirafe({
    # Require essential inputs for calculation
    req(input$monthly_housing_budget,
        monthly_non_mortgage_costs_excl_tax(),
        input$mortgage_term,
        input$prop_tax_rate_annual,
        input$pmi_rate_annual,
        input$pmi_threshold_pct,
        down_payment_details_for_plots()) # Need the calculated dollar amount

    # Get calculated down payment dollar amount
    dp_details <- down_payment_details_for_plots()
    req(!is.na(dp_details$dp_dollars)) # Ensure the dollar amount calculation succeeded

    # Define rate range (e.g., +/- 2-3% from current rate, or use default)
    current_rate <- req(input$annual_rate_pct)
    rate_range <- seq(max(0.1, current_rate - 3), current_rate + 3, by = 0.1)

    # Call the plotting function
    plot_price_vs_rate(
      monthly_housing_budget = input$monthly_housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage_costs_excl_tax(),
      mortgage_term_months = as.numeric(input$mortgage_term),
      prop_tax_rate_annual = input$prop_tax_rate_annual,
      pmi_rate_annual = input$pmi_rate_annual,
      pmi_threshold_pct = input$pmi_threshold_pct,
      down_payment_input = list(dollars = dp_details$dp_dollars),
      rate_pct_range = rate_range
    )
  })

}
