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
    # Treat NA other insurance and HOA dues as 0
    other_ins_annual <- ifelse(is.na(input$other_insurance_annual), 0, input$other_insurance_annual)
    hoa_dues_monthly <- ifelse(is.na(input$hoa_dues_monthly), 0, input$hoa_dues_monthly)

    req(input$home_insurance_annual) # Home insurance is required
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

  # 1) Monthly Housing Budget Calculation (if Apply button is used)
  observeEvent(input$apply_budget_calc, {
    # Ensure necessary inputs are available
    req(input$income_interval, input$income_amount, input$housing_percent)

    # Validate inputs
    validate(
      need(input$income_amount >= 0, "Income amount cannot be negative."),
      need(input$housing_percent >= 0 & input$housing_percent <= 100, "Housing budget percentage must be between 0 and 100.")
    )

    # Calculate gross monthly income based on interval
    gross_monthly_income <- switch(input$income_interval,
                                   "Weekly" = input$income_amount * 52 / 12,
                                   "Two Weeks" = input$income_amount * 26 / 12,
                                   "Twice Monthly" = input$income_amount * 2,
                                   "Monthly" = input$income_amount,
                                   "Annual" = input$income_amount / 12)

    # Calculate monthly housing budget
    calculated_budget <- gross_monthly_income * (input$housing_percent / 100)

    # Update the monthly_housing_budget input field
    shinyWidgets::updateAutonumericInput(session = session,
                                         inputId = "monthly_housing_budget",
                                         value = calculated_budget)
  })

  # 2) Monthly Mortgage Budget Calculation Table
  output$mortgage_budget_table <- renderUI({
    # Req inputs to ensure they are not NULL/NA before calculating
    # Use the reactive results which already have req() inside
    req(estimated_monthly_tax(),
        monthly_non_mortgage_costs_excl_tax(),
        input$monthly_housing_budget,
        input$home_insurance_annual) # Home insurance used directly

    # Get values from reactives or inputs
    monthly_housing_budget <- input$monthly_housing_budget
    monthly_home_insurance <- input$home_insurance_annual / 12
    # Other insurance and HOA already handled in monthly_non_mortgage_costs_excl_tax
    # but needed individually for the table breakdown:
    other_ins_annual <- ifelse(is.na(input$other_insurance_annual), 0, input$other_insurance_annual)
    monthly_other_insurance <- other_ins_annual / 12
    hoa_dues_monthly <- ifelse(is.na(input$hoa_dues_monthly), 0, input$hoa_dues_monthly)
    est_monthly_tax <- estimated_monthly_tax()

    # Calculate total non-mortgage costs using the estimated tax
    total_non_mortgage_monthly <- monthly_home_insurance + monthly_other_insurance + est_monthly_tax + hoa_dues_monthly

    # Calculate final mortgage budget
    monthly_mortgage_budget <- monthly_housing_budget - total_non_mortgage_monthly

    # Create data frame for the table
    df <- data.frame(
      Item = c("Monthly Housing Budget",
               "Less: Home Insurance (Monthly)",
               "Less: Other Insurance (Monthly)",
               "Less: Estimated Property Tax (Monthly, assumes 20% down)", # Updated label
               "Less: HOA Dues (Monthly)",
               "Equals: Monthly Mortgage Budget (P&I)"),
      Amount = scales::dollar(c(monthly_housing_budget,
                  -monthly_home_insurance,
                  -monthly_other_insurance,
                  -est_monthly_tax, # Use estimated tax
                  -hoa_dues_monthly,
                  monthly_mortgage_budget),
                  # Ensure negative numbers are displayed correctly with parentheses
                  style_negative = "parens") 
    )

    # Use kable and kableExtra for styling
    shiny::HTML(kable(df, format = "html", escape = FALSE, align = 'lr', table.attr = "style='width:auto;'") %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "bordered", "condensed"), 
                    full_width = FALSE) %>% 
      row_spec(nrow(df), bold = TRUE, background = "#D6EAF8")) # Style the last row

  }) # Formatting options removed as they are handled by kableExtra

  # --- 3) Money Down Summary Text ---
  output$money_down_summary_text <- renderUI({
    # Require inputs needed for the summary sentence
    req(input$monthly_housing_budget,
        estimated_monthly_tax(), # Uses 20% down assumption
        monthly_non_mortgage_costs_excl_tax(),
        monthly_rate_dec(),
        input$mortgage_term,
        input$annual_rate_pct,
        input$prop_tax_rate_annual)

    # Get reactive values/inputs
    monthly_non_mortgage <- monthly_non_mortgage_costs_excl_tax()
    est_monthly_tax <- estimated_monthly_tax()
    rate_monthly <- monthly_rate_dec()
    term_months <- as.numeric(input$mortgage_term)
    tax_rate_annual <- input$prop_tax_rate_annual
    housing_budget <- input$monthly_housing_budget
    annual_rate <- input$annual_rate_pct

    # Calculate Monthly Mortgage Budget (P&I) - consistent with table logic
    # Use the estimated tax which already assumes 20% down
    other_ins_annual <- ifelse(is.na(input$other_insurance_annual), 0, input$other_insurance_annual)
    monthly_other_insurance <- other_ins_annual / 12
    hoa_dues_monthly <- ifelse(is.na(input$hoa_dues_monthly), 0, input$hoa_dues_monthly)
    monthly_home_insurance <- input$home_insurance_annual / 12 # Required input
    total_non_mortgage_monthly <- monthly_home_insurance + monthly_other_insurance + est_monthly_tax + hoa_dues_monthly
    monthly_mortgage_budget_calc <- housing_budget - total_non_mortgage_monthly

    # Calculate affordable principal and home price assuming 20% down
    principal_at_20_pct_down <- compute_affordable_principal(
      monthly_housing_budget = housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage, # Base non-mortgage without tax
      rate_per_month = rate_monthly,
      n_payments_total = term_months,
      prop_tax_rate_annual = tax_rate_annual,
      down_payment_pct = 20
    )

    affordable_principal_fmt <- scales::dollar(0)
    affordable_home_price_fmt <- scales::dollar(0)

    if (principal_at_20_pct_down > 0) {
      affordable_principal_fmt <- scales::dollar(principal_at_20_pct_down)
      affordable_home_price_fmt <- scales::dollar(principal_at_20_pct_down / 0.80)
    } else {
      # If cannot afford anything, set descriptive text
      affordable_principal_fmt <- "$0 (cannot afford)"
      affordable_home_price_fmt <- "$0 (cannot afford)"
    }

    # Format mortgage term for display
    term_years_map <- c("60"="5 year", "120"="10 year", "180"="15 year", "240"="20 year", "360"="30 year")
    term_display <- term_years_map[as.character(term_months)]
    if (is.na(term_display)) term_display <- paste(term_months, "months") # Fallback

    # Format mortgage budget
    mortgage_budget_fmt <- scales::dollar(max(0, monthly_mortgage_budget_calc))

    # Construct the sentence
    summary_sentence <- sprintf(
      "Given a %.2f%% rate for a %s term and an estimated monthly mortgage budget of %s, assuming 20%% down results in an estimated affordable mortgage of %s and home price of %s.",
      annual_rate,
      term_display,
      mortgage_budget_fmt,
      affordable_principal_fmt,
      affordable_home_price_fmt
    )

    tags$p(tags$i(summary_sentence), style="color: #555;") # Display in italics and grey
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
    dp_pct_range_for_plot <- seq(dp_range_low, dp_range_high, by = 1)

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
