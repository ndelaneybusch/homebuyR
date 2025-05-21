#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import shinyhelper
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      titlePanel(tagList(
        tags$img(src = "www/favicon.ico", height = '35px', style = "margin-right:10px;"),
        "Home Buying Analysis",
        tags$p(tags$i("Placeholder for subtitle description"), style = "font-size: small;")
      )),

      sidebarLayout(
        sidebarPanel(
          tags$p(tags$i("Placeholder for text description at the top of the sidebar.")),
          hr(), # Add a horizontal rule for separation
          shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "loan_amount",
                                                               label = "Loan Amount ($)",
                                                               value = 500000,
                                                               currencySymbol = "$",
                                                               currencySymbolPlacement = "p",
                                                               decimalCharacter = ".",
                                                               digitGroupSeparator = ",",
                                                               minimumValue = 0),
                           content = "Enter the total loan amount. Can be filled automatically from the budgeting tab.",
                           style = "display: inline-block;",
                           type = "inline"),
          shinyhelper::helper(numericInput(inputId = "annual_rate_pct",
                                           label = "Annual Mortgage Rate (%)",
                                           value = 6.5,
                                           min = 0,
                                           step = 0.01),
                           content = "Enter the annual mortgage rate for the loan (e.g., 6.5 for 6.5%).",
                           style = "display: inline-block;",
                           type = "inline"),
          shinyhelper::helper(selectInput(inputId = "mortgage_term",
                                          label = "Mortgage Term",
                                          choices = c("5 year" = 60,
                                                      "10 year" = 120,
                                                      "15 year" = 180,
                                                      "20 year" = 240,
                                                      "30 year" = 360),
                                          selected = 360), 
                           content = "Select the duration of the mortgage.",
                           style = "display: inline-block;",
                           type = "inline"),
          shinyhelper::helper(dateInput(inputId = "loan_start",
                                        label = "Loan Start Date",
                                        value = as.Date(format(Sys.Date(), "%Y-%m-01")),
                                        format = "yyyy-mm",
                                        startview = "year"),
                           content = "Select the month and year the loan payments will begin.",
                           style = "display: inline-block;",
                           type = "inline"),
          shinyWidgets::materialSwitch(inputId = "advanced_controls", label = "Advanced Controls", status = "info"),
          conditionalPanel(
            condition = "input.advanced_controls",
            shinyhelper::helper(numericInput(inputId = "pmi_threshold_pct",
                                           label = "PMI Threshold (% Down)",
                                           value = 20, 
                                           min = 0,
                                           max = 100,
                                           step = 1),
                           content = "Enter the down payment percentage below which Private Mortgage Insurance (PMI) is typically required (e.g., 20 for 20%).",
                           style = "display: inline-block;",
                           type = "inline"),
            shinyhelper::helper(numericInput(inputId = "pmi_rate_annual",
                                           label = "PMI Rate (Annual %)",
                                           value = 0.5, 
                                           min = 0,
                                           step = 0.01),
                           content = "Enter the estimated annual Private Mortgage Insurance (PMI) rate, applied if down payment is below the threshold (e.g., 0.5 for 0.5%).",
                           style = "display: inline-block;",
                           type = "inline"),
            
          ) # Advanced controls
        ),
        mainPanel(
          tabsetPanel(id = "main_tabs",
                      tabPanel("Readme"),
                      tabPanel("Budgeting",
                               div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 5px;", # Opens Section 1 Div
                                   h3("1) Monthly Housing Budget"),
                                   tags$p(tags$i("How much you can spend on housing each month. Includes principal, interest, 
                                                 taxes, and insurance. Can be estimated on your behalf from % of your income 
                                                 (commonly used when applying for a mortgage), total debt to income ratio 
                                                 (also common), and financial stress resilience (most deliquencies are a 
                                                 consequence of a financial shock).")),
                                   # Monthly Budget Input
                                   shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "monthly_housing_budget",
                                                                                      label = "Monthly Housing Budget ($)",
                                                                                      value = NA, # Default empty
                                                                                      currencySymbol = "$",
                                                                                      currencySymbolPlacement = "p",
                                                                                      decimalCharacter = ".",
                                                                                      digitGroupSeparator = ",",
                                                                                      minimumValue = 0),
                                                    type = "inline",
                                                    content = "Enter your target monthly budget for housing (principal, interest, taxes, insurance).",
                                                    style = "display: inline-block;"),
                                   # Budget Model Dropdown
                                   selectInput(inputId = "budget_model",
                                               label = "Budget Model",
                                               choices = c("Manual", "Income %", "Debt to Income Ratio", "Financial Stress Resilience"),
                                               selected = "Manual"),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Income %' || input.budget_model == 'Debt to Income Ratio' || input.budget_model == 'Financial Stress Resilience'",
                                       selectInput(inputId = "income_interval",
                                                   label = "Income Interval",
                                                   choices = c("Weekly", "Two Weeks", "Twice Monthly", "Monthly", "Annual"),
                                                   selected = "Monthly")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Income %' || input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "income_amount",
                                                                                          label = "Household Income, before tax ($)",
                                                                                          value = NA,
                                                                                          currencySymbol = "$",
                                                                                          currencySymbolPlacement = "p",
                                                                                          decimalCharacter = ".",
                                                                                          digitGroupSeparator = ",",
                                                                                          minimumValue = 0),
                                                         type = "inline",
                                                         content = "Gross income, before taxes and deductions.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Debt to Income Ratio'",
                                       shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "take_home_income_amount",
                                                                                          label = "Take-Home Income ($)",
                                                                                          value = 0,
                                                                                          currencySymbol = "$",
                                                                                          currencySymbolPlacement = "p",
                                                                                          decimalCharacter = ".",
                                                                                          digitGroupSeparator = ",",
                                                                                          minimumValue = 0),
                                                         type = "inline",
                                                         content = "Take-home income, after taxes and withholdings.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Income %' || input.budget_model == 'Debt to Income Ratio'",
                                       shinyhelper::helper(numericInput(inputId = "housing_percent",
                                                                        label = "Housing Budget (% of Income)",
                                                                        value = 28,
                                                                        min = 0,
                                                                        max = 100,
                                                                        step = 0.1),
                                                         type = "inline",
                                                         content = "Recommended housing budget as a percentage of gross income.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Debt to Income Ratio'",
                                       shinyhelper::helper(numericInput(inputId = "max_total_debt_pct",
                                                                        label = "Max Total Debt Ratio",
                                                                        value = 0.45,
                                                                        min = 0,
                                                                        max = 1,
                                                                        step = 0.01),
                                                         type = "inline",
                                                         content = "Maximum allowable total debt-to-income ratio.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Debt to Income Ratio' || input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "other_debts",
                                                                                          label = "Other Debts ($)",
                                                                                          value = 0,
                                                                                          currencySymbol = "$",
                                                                                          currencySymbolPlacement = "p",
                                                                                          decimalCharacter = ".",
                                                                                          digitGroupSeparator = ",",
                                                                                          minimumValue = 0),
                                                         type = "inline",
                                                         content = "Total monthly non-housing debts (e.g., auto loans, student loans).",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(numericInput(inputId = "non_housing_essentials",
                                                                        label = "Non-Housing Essentials ($). Includes utilities, healthcare/insurance, groceries, childcare, transportation, but not shopping or entertainment.",
                                                                        value = 0,
                                                                        min = 0,
                                                                        step = 1),
                                                         type = "inline",
                                                         content = "Monthly essential expenses excluding housing and debts.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(numericInput(inputId = "savings",
                                                                        label = "Savings ($)",
                                                                        value = 0,
                                                                        min = 0,
                                                                        step = 1),
                                                         type = "inline",
                                                         content = "Current available savings.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(numericInput(inputId = "income_shock_pct",
                                                                        label = "Income Shock (%)",
                                                                        value = 0.2,
                                                                        min = 0,
                                                                        max = 1,
                                                                        step = 0.05),
                                                         type = "inline",
                                                         content = "Expected percentage drop in income during shock.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(numericInput(inputId = "shock_duration_months",
                                                                        label = "Shock Duration (Months)",
                                                                        value = 6,
                                                                        min = 0,
                                                                        step = 1),
                                                         type = "inline",
                                                         content = "Duration of income shock in months.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Financial Stress Resilience'",
                                       shinyhelper::helper(numericInput(inputId = "max_total_dti_stress",
                                                                        label = "Max DTI Stress Ratio",
                                                                        value = 0.5,
                                                                        min = 0,
                                                                        max = 1,
                                                                        step = 0.1),
                                                         type = "inline",
                                                         content = "Maximum debt-to-income ratio under stress scenario.",
                                                         style = "display: inline-block;")
                                   ),
                                   conditionalPanel(
                                       condition = "input.budget_model == 'Income %' || input.budget_model == 'Debt to Income Ratio' || input.budget_model == 'Financial Stress Resilience'",
                                       actionButton(inputId = "apply_budget_calc",
                                                    label = "Apply",
                                                    class = "btn-success",
                                                    style = "margin-top: 15px;")
                                   )
                               ), # Closes Section 1 Div (Monthly Housing Budget)
                               # Add a second box below the first one
                               div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 5px; margin-top: 20px;", # Opens Section 2 Div
                                   h3("2) Monthly Mortgage Budget"),
                                   tags$p(tags$i("The monthly housing budget minus non-mortgage recurring costs (insurance, taxes).")),
                                   hr(), # Add separator
                                   # Home Insurance Input
                                   shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "home_insurance_annual",
                                                                                      label = "Home Insurance (Annual)",
                                                                                      value = 1500,
                                                                                      currencySymbol = "$",
                                                                                      currencySymbolPlacement = "p",
                                                                                      decimalCharacter = ".",
                                                                                      digitGroupSeparator = ",",
                                                                                      minimumValue = 0),
                                                    type = "inline",
                                                    content = "Enter your estimated annual home insurance premium.",
                                                    style = "display: inline-block;"),
                                   # Other Insurance Input
                                   shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "other_insurance_annual",
                                                                                      label = "Other Insurance (Annual, e.g., Flood/Earthquake)",
                                                                                      value = 0,
                                                                                      currencySymbol = "$",
                                                                                      currencySymbolPlacement = "p",
                                                                                      decimalCharacter = ".",
                                                                                      digitGroupSeparator = ",",
                                                                                      minimumValue = 0),
                                                    type = "inline",
                                                    content = "Enter any additional annual insurance premiums (e.g., flood, earthquake) if applicable.",
                                                    style = "display: inline-block;"),
                                   # HOA Dues Input
                                   shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "hoa_dues_monthly",
                                                                                      label = "HOA Dues (Monthly)",
                                                                                      value = 0, # Default 0
                                                                                      currencySymbol = "$",
                                                                                      currencySymbolPlacement = "p",
                                                                                      decimalCharacter = ".",
                                                                                      digitGroupSeparator = ",",
                                                                                      minimumValue = 0),
                                                    type = "inline",
                                                    content = "Enter any monthly Homeowners Association dues, if applicable.",
                                                    style = "display: inline-block;"),
                                   # Property Tax Input
                                   shinyhelper::helper(numericInput(inputId = "prop_tax_rate_annual",
                                                                     label = "Property Tax Rate (Annual %)",
                                                                     value = 1.2,
                                                                     min = 0,
                                                                     step = 0.01),
                                                    type = "inline",
                                                    content = "Enter your estimated annual property tax rate as a percentage of home value (e.g., 1.2 for 1.2%). This will be used to estimate monthly tax.",
                                                    style = "display: inline-block;"),
                                   hr(), # Add separator
                                   # Output table for mortgage budget calculation
                                   h4("Monthly Housing Costs Breakdown:"), # Sub-header for table
                                   tableOutput(outputId = "mortgage_budget_table"),
                                   uiOutput(outputId = "affordable_mortgage_summary_text"),
                                   actionButton(inputId = "apply_mortgage_calc_assume_20_pct_down",
                                                    label = "Apply",
                                                    class = "btn-success",
                                                    style = "margin-top: 15px;")
                               ), # Closes Section 2 Div (Monthly Mortgage Budget)
                               div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 5px; margin-top: 20px;", 
                                   h3("3) Money Down"),
                                   div(style = "display: flex; align-items: flex-end; gap: 10px;",
                                       # Down Payment Input
                                       div(style = "flex-grow: 1;", # Allow input to take available space
                                           shinyhelper::helper(shinyWidgets::autonumericInput(inputId = "down_payment_dollars",
                                                                                      label = "Down Payment ($)",
                                                                                      value = 0, # Default 0
                                                                                      currencySymbol = "$",
                                                                                      currencySymbolPlacement = "p",
                                                                                      decimalCharacter = ".",
                                                                                      digitGroupSeparator = ",",
                                                                                      minimumValue = 0),
                                                                content = "Enter the total dollar amount you plan to put down.",
                                                                type = "inline",
                                                                style = "display: inline-block;")
                                       ),
                                       # Calculate 20% Button
                                       div(style = "margin-bottom: 20px;", # Opens Button Div
                                            actionButton(inputId = "calc_20_pct_down",
                                                    label = "Calculate 20%",
                                                    class = "btn-info") # Use info style for button
                                       ) # Closes Button Div
                                   ) # Closes inner flex div
                               ), # Closes Section 3 Div (Money Down)
                               # Plots
                               div(style = "border: 2px solid #28a745; padding: 15px; border-radius: 5px; margin-top: 20px;", # Opens Section 4 Div
                                   h3("4) Affordability Visualizations"),
                                   hr(),
                                   tags$p(tags$i("Explore how affordable home price changes with down payment and interest rate.")),
                                   # Price vs Down Payment Plot
                                   h4("Affordable Price vs. Down Payment (%)"),
                                   ggiraph::girafeOutput(outputId = "price_vs_dp_plot"),
                                   br(), # Add some space
                                   # Price vs Rate Plot
                                   h4("Affordable Price vs. Annual Rate (%)"),
                                   ggiraph::girafeOutput(outputId = "price_vs_rate_plot")
                               ) # Closes Section 4 Div (Plots)
                               # --- End Added Plot Section --- #
                      ), # Closes tabPanel("Budgeting", ...)
                      tabPanel("Buying"),
                      tabPanel("Paying"),
                      tabPanel("Refinancing")
          ) # Closes tabsetPanel
        ) # Closes mainPanel
      ) # Closes sidebarLayout
    ) # Closes fluidPage
  ) # Closes tagList
} # Closes app_ui function

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "homebuyR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
