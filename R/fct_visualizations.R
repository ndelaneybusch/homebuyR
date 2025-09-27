#' Plot Affordable Home Price vs. Down Payment Percentage
#'
#' Creates an interactive line plot showing the estimated maximum affordable
#' home price across a range of down payment percentages for a fixed monthly budget.
#' Lines are colored based on whether PMI is likely required.
#'
#' @param monthly_housing_budget Numeric. Total monthly housing budget.
#' @param monthly_non_mortgage_costs Numeric. Non-mortgage, non-tax costs per month.
#' @param annual_rate_pct Numeric. Annual interest rate (percentage).
#' @param mortgage_term_months Integer. Loan term in months.
#' @param prop_tax_rate_annual Numeric. Annual property tax rate (percentage).
#' @param pmi_rate_annual Numeric. Annual PMI rate (percentage).
#' @param pmi_threshold_pct Numeric. Down payment threshold for PMI (percentage).
#' @param dp_pct_range Numeric vector. Range of down payment percentages to plot (e.g., `seq(1, 30, by = 1)`).
#'
#' @return A ggiraph object.
#' @import ggplot2 ggiraph dplyr
#' @importFrom rlang .data
#' @export
plot_price_vs_down_payment <- function(monthly_housing_budget,
                                       monthly_non_mortgage_costs,
                                       annual_rate_pct,
                                       mortgage_term_months,
                                       prop_tax_rate_annual,
                                       pmi_rate_annual = 0.5, # Example default PMI rate
                                       pmi_threshold_pct = 20,
                                       dp_pct_range = seq(1, 30, by = 1)) {
  # Input validation (basic)
  stopifnot(is.numeric(monthly_housing_budget), monthly_housing_budget > 0)
  stopifnot(is.numeric(monthly_non_mortgage_costs), monthly_non_mortgage_costs >= 0)
  stopifnot(is.numeric(annual_rate_pct), annual_rate_pct >= 0)
  stopifnot(is.numeric(mortgage_term_months), mortgage_term_months > 0)
  stopifnot(is.numeric(prop_tax_rate_annual), prop_tax_rate_annual >= 0)
  stopifnot(is.numeric(pmi_rate_annual), pmi_rate_annual >= 0)
  stopifnot(is.numeric(pmi_threshold_pct), pmi_threshold_pct > 0, pmi_threshold_pct <= 100)
  stopifnot(is.numeric(dp_pct_range), all(dp_pct_range >= 0), all(dp_pct_range < 100))

  rate_per_month <- annual_rate_pct / 100 / 12

  # Calculate affordability for each down payment percentage
  affordability_data <- purrr::map_dfr(dp_pct_range, ~ {
    current_dp_pct <- .x

    affordable_principal <- compute_principal_with_pmi(
      monthly_housing_budget = monthly_housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage_costs,
      rate_per_month = rate_per_month,
      n_payments_total = mortgage_term_months,
      prop_tax_rate_annual = prop_tax_rate_annual,
      down_payment_pct = current_dp_pct,
      pmi_rate_annual = pmi_rate_annual,
      pmi_threshold_pct = pmi_threshold_pct
    )

    affordable_home_price <- if (affordable_principal > 0 && current_dp_pct < 100) {
      affordable_principal / (1 - (current_dp_pct / 100))
    } else {
      0
    }

    down_payment_dollars <- affordable_home_price * (current_dp_pct / 100)
    pmi_applies <- current_dp_pct < pmi_threshold_pct && pmi_rate_annual > 0 && affordable_principal > 0
    monthly_pmi_amount <- if (pmi_applies) {
      (affordable_principal * (pmi_rate_annual / 100)) / 12
    } else {
      0
    }

    tibble::tibble(
      dp_pct = current_dp_pct,
      affordable_home_price = affordable_home_price,
      down_payment_dollars = down_payment_dollars,
      pmi_applies = pmi_applies,
      monthly_pmi_amount = monthly_pmi_amount
    )
  }) %>%
    dplyr::filter(.data$affordable_home_price > 0) # Remove cases where nothing is affordable

  if (nrow(affordability_data) == 0) {
    # Return a message or empty plot if nothing is affordable in the range
    p <- ggplot() +
      labs(
        title = "Affordable Home Price vs. Down Payment %",
        subtitle = paste("Monthly Budget:", scales::dollar(monthly_housing_budget)),
        caption = "No affordable home price found for the given inputs and down payment range."
      ) +
      theme_minimal()
    return(p) # Return static plot
  }

  # Create tooltips
  affordability_data <- affordability_data %>%
    dplyr::mutate(
      tooltip_text = sprintf(
        "Price: %s
Down Payment: %.1f%% (%s)%s",
        scales::dollar(.data$affordable_home_price),
        .data$dp_pct,
        scales::dollar(.data$down_payment_dollars),
        ifelse(.data$pmi_applies, paste0("
Monthly PMI: ", scales::dollar(.data$monthly_pmi_amount)), "")
      )
    )

  # Create plot
  p <- ggplot(affordability_data, aes(
    x = .data$dp_pct, y = .data$affordable_home_price, color = .data$pmi_applies,
    tooltip = .data$tooltip_text, data_id = .data$dp_pct
  )) +
    geom_line_interactive(size = 1) +
    geom_point_interactive(size = 2) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"),
      labels = c(">= Threshold (No PMI)", "< Threshold (PMI likely)"),
      name = "Down Payment vs PMI Threshold"
    ) +
    labs(
      title = "Affordable Home Price vs. Down Payment %",
      subtitle = paste("Based on a Monthly Housing Budget of", scales::dollar(monthly_housing_budget)),
      x = "Down Payment Percentage",
      y = "Maximum Affordable Home Price",
      caption = paste(
        "Assumes:", annual_rate_pct, "% APR,", mortgage_term_months / 12, "yr term,",
        prop_tax_rate_annual, "% Tax,", pmi_rate_annual, "% PMI rate below", pmi_threshold_pct, "% down."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.caption = element_text(size = 8, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # Convert to ggiraph object
  girafe(ggobj = p)
}


#' Plot Affordable Home Price vs. Annual Interest Rate
#'
#' Creates an interactive line plot showing the estimated maximum affordable
#' home price across a range of annual interest rates for a fixed monthly budget
#' and down payment.
#'
#' @param monthly_housing_budget Numeric. Total monthly housing budget.
#' @param monthly_non_mortgage_costs Numeric. Non-mortgage, non-tax costs per month.
#' @param mortgage_term_months Integer. Loan term in months.
#' @param prop_tax_rate_annual Numeric. Annual property tax rate (percentage).
#' @param pmi_rate_annual Numeric. Annual PMI rate (percentage).
#' @param pmi_threshold_pct Numeric. Down payment threshold for PMI (percentage).
#' @param down_payment_input List containing either `pct = value` or `dollars = value`.
#'        Example: `list(pct = 20)` or `list(dollars = 50000)`.
#' @param rate_pct_range Numeric vector. Range of annual interest rates (percentage) to plot (e.g., `seq(3, 10, by = 0.25)`).
#'
#' @return A ggiraph object.
#' @import ggplot2 ggiraph dplyr
#' @importFrom rlang .data
#' @export
plot_price_vs_rate <- function(monthly_housing_budget,
                               monthly_non_mortgage_costs,
                               mortgage_term_months,
                               prop_tax_rate_annual,
                               pmi_rate_annual = 0.5,
                               pmi_threshold_pct = 20,
                               down_payment_input, # List: pct= or dollars=
                               rate_pct_range = seq(3, 10, by = 0.25)) {
  # Input validation (basic)
  stopifnot(is.numeric(monthly_housing_budget), monthly_housing_budget > 0)
  stopifnot(is.numeric(monthly_non_mortgage_costs), monthly_non_mortgage_costs >= 0)
  stopifnot(is.numeric(mortgage_term_months), mortgage_term_months > 0)
  stopifnot(is.numeric(prop_tax_rate_annual), prop_tax_rate_annual >= 0)
  stopifnot(is.numeric(pmi_rate_annual), pmi_rate_annual >= 0)
  stopifnot(is.numeric(pmi_threshold_pct), pmi_threshold_pct > 0, pmi_threshold_pct <= 100)
  stopifnot(is.list(down_payment_input), length(down_payment_input) == 1)
  stopifnot(names(down_payment_input) %in% c("pct", "dollars"))
  if (names(down_payment_input) == "pct") stopifnot(is.numeric(down_payment_input$pct), down_payment_input$pct >= 0, down_payment_input$pct < 100)
  if (names(down_payment_input) == "dollars") stopifnot(is.numeric(down_payment_input$dollars), down_payment_input$dollars >= 0)
  stopifnot(is.numeric(rate_pct_range), all(rate_pct_range >= 0))

  down_payment_type <- names(down_payment_input)[1]
  down_payment_value <- down_payment_input[[1]]

  # Calculate affordability for each rate
  affordability_data <- purrr::map_dfr(rate_pct_range, ~ {
    current_rate_pct <- .x
    rate_per_month <- current_rate_pct / 100 / 12

    pmi_args <- list(
      monthly_housing_budget = monthly_housing_budget,
      monthly_non_mortgage_costs = monthly_non_mortgage_costs,
      rate_per_month = rate_per_month,
      n_payments_total = mortgage_term_months,
      prop_tax_rate_annual = prop_tax_rate_annual,
      pmi_rate_annual = pmi_rate_annual,
      pmi_threshold_pct = pmi_threshold_pct
    )

    # Add the correct down payment argument
    if (down_payment_type == "pct") {
      pmi_args$down_payment_pct <- down_payment_value
    } else {
      pmi_args$down_payment_dollars <- down_payment_value
    }

    affordable_principal <- do.call(compute_principal_with_pmi, pmi_args)

    # Initialize affordable home price to zero (meaning not affordable)
    affordable_home_price <- 0
    if (affordable_principal > 0) {
      if (down_payment_type == "pct") {
        if (down_payment_value < 100) {
          # Formula: Home Price = Principal / (1 - Down Payment Fraction)
          affordable_home_price <- affordable_principal / (1 - (down_payment_value / 100))
        } else {
        }
      } else {
        # Down payment type is "dollars"
        affordable_home_price <- affordable_principal + down_payment_value
      }
    }

    if (affordable_home_price > 0) {
      return(tibble::tibble(
        rate_pct = current_rate_pct,
        rate_per_month = rate_per_month,
        affordable_home_price = affordable_home_price,
        affordable_principal = affordable_principal
      ))
    } else {
      # Return NULL if not affordable; purrr::map_dfr will automatically skip these
      return(NULL)
    }
  })

  if (nrow(affordability_data) == 0) {
    # Return a message or empty plot if nothing is affordable in the range
    p <- ggplot() +
      labs(
        title = "Affordable Home Price vs. Annual Rate %",
        subtitle = paste("Monthly Budget:", scales::dollar(monthly_housing_budget)),
        caption = "No affordable home price found for the given inputs and rate range."
      ) +
      theme_minimal()
    return(p) # Return static plot
  }

  # Create tooltips
  down_payment_label <- if (down_payment_type == "pct") {
    paste0(down_payment_value, "%")
  } else {
    scales::dollar(down_payment_value)
  }

  affordability_data$down_payment_cash <- affordability_data$affordable_home_price - affordability_data$affordable_principal
  affordability_data$pmi_cash <- ifelse(
    affordability_data$down_payment_cash / affordability_data$affordable_home_price * 100 < pmi_threshold_pct,
    (affordability_data$affordable_principal * (pmi_rate_annual / 100)) / 12,
    0
  )
  affordability_data$tax_cash <- (affordability_data$affordable_home_price * (prop_tax_rate_annual / 100)) / 12


  # Calculate monthly payment row by row to avoid vectorization issues
  affordability_data <- affordability_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      monthly_mortgage_payment = compute_monthly_payment(
        principal = .data$affordable_principal,
        rate_per_month = .data$rate_per_month,
        n_payments = mortgage_term_months
      )
    ) %>%
    dplyr::ungroup()
  affordability_data$monthly_housing_spend <- affordability_data$monthly_mortgage_payment +
    affordability_data$pmi_cash +
    affordability_data$tax_cash +
    monthly_non_mortgage_costs


  affordability_data <- affordability_data %>%
    dplyr::mutate(
      tooltip_text = sprintf(
        "Price: %s
Rate: %.2f%%
Down Payment: %s
Monthly Housing Spend: $%.0f",
        scales::dollar(.data$affordable_home_price),
        .data$rate_pct,
        down_payment_label,
        .data$monthly_housing_spend
      )
    )

  # Create plot
  p <- ggplot(affordability_data, aes(
    x = .data$rate_pct, y = .data$affordable_home_price,
    tooltip = .data$tooltip_text, data_id = .data$rate_pct
  ))

  # Add background rectangles if down_payment_type is not "pct"
  if (down_payment_type != "pct") {
    # Calculate down payment percentage for each point
    affordability_data <- affordability_data %>%
      dplyr::mutate(
        down_payment_pct = (down_payment_value / .data$affordable_home_price) * 100,
        monthly_payment = (.data$affordable_home_price - down_payment_value) *
          (rate_pct / 100 / 12) * (1 + rate_pct / 100 / 12)^mortgage_term_months /
          ((1 + rate_pct / 100 / 12)^mortgage_term_months - 1) +
          (.data$affordable_home_price * prop_tax_rate_annual / 100 / 12),
        zone = case_when(
          down_payment_pct < pmi_threshold_pct ~ "PMI",
          (down_payment_value / (pmi_threshold_pct / 100)) - .data$affordable_home_price < 0.001 * .data$affordable_home_price ~ "below budget to avoid PMI",
          TRUE ~ "No PMI"
        )
      )
    affordability_data$zone <- factor(
      affordability_data$zone,
      levels = c("PMI", "below budget to avoid PMI", "No PMI")
    )


    # Add background rectangles
    low_y_bound <- min(affordability_data$affordable_home_price) * 0.9
    rects <- affordability_data %>%
      group_by(.data$zone) %>%
      summarise(
        xmin = min(.data$rate_pct),
        xmax = max(.data$rate_pct),
        .groups = "drop"
      ) %>%
      mutate(
        ymin = low_y_bound,
        ymax = Inf
      )
    p <- p +
      geom_rect(
        data = rects,
        aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, fill = .data$zone),
        inherit.aes = FALSE,
        alpha = 0.3
      ) +
      scale_fill_manual(
        values = c(
          "PMI" = "#FFCCCC",
          "below budget to avoid PMI" = "#D9D9D9",
          "No PMI" = "#CCFFCC"
        ),
        name = "PMI"
      ) +
      scale_colour_manual(
        values = c("Zone A" = "black", "Zone B" = "black", "Zone C" = "black"),
        guide = "none" # hide color legend (it's used only for borders)
      )
  }

  # Add data layers
  p <- p +
    geom_line_interactive(size = 1, color = "black") +
    geom_point_interactive(size = 2, color = "black") +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    labs(
      title = "Affordable Home Price vs. Annual Rate %",
      subtitle = paste(
        "Based on a Monthly Housing Budget of", scales::dollar(monthly_housing_budget),
        "\nand a down payment of ", down_payment_value, down_payment_type
      ),
      x = "Annual Interest Rate",
      y = "Maximum Affordable Home Price",
      caption = paste(
        "Assumes:", mortgage_term_months / 12, "yr term,",
        prop_tax_rate_annual, "% Tax"
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.caption = element_text(size = 8, hjust = 0.5),
      legend.position = "bottom",
      legend.box.margin = margin(t = 10),
      legend.title = element_text(hjust = 0.5)
    ) +
    guides(fill = guide_legend(
      title.position = "left",
      title.hjust = 0,
      override.aes = list(colour = "black")
    ))

  # Convert to ggiraph object
  girafe(ggobj = p)
}




#' Plot Remaining Principal and Interest Over Time
#'
#' Creates an interactive line plot showing the remaining principal and total interest paid
#' over time for both the original loan and a paydown scenario.
#'
#' @param amortization_table A data frame containing the amortization schedule with columns:
#'   \describe{
#'     \item{payment_number}{Payment number (1, 2, 3, ...)}
#'     \item{original_remaining_principal}{Remaining principal for original loan}
#'     \item{new_remaining_principal}{Remaining principal for paydown scenario}
#'     \item{original_interest_paid}{Cumulative interest paid for original loan}
#'     \item{new_interest_paid}{Cumulative interest paid for paydown scenario}
#'   }
#'
#' @return A `ggiraph` object with interactive tooltips.
#' @import ggplot2 ggiraph dplyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using output from a hypothetical calculate_mortgage_savings()
#' # amortization_table <- calculate_mortgage_savings(
#' #   principal = 300000,
#' #   rate_per_month = 0.005,
#' #   n_payments_total = 360,
#' #   extra_monthly_payment = 200,
#' #   lump_sum_payment = 10000,
#' #   payment_number_for_prepay_start = 1,
#' #   cumulative_output = TRUE # Assuming this function populates the required columns
#' # )
#' # if (requireNamespace("ggiraph")) {
#' #   plot_principal_interest(amortization_table)
#' # }
plot_principal_interest <- function(amortization_table) {
  # --- Input Validation ---
  required_cols <- c(
    "payment_number",
    "original_remaining_principal",
    "new_remaining_principal",
    "original_interest_paid",
    "new_interest_paid"
  )

  missing_cols <- setdiff(required_cols, names(amortization_table))
  if (length(missing_cols) > 0) {
    stop(
      "Input data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # --- Data Preparation ---
  # 1. Add derived columns (months, interest_saved, tooltip_text) to the wide data.
  #    The tooltip is created here because it needs access to multiple wide columns.
  data_wide_prepared <- amortization_table %>%
    dplyr::mutate(
      months = .data$payment_number,
      interest_saved = .data$original_interest_paid - .data$new_interest_paid, # NA if new_interest_paid is NA
      tooltip_text = sprintf(
        paste0(
          "<b>Payment %s</b><br>",
          "Original Principal: %s<br>",
          "New Principal: %s<br><br>",
          "Original Interest: %s<br>",
          "New Interest: %s<br>",
          "Interest Saved: %s"
        ),
        .data$payment_number,
        scales::dollar(.data$original_remaining_principal, accuracy = 0.01),
        scales::dollar(.data$new_remaining_principal, accuracy = 0.01),
        scales::dollar(.data$original_interest_paid, accuracy = 0.01),
        scales::dollar(.data$new_interest_paid, accuracy = 0.01),
        scales::dollar(.data$interest_saved, accuracy = 0.01)
      )
    )

  # 2. Define series labels and their desired order for the legend and plotting.
  #    These are the raw column names to be pivoted and their corresponding desired legend labels.
  series_mapping <- c(
    "original_remaining_principal" = "Original Principal",
    "new_remaining_principal" = "New Principal",
    "original_interest_paid" = "Original Interest Paid",
    "new_interest_paid" = "New Interest Paid",
    "interest_saved" = "Interest Saved"
  )
  # Ensure the factor levels are in a logical order for the legend
  series_levels_ordered <- c(
    "Original Principal", "New Principal",
    "Original Interest Paid", "New Interest Paid",
    "Interest Saved"
  )

  # 3. Pivot to long format for idiomatic ggplot2 usage.
  #    Keep 'months' and 'tooltip_text' associated with each payment_number.
  plot_data_long <- data_wide_prepared %>%
    tidyr::pivot_longer(
      cols = all_of(names(series_mapping)),
      names_to = "series_key_raw",
      values_to = "amount",
      values_drop_na = FALSE # Keep NAs; geom_line handles them by breaking lines
    ) %>%
    dplyr::mutate(
      # Create the 'series_label' factor using the mapping and ordered levels
      series_label = factor(series_mapping[.data$series_key_raw], levels = series_levels_ordered)
    ) %>%
    # Filter out any rows where series_label might be NA (if a key wasn't in mapping)
    # or where amount is NA (geom_line does this, but good for points if they were visible)
    dplyr::filter(!is.na(series_label))
  # Note: We don't filter !is.na(amount) here to allow lines to naturally break
  # if a series ends (e.g. loan paid off). ggplot2 handles NA y-values for lines.

  # --- Define Aesthetics ---
  # Colors and linetypes for each series. Consider colorblind-friendly options.
  series_colors <- c(
    "Original Principal"     = "black",
    "New Principal"          = "#0072B2", # A distinct blue
    "Original Interest Paid" = "grey40", # Dark grey, distinct from black
    "New Interest Paid"      = "#56B4E9", # A lighter, distinct blue
    "Interest Saved"         = "#009E73" # A distinct green
  )

  series_linetypes <- c(
    "Original Principal"     = "solid",
    "New Principal"          = "solid",
    "Original Interest Paid" = "dotted",
    "New Interest Paid"      = "dotted",
    "Interest Saved"         = "dashed"
  )

  # --- Create Plot ---
  p <- ggplot(
    plot_data_long,
    aes(
      x = .data$months, y = .data$amount, group = .data$series_label,
      color = .data$series_label, linetype = .data$series_label
    )
  ) +
    geom_line_interactive(
      aes(data_id = .data$series_label), # data_id for potential line-specific interactivity
      size = 1
    ) +
    # Add nearly invisible points for robust tooltip interaction.
    # These points share the same comprehensive tooltip for each payment_number.
    geom_point_interactive(
      # Map y to 'amount' so points are correctly positioned for each series.
      # Tooltip and data_id are consistent for a given 'months'.
      aes(y = .data$amount, tooltip = .data$tooltip_text, data_id = .data$payment_number),
      size = 1,
      alpha = 0.01, # Visually hidden, but interactive
      show.legend = FALSE # Prevent these points from creating a legend
    ) +
    scale_y_continuous(
      name = "Amount ($)",
      labels = scales::dollar_format(accuracy = 1), # Whole dollars on axis
      expand = expansion(mult = c(0.01, 0.05)) # Slight padding
    ) +
    scale_x_continuous(
      name = "Payment Number",
      breaks = scales::pretty_breaks(n = min(10, max(plot_data_long$payment_number, na.rm = TRUE) %/% 30 + 1)), # Adaptive breaks
      labels = function(x) round(x, 0)
    ) +
    scale_color_manual(
      name = "Metric",
      values = series_colors,
      labels = series_levels_ordered, # Ensures legend items match factor levels
      breaks = series_levels_ordered # Ensures all defined series appear
    ) +
    scale_linetype_manual(
      name = "Metric",
      values = series_linetypes,
      labels = series_levels_ordered,
      breaks = series_levels_ordered
    ) +
    labs(
      title = "Loan Paydown Comparison",
      subtitle = "Remaining Principal and Cumulative Interest Over Time"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 5, b = 5, unit = "pt"),
      legend.key.width = unit(1.5, "lines"),
      legend.title = element_text(face = "bold", margin = margin(b = 5, unit = "pt")),
      legend.text = element_text(margin = margin(r = 10, unit = "pt")),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = rel(1.2), margin = margin(b = 5, unit = "pt")),
      plot.subtitle = element_text(color = "grey50", size = rel(1.0), margin = margin(b = 10, unit = "pt")),
      axis.title = element_text(size = rel(1.0), face = "bold")
    )

  # --- Convert to Interactive Plot ---
  girafe(
    ggobj = p,
    options = list(
      opts_tooltip(
        opacity = 0.95,
        use_fill = FALSE,
        use_stroke = TRUE, # Use aesthetics from plot for stroke
        css = paste0(
          "padding:8px;font-family:sans-serif;font-size:0.9em;",
          "background:white;color:black;",
          "border-radius:4px;box-shadow: 2px 2px 5px rgba(0,0,0,0.2);"
        )
      ),
      opts_sizing(rescale = TRUE, width = 1), # width = 1 means 100% of container
      opts_toolbar(position = "topright", saveaspng = TRUE),
      opts_hover(css = "stroke-width:2px;"), # Example: thicken line on hover
      opts_hover_inv(css = "opacity:0.3;") # Example: fade out non-hovered lines
    )
  )
}


#' Plot Refinance Benefit Curve
#'
#' Creates an interactive plot showing the cumulative net benefit of refinancing
#' over time, with breakeven point highlighting and hover details.
#'
#' @param refinance_data List returned by calculate_refinance_benefit_curve() containing:
#'   \describe{
#'     \item{months}{Vector of evaluation months (1 to max_eval_months)}
#'     \item{net_benefits}{Vector of cumulative net benefits for each month (total wealth impact)}
#'     \item{net_cash_benefits}{Vector of cumulative cash benefits for each month (excluding equity)}
#'     \item{equity_differences}{Vector of equity differences for each month}
#'     \item{breakeven_month}{First month where total benefit > 0 (NA if none)}
#'     \item{cash_breakeven_month}{First month where cash benefit > 0 (NA if none)}
#'     \item{old_payment}{Monthly payment on existing loan}
#'     \item{new_payment}{Monthly payment on refinance loan}
#'     \item{monthly_savings}{Difference in monthly payments}
#'   }
#' @param title Character. Optional plot title (default: "Refinance Benefit Over Time")
#' @param show_breakeven Logical. Whether to highlight breakeven point (default: TRUE)
#'
#' @return A ggiraph object with interactive tooltips.
#' @import ggplot2 ggiraph dplyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Example using output from calculate_refinance_benefit_curve()
#' # refinance_data <- calculate_refinance_benefit_curve(
#' #   principal = 300000,
#' #   rate_per_month_old = 0.0041667,  # 5% annual
#' #   rate_per_month_new = 0.0033333,  # 4% annual
#' #   n_payments_remaining = 240,
#' #   closing_costs = 5000,
#' #   tax_rate = 0.25,
#' #   investment_return_annual = 0.05
#' # )
#' # if (requireNamespace("ggiraph")) {
#' #   plot_refinance_benefit(refinance_data)
#' # }
plot_refinance_benefit <- function(refinance_data,
                                 title = "Refinance Benefit Over Time",
                                 show_breakeven = TRUE) {
  # --- Input Validation ---
  required_fields <- c("months", "net_benefits", "breakeven_month",
                      "old_payment", "new_payment", "monthly_savings")
  missing_fields <- setdiff(required_fields, names(refinance_data))

  if (length(missing_fields) > 0) {
    stop(
      "refinance_data is missing required fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  stopifnot(is.character(title), length(title) == 1)
  stopifnot(is.logical(show_breakeven), length(show_breakeven) == 1)

  # Validate that months and net_benefits have same length
  if (length(refinance_data$months) != length(refinance_data$net_benefits)) {
    stop("months and net_benefits must have the same length")
  }
  has_cash_benefits <- "net_cash_benefits" %in% names(refinance_data)
  has_equity_differences <- "equity_differences" %in% names(refinance_data)
  has_cash_breakeven <- "cash_breakeven_month" %in% names(refinance_data)

  # --- Data Preparation ---
  if (has_cash_benefits && has_equity_differences) {
    # New format with separate cash and wealth benefits
    plot_data_wide <- tibble::tibble(
      months = refinance_data$months,
      net_wealth_benefit = refinance_data$net_benefits,
      net_cash_benefit = refinance_data$net_cash_benefits,
      equity_difference = refinance_data$equity_differences
    )

    # Get dynamic monthly savings if available, otherwise use static value
    dynamic_savings <- if ("dynamic_monthly_savings" %in% names(refinance_data)) {
      refinance_data$dynamic_monthly_savings
    } else {
      rep(refinance_data$monthly_savings, length(refinance_data$months))
    }

    # Create comprehensive tooltips with dynamic values
    plot_data_wide <- plot_data_wide %>%
      dplyr::mutate(
        # Calculate dynamic old and new payments based on loan terms
        dynamic_old_payment = sapply(.data$months, function(k) {
          # Always use pattern recognition from dynamic_monthly_savings if available
          # This ensures consistency with the actual calculation logic
          if (length(dynamic_savings) >= k) {
            old_pay <- refinance_data$old_payment
            new_pay <- refinance_data$new_payment
            static_savings <- old_pay - new_pay
            current_savings <- dynamic_savings[k]

            # If dynamic savings equals -new_payment, old loan is paid off
            if (abs(current_savings + new_pay) < 0.01) return(0)
            # If dynamic savings equals old_payment, new loan is paid off, old continues
            if (abs(current_savings - old_pay) < 0.01) return(old_pay)
            # If dynamic savings equals static savings, both loans continue
            if (abs(current_savings - static_savings) < 0.01) return(old_pay)
          }

          # Fallback to loan terms if dynamic savings not available
          if ("n_payments_remaining" %in% names(refinance_data)) {
            if (k <= refinance_data$n_payments_remaining) refinance_data$old_payment else 0
          } else {
            # Default: assume old loan continues
            refinance_data$old_payment
          }
        }),
        dynamic_new_payment = sapply(.data$months, function(k) {
          # Always use pattern recognition from dynamic_monthly_savings if available
          # This ensures consistency with the actual calculation logic
          if (length(dynamic_savings) >= k) {
            old_pay <- refinance_data$old_payment
            new_pay <- refinance_data$new_payment
            static_savings <- old_pay - new_pay
            current_savings <- dynamic_savings[k]

            # If dynamic savings equals old_payment, new loan is paid off
            if (abs(current_savings - old_pay) < 0.01) return(0)
            # If dynamic savings equals -new_payment, old loan is paid off, new continues
            if (abs(current_savings + new_pay) < 0.01) return(new_pay)
            # If dynamic savings equals static savings, both loans continue
            if (abs(current_savings - static_savings) < 0.01) return(new_pay)
          }

          # Fallback to loan terms if dynamic savings not available
          if ("n_payments_new" %in% names(refinance_data)) {
            if (k <= refinance_data$n_payments_new) refinance_data$new_payment else 0
          } else {
            # Default: assume new loan continues
            refinance_data$new_payment
          }
        }),
        tooltip_text = sprintf(
          paste0(
            "<b>Month %d</b><br>",
            "<strong>Net Cash Benefit:</strong> %s<br>",
            "<strong>Net Wealth Benefit:</strong> %s<br>",
            "<strong>Equity Difference:</strong> %s<br><br>",
            "Old Payment: %s<br>",
            "New Payment: %s<br>",
            "Monthly Savings: %s"
          ),
          .data$months,
          scales::dollar(.data$net_cash_benefit, accuracy = 1),
          scales::dollar(.data$net_wealth_benefit, accuracy = 1),
          scales::dollar(.data$equity_difference, accuracy = 1),
          scales::dollar(.data$dynamic_old_payment, accuracy = 1),
          scales::dollar(.data$dynamic_new_payment, accuracy = 1),
          scales::dollar(dynamic_savings[.data$months], accuracy = 1)
        )
      )

    # Pivot to long format for multiple lines
    plot_data_long <- plot_data_wide %>%
      tidyr::pivot_longer(
        cols = c(.data$net_cash_benefit, .data$net_wealth_benefit),
        names_to = "benefit_type",
        values_to = "benefit_amount"
      ) %>%
      dplyr::mutate(
        benefit_type_label = factor(
          ifelse(.data$benefit_type == "net_cash_benefit",
                "Net Cash Benefit", "Net Wealth Benefit"),
          levels = c("Net Cash Benefit", "Net Wealth Benefit")
        )
      )

    # Define colors and line types
    benefit_colors <- c(
      "Net Cash Benefit" = "#0072B2",    # Blue - cash only
      "Net Wealth Benefit" = "#009E73"   # Green - total wealth
    )

    benefit_linetypes <- c(
      "Net Cash Benefit" = "dashed",     # Dashed for cash
      "Net Wealth Benefit" = "solid"     # Solid for total wealth
    )

  } else {
    # Legacy format - single line
    # Get dynamic monthly savings if available, otherwise use static value
    dynamic_savings <- if ("dynamic_monthly_savings" %in% names(refinance_data)) {
      refinance_data$dynamic_monthly_savings
    } else {
      rep(refinance_data$monthly_savings, length(refinance_data$months))
    }

    plot_data_wide <- tibble::tibble(
      months = refinance_data$months,
      net_wealth_benefit = refinance_data$net_benefits
    ) %>%
      dplyr::mutate(
        # Calculate dynamic old and new payments based on loan terms
        dynamic_old_payment = sapply(.data$months, function(k) {
          # Always use pattern recognition from dynamic_monthly_savings if available
          # This ensures consistency with the actual calculation logic
          if (length(dynamic_savings) >= k) {
            old_pay <- refinance_data$old_payment
            new_pay <- refinance_data$new_payment
            static_savings <- old_pay - new_pay
            current_savings <- dynamic_savings[k]

            # If dynamic savings equals -new_payment, old loan is paid off
            if (abs(current_savings + new_pay) < 0.01) return(0)
            # If dynamic savings equals old_payment, new loan is paid off, old continues
            if (abs(current_savings - old_pay) < 0.01) return(old_pay)
            # If dynamic savings equals static savings, both loans continue
            if (abs(current_savings - static_savings) < 0.01) return(old_pay)
          }

          # Fallback to loan terms if dynamic savings not available
          if ("n_payments_remaining" %in% names(refinance_data)) {
            if (k <= refinance_data$n_payments_remaining) refinance_data$old_payment else 0
          } else {
            # Default: assume old loan continues
            refinance_data$old_payment
          }
        }),
        dynamic_new_payment = sapply(.data$months, function(k) {
          # Always use pattern recognition from dynamic_monthly_savings if available
          # This ensures consistency with the actual calculation logic
          if (length(dynamic_savings) >= k) {
            old_pay <- refinance_data$old_payment
            new_pay <- refinance_data$new_payment
            static_savings <- old_pay - new_pay
            current_savings <- dynamic_savings[k]

            # If dynamic savings equals old_payment, new loan is paid off
            if (abs(current_savings - old_pay) < 0.01) return(0)
            # If dynamic savings equals -new_payment, old loan is paid off, new continues
            if (abs(current_savings + new_pay) < 0.01) return(new_pay)
            # If dynamic savings equals static savings, both loans continue
            if (abs(current_savings - static_savings) < 0.01) return(new_pay)
          }

          # Fallback to loan terms if dynamic savings not available
          if ("n_payments_new" %in% names(refinance_data)) {
            if (k <= refinance_data$n_payments_new) refinance_data$new_payment else 0
          } else {
            # Default: assume new loan continues
            refinance_data$new_payment
          }
        }),
        tooltip_text = sprintf(
          paste0(
            "<b>Month %d</b><br>",
            "Net Benefit: %s<br><br>",
            "Old Payment: %s<br>",
            "New Payment: %s<br>",
            "Monthly Savings: %s"
          ),
          .data$months,
          scales::dollar(.data$net_wealth_benefit, accuracy = 1),
          scales::dollar(.data$dynamic_old_payment, accuracy = 1),
          scales::dollar(.data$dynamic_new_payment, accuracy = 1),
          scales::dollar(dynamic_savings[.data$months], accuracy = 1)
        )
      )

    plot_data_long <- plot_data_wide %>%
      tidyr::pivot_longer(
        cols = .data$net_wealth_benefit,
        names_to = "benefit_type",
        values_to = "benefit_amount"
      ) %>%
      dplyr::mutate(
        benefit_type_label = factor("Net Benefit", levels = "Net Benefit")
      )

    benefit_colors <- c("Net Benefit" = "steelblue")
    benefit_linetypes <- c("Net Benefit" = "solid")
  }

  # --- Create Base Plot ---
  p <- ggplot(plot_data_long, aes(x = .data$months, y = .data$benefit_amount)) +
    # Add horizontal line at y=0
    geom_hline(yintercept = 0, color = "grey50", linetype = "solid", size = 0.5) +
    # Benefit curves
    geom_line_interactive(
      aes(
        color = .data$benefit_type_label,
        linetype = .data$benefit_type_label,
        tooltip = .data$tooltip_text,
        data_id = paste(.data$benefit_type_label, .data$months, sep = "_")
      ),
      size = 1.2
    ) +
    # Interactive points for better tooltip interaction
    geom_point_interactive(
      aes(
        color = .data$benefit_type_label,
        tooltip = .data$tooltip_text,
        data_id = paste(.data$benefit_type_label, .data$months, sep = "_")
      ),
      size = 1.5,
      alpha = 0.6
    ) +
    scale_color_manual(
      values = benefit_colors,
      name = "Benefit Type"
    ) +
    scale_linetype_manual(
      values = benefit_linetypes,
      name = "Benefit Type"
    ) +
    scale_y_continuous(
      name = "Cumulative Net Benefit ($)",
      labels = scales::dollar_format(accuracy = 1),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_x_continuous(
      name = "Months Since Refinance",
      breaks = scales::pretty_breaks(n = 8),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = title,
      subtitle = paste0(
        "Monthly Payment: ", scales::dollar(refinance_data$old_payment), " -> ",
        scales::dollar(refinance_data$new_payment), " (",
        ifelse(refinance_data$monthly_savings > 0, "saves ", "costs "),
        scales::dollar(abs(refinance_data$monthly_savings)), ")",
        if (has_cash_benefits) "\nCash benefit excludes equity; Wealth benefit includes equity changes" else ""
      ),
      caption = if (has_cash_benefits) {
        "Hover over lines for detailed information. Cash benefit shows refinance value excluding home equity."
      } else {
        "Hover over points for detailed information"
      }
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = rel(1.2), margin = margin(b = 5)),
      plot.subtitle = element_text(color = "grey50", size = rel(1.0), margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 0.5, color = "grey50"),
      axis.title = element_text(face = "bold")
    )

  # --- Add Breakeven Point Highlighting ---
  if (show_breakeven) {
    # Wealth breakeven
    if (!is.na(refinance_data$breakeven_month)) {
      breakeven_benefit <- refinance_data$net_benefits[refinance_data$breakeven_month]

      p <- p +
        geom_vline(
          xintercept = refinance_data$breakeven_month,
          color = "#e31a1c",
          linetype = "dotted",
          size = 0.8
        ) +
        geom_point(
          aes(x = refinance_data$breakeven_month, y = breakeven_benefit),
          color = "#e31a1c",
          size = 4,
          shape = 21,
          fill = "white",
          stroke = 2
        ) +
        annotate(
          "text",
          x = refinance_data$breakeven_month,
          y = max(plot_data_long$benefit_amount, na.rm = TRUE) * 0.9,
          label = paste0("Wealth Breakeven:\n", refinance_data$breakeven_month, " months"),
          color = "#e31a1c",
          fontface = "bold",
          size = 3.2,
          hjust = ifelse(refinance_data$breakeven_month > max(refinance_data$months) * 0.7, 1.1, -0.1)
        )
    }

    # Cash breakeven (if available)
    if (has_cash_breakeven && !is.na(refinance_data$cash_breakeven_month)) {
      cash_breakeven_benefit <- refinance_data$net_cash_benefits[refinance_data$cash_breakeven_month]

      p <- p +
        geom_vline(
          xintercept = refinance_data$cash_breakeven_month,
          color = "#0072B2",
          linetype = "dotted",
          size = 0.8,
          alpha = 0.8
        ) +
        annotate(
          "text",
          x = refinance_data$cash_breakeven_month,
          y = max(plot_data_long$benefit_amount, na.rm = TRUE) * 0.75,
          label = paste0("Cash Breakeven:\n", refinance_data$cash_breakeven_month, " months"),
          color = "#0072B2",
          fontface = "bold",
          size = 3.2,
          hjust = ifelse(refinance_data$cash_breakeven_month > max(refinance_data$months) * 0.7, 1.1, -0.1)
        )
    }

    # No breakeven message
    if (is.na(refinance_data$breakeven_month)) {
      p <- p +
        annotate(
          "text",
          x = max(refinance_data$months) * 0.75,
          y = max(plot_data_long$benefit_amount, na.rm = TRUE) * 0.9,
          label = "No breakeven\nwithin timeframe",
          color = "#d73027",
          fontface = "bold",
          size = 3.5,
          hjust = 0.5
        )
    }
  }

  # --- Convert to Interactive Plot ---
  girafe(
    ggobj = p,
    options = list(
      opts_tooltip(
        opacity = 0.95,
        use_fill = FALSE,
        use_stroke = TRUE,
        css = paste0(
          "padding:10px;font-family:sans-serif;font-size:0.9em;",
          "background:white;color:black;",
          "border-radius:6px;box-shadow: 3px 3px 8px rgba(0,0,0,0.3);",
          "border:1px solid #cccccc;"
        )
      ),
      opts_sizing(rescale = TRUE, width = 1),
      opts_toolbar(position = "topright", saveaspng = TRUE),
      opts_hover(css = "stroke-width:3px;r:4px;"),
      opts_hover_inv(css = "opacity:0.4;")
    )
  )
}
