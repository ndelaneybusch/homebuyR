
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
#' @import ggplot2 ggiraph scales dplyr purrr
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
  affordability_data <- purrr::map_dfr(dp_pct_range, ~{
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
    dplyr::filter(affordable_home_price > 0) # Remove cases where nothing is affordable

  if (nrow(affordability_data) == 0) {
     # Return a message or empty plot if nothing is affordable in the range
     p <- ggplot() +
          labs(title = "Affordable Home Price vs. Down Payment %",
               subtitle = paste("Monthly Budget:", scales::dollar(monthly_housing_budget)),
               caption = "No affordable home price found for the given inputs and down payment range.") +
          theme_minimal()
     return(p) # Return static plot
  }

  # Create tooltips
  affordability_data <- affordability_data %>%
    dplyr::mutate(
      tooltip_text = sprintf(
        "Price: %s
Down Payment: %.1f%% (%s)%s",
        scales::dollar(affordable_home_price),
        dp_pct,
        scales::dollar(down_payment_dollars),
        ifelse(pmi_applies, paste0("
Monthly PMI: ", scales::dollar(monthly_pmi_amount)), "")
      )
    )

  # Create plot
  p <- ggplot(affordability_data, aes(x = dp_pct, y = affordable_home_price, color = pmi_applies,
                                      tooltip = tooltip_text, data_id = dp_pct)) +
    geom_line_interactive(size = 1) +
    geom_point_interactive(size = 2) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                       labels = c(">= Threshold (No PMI)", "< Threshold (PMI likely)"),
                       name = "Down Payment vs PMI Threshold") +
    labs(
      title = "Affordable Home Price vs. Down Payment %",
      subtitle = paste("Based on a Monthly Housing Budget of", scales::dollar(monthly_housing_budget)),
      x = "Down Payment Percentage",
      y = "Maximum Affordable Home Price",
      caption = paste("Assumes:", annual_rate_pct, "% APR,", mortgage_term_months / 12, "yr term,",
                      prop_tax_rate_annual, "% Tax,", pmi_rate_annual, "% PMI rate below", pmi_threshold_pct, "% down.")
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.caption = element_text(size = 8, hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))

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
#' @import ggplot2 ggiraph scales dplyr purrr
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
  if(names(down_payment_input) == "pct") stopifnot(is.numeric(down_payment_input$pct), down_payment_input$pct >= 0, down_payment_input$pct < 100)
  if(names(down_payment_input) == "dollars") stopifnot(is.numeric(down_payment_input$dollars), down_payment_input$dollars >= 0)
  stopifnot(is.numeric(rate_pct_range), all(rate_pct_range >= 0))

  down_payment_type <- names(down_payment_input)[1]
  down_payment_value <- down_payment_input[[1]]

  # Calculate affordability for each rate
  affordability_data <- purrr::map_dfr(rate_pct_range, ~{
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

    affordable_home_price <- 0
    if (affordable_principal > 0) {
        if (down_payment_type == "pct") {
            if (down_payment_value < 100) {
                affordable_home_price <- affordable_principal / (1 - (down_payment_value / 100))
            }
        } else { # dollars
            affordable_home_price <- affordable_principal + down_payment_value
        }
    }

    tibble::tibble(
      rate_pct = current_rate_pct,
      affordable_home_price = affordable_home_price
    )
  }) %>%
    dplyr::filter(affordable_home_price > 0) # Remove cases where nothing is affordable

  if (nrow(affordability_data) == 0) {
     # Return a message or empty plot if nothing is affordable in the range
      p <- ggplot() +
           labs(title = "Affordable Home Price vs. Annual Rate %",
                subtitle = paste("Monthly Budget:", scales::dollar(monthly_housing_budget)),
                caption = "No affordable home price found for the given inputs and rate range.") +
           theme_minimal()
      return(p) # Return static plot
  }
  # Create tooltips
  down_payment_label <- if (down_payment_type == "pct") {
      paste0(down_payment_value, "%")
  } else {
      scales::dollar(down_payment_value)
  }

  affordability_data <- affordability_data %>%
    dplyr::mutate(
      tooltip_text = sprintf(
        "Price: %s
Rate: %.2f%%
Down Payment: %s",
        scales::dollar(affordable_home_price),
        rate_pct,
        down_payment_label
      )
    )

  # Create plot
  p <- ggplot(affordability_data, aes(x = rate_pct, y = affordable_home_price,
                                      tooltip = tooltip_text, data_id = rate_pct))

  # Add background rectangles if down_payment_type is not "pct"
  if (down_payment_type != "pct") {
    # Calculate down payment percentage for each point
    affordability_data <- affordability_data %>%
      dplyr::mutate(
        down_payment_pct = (down_payment_value / affordable_home_price) * 100,
        monthly_payment = (affordable_home_price - down_payment_value) *
                         (rate_pct/100/12) * (1 + rate_pct/100/12)^mortgage_term_months /
                         ((1 + rate_pct/100/12)^mortgage_term_months - 1) +
                         (affordable_home_price * prop_tax_rate_annual/100/12),
        zone = case_when(
          down_payment_pct < pmi_threshold_pct ~ "PMI",
          (down_payment_value / (pmi_threshold_pct/100)) - affordable_home_price < 0.001 * affordable_home_price ~ "below budget to avoid PMI",
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
      group_by(zone) %>%
      summarise(
        xmin = min(rate_pct),
        xmax = max(rate_pct),
        .groups = "drop"
      ) %>%
      mutate(
        ymin = low_y_bound,
        ymax = Inf
      )
    p <- p +
      geom_rect(
        data = rects,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone),
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
        guide = "none"  # hide color legend (it's used only for borders)
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
      subtitle = paste("Based on a Monthly Housing Budget of", scales::dollar(monthly_housing_budget),
      "\nand a down payment of ", down_payment_value, down_payment_type),
      x = "Annual Interest Rate",
      y = "Maximum Affordable Home Price",
      caption = paste("Assumes:", mortgage_term_months / 12, "yr term,",
                      prop_tax_rate_annual, "% Tax")
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.caption = element_text(size = 8, hjust = 0.5)) +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(t = 10),
      legend.title.align = 0
    ) +
    guides(fill = guide_legend(
      title.position = "left",
      title.hjust = 0,
      override.aes = list(colour = "black")
    ))

  # Convert to ggiraph object
  girafe(ggobj = p)
}
