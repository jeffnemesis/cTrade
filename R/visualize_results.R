#' Visualize Results
#'
#' This function preprocesses the results from different weight types, combines them,
#' and generates visualizations for CDR traded, financial inflows/outflows, and job potentials.
#' Visualizations include bar charts with facet grids to explore differences by region, scenario,
#' technology, and weight type.
#'
#' @param output_path Path to the folder containing the weight-type result CSVs and saving combined results.
#' @param regions Vector of regions to include in the visualizations. Default is NULL (include all regions).
#' @export
visualize_results <- function(output_path, regions = NULL) {
  library(dplyr)
  library(ggplot2)

  # Preprocess CSVs to create a unified dataset
  preprocess_data <- function(output_path) {
    # Load individual weight-type CSVs
    responsibility_data <- read.csv(file.path(output_path, "Fairness_CDR_responsibility.csv"))
    population_data <- read.csv(file.path(output_path, "Fairness_CDR_population.csv"))
    gdp_data <- read.csv(file.path(output_path, "Fairness_CDR_GDP.csv"))

    # Add weight_type column to distinguish datasets
    responsibility_data <- responsibility_data %>% mutate(weight_type = "responsibility")
    population_data <- population_data %>% mutate(weight_type = "population")
    gdp_data <- gdp_data %>% mutate(weight_type = "GDP")

    # Combine all datasets into one
    combined_data <- bind_rows(responsibility_data, population_data, gdp_data)

    # Scale values down to millions
    combined_data <- combined_data %>%
      mutate(
        financial_flow_weighted = financial_flow_weighted / 1e9,
        job_potential = job_potential / 1e6
      )

    # Apply cumulative logic (second type)
    combined_data <- combined_data %>%
      group_by(region, scenario, weight_type, main_technology) %>%
      arrange(Year) %>%
      mutate(
        cdr_traded_cumulative = cumsum(gross_traded_weighted),
        financial_flow_weighted_cumulative = cumsum(financial_flow_weighted),
        job_potential_cumulative = cumsum(job_potential)
      ) %>%
      ungroup()

    # Debugging: Check structure
    print("Combined Data Preview:")
    print(head(combined_data))

    # Save the combined dataset for reference
    write.csv(combined_data, file = file.path(output_path, "Fairness_CDR_all.csv"), row.names = FALSE)

    return(combined_data)
  }

  # Preprocess and load the combined data
  combined_data <- preprocess_data(output_path)

  # Filter data based on specified regions
  if (!is.null(regions)) {
    combined_data <- combined_data %>% filter(region %in% regions)
    if (nrow(combined_data) == 0) {
      stop("Error: No data available for the specified regions.")
    }
  }


  # Summarize cumulative results
  data_summary <- combined_data %>%
    group_by(region, scenario, weight_type, main_technology) %>%
    summarize(
      cdr_traded_cumulative = sum(gross_traded_weighted, na.rm = TRUE),
      financial_flow_weighted_cumulative = sum(financial_flow_weighted, na.rm = TRUE),
      job_potential_cumulative = sum(job_potential, na.rm = TRUE),
      .groups = "drop"
    )

  # Define a consistent theme
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.2),
      strip.background = element_rect(fill = "lightgrey", color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 8, color = "black", face = "bold", angle = 90, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10)
    )

  # Visualization 1: CDR traded annually by weight type
  plot1 <- ggplot(combined_data, aes(x = Year, y = gross_traded_weighted, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Annual CDR Traded",
      x = "Year",
      y = "CDR Traded (MtCO2)",
      fill = "Burden-sharing Type"
    ) +
    custom_theme
  ggsave(file.path(output_path, "CDR_Traded_Annual.png"), plot1)

  # Visualization 2: Cumulative CDR traded by weight type
  plot2 <- ggplot(data_summary, aes(x = main_technology, y = cdr_traded_cumulative, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Cumulative CDR Traded",
      x = "Technology",
      y = "CDR Traded (MtCO2)",
      fill = "Burden-sharing Type"
    ) +
    custom_theme
  ggsave(file.path(output_path, "CDR_Traded_cumulative.png"), plot2)

  # Visualization 3: Cumulative financial flows
  plot3 <- ggplot(data_summary, aes(x = main_technology, y = financial_flow_weighted_cumulative, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Cumulative Financial Flows",
      x = "Technology",
      y = "Financial Flows (Billion USD)",
      fill = "Burden-sharing Type"
    ) +
    custom_theme
  ggsave(file.path(output_path, "Financial_Flows_Cumulative.png"), plot3)

  # Visualization 4: Annual financial flows
  plot4 <- ggplot(combined_data, aes(x = Year, y = financial_flow_weighted, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Annual Financial Flows",
      x = "Technology",
      y = "Financial Flows (Billion USD)",
      fill = "Burden-sharing Type"
    ) +
    custom_theme
  ggsave(file.path(output_path, "Financial_Flows_annual.png"), plot4)

  # Visualization 5: Job potentials cumulative
  plot5 <- ggplot(data_summary, aes(x = main_technology, y = job_potential_cumulative, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Cumulative Job Potential",
      x = "Technology",
      y = "Job Potential (Million Jobs)",
      fill = "Burden-sharing Type"
    )+
    custom_theme
  ggsave(file.path(output_path, "Job_Potential_Cumulative.png"), plot5)

  # Visualization 6: Job potentials
  plot6 <- ggplot(combined_data, aes(x = Year, y = job_potential, fill = weight_type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(region ~ scenario, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Annual Job Potentials",
      x = "Year",
      y = "Job Potential (Million Jobs)",
      fill = "Burden-sharing Type"
    )+
    custom_theme
  ggsave(file.path(output_path, "Job_Potential_annual.png"), plot6)
}
