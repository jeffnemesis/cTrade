#' Allocate Fairness CDR with Financial Flows
#'
#' This function calculates fairness-based Carbon Dioxide Removal (CDR) allocations
#' by region, applies trading rules for Global North and Global South, calculates financial
#' flows based on a CO2 price pathway, and includes customizable CO2 price options.
#'
#' @param db_path Path to the GCAM database directory.
#' @param db_name Name of the GCAM database.
#' @param dat_file Name of the .dat file to query data (if applicable).
#' @param scenario_list A vector of GCAM scenarios to include.
#' @param region_list A vector of regions to include.
#' @param output_path Path to save output files.
#' @param year Year to filter the data for.
#' @param conversion_factor Conversion factor from MtC to MtCO2. Default is 3.67.
#' @param selling_mechanism Choose one of "equal_opportunity", "weighted_by_surplus", or NULL (default: NULL).
#' @param co2_price_pathway Optional user-specified CO2 price pathway. Default: NULL (uses default pathway).
#' @param base_co2_price Initial CO2 price in 2025 if user specifies custom pathway.
#' @param co2_growth_rate Annual growth rate (0.03 or 0.05) for custom pathway.
#' @param output_type Output format (e.g., "csv"). Default is "csv".
#' @param save_data Logical; whether to save output data. Default is TRUE.
#' @return A list with allocation results, selling results (both mechanisms), and financial flows.
#' @export
allocate_fairness_cdr <- function(
    db_path,
    db_name,
    dat_file = NULL,
    scenario_list,
    region_list = NULL,
    output_path,
    year = NULL,
    conversion_factor = 3.67,
    co2_price_pathway = NULL,
    base_co2_price = NULL,
    co2_growth_rate = NULL,
    output_type = "csv",
    save_data = TRUE
) {
  # Load necessary libraries
  library(dplyr)
  library(tidyr)

  # Define Global North and Global South
  global_north <- c(
    "Australia_NZ", "Canada", "EU-12", "EU-15", "Europe_Eastern",
    "Europe_Non_EU", "European Free Trade Association", "Japan",
    "Russia", "South Korea", "USA"
  )
  global_south <- c(
    "Africa_Eastern", "Africa_Northern", "Africa_Southern", "Africa_Western",
    "Argentina", "Brazil", "Southeast Asia", "Taiwan", "South Africa",
    "South America_Northern", "South America_Southern", "South Asia",
    "Mexico", "Middle East", "Pakistan", "India", "Indonesia",
    "Central America and Caribbean", "Central Asia", "China", "Colombia"
  )

  # Define Global North and Global South
  all_regions <- c(
    "Australia_NZ", "Canada", "EU-12", "EU-15", "Europe_Eastern",
    "Europe_Non_EU", "European Free Trade Association", "Japan",
    "Russia", "South Korea", "USA", "Africa_Eastern", "Africa_Northern", "Africa_Southern", "Africa_Western",
    "Argentina", "Brazil", "Southeast Asia", "Taiwan", "South Africa",
    "South America_Northern", "South America_Southern", "South Asia",
    "Mexico", "Middle East", "Pakistan", "India", "Indonesia",
    "Central America and Caribbean", "Central Asia", "China", "Colombia"
  )

  # Default CO2 Price Pathway
  default_co2_prices <- data.frame(
    year = seq(2025, 2100, by = 5),
    price = seq(55, 900, length.out = 16)
  )

  # Generate CO2 Price Pathway (Custom or Default)
  if (!is.null(co2_price_pathway)) {
    # Custom pathway is provided, use it directly
    custom_co2_prices <- generate_fixed_tax_values(
      base_year = 2025,
      base_value = base_co2_price,
      growth_rate = co2_growth_rate,
      intervals = 5,
      end_year = 2100
    )
    co2_price_pathway <- data.frame(year = custom_co2_prices$year, price = custom_co2_prices$price)
  } else {
    # Use the default pathway
    base_price <- ifelse(is.null(base_co2_price), 55, base_co2_price)  # Default to 55 if not provided
    default_co2_prices <- data.frame(
      year = seq(2025, 2100, by = 5),
      price = seq(base_price, 900, length.out = 16)  # Adjust the base price dynamically
    )
    co2_price_pathway <- default_co2_prices
  }

  # Print for Debugging
  print("CO2 Price Pathway:")
  print(co2_price_pathway)


  # Connect to GCAM database
  conn <- localDBConn(db_path, db_name)
  query_file <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0"?>
<queries>
  <aQuery>
    <all-regions/>
    <emissionsQueryBuilder title="CO2 sequestration by tech">
      <axis1 name="subsector">subsector</axis1>
      <axis2 name="Year">emissions-sequestered</axis2>
      <xPath buildList="true" dataName="emissions" group="false" sumAll="false">
        *[@type = "sector"]/*[@type="subsector"]/*[@type="technology"]//CO2/emissions-sequestered/node()
      </xPath>
    </emissionsQueryBuilder>
  </aQuery>
</queries>', query_file)

  CDR_Output <- getQuery(
    addScenario(conn, paste0(dat_file, ".dat"), scenario_list, query_file, clobber = TRUE),
    "CO2 sequestration by tech"
  )%>%filter(scenario %in% scenario_list)

  print("Query Results:")
  print(head(CDR_Output))

  # Ensure the column 'Year' exists in the query results
  if (!"Year" %in% colnames(CDR_Output)) {
    if ("year" %in% colnames(CDR_Output)) {
      CDR_Output <- CDR_Output %>% rename(Year = year)
    } else {
      stop("The query results do not contain a 'Year' column. Check your GCAM database and query file.")
    }
  }

  # Filter by year if specified
  if (!is.null(year)) {
    CDR_Output <- CDR_Output %>% filter(Year == year)
  }

  # Exclude years before 2025
  CDR_Output <- CDR_Output %>% filter(Year >= 2025)

  # Filter and group relevant technologies
  CDR_Output <- CDR_Output %>%filter(scenario %in% scenario_list)%>%
    mutate(main_technology = case_when(
      subsector %in% c("biomass liquids", "biomass (IGCC CCS)", "biomass (conv CCS)", "biomass") ~ "BECCS",
      subsector == "rock weathering" ~ "ERW",
      subsector == "dac" ~ "DAC",
      TRUE ~ "Other"
    )) %>%
    filter(main_technology != "Other") %>%
    group_by(region, Year, scenario, main_technology) %>%
    summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

  print("Filtered and Grouped Query Results (Main Technology):")
  print(head(CDR_Output))

  # Add missing regions and main_technologies
  all_combinations <- expand.grid(
    region = all_regions,
    Year = unique(CDR_Output$Year),
    scenario = scenario_list,
    main_technology = unique(CDR_Output$main_technology)
  )

  CDR_Output <- all_combinations %>%
    left_join(CDR_Output, by = c("region", "Year", "scenario", "main_technology")) %>%
    mutate(value = ifelse(is.na(value), 0, value))

  print("CDR Output with Missing Data Added:")
  print(head(CDR_Output))

  # Function to retrieve weights based on type
  get_weights <- function(weight_type, year) {
    if (weight_type == "responsibility") {
      return(get_responsibility_weights(year))
    } else if (weight_type == "population") {
      return(get_population_weights(year))
    } else if (weight_type == "GDP") {
      return(get_GDP_weights(year))
    } else {
      stop("Invalid weight type specified.")
    }
  }

  years <- unique(CDR_Output$Year)
  all_results_by_weight <- list()

  for (weight_type in c("responsibility", "population", "GDP")) {
    all_results <- list()

    for (yr in years) {
      year_data <- CDR_Output %>% filter(Year == yr)
      year_weights <- get_weights(weight_type, yr)

      # Add missing regions with 0 CDR but include fairness
      missing_regions <- setdiff(names(year_weights), year_data$region)
      if (length(missing_regions) > 0) {
        zero_data <- data.frame(
          region = missing_regions,
          Year = yr,
          scenario = scenario_list[1],
          main_technology = "BECCS",
          value = 0
        )
        year_data <- bind_rows(year_data, zero_data)
      }

      year_data <- year_data %>%
        mutate(weight = year_weights[match(region, names(year_weights))])

      fairness_allocation <- year_data %>%
        group_by(scenario, Year, main_technology) %>%
        mutate(fairness_cdr = sum(value * conversion_factor, na.rm = TRUE) * weight) %>%
        ungroup()

      allocation <- fairness_allocation %>%
        mutate(
          total_cdr_MtCO2 = value * conversion_factor,
          net_cdr = total_cdr_MtCO2 - fairness_cdr,
          surplus = ifelse(net_cdr > 0, net_cdr, 0),
          deficit = ifelse(net_cdr < 0, -net_cdr, 0),
          trading_role = case_when(
            region %in% global_north & deficit > 0 ~ "Buyer",
            region %in% global_south & surplus > 0 ~ "Seller",
            TRUE ~ "Non-Participant"
          )
        )

      north_deficit_total <- allocation %>%
        filter(trading_role == "Buyer") %>%
        summarize(total_deficit = sum(deficit, na.rm = TRUE)) %>%
        pull(total_deficit)

      weighted_surplus <- allocation %>%
        filter(trading_role == "Seller") %>%
        mutate(gross_traded = surplus / sum(surplus, na.rm = TRUE) * north_deficit_total)

      allocation <- allocation %>%
        mutate(
          gross_traded_weighted = ifelse(region %in% global_north, -deficit,
                                         ifelse(region %in% global_south & trading_role == "Seller",
                                                weighted_surplus$gross_traded, 0)),
          co2_price = co2_price_pathway$price[co2_price_pathway$year == yr],
          financial_flow_weighted = gross_traded_weighted * co2_price * 10^6
        )


      # Write results to CSV for each weight type
      write.csv(allocation, file = file.path(output_path, paste0("Fairness_CDR_", weight_type, ".csv")), row.names = FALSE)



      # Define job intensities for each main technology
      job_intensity_df <- data.frame(
        main_technology = c("BECCS", "DAC", "ERW", "Biochar"),
        job_intensity = c(0.003, 0.003, 0.002, 0.02)
      )

      allocation <- allocation %>%
        left_join(job_intensity_df, by = "main_technology") %>%
        mutate(
          co2_price = co2_price_pathway$price[co2_price_pathway$year == yr],
          job_potential = gross_traded_weighted * 10^6 * job_intensity
        )


      all_results[[as.character(yr)]] <- allocation
    }

    combined_results <- bind_rows(all_results, .id = "Year")

    if (save_data) {
      write.csv(combined_results, file = file.path(output_path, paste0("Fairness_CDR_", weight_type, ".csv")), row.names = FALSE)
    }

    all_results_by_weight[[weight_type]] <- combined_results
  }

  return(list(
    allocation_results = all_results_by_weight
  ))
}
