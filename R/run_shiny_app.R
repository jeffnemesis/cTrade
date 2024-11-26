#' Run the Shiny App for Fairness CDR Analysis
#'
#' This function launches an interactive Shiny application for exploring
#' Fairness CDR results and performing sensitivity analysis.
#'
#' @param output_path Path to the folder containing the combined dataset CSV.
#' @param regions Vector of regions to include in visualizations. Default is NULL (include all).
#' @export
run_shiny_app <- function(output_path, regions = NULL) {
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(rlang)

  # Load the combined dataset
  combined_file <- file.path(output_path, "Fairness_CDR_all.csv")
  if (!file.exists(combined_file)) {
    stop("The combined dataset file 'Fairness_CDR_all.csv' was not found.")
  }

  combined_data <- read.csv(combined_file)

  # Apply cumulative logic
  combined_data <- combined_data %>%
    group_by(region, scenario, weight_type, main_technology) %>%
    arrange(Year) %>%
    mutate(
      cdr_traded_cumulative = cumsum(ifelse(Year >= 2025, gross_traded_weighted, 0)),
      financial_flow_weighted_cumulative = cumsum(ifelse(Year >= 2025, financial_flow_weighted, 0)),
      job_potential_cumulative = cumsum(job_potential)
    ) %>%
    ungroup()

  # Define UI components
  weight_type_choices <- unique(combined_data$weight_type)
  scenario_choices <- unique(combined_data$scenario)
  region_choices <- unique(combined_data$region)
  year_choices <- c("All", seq(2020, 2100, by = 5))
  result_types <- c(
    "gross_traded_weighted", "cdr_traded_cumulative",
    "financial_flow_weighted", "financial_flow_weighted_cumulative",
    "job_potential", "job_potential_cumulative"
  )

  # UI definition
  ui <- fluidPage(
    titlePanel("Socio-ecomic implications under CDR trading policies (cTrade)"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("visualize_top", "Visualize Top Buyers/Sellers?", choices = c("Yes", "No"), selected = "No"),
        conditionalPanel(
          condition = "input.visualize_top == 'Yes'",
          radioButtons("role", "Select Role",
                       choices = c("Top Buyers" = "Buyer", "Top Sellers" = "Seller")),
          selectInput("main_technology_top", "Select Main Technology",
                      choices = c("All", unique(combined_data$main_technology)), selected = "All"),
          numericInput("top_n", "Number of Top Countries", value = 10, min = 1, max = 20),
          selectInput("year_selection_top", "Select Year",
                      choices = sort(unique(combined_data$Year)), selected = "2025"),
          selectInput("result_type_top", "Select Result Type",
                      choices = c(
                        "cdr_traded_cumulative", "financial_flow_weighted_cumulative",
                        "job_potential", "gross_traded_weighted",
                        "financial_flow_weighted", "job_potential_cumulative"
                      )),
          checkboxGroupInput("scenarios_top", "Select Scenarios",
                             choices = unique(combined_data$scenario),
                             selected = unique(combined_data$scenario))  # Allow multi-selection
        ),
        # Conditional inputs specific to 'No'
        conditionalPanel(
          condition = "input.visualize_top == 'No'",
          checkboxGroupInput("weight_types", "Select Weight Types",
                             choices = weight_type_choices,
                             selected = weight_type_choices),
          selectizeInput("scenario", "Select Scenarios",
                         choices = scenario_choices,
                         selected = scenario_choices, multiple = TRUE),
          selectInput("year_selection", "Select Year(s)",
                      choices = year_choices, selected = "All"),
          selectizeInput("regions", "Select Regions",
                         choices = region_choices,
                         selected = regions, multiple = TRUE),
          radioButtons("type", "Select Plot Type",
                       choices = c(
                         "Cumulative CDR trade" = "cdr_traded_cumulative",
                         "Annual CDR trade" = "gross_traded_weighted",
                         "Cumulative financial transfer" = "financial_flow_weighted_cumulative",
                         "Annual financial transfer" = "financial_flow_weighted",
                         "Cumulative job potential" = "job_potential_cumulative",
                         "Annual job potential" = "job_potential"
                       ),
                       selected = "cdr_traded_cumulative"),
          actionButton("apply_filters", "Apply Filters")
        )
      ),
      mainPanel(
        plotOutput("result_plot"),
        textOutput("validation_message")
      )
    )
  )

  # Server definition
  server <- function(input, output, session) {
    # Reactive function to filter data
    filtered_data <- reactive({
      data <- combined_data

      # Apply filters
      if (!"All" %in% input$year_selection) {
        data <- data %>% filter(Year %in% as.numeric(input$year_selection))
      }

      if (!"All" %in% input$regions) {
        data <- data %>% filter(region %in% input$regions)
      }

      data <- data %>%
        filter(
          weight_type %in% input$weight_types,
          scenario %in% input$scenario
        )

      return(data)
    })

    # Reactive function for top countries
    top_countries <- reactive({
      if (input$visualize_top == "No") return(NULL)

      role_data <- if (input$role == "Buyer") {
        combined_data %>% filter(trading_role == "Buyer")
      } else {
        combined_data %>% filter(trading_role == "Seller")
      }

      if (input$main_technology_top != "All") {
        role_data <- role_data %>% filter(main_technology == input$main_technology_top)
      }

      if (grepl("cumulative", input$result_type_top)) {
        role_data <- role_data %>% filter(Year <= as.numeric(input$year_selection_top))
      } else {
        role_data <- role_data %>% filter(Year == as.numeric(input$year_selection_top))
      }

      role_data <- role_data %>% filter(scenario %in% input$scenarios_top)

      top_data <- role_data %>%
        group_by(region, weight_type, scenario) %>%
        summarize(total_value = sum(!!sym(input$result_type_top), na.rm = TRUE)) %>%
        arrange(desc(total_value)) %>%
        slice_head(n = input$top_n)

      return(top_data)
    })

    # Render the plot dynamically
    output$result_plot <- renderPlot({
      if (input$visualize_top == "Yes") {
        # Logic for Top N visualization
        data <- top_countries()

        if (is.null(data) || nrow(data) == 0) {
          validate(need(FALSE, "No data available for the selected filters."))
        }

        # Define a mapping of result types to units
        y_axis_units <- list(
          "cdr_traded_cumulative" = "Cumulative traded CDR (MtCO2)",
          "financial_flow_weighted_cumulative" = "Cumulative financial transfer ($ Billion)",
          "job_potential_cumulative" = "Cumulative job potential (Million Jobs)",
          "gross_traded_weighted" = "Traded CDR (MtCO2)",
          "financial_flow_weighted" = "Financial transfer ($ Billion)",
          "job_potential" = "Job potential (Million Jobs)"
        )

        # Dynamically set the y-axis label based on the result type
        y_label <- y_axis_units[[input$result_type_top]]

        ggplot(data, aes(
          x = reorder(region, total_value),
          y = total_value,
          fill = scenario
        )) +
          geom_bar(stat = "identity", position = "stack") +
          coord_flip() +
          facet_grid(~weight_type, scales = "free_y") +
          labs(
            title = paste("Top", input$top_n, input$role, "Major countries and regions"),
            x = "Region",
            y = y_label,  # Dynamically set the y-axis label
            fill = "Scenario"
          ) +
          theme_minimal()
      } else if (input$visualize_top == "No") {
        # New logic for other visualizations
        # Add the validation here
        if (!is.null(input$result_type) && input$result_type %in% c("gross_traded_weighted", "financial_flow_weighted", "job_potential")) {
          if (!is.null(input$weight_types) && length(input$weight_types) > 1) {
            validate(
              need(FALSE, "Please select only one Weight Type for the selected result type.")
            )
          }
        } else if (!is.null(input$result_type) && input$result_type %in% c("cdr_traded_cumulative", "financial_flow_weighted_cumulative", "job_potential_cumulative")) {
          if (is.null(input$weight_types) || length(input$weight_types) < 1) {
            validate(
              need(FALSE, "Please select at least one Weight Type for the selected cumulative result type.")
            )
          }
        }

        data <- filtered_data()

        if (is.null(data) || nrow(data) == 0) {
          validate(need(FALSE, "No data available for the selected filters."))
        }

        # Conditional plots for different result types
        if (input$type == "gross_traded_weighted") {
          ggplot(data, aes(x = Year, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = "stack", width = 3) +
            facet_grid(region ~ scenario, scales = "free") +
            labs(
              title = "Annual CDR Trade",
              x = "Year",
              y = "MtCO2"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$type == "cdr_traded_cumulative") {
          ggplot(data, aes(x = region, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.8) +
            facet_grid(weight_type ~ scenario, scales = "free") +
            labs(
              title = "Cumulative CDR Trade",
              x = "Region",
              y = "MtCO2"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$type == "financial_flow_weighted") {
          ggplot(data, aes(x = Year, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = "stack", width = 3) +
            facet_grid(region ~ scenario, scales = "free") +
            labs(
              title = "Annual Financial Transfers",
              x = "Year",
              y = "$ Billion"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$type == "financial_flow_weighted_cumulative") {
          ggplot(data, aes(x = region, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.8) +
            facet_grid(weight_type ~ scenario, scales = "free") +
            labs(
              title = "Cumulative Financial Transfers",
              x = "Region",
              y = "$ Billion"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$type == "job_potential") {
          ggplot(data, aes(x = Year, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = "stack", width = 3) +
            facet_grid(region ~ scenario, scales = "free") +
            labs(
              title = "Annual Job Potential",
              x = "Year",
              y = "Million Jobs"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$type == "job_potential_cumulative") {
          ggplot(data, aes(x = region, y = .data[[input$type]], fill = main_technology)) +
            geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.8) +
            facet_grid(weight_type ~ scenario, scales = "free") +
            labs(
              title = "Cumulative Job Potential",
              x = "Region",
              y = "Million Jobs"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90))
        }
      }  # Close if for "No" condition
    })  # Close renderPlot
  }  # Close server

  shinyApp(ui = ui, server = server)
}  # Close run_shiny_app function
