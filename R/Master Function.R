# Date: 07/27/2026
# Objective: Create Master Function Package for Website Pages
# Project: Brazos Nexus

# Packages ----
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)

# Master disease-report function ----
build_disease_report <- function(data, disease_name) {
  
  # Check required variables
  required_variables <- c(
    "disease",
    "report_date",
    "age",
    "sex",
    "race"
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(data)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      paste(
        "The dataset is missing required variables:",
        paste(missing_variables, collapse = ", ")
      )
    )
  }
  
  # Filter to selected disease
  df <- data |>
    filter(.data$disease == disease_name)
  
  if (nrow(df) == 0) {
    stop(
      paste(
        "No records found for:",
        disease_name
      )
    )
  }
  
  # Create standardized variables
  df <- df |>
    mutate(
      report_date = as.Date(report_date),
      
      age_group = cut(
        age,
        breaks = c(-Inf, 4, 17, 24, 44, 64, Inf),
        labels = c(
          "0–4",
          "5–17",
          "18–24",
          "25–44",
          "45–64",
          "65+"
        )
      ),
      
      report_month = floor_date(
        report_date,
        unit = "month"
      ),
      
      report_week = floor_date(
        report_date,
        unit = "week",
        week_start = 1
      )
    )
  
  # Summary counts ----
  
  total_cases <- nrow(df)
  
  age_summary <- df |>
    count(age_group, name = "cases", .drop = FALSE) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  sex_summary <- df |>
    mutate(
      sex = fct_explicit_na(
        as.factor(sex),
        na_level = "Unknown"
      )
    ) |>
    count(sex, name = "cases", .drop = FALSE) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  race_summary <- df |>
    mutate(
      race = fct_explicit_na(
        as.factor(race),
        na_level = "Unknown"
      )
    ) |>
    count(race, name = "cases", .drop = FALSE) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  # Weekly time trend
  valid_weeks <- df |>
    filter(!is.na(report_week))
  
  if (nrow(valid_weeks) > 0) {
    weekly_summary <- valid_weeks |>
      count(report_week, name = "cases") |>
      complete(
        report_week = seq(
          min(report_week),
          max(report_week),
          by = "week"
        ),
        fill = list(cases = 0)
      )
  } else {
    weekly_summary <- tibble(
      report_week = as.Date(character()),
      cases = integer()
    )
  }
  
  # Monthly time trend
  valid_months <- df |>
    filter(!is.na(report_month))
  
  if (nrow(valid_months) > 0) {
    monthly_summary <- valid_months |>
      count(report_month, name = "cases") |>
      complete(
        report_month = seq(
          min(report_month),
          max(report_month),
          by = "month"
        ),
        fill = list(cases = 0)
      )
  } else {
    monthly_summary <- tibble(
      report_month = as.Date(character()),
      cases = integer()
    )
  }
  
  # Plots ----
  
  age_plot <- ggplot(
    age_summary,
    aes(x = age_group, y = cases)
  ) +
    geom_col() +
    labs(
      title = paste("Cases by Age Group:", disease_name),
      x = "Age group",
      y = "Cases"
    ) +
    theme_minimal()
  
  sex_plot <- ggplot(
    sex_summary,
    aes(
      x = fct_reorder(sex, cases),
      y = cases
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Cases by Sex:", disease_name),
      x = NULL,
      y = "Cases"
    ) +
    theme_minimal()
  
  race_plot <- ggplot(
    race_summary,
    aes(
      x = fct_reorder(race, cases),
      y = cases
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste(
        "Cases by Race and Ethnicity:",
        disease_name
      ),
      x = NULL,
      y = "Cases"
    ) +
    theme_minimal()
  
  weekly_plot <- ggplot(
    weekly_summary,
    aes(x = report_week, y = cases)
  ) +
    geom_col() +
    labs(
      title = paste(
        "Weekly Cases by Date of Report:",
        disease_name
      ),
      x = "Week of report",
      y = "Cases"
    ) +
    theme_minimal()
  
  monthly_plot <- ggplot(
    monthly_summary,
    aes(x = report_month, y = cases)
  ) +
    geom_line() +
    geom_point() +
    labs(
      title = paste(
        "Monthly Cases by Date of Report:",
        disease_name
      ),
      x = "Month of report",
      y = "Cases"
    ) +
    theme_minimal()
  
  # Return named report object ----
  list(
    disease = disease_name,
    data = df,
    total_cases = total_cases,
    age_summary = age_summary,
    sex_summary = sex_summary,
    race_summary = race_summary,
    weekly_summary = weekly_summary,
    monthly_summary = monthly_summary,
    age_plot = age_plot,
    sex_plot = sex_plot,
    race_plot = race_plot,
    weekly_plot = weekly_plot,
    monthly_plot = monthly_plot
  )
}

# Disease Page ----
build_disease_page <- function(data, disease_name) {
  
  report <- build_disease_report(
    data = data,
    disease_name = disease_name
  )
  
  list(
    title = paste(disease_name, "Surveillance"),
    disease = disease_name,
    total_cases = report$total_cases,
    
    demographics = list(
      age_summary = report$age_summary,
      sex_summary = report$sex_summary,
      race_summary = report$race_summary,
      age_plot = report$age_plot,
      sex_plot = report$sex_plot,
      race_plot = report$race_plot
    ),
    
    time_trends = list(
      weekly_summary = report$weekly_summary,
      monthly_summary = report$monthly_summary,
      weekly_plot = report$weekly_plot,
      monthly_plot = report$monthly_plot
    ),
    
    case_data = report$data
  )
}
