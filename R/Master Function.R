# Date: 07/27/2026
# Objective: Create Reproducible Disease Dashboard Pages
# Project: Brazos Nexus


# Packages ----

library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)
library(sf)
library(leaflet)


# Brazos Nexus Colors ----

nexus_colors <- list(
  black = "#000000",
  white = "#FEFEFE",
  beige = "#998467",
  gray = "#737373",
  slate = "#213145",
  navy = "#072D5E",
  blue = "#155A8D",
  light_blue = "#4198D4",
  background = "#ECEFE7"
)


# Brazos Nexus ggplot Theme ----

theme_nexus <- function() {
  
  theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(
        color = nexus_colors$navy,
        face = "bold",
        size = 14
      ),
      
      plot.subtitle = element_text(
        color = nexus_colors$slate
      ),
      
      axis.title = element_text(
        color = nexus_colors$slate,
        face = "bold"
      ),
      
      axis.text = element_text(
        color = nexus_colors$slate
      ),
      
      panel.grid.major = element_line(
        color = nexus_colors$background,
        linewidth = 0.5
      ),
      
      panel.grid.minor = element_blank(),
      
      plot.background = element_rect(
        fill = "transparent",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "transparent",
        color = NA
      ),
      
      legend.background = element_rect(
        fill = "transparent",
        color = NA
      ),
      
      legend.key = element_rect(
        fill = "transparent",
        color = NA
      )
    )
}


# Reusable Leaflet Map Function ----

build_disease_map <- function(
    map_data,
    disease_name,
    case_variable = "cases",
    geography_variable = "GEOID",
    geography_label = "Area"
) {
  
  # Confirm map data is spatial
  if (!inherits(map_data, "sf")) {
    stop("The map_data object must be an sf spatial object.")
  }
  
  # Check required variables
  required_variables <- c(
    case_variable,
    geography_variable
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(map_data)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      paste(
        "The map data is missing required variables:",
        paste(missing_variables, collapse = ", ")
      )
    )
  }
  
  # Convert case counts to numeric
  map_data <- map_data |>
    mutate(
      map_cases = as.numeric(
        .data[[case_variable]]
      ),
      
      popup_text = paste0(
        "<strong>",
        geography_label,
        ":</strong> ",
        .data[[geography_variable]],
        "<br>",
        "<strong>Cases:</strong> ",
        scales::comma(map_cases)
      ),
      
      label_text = paste0(
        geography_label,
        " ",
        .data[[geography_variable]],
        ": ",
        scales::comma(map_cases),
        " cases"
      )
    )
  
  # Create color palette
  pal <- leaflet::colorNumeric(
    palette = c(
      nexus_colors$background,
      nexus_colors$light_blue,
      nexus_colors$blue,
      nexus_colors$navy
    ),
    domain = map_data$map_cases,
    na.color = nexus_colors$gray
  )
  
  # Build map
  leaflet::leaflet(map_data) |>
    
    leaflet::addProviderTiles(
      provider = leaflet::providers$CartoDB.Positron
    ) |>
    
    leaflet::addPolygons(
      fillColor = ~pal(map_cases),
      fillOpacity = 0.75,
      color = nexus_colors$white,
      weight = 1,
      opacity = 1,
      popup = ~popup_text,
      label = ~label_text,
      
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        color = nexus_colors$beige,
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    
    leaflet::addLegend(
      pal = pal,
      values = map_data$map_cases,
      title = paste(disease_name, "Cases"),
      opacity = 0.8,
      position = "bottomright",
      labFormat = leaflet::labelFormat(
        big.mark = ","
      )
    )
}


# Master Disease Report Function ----

build_disease_report <- function(
    data,
    disease_name,
    map_shapes = NULL,
    case_geography_variable = "GEOID",
    shape_geography_variable = "GEOID",
    geography_label = "Area"
) {
  
  # Required case variables
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
    filter(
      stringr::str_to_lower(
        stringr::str_trim(.data$disease)
      ) ==
        stringr::str_to_lower(
          stringr::str_trim(disease_name)
        )
    )
  
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
        breaks = c(
          -Inf,
          4,
          17,
          24,
          44,
          64,
          Inf
        ),
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
  
  
  # Overall Summary ----
  
  total_cases <- nrow(df)
  
  first_report_date <- min(
    df$report_date,
    na.rm = TRUE
  )
  
  last_report_date <- max(
    df$report_date,
    na.rm = TRUE
  )
  
  
  # Age Summary ----
  
  age_summary <- df |>
    count(
      age_group,
      name = "cases",
      .drop = FALSE
    ) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  
  # Sex Summary ----
  
  sex_summary <- df |>
    mutate(
      sex = fct_explicit_na(
        as.factor(sex),
        na_level = "Unknown"
      )
    ) |>
    count(
      sex,
      name = "cases",
      .drop = FALSE
    ) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  
  # Race Summary ----
  
  race_summary <- df |>
    mutate(
      race = fct_explicit_na(
        as.factor(race),
        na_level = "Unknown"
      )
    ) |>
    count(
      race,
      name = "cases",
      .drop = FALSE
    ) |>
    mutate(
      percent = cases / sum(cases) * 100
    )
  
  
  # Weekly Summary ----
  
  valid_weeks <- df |>
    filter(
      !is.na(report_week)
    )
  
  if (nrow(valid_weeks) > 0) {
    
    weekly_summary <- valid_weeks |>
      count(
        report_week,
        name = "cases"
      ) |>
      complete(
        report_week = seq(
          min(report_week),
          max(report_week),
          by = "week"
        ),
        fill = list(
          cases = 0
        )
      )
    
  } else {
    
    weekly_summary <- tibble(
      report_week = as.Date(character()),
      cases = integer()
    )
  }
  
  
  # Monthly Summary ----
  
  valid_months <- df |>
    filter(
      !is.na(report_month)
    )
  
  if (nrow(valid_months) > 0) {
    
    monthly_summary <- valid_months |>
      count(
        report_month,
        name = "cases"
      ) |>
      complete(
        report_month = seq(
          min(report_month),
          max(report_month),
          by = "month"
        ),
        fill = list(
          cases = 0
        )
      )
    
  } else {
    
    monthly_summary <- tibble(
      report_month = as.Date(character()),
      cases = integer()
    )
  }
  
  
  # Demographic Plots ----
  
  age_plot <- ggplot(
    age_summary,
    aes(
      x = age_group,
      y = cases
    )
  ) +
    geom_col(
      fill = nexus_colors$blue,
      width = 0.75
    ) +
    labs(
      x = "Age group",
      y = "Cases"
    ) +
    theme_nexus()
  
  
  sex_plot <- ggplot(
    sex_summary,
    aes(
      x = fct_reorder(
        sex,
        cases
      ),
      y = cases
    )
  ) +
    geom_col(
      fill = nexus_colors$blue,
      width = 0.75
    ) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Cases"
    ) +
    theme_nexus()
  
  
  race_plot <- ggplot(
    race_summary,
    aes(
      x = fct_reorder(
        race,
        cases
      ),
      y = cases
    )
  ) +
    geom_col(
      fill = nexus_colors$blue,
      width = 0.75
    ) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Cases"
    ) +
    theme_nexus()
  
  
  # Time Trend Plots ----
  
  weekly_plot <- ggplot(
    weekly_summary,
    aes(
      x = report_week,
      y = cases
    )
  ) +
    geom_col(
      fill = nexus_colors$light_blue,
      width = 6
    ) +
    scale_x_date(
      date_labels = "%b %d",
      date_breaks = "1 month"
    ) +
    labs(
      x = "Week of report",
      y = "Cases"
    ) +
    theme_nexus() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  
  monthly_plot <- ggplot(
    monthly_summary,
    aes(
      x = report_month,
      y = cases
    )
  ) +
    geom_line(
      color = nexus_colors$navy,
      linewidth = 1
    ) +
    geom_point(
      color = nexus_colors$beige,
      size = 2.5
    ) +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "1 month"
    ) +
    labs(
      x = "Month of report",
      y = "Cases"
    ) +
    theme_nexus() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  
  # Geographic Summary and Map ----
  
  map_summary <- NULL
  map_data <- NULL
  disease_map <- NULL
  
  if (!is.null(map_shapes)) {
    
    # Check case geography variable
    if (
      !case_geography_variable %in% names(df)
    ) {
      stop(
        paste(
          "The case dataset is missing the geography variable:",
          case_geography_variable
        )
      )
    }
    
    # Check shape geography variable
    if (
      !shape_geography_variable %in% names(map_shapes)
    ) {
      stop(
        paste(
          "The map shapes are missing the geography variable:",
          shape_geography_variable
        )
      )
    }
    
    if (!inherits(map_shapes, "sf")) {
      stop(
        "The map_shapes object must be an sf spatial object."
      )
    }
    
    # Count disease cases by geography
    map_summary <- df |>
      filter(
        !is.na(
          .data[[case_geography_variable]]
        )
      ) |>
      mutate(
        join_geography = as.character(
          .data[[case_geography_variable]]
        )
      ) |>
      count(
        join_geography,
        name = "cases"
      )
    
    # Prepare polygon identifier
    map_shapes_prepared <- map_shapes |>
      mutate(
        join_geography = as.character(
          .data[[shape_geography_variable]]
        )
      )
    
    # Join counts to polygons
    map_data <- map_shapes_prepared |>
      left_join(
        map_summary,
        by = "join_geography"
      ) |>
      mutate(
        cases = replace_na(
          cases,
          0L
        )
      )
    
    # Build Leaflet map
    disease_map <- build_disease_map(
      map_data = map_data,
      disease_name = disease_name,
      case_variable = "cases",
      geography_variable = "join_geography",
      geography_label = geography_label
    )
  }
  
  
  # Return Complete Report Object ----
  
  list(
    disease = disease_name,
    
    data = df,
    
    total_cases = total_cases,
    
    first_report_date = first_report_date,
    
    last_report_date = last_report_date,
    
    age_summary = age_summary,
    
    sex_summary = sex_summary,
    
    race_summary = race_summary,
    
    weekly_summary = weekly_summary,
    
    monthly_summary = monthly_summary,
    
    map_summary = map_summary,
    
    map_data = map_data,
    
    age_plot = age_plot,
    
    sex_plot = sex_plot,
    
    race_plot = race_plot,
    
    weekly_plot = weekly_plot,
    
    monthly_plot = monthly_plot,
    
    disease_map = disease_map
  )
}


# Disease Page Function ----

build_disease_page <- function(
    data,
    disease_name,
    map_shapes = NULL,
    case_geography_variable = "GEOID",
    shape_geography_variable = "GEOID",
    geography_label = "Area"
) {
  
  report <- build_disease_report(
    data = data,
    disease_name = disease_name,
    map_shapes = map_shapes,
    case_geography_variable = case_geography_variable,
    shape_geography_variable = shape_geography_variable,
    geography_label = geography_label
  )
  
  list(
    title = paste(
      disease_name,
      "Surveillance"
    ),
    
    disease = disease_name,
    
    total_cases = report$total_cases,
    
    first_report_date = report$first_report_date,
    
    last_report_date = report$last_report_date,
    
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
    
    geography = list(
      summary = report$map_summary,
      data = report$map_data,
      map = report$disease_map
    ),
    
    case_data = report$data
  )
}