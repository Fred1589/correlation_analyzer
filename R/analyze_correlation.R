library(readr)
library(stats)
library(base)

# Exercise Sheet 6
# Authors: Frederik Borutta (10345490), Felix Borutta (10049639)

msci_data_return <- read_csv("./data/MSCI_World_Return.csv")

msci_data_evolution_unprepared <- read_csv("./data/MSCI_World_Evolution_in EUR.csv")
msci_data_evolution <- msci_data_evolution_unprepared[grepl("^01\\.", msci_data_evolution_unprepared$Year), ]
msci_data_evolution$Year <- sub("^01\\.", "", msci_data_evolution$Year)

#' Analyze Correlation with MSCI World Returns
#'
#' This function checks whether the input data has a 'Year' column, merges it with MSCI World
#' Index return data, and calculates correlation and p-values for all numeric variables
#' with respect to MSCI returns.
#'
#' @param input_data A data.frame containing at least a column named 'Year' and other numeric columns.
#'
#' @return A data.frame with variables, p-values, significance ('highly significant', 'very significant', 'significant', 'not significant'), r-values and correlation strength ('high', 'medium', 'low').
#'
#' @examples
#' test_data <- data.frame(
#'   Year = 2000:2020,
#'   Inflation = runif(21, 0, 0.1),
#'   GDP = runif(21, 1, 3)
#' )
#'
#' result <- analyze_correlation_to_msci_world_returns(test_data)
#'
#' @export
analyze_correlation_to_msci_world_returns <- function(input_data) {
  return(analyze_correlation(msci_data_return, input_data))
}

#' Analyze Correlation with MSCI World Value Evolution
#'
#' This function checks whether the input data has a 'Year' column, merges it with MSCI World
#' Index Value Evolution data, and calculates correlation and p-values for all numeric variables
#' with respect to MSCI returns.
#'
#' @param input_data A data.frame containing at least a column named 'Year' and other numeric columns.
#'
#' @return A data.frame with variables, p-values, significance ('highly significant', 'very significant', 'significant', 'not significant'), r-values and correlation strength ('high', 'medium', 'low').
#'
#' @examples
#' test_data <- data.frame(
#'   Year = 2000:2020,
#'   Inflation = runif(21, 0, 0.1),
#'   GDP = runif(21, 1, 3)
#' )
#'
#' result <- analyze_correlation_to_msci_world_value_evolution(test_data)
#'
#' @export
analyze_correlation_to_msci_world_value_evolution <- function(input_data) {
  return(analyze_correlation(msci_data_evolution, input_data))
}

analyze_correlation <- function(reference_data, input_data) {
  # Check if input data contains column named year
  if (!"Year" %in% names(input_data)) {
    stop("No Column named Year found")
  }

  # Join datasets on 'Year' column
  merged_data <- merge(reference_data, input_data, by = "Year")
  vars_to_check <- setdiff(names(merged_data), c("Year", "Return"))

  results <- data.frame(
    Variable = character(),
    P_Value = numeric(),
    R_Value = numeric(),
    Correlation_Strength = character(),
    stringsAsFactors = FALSE
  )

  for (var in vars_to_check) {
    if (is.numeric(merged_data[[var]])) {
      cor_test <- cor.test(merged_data[[var]], merged_data$Return)
      p_val <- cor_test$p.value
      r_val <- cor_test$estimate

      # Evaluation of correlation
      strength <- if (abs(r_val) > 0.7) {
        "high"
      } else if (abs(r_val) > 0.4) {
        "medium"
      } else {
        "low"
      }

      # Evaluation of significance
      significance <- if (p_val < 0.001) {
        "highly significant"
      } else if (p_val < 0.01) {
        "very significant"
      } else if (p_val < 0.05) {
        "significant"
      } else {
        "not significant"
      }

      results <- rbind(results, data.frame(
        Variable = var,
        P_Value = round(p_val, 4),
        Significance_Strength = significance,
        R_Value = round(r_val, 4),
        Correlation_Strength = strength,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(results)
}

